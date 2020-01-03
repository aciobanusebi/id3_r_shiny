myLog2 <- function(x) {
  result <- log2(x)
  result[is.infinite(result)] <- 0
  result
}

entropy <- function(counts) {
  if(sum(counts) == 0) {
    return(0)
  }
  probs <- counts/sum(counts)
  - probs %*% myLog2(probs)
}

avgEntropy <- function(nodeCounts,kidsCounts) {
  result <- 0
  for(i in 1:nrow(kidsCounts)) {
    result <- result + entropy(kidsCounts[i,]) * sum(kidsCounts[i,]) / sum(nodeCounts)
  }
  result
}

IG <- function(nodeCounts,kidsCounts) {
  entropy(nodeCounts) - avgEntropy(nodeCounts,kidsCounts)
}



readData <- function(fileName) {
  data <- read.table(fileName,header=FALSE,sep="",
                     stringsAsFactors = FALSE)
  types <- as.character(data[1,])
  colnames(data) <- data[2,]
  data <- data[c(-1,-2),]
  for(i in 1:length(types)) {
    if(types[i] == "d") {
      data[,i] <- factor(data[,i])
    } else if(types[i] == "c") {
      data[,i] <- as.numeric(data[,i])
    } else {
      stop("ERROR: type must be either d or c")
    }
  }
  
  # cols <- colnames(data)[2:length(types)]
  # for(col in cols) {
  #   if(is.numeric(data[,col])) {
  #     data <- discretizeData(data,col)
  #   }
  # }
  data
}


vectorToDataFrame <- function(v,name) {
  df <- data.frame(v)
  colnames(df) <- name
  df
}

getInputData <- function(data) {
  if(ncol(data) == 2) {
    inputData <- vectorToDataFrame(data[,-1],colnames(data)[2])
  } else {
    inputData <- data[,-1]
  }
  inputData
}

getTreeFromPreorder <- function(pos,preorder,realIndexes) {
  node <- new.env()
  node$index <- realIndexes[pos]
  if(preorder[pos] != "<leaf>") {
    result <- getTreeFromPreorder(pos+1,preorder,realIndexes)
    node$left <- result$node
    
    result <- getTreeFromPreorder(result$pos+1,preorder,realIndexes)
    node$right <- result$node
    
    env <- new.env()
    env$pos <- result$pos
    env$node <- node
    return(env)
  }
  env <- new.env()
  env$pos <- pos
  env$node <- node
  return(env)
}

getParentsChildren <- function(object,preorder) {
  parentsChildren <- matrix(ncol=3,nrow=sum(preorder != "<leaf>"))
  index <<- 1
  realIndexes <- as.integer(rownames(object$frame))
  getTreeFromPreorder <- function(pos,preorder,realIndexes) {
    node <- new.env()
    node$index <- realIndexes[pos]
    if(preorder[pos] != "<leaf>") {
      result <- getTreeFromPreorder(pos+1,preorder,realIndexes)
      node$left <- result$node
      
      result <- getTreeFromPreorder(result$pos+1,preorder,realIndexes)
      node$right <- result$node
      
      parentsChildren[index,] <<- c(realIndexes[pos],node$left$index,node$right$index)
      index <<- index + 1
      
      env <- new.env()
      env$pos <- result$pos
      env$node <- node
      return(env)
    }
    env <- new.env()
    env$pos <- pos
    env$node <- node
    return(env)
  }
  getTreeFromPreorder(1,preorder,realIndexes)
  parentsChildren
}

getIGs <- function(object) {
  nodesN <- object$frame$n
  preorder <- as.character(object$frame[,1])
  parentsChildren <- getParentsChildren(object,preorder)
  
  infoClass <- attributes(object)$ylevels
  nlevelsClass <- length(infoClass)
  probaClass <- object$frame[,"yval2"]
  effectif <- data.frame(probaClass[,2:(nlevelsClass+1), drop = F])
  rownames(effectif) <- rownames(object$frame)
  effectif <- as.matrix(effectif)
  
  IGs <- rep("-",length(nodesN))
  avgEntropies <- rep("-",length(nodesN))
  
  if(nrow(parentsChildren) == 0) {
    return(list(
      IGs = IGs,
      avgEntropies = avgEntropies
      )
    )
  }
  for(i in 1:nrow(parentsChildren)) {
    index <- parentsChildren[i,1]
    indexLeft <- parentsChildren[i,2]
    indexRight <- parentsChildren[i,3]
    IGs[which(rownames(object$frame) == index)] <- round(IG(effectif[as.character(index),],
                           effectif[c(as.character(indexLeft),
                                      as.character(indexRight)),]),
                        digits = 6)
    avgEntropies[which(rownames(object$frame) == index)] <- round(avgEntropy(effectif[as.character(index),],
                                                            effectif[c(as.character(indexLeft),
                                                                       as.character(indexRight)),]),
                                                         digits = 6)
  }
  list(IGs=IGs,
       avgEntropies=avgEntropies)
}

accuracy <- function(real,predicted) {
  sum(real == predicted)/length(predicted)
}

getAccuracy <- function(model, newDataInput, newDataOutput) {
  predicted <- predict(model, newdata = newDataInput,type="class")
  accuracy(newDataOutput,predicted)
}

getError <- function(model, newDataInput, newDataOutput) {
  1 - getAccuracy(model, newDataInput, newDataOutput)
}

getAccuracyImprovement <- function(model, newDataInput, newDataOutput, snipIndex) {
  modelAccuracy <- getAccuracy(model,newDataInput,newDataOutput)
  
  snippedModel <- snip.rpart(model, snipIndex)
  snippedAccuracy <- getAccuracy(snippedModel,newDataInput,newDataOutput)
  
  snippedAccuracy - modelAccuracy
}

getAccuracyImprovements <- function(model, newDataInput, newDataOutput) {
  snipIndexes <- as.integer(rownames(model$frame))
  result <- c()
  for(snipIndex in snipIndexes) {
    accImpr <- getAccuracyImprovement(model, newDataInput, newDataOutput, snipIndex)
    accImpr <- round(accImpr,
                      digits = 6)
    result <- c(result,accImpr)
  }
  result
}

augmentRPartObject <- function(object,validationDataInput,validationDataOutput,data) {
  aux <- getIGs(object)
  object$IGs <- aux$IGs
  object$avgEntropies <- aux$avgEntropies
  object$accImprovements <- getAccuracyImprovements(object,
                                                    validationDataInput,
                                                    validationDataOutput)
  aux <- get_chi_info(object,data)
  object$chi <- aux$chi
  object$pvalue <- aux$pvalue
  object
}

pruneByIGNOTEFFICIENT <- function(augmObject, epsilon) {
  aux <- (as.numeric(augmObject$IGs) < epsilon)
  aux[is.na(aux)] <- FALSE
  aux <- as.integer(rownames(augmObject$frame))[which(aux)]
  if(length(aux)) {
    res <- snip.rpart(augmObject,
                    aux)
    newIndexes <- as.integer(rownames(res$frame))
    res$IGs <- augmObject$IGs[newIndexes]
    res$accImprovements <- augmObject$accImprovements[newIndexes]
    res$IGs[res$frame$var == "<leaf>"] <- "-"
    res$accImprovements[res$frame$var == "<leaf>"] <- 0
  } else {
    res <- augmObject
  }
  res
}

reducedErrorPruning <- function(augmObject,validationDataInput,validationDataOutput,data) {
  while(1) {
    realIndexes <- as.integer(rownames(augmObject$frame))
    indexesNonLeaf <- which(augmObject$frame$var != "<leaf>")
    maxAccImpr <- max(augmObject$accImprovements[indexesNonLeaf])
    if(maxAccImpr <= 0) {
      break
    }
    indexToRemove <- realIndexes[which(augmObject$accImprovements == maxAccImpr)[1]]
    augmObject <- snip.rpart(augmObject,indexToRemove)
    augmObject <- augmentRPartObject(augmObject,validationDataInput,validationDataOutput,data)
  }
  augmObject
}

getTreeFromRPartObject <- function(object) {
  preorder <- as.character(object$frame[,1])
  realIndexes <- as.integer(rownames(object$frame))
  getTreeFromPreorder(1,preorder,realIndexes)$node
}

bottomUpPruningIG <- function(augmObject,epsilon,
                              validationDataInput,
                              validationDataOutput,data) {
  tree <- getTreeFromRPartObject(augmObject)
  IGs <- augmObject$IGs
  realIndexes <- as.integer(rownames(augmObject$frame))
  indexesToDelete <- c()
  bottomUpPruningIGRec <- function(tree) {
    if(is.null(tree)) {
      return()
    }
    bottomUpPruningIGRec(tree$left)
    bottomUpPruningIGRec(tree$right)
    
    if(is.null(tree$left) && is.null(tree$right)) {
      #leaf
      tree$toDelete <- TRUE
      return()
    }
    #non leaf
    tree$toDelete <- FALSE
    if(as.numeric(IGs[which(realIndexes == tree$index)]) < epsilon && 
       tree$left$toDelete == TRUE &&
       tree$right$toDelete == TRUE) {
       tree$toDelete <- TRUE
       indexesToDelete <<- c(indexesToDelete,tree$index)
    }
  }
  bottomUpPruningIGRec(tree)
  if(!length(indexesToDelete)) {
    return(augmObject)
  }
  result <- snip.rpart(augmObject,indexesToDelete)
  result <- augmentRPartObject(result,validationDataInput,
                               validationDataOutput,data)
  result
}


topDownPruningIG <- function(augmObject,epsilon,
                              validationDataInput,
                              validationDataOutput,data) {
  tree <- getTreeFromRPartObject(augmObject)
  IGs <- augmObject$IGs
  realIndexes <- as.integer(rownames(augmObject$frame))
  indexesToDelete <- c()
  topDownPruningIGRec <- function(tree) {
    if(is.null(tree)) {
      return()
    }
    
    if(is.null(tree$left) && is.null(tree$right)) {
      #leaf
      return()
    }
    #non leaf
    tree$toDelete <- FALSE
    if(as.numeric(IGs[which(realIndexes == tree$index)]) < epsilon) {
      indexesToDelete <<- c(indexesToDelete,tree$index)
    } else {
      topDownPruningIGRec(tree$left)
      topDownPruningIGRec(tree$right)
    }
  }
  topDownPruningIGRec(tree)
  if(!length(indexesToDelete)) {
    return(augmObject)
  }
  result <- snip.rpart(augmObject,indexesToDelete)
  result <- augmentRPartObject(result,validationDataInput,
                               validationDataOutput,data)
  result
}

bottomUpPruningAccImpNOT_EFFICIENT <- function(augmObject,epsilon,
                              validationDataInput,
                              validationDataOutput,data) {
  tree <- getTreeFromRPartObject(augmObject)
  indexesToDelete <- c()
  bottomUpPruningAccImpRec <- function(tree) {
    if(is.null(tree)) {
      return()
    }
    bottomUpPruningAccImpRec(tree$left)
    bottomUpPruningAccImpRec(tree$right)
    
    if(is.null(tree$left) && is.null(tree$right)) {
      #leaf
      tree$toDelete <- TRUE
      return()
    }
    #non leaf
    tree$toDelete <- FALSE
    realIndexes <- as.integer(rownames(augmObject$frame))
    if(as.numeric(augmObject$accImprovements[
      which(realIndexes == tree$index)
      ]) > epsilon && 
       tree$left$toDelete == TRUE &&
       tree$right$toDelete == TRUE) {
      tree$toDelete <- TRUE
      augmObject <<- snip.rpart(augmObject,tree$index)
      augmObject <<- augmentRPartObject(augmObject,validationDataInput,
                         validationDataOutput,data)
    }
  }
  bottomUpPruningAccImpRec(tree)
  augmObject
}

bottomUpPruningAccImp <- function(augmObject,epsilon,
                                  validationDataInput,
                                  validationDataOutput,data) {
  tree <- getTreeFromRPartObject(augmObject)
  indexesToDelete <- c()
  bottomUpPruningAccImpRec <- function(tree) {
    if(is.null(tree)) {
      return()
    }
    bottomUpPruningAccImpRec(tree$left)
    bottomUpPruningAccImpRec(tree$right)
    
    if(is.null(tree$left) && is.null(tree$right)) {
      #leaf
      tree$toDelete <- TRUE
      return()
    }
    #non leaf
    tree$toDelete <- FALSE
    realIndexes <- as.integer(rownames(augmObject$frame))
    
    accImpr <- getAccuracyImprovement(augmObject, validationDataInput, 
                                      validationDataOutput, tree$index)
    
    if(accImpr > epsilon && 
      tree$left$toDelete == TRUE &&
      tree$right$toDelete == TRUE) {
      tree$toDelete <- TRUE
      augmObject <<- snip.rpart(augmObject,tree$index)
    }
  }
  bottomUpPruningAccImpRec(tree)
  augmObject <- augmentRPartObject(augmObject,validationDataInput,
                                    validationDataOutput,data)
  augmObject
}


topDownPruningAccImp <- function(augmObject,epsilon,
                                  validationDataInput,
                                  validationDataOutput,data) {
  tree <- getTreeFromRPartObject(augmObject)
  indexesToDelete <- c()
  topDownPruningAccImpRec <- function(tree) {
    if(is.null(tree)) {
      return()
    }
    if(is.null(tree$left) && is.null(tree$right)) {
      #leaf
      return()
    }
    #non leaf
    realIndexes <- as.integer(rownames(augmObject$frame))
    if(as.numeric(augmObject$accImprovements[
      which(realIndexes == tree$index)
      ]) > epsilon) {
      augmObject <<- snip.rpart(augmObject,tree$index)
      augmObject <<- augmentRPartObject(augmObject,validationDataInput,
                                        validationDataOutput,data)
    } else {
      topDownPruningAccImpRec(tree$left)
      topDownPruningAccImpRec(tree$right)
    }
  }
  topDownPruningAccImpRec(tree)
  augmObject
}

getRPartObject <- function(data) {
  formula <- as.formula(paste0(colnames(data)[1],"~."))
  res <- rpart(formula, data=data, 
               parms=list(split='information'),
               control = rpart.control(cp = -Inf,
                                       minsplit = 0,
                                       maxcompete = Inf,
                                       maxsurrogate = 0,
                                       maxdepth = 30,
                                       xval = 0))
  res
}
