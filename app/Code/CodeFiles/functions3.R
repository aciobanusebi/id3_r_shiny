get_node_data <- function(tree, data, node){
  rule <- path.rpart(tree, node,print.it = F)
  if(rule == "root") {
    return(data)
  }
  rule_2 <- sapply(rule[[1]][-1], function(x) strsplit(x, '(?<=[><=])(?=[^><=])|(?<=[^><=])(?=[><=])', perl = TRUE))
  for(i in 1:length(rule_2)) {
    if(rule_2[[i]][2] == "=") {
      rule_2[[i]][2] <- "%in%"
      rule_2[[i]][3] <- paste0("c('",
                          paste0(strsplit(rule_2[[i]][3],",")[[1]],collapse = "','")
                          ,"')")
    }
  }
  ind <- apply(do.call(cbind, lapply(rule_2, function(x) eval(call(x[2], data[,x[1]], (
    eval(parse(text=x[3]))
    ))))), 1, all)
  data[ind,]
}
get_not_leaf_nodes <- function(fit) {
  indexes <- as.integer(rownames(fit$frame[fit$frame$var != "<leaf>",]))
  indexes
}
findCondition <- function(fit,node,parentsChildren) {
  leftNode <- parentsChildren[parentsChildren[,1] == node,2]
  x <- path.rpart(fit, leftNode,print.it = F)
  x <- x[[1]]
  return(tail(x,1))
}
filter_data <- function(data,condition) {
  rule_2 <- sapply(condition, function(x) strsplit(x, '(?<=[><=])(?=[^><=])|(?<=[^><=])(?=[><=])', perl = TRUE))
  for(i in 1:length(rule_2)) {
    if(rule_2[[i]][2] == "=") {
      rule_2[[i]][2] <- "=="
    }
  }
  ind <- apply(do.call(cbind, lapply(rule_2, function(x) eval(call(x[2], data[,x[1]], (x[3]))))), 1, all)
  x <- rep(0,nrow(data))
  x[ind] <- 1
  y <- data[[1]]
  list(x=x,
       y=y)
}
my.chi.squared <- function(x,y) {

  # if(is.null(x) || length(x) == 0) {
  #   print("DA")
  #   return(NULL)
  # }
  O <- table(x,y)
  
  E <- matrix(ncol=ncol(O),nrow=nrow(O))
  N <- length(x)
  for(i in 1:nrow(E)) {
    for(j in 1:ncol(E)) {
      E[i,j] <- sum(O[i,]) * sum(O[,j]) / N
    }
  }
  xi <- sum((O - E)^2 / E)
  pv <- pchisq(xi, df=(nrow(E)-1)*(ncol(E)-1), lower.tail=FALSE)
  list(xi = xi,
       pvalue = pv)
}
get_chi_squared_for_node <- function(fit,data,parentsChildren,index) {
  condition <- findCondition(fit,index,parentsChildren)
  mini.data <- get_node_data(tree=fit,data=data,node=index)
  # print("condition=")
  # print(condition)
  # print("mini.data=")
  # print(mini.data)
  xy <- filter_data(mini.data,condition)
  my.chi.squared(xy$x,xy$y)
}
get_chi_info <- function(fit,data) {
  nNodes <- fit$frame$n
  indexes <- get_not_leaf_nodes(fit)
  preorder <- as.character(fit$frame[,1])
  parentsChildren <- getParentsChildren(fit,preorder)
  chi <- rep("-",length(nNodes))
  pvalue <- rep("-",length(nNodes))
  if(nrow(parentsChildren) == 0) {
    return(list(chi=chi,
                pvalue=pvalue))
  }
  if(length(indexes) != 0) {
    for(i in 1:nrow(parentsChildren)) {
      index <- parentsChildren[i,1]
      if(index %in% indexes) {
        r <- get_chi_squared_for_node(fit,data,parentsChildren,index)
        if(is.null(r)) {
          next()
        }
        aux <- which(rownames(fit$frame) == index)
        chi[aux] <- round(r$xi,6)
        pvalue[aux] <- round(r$pvalue,6)
      }
    }
  }
  list(chi=chi,
       pvalue=pvalue)
}
