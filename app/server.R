function(input, output, session) {
  
  sendSweetAlert(
    session = session,
    title = "WARNING",
    html = TRUE,
    text = HTML("This app implements ID3, but has some limitations:
      <ul>
        <li><b>binary</b>: it always produces a binary tree, even if an input attribute has more than 2 values [in general, the ID3 tree is not binary!]</li>
        <li><b>non-zero IG</b>: if all the IGs of the candidate nodes are zero, then it won't select any of them and there will be a leaf [in this case, ID3 would CHOOSE any of them]</li>
        <li><b>depth <= 30</b>: the maximum depth of the tree is set to 30 [ID3 does not have this limitation]</li>
      </ul>"),
    type = "warning"
  )
  
  output$rpartObject <- renderVisNetwork({
    
    trainData()
    validationData()
    input$typeOfTree
    epsilon()
    
    shinyjs::hide("epsilon")
    shinyjs::hide("epsilonGo")
    
    if(is.null(trainData())) {
      return()
    }
    
    if(is.null(validationData())) {
      return()
    }
    
    if(is.null(input$typeOfTree)) {
      return()
    }
    
    
    # trainData <- readData(input$trainFile$datapath)
    
    # validationData <- readData(input$validationFile$datapath)
    validationDataInput <- getInputData(validationData())
    validationDataOutput <- validationData()[,1]
    res <- getRPartObject(trainData())
    res <- augmentRPartObject(res,validationDataInput,validationDataOutput,trainData())
    
    title <- ""
    
    if(input$typeOfTree == "No pruning") {
      # ...
    } else if(input$typeOfTree == "rep"){
      res <- reducedErrorPruning(res,validationDataInput,validationDataOutput,trainData())
    } else if(input$typeOfTree == "buig") {
      shinyjs::show("epsilon")
      shinyjs::show("epsilonGo")
      
      if(is.null(epsilon())) {
        return()
      }
      
      title <- paste0("Epsilon = ",epsilon())
      
      res <- bottomUpPruningIG(res,epsilon(),validationDataInput,validationDataOutput,trainData())
    } else if(input$typeOfTree == "tdig") {
      shinyjs::show("epsilon")
      shinyjs::show("epsilonGo")
      
      if(is.null(epsilon())) {
        return()
      }
      
      title <- paste0("Epsilon = ",epsilon())
      
      res <- topDownPruningIG(res,epsilon(),validationDataInput,validationDataOutput,trainData())
    } else if(input$typeOfTree == "buacc") {
      shinyjs::show("epsilon")
      shinyjs::show("epsilonGo")
      
      if(is.null(epsilon())) {
        return()
      }
      
      title <- paste0("Epsilon = ",epsilon())
      
      res <- bottomUpPruningAccImp(res,epsilon(),validationDataInput,validationDataOutput,trainData())
    } else if(input$typeOfTree == "tdacc") {
      shinyjs::show("epsilon")
      shinyjs::show("epsilonGo")
      
      if(is.null(epsilon())) {
        return()
      }
      
      title <- paste0("Epsilon = ",epsilon())
      
      res <- topDownPruningAccImp(res,epsilon(),validationDataInput,validationDataOutput,trainData())
    }
    
    visTree(res,main=title)
  })

  trainData <- reactive({
    if(is.null(input$trainFile)) {
      return()
    } else {
      trainData <- try(readData(input$trainFile$datapath),TRUE) 
      if(class(trainData) == "try-error") {
        sendSweetAlert(
          session = session,
          title = "Error",
          html = TRUE,
          text = HTML("Error when reading training data... ", 
                      Functions$readWholeFile(Others$fileErrorMessage.html)),
          type = "error"
        )
        return()
      }
      return(trainData)
    }
  })
  
  validationData <- reactive({
    if(is.null(input$validationFile)) {
      return(trainData())
    } 
    
    if(is.null(trainData())) {
      sendSweetAlert(
        session = session,
        title = "Error",
        html = TRUE,
        text = HTML("First, upload the training data... ", 
                    Functions$readWholeFile(Others$fileErrorMessage.html)),
        type = "error"
      )
      return()
    }
    
    validationData <- try(readData(input$validationFile$datapath),TRUE) 
    if(class(validationData) == "try-error") {
      sendSweetAlert(
        session = session,
        title = "Error",
        html = TRUE,
        text = HTML("Error when reading validation data... ", 
                    Functions$readWholeFile(Others$fileErrorMessage.html)),
        type = "error"
      )
      return()
    }
    if(length(colnames(trainData())) != length(colnames(validationData)) || any(colnames(trainData()) != colnames(validationData))) {
      sendSweetAlert(
        session = session,
        title = "Error",
        html = TRUE,
        text = HTML("Training data and validation data must have the same column names... ", 
                    Functions$readWholeFile(Others$fileErrorMessage.html)),
        type = "error"
      )
      return()
    }
    return(validationData)
    
  })
  
  epsilon <- eventReactive(input$epsilonGo,{
    if(is.null(input$epsilon)) {
      return()
    }
    epsilon <- input$epsilon
    
    if (suppressWarnings(!is.na(as.numeric(epsilon)))) {
      epsilon <- as.numeric(epsilon)
    } else {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Epsilon must be a number",
        type = "error"
      )
      return()
    }
  },ignoreNULL=FALSE)
  
}
