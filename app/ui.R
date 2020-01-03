fluidPage(
  
  # mandatory
  Variables$toIncludeList,
  
  # Application title
  titlePanel("(Binary) (Non-zero IG) (Discrete/Continuous Input) (Depth <= 30) ID3"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$a(href="data.zip","Examples of data files"),
      fileInput("trainFile", 
                "Training data file",
                multiple = FALSE,
                accept = "text/*"),
      fileInput("validationFile", 
                "Validation data file",
                multiple = FALSE,
                accept = "text/*"),
      radioButtons(inputId = "typeOfTree", 
                   label = "Type of tree", 
                   choices = c("No pruning" = "noPrune",  
                               "Reduced Error Pruning" = "rep",
                               "Bottom up pruning IG<epsilon" = "buig",
                               "Top down pruning IG<epsilon" = "tdig",
                               "Bottom up pruning Acc. Impr.>epsilon" = "buacc",
                               "Top down pruning Acc. Impr.>epsilon" = "tdacc")),
      tags$div(style="display:inline-block",
               textInput("epsilon","Epsilon","0.5")),
      actionButton("epsilonGo","Set Epsilon")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      visNetworkOutput("rpartObject")
    )
  )
)
