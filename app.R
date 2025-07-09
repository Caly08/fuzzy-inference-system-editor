
library(shiny)
library(shinydashboard)
library(FuzzyR)
library(splines)
library(plyr)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinycssloaders )
library(rsconnect)

# Initialize an empty FIS
fis <- newfis("Sistem")


ui <- dashboardPage(
  dashboardHeader(title = paste("Inferenta Fuzzy Mandami : ", fis$name), titleWidth = 450),
  dashboardSidebar( disable = TRUE),
  dashboardBody(
    fluidPage(
      title = paste("Inferenta Fuzzy Mandami : ", fis$name),
      # titlePanel(title = h1(paste("Inferenta Fuzzy Mandami : ", fis$name), align = "center")),
      # br(),
      sidebarLayout(
        sidebarPanel(fluid = TRUE,
                     
                     
          # Section to add variables
          box(width = "100%",title = "Adauga Variabila",collapsible = TRUE,
             
              selectInput("var_type", "Tip Variabila:", choices = c("Input", "Output")),
              textInput("var_name", "Nume Variabila:", ""),
              numericInput("var_range_min", "Range Min:", value = 0),
              numericInput("var_range_max", "Range Max:", value = 10),
              actionButton("add_var", "Adauga Variabila"),
          ),

          # Section to add membership functions
          box(width = "100%",title = "Adauga functie de apartenenta", collapsible = TRUE,collapsed = TRUE,
             
              selectInput("var_name_select", "Alege variabila:", choices = NULL),
          
              textInput("mf_name", "Nume Functie:"),
              selectInput("mf_type", "Tip Functie:", choices = c("trimf", "trapmf", "gaussmf")),
              uiOutput("mf_params_ui"),
              actionButton("add_mf", "Adauga functie"),
          ),
          
          # Section to add rules
          box(width = "100%",title = "Adauga Regula", collapsible = TRUE,collapsed = TRUE,
        
              # textInput("rule_text", "Enter Rule (e.g., '1 1, 2 (1.0)')"),
              textInput("rule_name", "Nume Regula:"),
              numericInput("rule_weight", "Pondere Regula:", value = 1),
              radioButtons("rule_connection", "Operator Regula:", choices = c("AND", "OR")),
              h4("If"),
              tags$hr(style="border-color: purple;"),
              uiOutput("rule_input1"),
              h4("Then"),
              tags$hr(style="border-color: purple;"),
              uiOutput("rule_input2"),
              
              actionButton("add_rule", "Adauga Regula"),
          ),
      
          # Evaluation and sliders
          box(width = "100%",
              radioButtons("eva", "Evaluare Sistem ", list( "Reset" = 1, "Evaluare" = 2), selected = 2),
              
              conditionalPanel("input.eva == 2",
                               materialSwitch(
                                 inputId = "input_type_switch",
                                 label = "Tip Input (Slider / Numeric)",
                                 value = TRUE,  # TRUE for sliders by default
                                 status = "primary",
                                 right = FALSE,
                                 inline = TRUE
                               )),
              uiOutput("sliders"),
              
              actionButton("do", "Exit"),
              actionButton("run", "Run")
              
          )
          
        ),
        mainPanel(
          tabsetPanel(
            type = "tab",
            tabPanel("Sumar", style = "margin-left: -15px;",
                     # fluidPage(
                       box(width = 12,
                           fluidRow(
                             column(
                               width = 2,
                               actionButton(
                                 "add_example1",
                                 "Ex Temperatura",
                                 class = "btn-success",
                                 style = "color: #fff;",
                                 icon = icon('plus'),
                                 width = '100%'
                               ),
                               tags$br(),
                               tags$br()
                             ),
                             column(
                               width = 2,
                               actionButton(
                                 "add_example2",
                                 "Ex Franare",
                                 class = "btn-success",
                                 style = "color: #fff;",
                                 icon = icon('plus'),
                                 width = '100%'
                               ),
                               tags$br(),
                               tags$br()
                             ),
                             column(
                               width = 2,
                               actionButton(
                                 "add_example3",
                                 "Ex Tip",
                                 class = "btn-success",
                                 style = "color: #fff;",
                                 icon = icon('plus'),
                                 width = '100%'
                               ),
                               tags$br(),
                               tags$br()
                             )
                           ),
                         fluidRow(
                           box(width = 12,title = "Variabile",collapsible = TRUE,
                               DTOutput('variables_table'),
                               tags$script("$(document).on('click', '#variables_table button', function () {
                    Shiny.onInputChange('lastClickId1',this.id);
                    Shiny.onInputChange('lastClick1', Math.random())
                        });")
                           ),
                           box(width = 12,title = "Functii de apartenenta",collapsible = TRUE,
                               DTOutput('membership_functions_table'),
                               tags$script("$(document).on('click', '#membership_functions_table button', function () {
                    Shiny.onInputChange('lastClickId2',this.id);
                    Shiny.onInputChange('lastClick2', Math.random())
                          });")
                           ),
                           box(width = 12,title = "Reguli",collapsible = TRUE,
                               DTOutput('rules_table'),
                               tags$script("$(document).on('click', '#rules_table button', function () {
                    Shiny.onInputChange('lastClickId3',this.id);
                    Shiny.onInputChange('lastClick3', Math.random())
                          });")
                           )
                         )
                       )
                       
                        
                     # )
            ),
            tabPanel("Functii de Apartenenta",
                     selectInput("input_output","Alege variabila input/output",choices = NULL),
                     plotOutput("plot1")%>% withSpinner(color="#3c8dbc"), downloadButton(outputId = "plot1down", label = "Descarca Grafic")),
            tabPanel("Reguli", verbatimTextOutput("RulesInWords")),
            tabPanel("Grafic regula (single)", selectInput("rule", "Alege o regula:", choices = NULL), plotOutput("plotRules")%>% withSpinner(color="#3c8dbc")),
            tabPanel("Grafici reguli (toate)", plotOutput("plotRulesAll")%>% withSpinner(color="#3c8dbc")),
            tabPanel("Grafic set reguli", selectInput("rule2", "Alege un set de reguli:", choices = NULL), plotOutput("plotRulesAll2")%>% withSpinner(color="#3c8dbc")),
            tabPanel("Functie de apartenenta la iesire", plotOutput("plotRules2")%>% withSpinner(color="#3c8dbc")),
            tabPanel("Iesire/Defuzzifier", plotOutput("defu") %>% withSpinner(color="#3c8dbc"), downloadButton(outputId = "defudown", label = "Descarca Grafic"))
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  # list of rules
  
  fis_data = reactiveValues(fis = fis,
                            variables = data.frame(varType = character(), 
                                                   varName = character(), 
                                                   varBoundsMin = integer(), 
                                                   varBoundsMax = integer(),stringsAsFactors = FALSE),
                            memberFunctions = data.frame(varType = character(),
                                                         varIndex = integer(), 
                                                         mfName = character(), 
                                                         mfType = character(), 
                                                         mfParams1 = numeric(),
                                                         mfParams2 = numeric(),
                                                         mfParams3 = numeric(),
                                                         mfParams4 = numeric(),stringsAsFactors = FALSE),
                           
                            rules = data.frame(rule_name = character(),
                                               rule_weight = integer(),
                                               rule_connection = character(),
                                               inputs = character(),
                                               outputs = character(),
                                               stringsAsFactors = FALSE),
                            lRules = vector("list"),
                            lRules_label = vector("character")
                            )


  observeEvent(input$add_example1, {
    # Example Variables
    fis_data$variables <- data.frame(
      varType = c("Input", "Output"),
      varName = c("Temperature", "FanSpeed"),
      varBoundsMin = c(0, 0),
      varBoundsMax = c(100, 10),
      stringsAsFactors = FALSE
    )
    
    # Example Member Functions
    fis_data$memberFunctions <- data.frame(
      varType = c("Input", "Input", "Input", "Output", "Output", "Output"),
      varIndex = c(1, 1, 1, 1, 1, 1),
      mfName = c("Cold", "Warm", "Hot", "Low", "Medium", "High"),
      mfType = c("trapmf", "trapmf", "trapmf", "trapmf", "trapmf", "trapmf"),
      mfParams1 = c(0, 15, 60, 0, 2, 6),
      mfParams2 = c(0, 30, 75, 0, 4, 8),
      mfParams3 = c(15, 60, 100, 2, 6, 10),
      mfParams4 = c(30, 75, 100, 4, 8, 10),
      stringsAsFactors = FALSE
    )
    
    # Example Rules
    fis_data$rules <- data.frame(
      rule_name = c("Rule1", "Rule2", "Rule3"),
      rule_weight = c(1, 1, 1),
      rule_connection = c("AND", "AND", "AND"),
      inputs = c("Temperature:Cold", "Temperature:Warm", "Temperature:Hot"),
      outputs = c("FanSpeed:Low", "FanSpeed:Medium", "FanSpeed:High"),
      stringsAsFactors = FALSE
    )
    
    updateSelectInput(session, "var_name_select", choices = fis_data$variables$varName)
   
    
    
  })
  
  observeEvent(input$add_example2, {
    # Example Variables
    fis_data$variables <- data.frame(
      varType = c("Input", "Input", "Output"),
      varName = c("Distance", "Speed", "BrakingForce"),
      varBoundsMin = c(0, 0, 0),
      varBoundsMax = c(100, 120, 1),
      stringsAsFactors = FALSE
    )
    
    # Example Member Functions
    fis_data$memberFunctions <- data.frame(
      varType = c("Input", "Input", "Input", "Input", "Input", "Input", 
                  "Output", "Output", "Output"),
      varIndex = c(1, 1, 1, 2, 2, 2, 1, 1, 1),
      mfName = c("Near", "Medium", "Far", "Slow", "Medium", "Fast", "Low", "Medium", "High"),
      mfType = c("trapmf", "trapmf", "trapmf", "trapmf", "trapmf", "trapmf", "trapmf", "trapmf", "trapmf"),
      mfParams1 = c(0, 10, 30, 0, 30, 60, 0, 0.2, 0.5),
      mfParams2 = c(0, 20, 50, 0, 50, 80, 0.1, 0.5, 0.8),
      mfParams3 = c(10, 30, 70, 30, 60, 100, 0.2, 0.6, 1),
      mfParams4 = c(20, 50, 100, 50, 80, 120, 0.4, 0.8, 1),
      stringsAsFactors = FALSE
    )
    
    # Example Rules
    fis_data$rules <- data.frame(
      rule_name = c("Rule1", "Rule2", "Rule3"),
      rule_weight = c(1, 1, 1),
      rule_connection = c("AND", "AND", "AND"),
      inputs = c("Distance:Near,Speed:Fast", "Distance:Medium,Speed:Medium", "Distance:Far,Speed:Slow"),
      outputs = c("BrakingForce:High", "BrakingForce:Medium", "BrakingForce:Low"),
      stringsAsFactors = FALSE
    )
    
    updateSelectInput(session, "var_name_select", choices = fis_data$variables$varName)
  })
  observeEvent(input$add_example3, {
    
    fis_data$variables <- data.frame(
      varType = c("Input", "Input", "Output"),
      varName = c("service", "food", "tip"),
      varBoundsMin = c(0, 0, 0),
      varBoundsMax = c(10, 10, 30),
      stringsAsFactors = FALSE
    )
    
    # Example Member Functions
    fis_data$memberFunctions <- data.frame(
      varType = c("Input", "Input", "Input", "Input","Input", "Output", "Output", "Output"),
      varIndex = c(1, 1, 1,2, 2, 1, 1, 1),
      mfName = c("poor", "good", "excellent", "rancid", "delicious", "cheap", "average", "generous"),
      mfType = c("gaussmf", "gaussmf", "gaussmf", "trapmf", "trapmf", "trimf", "trimf", "trimf"),
      mfParams1 = c(1.5, 1.5, 1.5, 0, 7, 0, 10, 20),
      mfParams2 = c(0, 5, 10, 0, 9, 5, 15, 25),
      mfParams3 = c(NA, NA, NA, 1, 10, 10, 20, 30),
      mfParams4 = c(NA, NA, NA, 3, 10, NA, NA, NA),
      stringsAsFactors = FALSE
    )
    
    # Example Rules
    fis_data$rules <- data.frame(
      rule_name = c("Rule1", "Rule2", "Rule3"),
      rule_weight = c(1, 1, 1),
      rule_connection = c("AND", "AND", "AND"),
      inputs = c("service:poor,food:rancid", "service:good,food:rancid", "service:excellent,food:delicious"),
      outputs = c("tip:cheap", "tip:average", "tip:generous"),
      stringsAsFactors = FALSE
    )
    
    updateSelectInput(session, "var_name_select", choices = fis_data$variables$varName)
  })
  
  #########################################################
  ##################### Variables #########################
  #########################################################
  observeEvent(input$add_var, {
    var_type <- input$var_type
    var_name <- input$var_name
    var_range_min <- input$var_range_min
    var_range_max <- input$var_range_max
    
    fis_data$variables <- rbind(fis_data$variables, 
                                data.frame(varType = var_type, 
                                           varName = var_name, 
                                           varBoundsMin = var_range_min, 
                                           varBoundsMax = var_range_max))
    
    # if (var_type == "Input") {
    #   fis_data$fis <- addvar(fis_data$fis, "input", var_name, c(var_range_min, var_range_max))
    # } else {
    #   fis_data$fis <- addvar(fis_data$fis, "output", var_name, c(var_range_min, var_range_max))
    # }
    # 
    updateSelectInput(session, "var_name_select", choices = fis_data$variables$varName)
    
  })

  output$variables_table <- renderDataTable({

    if (nrow(fis_data$variables) == 0) {
      return(data.frame())
    }
      
    
    DT= fis_data$variables
    
    DT[["Actions"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(fis_data$variables),'>Delete</button>
                <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(fis_data$variables),'>Modify</button>
             </div>
             
             ')
    
    datatable(DT, escape = FALSE, options = list(dom = 't', paging = FALSE), selection = "none")
  })
  
  observeEvent(input$lastClick1,
               {
                 
                 
                 
                 
                 if ( grepl ("delete",input$lastClickId1,fixed = TRUE))
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId1))
                   fis_data$variables <- fis_data$variables[-row_to_del, ]
                   updateSelectInput(session, "var_name_select", choices = fis_data$variables$varName)

                 }
                 else if ( grepl ("modify",input$lastClickId1,fixed = TRUE))
                 {
                  
                   row_to_modify=as.numeric(gsub("modify_","",input$lastClickId1))
                   temp = fis_data$variables[row_to_modify, ]

                   showModal(modalDialog(
                     tags$h2('Update Variable'),
                     selectInput("var_type1", "Variable Type:", choices = c("Input", "Output"), selected  = temp$var_type),
                     textInput("var_name1", "Variable Name:", temp$varName),
                     numericInput("var_range_min1", "Range Min:", value = temp$varBoundsMin),
                     numericInput("var_range_max1", "Range Max:", value = temp$varBoundsMax),
                   

                     footer=tagList(
                       actionButton('modify1', 'Modify'),
                       modalButton('cancel')
                     )
                   ))
                   
                 }
               }
  )
  
  observeEvent(input$modify1, {
    removeModal()
    row_to_modify = as.numeric(gsub("modify_","",input$lastClickId1))
    
    fis_data$variables[row_to_modify,1] = input$var_type1
    fis_data$variables[row_to_modify,2] = input$var_name1
    fis_data$variables[row_to_modify,3] = input$var_range_min1
    fis_data$variables[row_to_modify,4] = input$var_range_max1
  })
  
  
  
  #########################################################
  ################ Member Functions #######################
  #########################################################
  observeEvent(input$mf_type, {
    output$mf_params_ui <- renderUI({
      switch(input$mf_type,
             "trimf" = tagList(
               numericInput("mf_param1", "Parameter 1:", value = 0),
               numericInput("mf_param2", "Parameter 2:", value = 5),
               numericInput("mf_param3", "Parameter 3:", value = 10)
             ),
             "trapmf" = tagList(
               numericInput("mf_param1", "Parameter 1:", value = 0),
               numericInput("mf_param2", "Parameter 2:", value = 5),
               numericInput("mf_param3", "Parameter 3:", value = 10),
               numericInput("mf_param4", "Parameter 4:", value = 15)
             ),
             "gaussmf" = tagList(
               numericInput("mf_param1", "Parameter 1:", value = 5),
               numericInput("mf_param2", "Parameter 2:", value = 10)
             )
      )
    })
  })
  observeEvent(input$mf_type1, {
    output$mf_params_ui1 <- renderUI({
      switch(input$mf_type1,
             "trimf" = tagList(
               numericInput("mf_param1", "Parameter 1:", value = 0),
               numericInput("mf_param2", "Parameter 2:", value = 5),
               numericInput("mf_param3", "Parameter 3:", value = 10)
             ),
             "trapmf" = tagList(
               numericInput("mf_param1", "Parameter 1:", value = 0),
               numericInput("mf_param2", "Parameter 2:", value = 5),
               numericInput("mf_param3", "Parameter 3:", value = 10),
               numericInput("mf_param4", "Parameter 4:", value = 15)
             ),
             "gaussmf" = tagList(
               numericInput("mf_param1", "Parameter 1:", value = 5),
               numericInput("mf_param2", "Parameter 2:", value = 10)
             )
      )
    })
  })
  
  observeEvent(input$add_mf, {

    
    var_name_select  <- input$var_name_select
    mf_name <- input$mf_name
    mf_type <- input$mf_type

    
    varaible = fis_data$variables[fis_data$variables$varName == var_name_select, ]
    var_type <- varaible$varType
    
    # Filter variables by input type
    input_variables <- fis_data$variables[fis_data$variables$varType == varaible$varType, ]
    
    index <- which(input_variables$varName == var_name_select)

    mf_params <- switch(input$mf_type,
                        "trimf" = c(input$mf_param1, input$mf_param2, input$mf_param3),
                        "trapmf" = c(input$mf_param1, input$mf_param2, input$mf_param3, input$mf_param4),
                        "gaussmf" = c(input$mf_param1, input$mf_param2))
    

    # fis_data$fis <- addmf(fis_data$fis, index, mf_type, mf_name, mf_params)
    
    fis_data$memberFunctions <- rbind(fis_data$memberFunctions, 
                                      data.frame(
                                        varType = var_type, 
                                        varIndex = index, 
                                        mfName = mf_name, 
                                        mfType = mf_type, 
                                        mfParams1 = mf_params[1], 
                                        mfParams2 = mf_params[2], 
                                        mfParams3 = mf_params[3], 
                                        mfParams4 = ifelse(length(mf_params) == 4, mf_params[4], NA)
                                        )
                                      )
    
  })
  
  output$membership_functions_table <- renderDataTable({
    
    if (nrow(fis_data$memberFunctions) == 0) {
      return(data.frame())
    }
    DT= fis_data$memberFunctions 
    
    DT[["Actions"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(fis_data$memberFunctions),'>Delete</button>
                <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(fis_data$memberFunctions),'>Modify</button>
             </div>
             
             ')
    
    datatable(DT, escape = FALSE, options = list(dom = 't', paging = FALSE), selection = "none")
  })
  getNameFromTypeAndIndex <- function(type, index){
    variable = fis_data$variables[fis_data$variables$varType == type, ]
    name <- variable$varName[index]
    return(name)
  }
  observeEvent(input$lastClick2,
               {

                 if ( grepl ("delete",input$lastClickId2,fixed = TRUE))
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId2))
                   fis_data$memberFunctions <- fis_data$memberFunctions[-row_to_del, ]
                 }
                 else if ( grepl ("modify",input$lastClickId2,fixed = TRUE))
                 {
                   
                   row_to_modify=as.numeric(gsub("modify_","",input$lastClickId2))

                   temp = fis_data$memberFunctions[row_to_modify, ]
              
                   var_name_select  <- getNameFromTypeAndIndex(temp$varType, temp$varIndex)
                  
                   showModal(modalDialog(
                     tags$h2('Update Membership Function'),
                     selectInput("var_name_select1", "Select Variable:", choices = fis_data$variables$varName,
                                 selected = var_name_select),
                     textInput("mf_name1", "Membership Function Name:", temp$mfName),
                     selectInput("mf_type1", "Membership Function Type:", choices = c("trimf", "trapmf", "gaussmf"),
                                 selected = temp$mfType),

                     conditionalPanel(
                       condition = "input.mf_type1 == 'trimf'",
                         
                                      numericInput("mf_param11", "Parameter 1:", value = temp$mfParams1),
                                      numericInput("mf_param22", "Parameter 2:", value = temp$mfParams2),
                                      numericInput("mf_param33", "Parameter 3:", value = temp$mfParams3)
                     ),
                     conditionalPanel(
                       condition = "input.mf_type1 == 'trapmf'",
                                    numericInput("mf_param11", "Parameter 1:", value = temp$mfParams1),
                                    numericInput("mf_param22", "Parameter 2:", value = temp$mfParams2),
                                    numericInput("mf_param33", "Parameter 3:", value = temp$mfParams3),
                                    numericInput("mf_param44", "Parameter 4:", value = temp$mfParams4)
                       ),
                     conditionalPanel(
                       condition = "input.mf_type1 == 'gaussmf'",
                                    numericInput("mf_param11", "Parameter 1:", value = temp$mfParams1),
                                    numericInput("mf_param22", "Parameter 2:", value = temp$mfParams2)
                     ),

                     
                     footer = tagList(
                       actionButton('modify2', 'Modify'),
                       modalButton('cancel')
                     )
                   ))
                   
                 }
               }
  )
  
  
  
  
  observeEvent(input$modify2, {
    removeModal()
    row_to_modify=as.numeric(gsub("modify_","",input$lastClickId2))
   
    var_name_select  <- input$var_name_select1
    mf_name <- input$mf_name1
    mf_type <- input$mf_type1

    
    varaible = fis_data$variables[fis_data$variables$varName == var_name_select, ]
    var_type <- varaible$varType
    
    # Filter variables by input type
    input_variables <- fis_data$variables[fis_data$variables$varType == varaible$varType, ]
    
    index <- which(input_variables$varName == var_name_select)
    
    
    
    
    mf_params <- switch(input$mf_type1,
                        "trimf" = c(input$mf_param11, input$mf_param22, input$mf_param33),
                        "trapmf" = c(input$mf_param11, input$mf_param22, input$mf_param33, input$mf_param44),
                        "gaussmf" = c(input$mf_param11, input$mf_param22))
    
     
    fis_data$memberFunctions[row_to_modify, 1] = var_type
    fis_data$memberFunctions[row_to_modify, 2] = index
    fis_data$memberFunctions[row_to_modify, 3] = mf_name
    fis_data$memberFunctions[row_to_modify, 4] = mf_type
    
    fis_data$memberFunctions[row_to_modify, 5] = mf_params[1]
    fis_data$memberFunctions[row_to_modify, 6] = mf_params[2] 
    fis_data$memberFunctions[row_to_modify, 7] = ifelse(length(mf_params) == 3, mf_params[3],NA)
    fis_data$memberFunctions[row_to_modify, 8] = ifelse(length(mf_params) == 4, mf_params[4], NA)

  })
  

  #########################################################
  ######################## Rules ##########################
  #########################################################
  
  
  output$rule_input1 <- renderUI({
    # Filter variables by input type
    input_variables <- fis_data$variables[fis_data$variables$varType == "Input", ]
    
    if (nrow(input_variables) == 0) {
      return()
    }
    # Initialize an empty list to store input options
    input_options <- list()
    
    # Iterate through each input variable
    for (i in 1:nrow(input_variables)) {
      var_name <- input_variables[i, "varName"]
      mfs <- fis_data$memberFunctions[fis_data$memberFunctions$varType == "Input" & fis_data$memberFunctions$varIndex == i, "mfName"]
      
      options <- fluidRow(
        column(width = 3, HTML(paste0("\"", var_name, "\" is"))),
        column(width = 9, selectizeInput(inputId = paste0("rule_input_", i), label = NULL, choices = mfs, selected = NULL))
        
      )
      
      input_options[[paste0("input_", i)]] <- options
    }
    
    do.call(tagList, input_options)
  })
  
  output$rule_input2 <- renderUI({
    # Filter variables by output type
    output_variables <- fis_data$variables[fis_data$variables$varType == "Output", ]
    
    if (nrow(output_variables) == 0) {
      return()
    }
    # Initialize an empty list to store output options
    output_options <- list()
    
    # Iterate through each output variable
    for (i in 1:nrow(output_variables)) {
      var_name <- output_variables[i, "varName"]
      mfs <- fis_data$memberFunctions[fis_data$memberFunctions$varType == "Output" & fis_data$memberFunctions$varIndex == i, "mfName"]
      
      options <- fluidRow(
        column(width = 3, HTML(paste0("\"", var_name, "\" is"))),
        column(width = 9,selectizeInput(inputId = paste0("rule_output_", i), label = NULL, choices = mfs, selected = NULL))
      )
      
      output_options[[paste0("output_", i)]] <- options
    }
    
    do.call(tagList, output_options)
  })
  
  observeEvent(input$add_rule, {
    rule_name <- input$rule_name
    rule_weight <- input$rule_weight
    rule_connection <- input$rule_connection
    
    input_vars <- fis_data$variables[fis_data$variables$varType == "Input", ]
    output_vars <- fis_data$variables[fis_data$variables$varType == "Output", ]
    
    rule_inputs <- data.frame(var = character(), mf = character(), stringsAsFactors = FALSE)
    for (i in 1:nrow(input_vars)) {
      rule_inputs <- rbind(rule_inputs, data.frame(var = input_vars$varName[i], mf = input[[paste0("rule_input_", i)]]))
    }
    
    #rule_inputs <- rule_inputs[-1, ]  # Remove the initial empty row
    
    rule_outputs <- data.frame(var = character(), mf = character(), stringsAsFactors = FALSE)
    for (i in 1:nrow(output_vars)) {
      rule_outputs <- rbind(rule_outputs, data.frame(var = output_vars$varName[i], mf = input[[paste0("rule_output_", i)]]))
    }
   
    #rule_outputs <- rule_outputs[-1, ]  # Remove the initial empty row
    # Create a new rule data frame
    new_rule <- data.frame(
      rule_name = rule_name,
      rule_weight = rule_weight,
      rule_connection = rule_connection,
      inputs = paste(rule_inputs$var, rule_inputs$mf, sep = ":", collapse = ","),
      outputs = paste(rule_outputs$var, rule_outputs$mf, sep = ":", collapse = ",")
    )
    
    # Add rule to the reactive values
    fis_data$rules <- rbind(fis_data$rules, new_rule)
    

  })

  
  getRuleIndex <- function(MemberName, index, type) {
    filtered = fis_data$memberFunctions[fis_data$memberFunctions$varIndex == index & fis_data$memberFunctions$varType == type,]
  
    rule_index <- which(filtered$mfName == MemberName)
    
    if (length(rule_index) == 0) {
      return(NA)
    } else {
      return(rule_index)
    }
    
    
  }

  output$rules_table <-
    renderDataTable({

      if (nrow(fis_data$rules) == 0) {
        return(data.frame())
      }

      # Create the rules_df data frame
      rules_df <- fis_data$rules

      # Add actions column
      rules_df[["Actions"]] <- paste0(
        '<div class="btn-group" role="group" aria-label="Basic example">',
        '<button type="button" class="btn btn-secondary delete" id=delete_', 1:nrow(rules_df), '>Delete</button>',
        '<button type="button" class="btn btn-secondary modify" id=modify_', 1:nrow(rules_df), '>Modify</button>',
        '</div>'
      )

      # Return the datatable
      datatable(rules_df, escape = FALSE, options = list(dom = 't', paging = FALSE), selection = "none")

    })

  observeEvent(input$lastClick3,
               {

                 if ( grepl ("delete",input$lastClickId3,fixed = TRUE))
                 {

                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId3))
                   
                   fis_data$rules <- fis_data$rules[-row_to_del,]

                 }
                 else if ( grepl ("modify",input$lastClickId3,fixed = TRUE))
                 {
                   
                   row_to_modify=as.numeric(gsub("modify_","",input$lastClickId3))

                   temp_rule_df <- fis_data$rules[row_to_modify,]
                   
                   split_input_string <- strsplit(temp_rule_df$inputs, ",")[[1]]
                   split_output_string <- strsplit(temp_rule_df$outputs, ",")[[1]]

                   input_variables <- fis_data$variables[fis_data$variables$varType == "Input", ]
                   input_options <- list()
                   
                     for (i in 1:nrow(input_variables)) {
                       input_member_function_name = split_input_string[i]
                       split_item <- strsplit(input_member_function_name, ":")[[1]][2]
                       var_name <- input_variables[i, "varName"]
                       
                       mfs <- fis_data$memberFunctions[fis_data$memberFunctions$varType == "Input" & fis_data$memberFunctions$varIndex == i, "mfName"]

                       options <- fluidRow(
                         column(width = 3, HTML(paste0("\"", var_name, "\" is"))),
                         column(width = 9, selectizeInput(inputId = paste0("rule_input1_", i), label = NULL, choices = mfs, selected = split_item))

                       )

                       input_options[[paste0("input_", i)]] <- options
                     }
                   
                   output_variables <- fis_data$variables[fis_data$variables$varType == "Output", ]
                     output_options <- list()

                     # Iterate through each output variable
                     for (i in 1:nrow(output_variables)) {
                       output_member_function_name = split_output_string[i]
                       split_item <- strsplit(output_member_function_name, ":")[[1]][2]
                       var_name <- output_variables[i, "varName"]
                       mfs <- fis_data$memberFunctions[fis_data$memberFunctions$varType == "Output" & fis_data$memberFunctions$varIndex == i, "mfName"]

                       options <- fluidRow(
                         column(width = 3, HTML(paste0("\"", var_name, "\" is"))),
                         column(width = 9,selectizeInput(inputId = paste0("rule_output1_", i), label = NULL, choices = mfs, selected = split_item))
                       )

                       output_options[[paste0("output_", i)]] <- options
                     }
                  
                   
                   showModal(modalDialog(
                     tags$h2('update Rule'),
                     textInput('rule_name1', 'Rule Name',value = temp_rule_df$rule_name ),
                     numericInput("rule_weight1", "Rule Weight:", value = temp_rule_df$rule_weight ),
                     radioButtons("rule_connection1", "Rule Connection:", choices = c("AND", "OR"), 
                                  selected = temp_rule_df$rule_connection        ),
                     h4("If"),
                     tags$hr(style="border-color: purple;"),
                     do.call(tagList, input_options),
                     h4("Then"),
                     tags$hr(style="border-color: purple;"),
                     do.call(tagList, output_options),

                     footer=tagList(
                       actionButton('modify3', 'Modify'),
                       modalButton('cancel')
                     )
                   ))
                   
                 }
               }
  )
  
  
  observeEvent(input$modify3, {
    removeModal()
    row_to_modify=as.numeric(gsub("modify_","",input$lastClickId3))
    
    modified_rule_name <- input$rule_name1
    modified_rule_weight <- input$rule_weight1
    modified_rule_connection <- input$rule_connection1
    
    input_variables <- fis_data$variables[fis_data$variables$varType == "Input", ]
    output_variables <- fis_data$variables[fis_data$variables$varType == "Output", ]
    
    modified_inputs <- data.frame(var = character(), mf = character(), stringsAsFactors = FALSE)
    for (i in 1:nrow(input_variables)) {
      input_name <- paste0("rule_input1_", i)
      modified_inputs <- rbind(modified_inputs, data.frame(var = input_variables$varName[i], mf = input[[input_name]]))
    }
   
    modified_outputs <- data.frame(var = character(), mf = character(), stringsAsFactors = FALSE)
    for (i in 1:nrow(output_variables)) {
      output_name <- paste0("rule_output1_", i)
      modified_outputs <- rbind(modified_outputs, data.frame(var = output_variables$varName[i], mf = input[[output_name]]))
    }
    
    
    fis_data$rules[row_to_modify,1] <- modified_rule_name
    fis_data$rules[row_to_modify,2] <- modified_rule_weight
    fis_data$rules[row_to_modify,3] <- modified_rule_connection
  
    fis_data$rules[row_to_modify, 4] <- paste(modified_inputs$var, modified_inputs$mf, sep = ":", collapse = ",")
    fis_data$rules[row_to_modify, 5] <- paste(modified_outputs$var, modified_outputs$mf, sep = ":", collapse = ",")
    
    
  })
  #########################################################
  ########################## run ##########################
  #########################################################

  
  observeEvent(input$run,{
  
    fis_data$fis <- newfis("MyFIS")
    for (i in 1:nrow(fis_data$variables)) {
      type = fis_data$variables$varType[i]
      name = fis_data$variables$varName[i]
      min = fis_data$variables$varBoundsMin[i]
      max = fis_data$variables$varBoundsMax[i]
  
      
      if (type == "Input") {
        fis_data$fis <- addvar(fis_data$fis, "input", name, c(min, max))
      } else {
        fis_data$fis <- addvar(fis_data$fis, "output", name, c(min, max))
      }
    }

    
    # Initialize FIS membership functions
    # print("member functions")
    for (i in 1:nrow(fis_data$memberFunctions)) {
      type = fis_data$memberFunctions$varType[i]
      varIndex = fis_data$memberFunctions$varIndex[i]
      mfName = fis_data$memberFunctions$mfName[i]
      mfType = fis_data$memberFunctions$mfType[i]
      mfParams1 = fis_data$memberFunctions$mfParams1[i]
      mfParams2 = fis_data$memberFunctions$mfParams2[i]
      mfParams3 = fis_data$memberFunctions$mfParams3[i]
      mfParams4 = fis_data$memberFunctions$mfParams4[i]
      
      params <- c(mfParams1, mfParams2, mfParams3, mfParams4)
      params <- params[!is.na(params)]  # Remove NA values from params
      
      
      type = ifelse(type == "Input", "input","output")
 
      
      fis_data$fis <- addmf(fis_data$fis, type, varIndex, mfName, mfType, params)
      
    }
    

   
    # Convert rules to the required format
    rules <- lapply(1:nrow(fis_data$rules), function(i) {
      rule <- fis_data$rules[i,]
      inputs <- strsplit(rule$inputs, ",")[[1]]
      outputs <- strsplit(rule$outputs, ",")[[1]]

      input_indices <- sapply(inputs, function(x) {
        var_mf <- strsplit(x, ":")[[1]]
        var_name <- var_mf[1]
        mf_name <- var_mf[2]
        input_only = fis_data$variables[fis_data$variables$varType == "Input", ]
        var_index <- which(input_only$varName == var_name )
        specific_input_member_function_only = fis_data$memberFunctions[fis_data$memberFunctions$varType == "Input" & fis_data$memberFunctions$varIndex== var_index, ]
        mf_index <- which(specific_input_member_function_only$mfName == mf_name)

        mf_index
      })

      # print("input_indices")
      # print(input_indices)

      output_indices <- sapply(outputs, function(x) {
        var_mf <- strsplit(x, ":")[[1]]
        var_name <- var_mf[1]
        mf_name <- var_mf[2]
        output_only = fis_data$variables[fis_data$variables$varType == "Output", ]
        var_index <- which(output_only$varName == var_name )

        specific_output_member_function_only = fis_data$memberFunctions[fis_data$memberFunctions$varType == "Output" & fis_data$memberFunctions$varIndex== var_index, ]
        mf_index <- which(specific_output_member_function_only$mfName == mf_name)

        mf_index
      })
      # print("output_indices")
      # print(output_indices)

      weight <- rule$rule_weight
      connection <- ifelse(rule$rule_connection == "AND", 1, 2)
      input_indices = unname(input_indices)
      output_indices = unname(output_indices)

      c(input_indices, output_indices, weight, connection)
    })

    

    rules <- do.call(rbind, rules)
    fis_data$fis <- addrule(fis_data$fis, rules)

    
    updateSelectInput(session, "input_output", choices = fis_data$variables$varName)
    updateSelectInput(session, "rule", choices = 1:nrow(fis_data$rules))
    # Number of rules:
    nRules <- nrow(fis_data$fis$rule)
    # nRules5 is used by subpage to show max 5 rules at once: quotient and remainder
    nRules5 <- c(nRules %/% 5, nRules %% 5)
    # list of rules
    fis_data$lRules <- vector("list")
    fis_data$lRules_label <- vector("character")
    rule_index <- 0
    # split up rules into smaller lists with max. five elements
    # value nRules5[[1]] contains the info how many lists with 5 elements are needed (quotient)
    while (rule_index < nRules5[[1]]) {
      
      fis_data$lRules[[rule_index + 1]] <- c((rule_index * 5 + 1):(rule_index * 5 + 5))
      fis_data$lRules_label[[rule_index + 1]] <- paste(min(fis_data$lRules[[rule_index + 1]]), max(fis_data$lRules[[rule_index + 1]]), sep = '-')
      rule_index <- rule_index + 1
      
    }
    # value nRules5[[2]] contains the info how long the list with less than 5 elements is (remainder)
    if (nRules5[[2]]) {
      
      fis_data$lRules[[rule_index + 1]] <- c((rule_index * 5 + 1):(rule_index * 5 + nRules5[[2]]))
      fis_data$lRules_label[[rule_index + 1]] <- paste(min(fis_data$lRules[[rule_index + 1]]), max(fis_data$lRules[[rule_index + 1]]), sep = '-')
      
    }
    updateSelectInput(session, "rule2", choices = fis_data$lRules_label)

    
  })
  
  ##################################################
  ########## Member Function Tab ###################
  ##################################################

  output$plot1 <- renderPlot({
    req(input$input_output)
    # Plot membership function for the selected variable
    varType = fis_data$variables[fis_data$variables$varName == input$input_output, "varType"]
    only_input_output = fis_data$variables[fis_data$variables$varType == varType, ]
    
    varIndex <- which(only_input_output$varName == input$input_output)
    
    
    varType = ifelse (varType == "Input", "input", "output")
    # print(varIndex)
    # print(varType)
    if (varType == "input"){
      if (input$eva == 2) {
        xx =  get_inputvalues(input)[[varIndex]]
      } else {
        xx = NULL
      }
      plotmf(fis_data$fis, varType, varIndex, main = "Membership function plots", xx = xx)
    } else {
      plotmf(fis_data$fis, varType, varIndex, main = "Membership function plots")
    }
    
    
  })
  # Download handlers for plot1
  output$plot1down <- downloadHandler(
    filename = function() { paste("plot1", "png", sep = ".") },
    content = function(file) {
      png(file,width = 1024 , height = 768 )
      
        varType = fis_data$variables[fis_data$variables$varName == input$input_output, "varType"]
        only_input_output = fis_data$variables[fis_data$variables$varType == varType, ]
        
        varIndex <- which(only_input_output$varName == input$input_output)
        
        
        varType = ifelse (varType == "Input", "input", "output")
        if (varType == "input"){
          if (input$eva == 2) {
            xx =  get_inputvalues(input)[[varIndex]]
          } else {
            xx = NULL
          }
          plotmf(fis_data$fis, varType, varIndex, main = "Membership function plots", xx = xx)
        } else {
          plotmf(fis_data$fis, varType, varIndex, main = "Membership function plots")
        }
      
      
      # print(varIndex)
      # print(varType)
      
      dev.off()
    }
  )

  ##################################################
  ##################### Rule Tab ###################
  ##################################################
  output$RulesInWords = renderPrint({
    
    showrule(fis_data$fis)
  })
  
  ##################################################
  ################ Plot rules (single) Tab #########
  ##################################################

  # Show how much a single rule applies and contributes
  output$plotRules <- renderPlot({
    req(input$rule)
    inputvalues <- get_inputvalues(input)
    r <- as.numeric(input$rule)

    
    evalfis(inputvalues, fis_data$fis)
    plotmf(fis_data$fis, "output", 1, main = "Membership function plots")
    
    if (input$eva == 2) {
      ## lines(D_x,OUT_RULE_CONS[r,], ylim=c(0,1))
      if (exists("D_x"))
      polygon(c(D_x[1], D_x, D_x[length(D_x)]), c(0, OUT_RULE_CONS[r, ], 0), border = NA,col = 'blue')
    }
  })
  
  
  # import input values:
  get_inputvalues <- function(input) {
    inputvalues <- c(input$slider1)
    NumInput = length(fis_data$fis$input)
    # add another input if statement is true
    if (NumInput >= 2) {inputvalues <- c(inputvalues, input$slider2)}
    if (NumInput >= 3) {inputvalues <- c(inputvalues, input$slider3)}
    if (NumInput >= 4) {inputvalues <- c(inputvalues, input$slider4)}
    if (NumInput >= 5) {inputvalues <- c(inputvalues, input$slider5)}
    if (NumInput >= 6) {inputvalues <- c(inputvalues, input$slider6)}
    if (NumInput >= 7) {inputvalues <- c(inputvalues, input$slider7)}
    if (NumInput >= 8) {inputvalues <- c(inputvalues, input$slider8)}
    if (NumInput >= 9) {inputvalues <- c(inputvalues, input$slider9)}
    if (NumInput >= 10) {inputvalues <- c(inputvalues, input$slider10)}
    return(inputvalues)
  }
  
  
  
  output$sliders <- renderUI({
   
    
    if(length(fis_data$fis$input) == 0){
      return()
    }
    
    NumOutputs <- length(fis_data$fis$output)
    NumInput <- length(fis_data$fis$input)
   
    # LabelList is list of labels
    LabelList <- vector("list", 10)
    
    # MinMaxList contains lower and upper limit of input values
    MinMaxList <- vector("list", 10)    
    
    # give some init values
    for (a in 1:length(MinMaxList)) {
      LabelList[[a]] <- "NULL"
      MinMaxList[[a]] <- c(0, 1)  # Default range from 0 to 1
    }
    
    # import values
    for (a in 1:NumInput) {
      LabelList[[a]] <- fis_data$fis$input[[a]]$name
      MinMaxList[[a]] <- fis_data$fis$input[[a]]$range
    }
    
    # Generate sliders based on the evaluation value
    if (input$eva == 2) {
      slider_list <- lapply(1:10, function(i) {
        if (i <= NumInput) {
          
          if (input$input_type_switch) {
            sliderInput(paste0("slider", i), LabelList[[i]], 
                        min = MinMaxList[[i]][1], max = MinMaxList[[i]][2], 
                        value = MinMaxList[[i]][1] + 0.1, step = 0.1)
          } else{
            numericInput(paste0("slider", i), LabelList[[i]], 
                          min = MinMaxList[[i]][1], max = MinMaxList[[i]][2],
                         value = MinMaxList[[i]][1]+ 0.1,step = 0.1)
          }
          
        } else {
          NULL
        }
      })
      do.call(tagList, slider_list)
    } else {
      NULL
    }
  })
 
  ##################################################
  ########## Plot rules (all) Tab ###################
  ##################################################
  # Show how much each rule applies and contributes (all rules at once)
  output$plotRulesAll <- renderPlot({
    
    req(fis_data$fis$rule)
    inputvalues <- get_inputvalues(input)
    
    ## out_name = fis$output[[1]]$name
    evalfis(inputvalues, fis_data$fis)
    
    # print(OUT_RULE_CONS)
    ## plot(D_x,D_y, type="l", main=c(out_name,D_out), col="blue", lwd=2, ylim=c(0,1))
    ## lines(c(D_out,D_out),c(-1,2),col="red",lty="dashed");
    
    par(mfrow = c(nrow(OUT_RULE_CONS), 1), mar = c(1, 1, 1, 1))
    
    for (r in 1:nrow(OUT_RULE_CONS)) {
      # TODO: if (r>1){} # add extra space between plots
      plotmf(fis_data$fis, "output", 1, main = paste("rule",as.character(r), sep=" "))#, ylim = c(0, 1))
      if (input$eva == 2) {
        ## lines(D_x,OUT_RULE_CONS[r,], ylim=c(0,1))
        if (exists("D_x"))
        polygon( c(D_x[1], D_x, D_x[length(D_x)]), c(0, OUT_RULE_CONS[r, ], 0), border = NA, col = 'blue')}
    }
  })
  ##################################################
  ########## Plot rules (chosen) Tab ###############
  ##################################################
  # Show how much each rule applies and contributes (max. five rules at once)
  output$plotRulesAll2 <- renderPlot({
    req(fis_data$fis$rule)
    inputvalues <- get_inputvalues(input)
    ## out_name = fis$output[[1]]$name
    evalfis(inputvalues, fis_data$fis)
    # vars <- mget(ls(envir = .GlobalEnv), envir = .GlobalEnv)
    
    rule_index = match(input$rule2, fis_data$lRules_label)            
    
    # print(OUT_RULE_CONS)
    ## plot(D_x,D_y, type="l", main=c(out_name,D_out), col="blue", lwd=2, ylim=c(0,1))
    ## lines(c(D_out,D_out),c(-1,2),col="red",lty="dashed");
    
    par(mfrow = c(length(fis_data$lRules[[rule_index]]), 1), mar = c(1, 1, 1, 1))
    
    for (r in fis_data$lRules[[rule_index]]) {
      # TODO: if (r>1){} # add extra space between plots
      plotmf(fis_data$fis,"output", 1, main = paste("rule",as.character(r), sep=" "))#, ylim = c(0, 1))
      if (input$eva == 2) {
        ## lines(D_x,OUT_RULE_CONS[r,], ylim=c(0,1))
        if (exists("D_x"))
        polygon( c(D_x[1], D_x, D_x[length(D_x)]), c(0, OUT_RULE_CONS[r, ], 0), border = NA, col = 'blue')}
    }
  })
  
  
  ##################################################
  ################# Output-MF Tab #################
  ##################################################
  
  # added rules as graph (like defuzzifier but single contributes by rule are visible)
  output$plotRules2 <- renderPlot({
    req(fis_data$fis$rule)
    inputvalues <- get_inputvalues(input)
    evalfis(inputvalues, fis_data$fis)
    if (exists("D_x"))
    for (r in 1:nrow(OUT_RULE_CONS)) {
      if (r == 1) {
        plot(D_x,OUT_RULE_CONS[r, ], ylim = c(0, 1), type = 'l', xlab=fis_data$fis$output[[1]]$name ,ylab="Output MF") }
      else {
        lines(D_x, OUT_RULE_CONS[r, ])}
    }
  })
  
  ##################################################
  ################# Defuzzifier Tab ################
  ##################################################
  
  # Defuzzification plot
  output$defu <- renderPlot({
    req(fis_data$fis$rule)
    inputvalues <- get_inputvalues(input)
   
    out_name = fis_data$fis$output[[1]]$name
    evalfis(inputvalues, fis_data$fis)
    
    if (exists("D_x")){
      # blue line shows output MF
      plot(D_x, D_y, type = "l", main = c(out_name, D_out), col = "blue",lwd = 2, ylim = c(0, 1))
      
      
      # red line show defuzzified value
      lines(c(D_out, D_out), c(-1, 2), col = "red", lty = "dashed")
    }
   
    
  })
  
  # Defuzzification plot (download)
  output$defudown <- downloadHandler(
    filename =  function() {paste("MFs", "png", sep = ".")},
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      
      inputvalues <- get_inputvalues(input)
      out_name = fis_data$fis$output[[1]]$name
      evalfis(inputvalues, fis_data$fis)
      
      png(file,width = 1024 , height = 768 )
      
      
      if (exists("D_x")){
        
        # blue line shows output MF
        plot(D_x, D_y, type = "l", main = c(out_name, D_out), col = "blue",lwd = 2, ylim = c(0, 1))
        
        
        # red line show defuzzified value
        lines(c(D_out, D_out), c(-1, 2), col = "red", lty = "dashed")
      }
      
      dev.off()
    }
  )
  
}


shinyApp(ui = ui, server = server)