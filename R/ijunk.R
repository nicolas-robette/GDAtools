# library(shiny)
# library(miniUI)
# 
# library(rclipboard)
# library(esquisse)
# 
# library(GDAtools)
# 
# data(Music)
# str(Music)
# Music_active <- Music[,1:5]
# getindexcat(Music_active)

ijunk <- function(data, init_junk = NULL) {
  
  all_categories <- GDAtools::getindexcat(data)
  data_name <- deparse(substitute(data))
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Define junk categories for specific MCA"),
    
    miniUI::miniContentPanel(
      fillRow(
        flex = c(2,2),
        height = "40px",
        textInput("suffixes", label = NULL, value = "Suffixes of junk categories"),
        actionButton("do", label = " Dump", icon = icon("trash"), 
                     # style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                     style="color: #2c2c2c; background-color: #d3d3d3; border-color: #545454")
      ),
      em("Use '|' to separate multiple suffixes"),
      hr(),
      esquisse::dragulaInput(inputId = "categories",
                             sourceLabel = "Active categories",
                             targetsLabels = "Junk categories",
                             targetsIds = "junks",
                             choices = all_categories,
                             ncolSource = 1,
                             ncolGrid = 2,
                             height = "200px"),
      br(),
      h5(strong("R script example")),
      rclipboard::rclipboardSetup(),
      verbatimTextOutput("script"),
      uiOutput("clip")
    )
  )
  
  server <- function(input, output, session) {

    if(!is.null(init_junk)) {
      if(is.numeric(init_junk)) {
        esquisse::updateDragulaInput(session = session,
                                     inputId = "categories",
                                     choices = all_categories[-init_junk],
                                     selected = list(junks = all_categories[init_junk]))
      }
      if(is.character(init_junk)) {
        esquisse::updateDragulaInput(session = session,
                                     inputId = "categories",
                                     choices = all_categories[!(all_categories %in% init_junk)],
                                     selected = list(junks = init_junk))        
      }
    }
    
    observeEvent(input$do, {
      # session$sendCustomMessage(type = 'testmessage',
      #                           message = 'Thank you for clicking')
      pattern <- paste(sapply(unlist(strsplit(input$suffixes,split="|",fixed=TRUE)), function(x) paste0(x,"$")),collapse="|")
      new_junk <- grep(pattern, all_categories, value=TRUE)
      # new_junk <- input$suffixes[input$suffixes %in% all_categories]
      updated_junk <- union(new_junk,input$categories$target$junks)
      esquisse::updateDragulaInput(session = session,
                                   inputId = "categories",
                                   choices = all_categories[!(all_categories %in% updated_junk)],
                                   selected = list(junks = updated_junk))  
    })

    scriptInput <- reactive({
      code <- "# This could be like this\n"
      code <- paste0(code, sprintf("junk <- c(%s)\n", paste(sapply(input$categories$target$junks,function(x) paste0('"',x,'"')),collapse=",")))
      code <- paste0(code, sprintf("mca <- speMCA(%s, excl = junk)", data_name))
      return(code)
    })
    
    output$script <- renderPrint({
      req(input$categories$target$junks)
      cat(scriptInput())
    })
    
    output$clip <- renderUI({
      req(input$categories$target$junks)
      rclipboard::rclipButton(
        inputId = "clipbtn",
        label = "Copy",
        clipText = as.character(scriptInput()), 
        icon = icon("clipboard")
      )
    })
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- input$categories$target$junks
      stopApp(returnValue)
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer(""))
}

# myjunk <- ijunk(Music_active)
# myjunk
# 
# myjunk <- ijunk(Music_active, c(3,6,9,15))
# myjunk
# 
# myjunk <- ijunk(Music_active, c("FrenchPop.NA","Rap.NA","Rock.NA","Classical.NA"))
# myjunk