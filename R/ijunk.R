ijunk <- function(data, init_junk = NULL) {

  if (!requireNamespace("shiny", quietly = TRUE))
    stop("shiny package should be installed to use this function")
  if (!requireNamespace("rclipboard", quietly = TRUE))
    stop("rclipboard package should be installed to use this function")
  if (!requireNamespace("esquisse", quietly = TRUE))
    stop("esquisse package should be installed to use this function")
  if (!requireNamespace("miniUI", quietly = TRUE))
    stop("miniUI package should be installed to use this function")
  
  data <- as.data.frame(data)
  all_categories <- getindexcat(data)
  data_name <- deparse(substitute(data))
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Define junk categories for specific MCA"),
    
    miniUI::miniContentPanel(
      shiny::fillRow(
        flex = c(2,2),
        height = "40px",
        shiny::textInput("suffixes", label = NULL, value = "Suffixes of junk categories"),
        shiny::actionButton("do", label = " Dump", icon = shiny::icon("trash"), 
                     style="color: #2c2c2c; background-color: #d3d3d3; border-color: #545454")
      ),
      shiny::em("Use '|' to separate multiple suffixes"),
      shiny::hr(),
      esquisse::dragulaInput(inputId = "categories",
                             sourceLabel = "Active categories",
                             targetsLabels = "Junk categories",
                             targetsIds = "junks",
                             choices = all_categories,
                             ncolSource = 1,
                             ncolGrid = 2,
                             height = "200px"),
      shiny::br(),
      shiny::h5(shiny::strong("R script example")),
      rclipboard::rclipboardSetup(),
      shiny::verbatimTextOutput("script"),
      shiny::uiOutput("clip")
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
    
    shiny::observeEvent(input$do, {
      pattern <- paste(sapply(unlist(strsplit(input$suffixes,split="|",fixed=TRUE)), function(x) paste0(x,"$")),collapse="|")
      new_junk <- grep(pattern, all_categories, value=TRUE)
      updated_junk <- union(new_junk,input$categories$target$junks)
      esquisse::updateDragulaInput(session = session,
                                   inputId = "categories",
                                   choices = all_categories[!(all_categories %in% updated_junk)],
                                   selected = list(junks = updated_junk))  
    })

    scriptInput <- shiny::reactive({
      code <- "# This could be like this\n"
      code <- paste0(code, sprintf("junk <- c(%s)\n", paste(sapply(input$categories$target$junks,function(x) paste0('"',x,'"')),collapse=",")))
      code <- paste0(code, sprintf("mca <- speMCA(%s, excl = junk)", data_name))
      return(code)
    })
    
    output$script <- shiny::renderPrint({
      shiny::req(input$categories$target$junks)
      cat(scriptInput())
    })
    
    output$clip <- shiny::renderUI({
      shiny::req(input$categories$target$junks)
      rclipboard::rclipButton(
        inputId = "clipbtn",
        label = "Copy",
        clipText = as.character(scriptInput()), 
        icon = shiny::icon("clipboard")
      )
    })
    
    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- input$categories$target$junks
      shiny::stopApp(returnValue)
    })
  }
  
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(""))
}
