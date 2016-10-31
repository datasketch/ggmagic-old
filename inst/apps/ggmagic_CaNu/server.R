library(shiny)
library(tidyverse)
library(pystr)
library(rio)
library(rhandsontable)
library(ggmagic)
library(datafringe)

shinyServer(function(input, output, session){

  ## DATA INPUT SECTION
  output$dataInputSectionDebug <- renderPrint({
    data <- data()
    if(is.null(data)) return()
    guessFtype(data)
  })

  output$dataInputSection <- renderUI({
    dataInputTypeChoices <- list("Copiar y Pegar"="pasted",
                                 "Cargar archivo"="fileUpload",
                                 "Datos de muestra"="sampleData")

    list(
      radioButtons("dataInputType", "upload Data",
                   choices = dataInputTypeChoices, selected = dataInputTypeChoices[[1]])
    )
  })

  output$dataInputControls <- renderUI({
    textareaTpl <- '<textarea id="inputDataPasted" placeholder="{prefill}"></textarea>'
    textArea <- pystr_format(textareaTpl, prefill = "Pegue datos aquí")
    dataInputControls <- list(
      "pasted" = HTML(textArea),
      "fileUpload" =  fileInput('inputDataUpload', 'Choose CSV File',
                                accept=c('text/csv',
                                         'text/comma-separated-values,text/plain',
                                         '.csv','.xls')),
      "sampleData" = selectInput("inputDataSample","Seleccione Datos de Muestra",
                                 choices = list("First"="data/sample/first.csv",
                                                "Second"="data/sample/second.csv"))
    )
    dataInputControls[[input$dataInputType]]
  })

  inputData <- reactive({
    inputType <- input$dataInputType
    #readDataFromInputType(inputType)
    if(inputType == "pasted"){
      if(input$inputDataPasted == "")
        return()
      df <- read_tsv(input$inputDataPasted)
    }
    if(inputType ==  "fileUpload"){
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(),input$inputDataUpload$name)
      file.copy(old_path,path)
      df <- rio::import(path)
    }
    if(inputType ==  "sampleData"){
      file <- input$inputDataSample
      df <- read_csv()
    }
    return(df)
  })

  output$dataInputPreview <- renderRHandsontable({
    d <- inputData()
    if(is.null(inputData()))
      return()
    h <- rhandsontable(d, useTypes = FALSE, readOnly = FALSE,
                       width = "100%",height = 200) %>%
      hot_table(stretchH = "none") %>%
      hot_cols(manualColumnMove = TRUE)
    h
  })

  data <- reactive({
    if(is.null(input$dataInputPreview))
      return()
    as_tibble(hot_to_r(input$dataInputPreview))
  })

  output$dataControls <- renderUI({
    if(is.null(data())) return()
    d <- data()
    list(
      selectizeInput("selectedCols","Select Cols",
                     choices = names(d),
                     selected = names(d)[1:2],
                     multiple = TRUE)
    )
  })

  fringe <- reactive({
    if(is.null(input$selectedCols)) return()
    if(is.null(data())) return()
    selectedCols <- input$selectedCols
    data <- data()
    d <- data %>% select_(.dots = selectedCols)
    #f <- fringe(d)
    d
  })

  ## VIZ SECTION

  output$debugViz <- renderPrint({
    #if(is.null(fringe())) return()
    #data <- fringe()
    #ftype <- "Ca-Nu"
    #data
    #guessFtype(data)
    d <- data()
    names(d)[1:2]
  })

  output$vizControls <- renderUI({
    if(is.null(fringe())) return()
    data <- fringe()
    #ftype <- "Ca-Nu"
    ftype <- guessFtype(data)
    # pattern <- paste(ftype,collapse = "")
    # pattern <- paste0("_",pattern,"\\.")
    pattern <- "bar.*_CaNu\\."
    bars_CaNu <- ggList(pattern)
    list(
      textInput("whichVizPattern",label = "Which Viz Pattern", value = "CaNu")
    )
  })
    output$vizControls2 <- renderUI({
    if(is.null(fringe())) return()
    data <- fringe()
    if(is.null(input$whichVizPattern)) return()
    #ftype <- "Ca-Nu"
    ftype <- guessFtype(data)
    # pattern <- paste(ftype,collapse = "")
    # pattern <- paste0("_",pattern,"\\.")
    #pattern <- "bar.*_CaNu\\."
    pattern <- input$whichVizPattern %||% NULL
    bars_CaNu <- ggList(pattern)
    list(
      selectInput("whichViz",label = "Which Viz",choices = bars_CaNu),
      textInput("vizTitle", "Title"),
      #textInput("xLabel","xLabel"),
      #textInput("yLabel","yLabel"),
      br()
    )
  })

  plot <- reactive({
    data <- fringe()
    if(is.null(data)) return("null data")
    gg <- input$whichViz
    title <- input$vizTitle
    #xLabel <- input$xLabel
    #yLabel <- input$yLabel
    # p <- do.call(gg,list(data, title = title,
    #                      xLabel = xLabel, yLabel = yLabel))
    p <- do.call(gg,list(data, title = title))
    p
  })

  output$viz <- renderUI({
    p <- plot()
    if(is.null(p)) return()
    list(
      renderPlot(p),
      downloadButton('downloadData', 'Download')
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {

      paste0(input$vizTitle,gsub(" ","_",now()), ".jpg")
    },
    content = function(file) {
      p <- plot()
      save_ggmagic(p,file)
    }
  )

  ## PUBLISH SECTION

  output$debug <- renderPrint({
  })


})





