library(shiny)
library(shinyjs)
library(rhandsontable)


shinyUI(fluidPage(

  titlePanel(""),
  fluidRow(
    useShinyjs()
  ),
  fluidRow(id="dataPage",
           column(6,
                  div(id = "dataControlsSection",
                      #verbatimTextOutput("debug"),
                      uiOutput("dataInputSection"),
                      uiOutput("dataInputControls"),
                      verbatimTextOutput("dataInputSectionDebug"),
                      uiOutput("dataControls")
                  )
           ),
           column(6,
                  div(id="dataSection",
                      rHandsontableOutput("dataInputPreview",height = 200),
                      br()
                  )

           )),
  fluidRow(id="vizPage",
           column(6,
                  div(id = "vizControlsSection",
                      uiOutput("vizControls"),
                      uiOutput("vizControls2")
                  ),
                  verbatimTextOutput("debugViz")
           ),
           column(6,
                  uiOutput("viz")           )
  )
))
