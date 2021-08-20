library(shiny)
library(shinythemes)
library(tidyverse)
source("api.r")


updateData <- function(input, output) {
  output$data <- DT::renderDataTable(
    deleteButtonColumn(getAllInfractions(), "del")
    )
  output$agentData <- DT::renderDataTable(
    deleteButtonColumn(getInfractions(input$agentSelect), "del"))
}

updatePlot <- function(input, output) {
  data = getThreeMonths(input$agentSelectPlot)
  if (nrow(data) > 0) {
    output$agentPlot <- renderPlot({
      ggplot(data, aes(x = type, fill = type)) +
        geom_bar() + scale_x_discrete(drop = FALSE)
    })
    output$noPlot <- renderText({""})
  }
  else {
    output$agentPlot <- NULL
    output$noPlot <- renderText({"<H3>Agent has no infractions in last 3 months.</H3>"})
  }
}

updateAgents <- function(input, output, session) {
  output$agents <- DT::renderDataTable(agents, options = list(pageLength = 10))
  output$agents2 <- DT::renderDataTable(agents, options = list(pageLength = 10))
  updateSelectInput(session, "agentSelect", "Agent Names", choices = getNames())
  updateSelectInput(session, "agentSelectDel", "Agent Names", choices = getNames())
  updateSelectInput(session, "agentSelectPlot", "Agent Names", choices = getNames())
  updateSelectInput(session, "agentInfractionsSelect", "Agent Names", choices = getNames())
  updateTextInput(session, "agentName", "Agent Name", value = "")
}

deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(id, i, sep="_"),
        label = "Remove",
        onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'))
  }
  
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(cbind(df, delete = deleteCol),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(
                    list(targets = 1, sortable = FALSE))
                ))
}

parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}


###############################################################################

ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Agent Records"),
  
  sidebarLayout(
    sidebarPanel(
      p("Author: Shai Guelman"),
      p("Last updated: August 20, 2021")
    ),
      
    mainPanel(
      
      tabsetPanel(
        tabPanel("Add Agent",
                 DT::dataTableOutput("agents"),
                 
                 br(), br(), br(),
                 
                 textInput(
                   inputId = "agentName",
                   label = "Agent to insert"
                 ),
                 
                 actionButton(
                   inputId = "addAgent",
                   label = "Add Agent"
                 )),
        
        tabPanel("Delete Agent",
                 DT::dataTableOutput("agents2"),
                 
                 br(), br(), br(),
                 
                 htmlOutput(
                   outputId = "delAgentText"
                 ),
                 
                 selectInput(
                   inputId = "agentSelectDel",
                   label = "Agent",
                   choices = getNames()
                 ),
                 
                 actionButton(
                   inputId = "deleteAgent",
                   label = "Remove Agent"
                 )),
        
        tabPanel("All Infractions",
                 selectInput(
                   inputId = "agentInfractionsSelect",
                   label = "Filter",
                   choices = c("All", getNames())
                 ),
                 
                 br(), br(),
                 
                 DT::dataTableOutput("data")
        ),
        
        tabPanel("Add Infraction",
                 selectInput(
                   inputId = "agentSelect",
                   label = "Agent",
                   choices = getNames()
                 ),
                 
                 selectInput(
                   inputId = "infractionType",
                   label = "Infraction type",
                   choices = getTypes()),
                 
                 dateInput(
                   inputId = "date",
                   label = "date"
                 ),
                 
                 actionButton(
                   inputId = "addInfraction",
                   label = "Add Infraction"
                 ),
                 
                 br(), br(), br(),
                 
                 DT::dataTableOutput("agentData")       
        ),
        
        tabPanel("Last 3 months",
                 
                 selectInput(
                   inputId = "agentSelectPlot",
                   label = "Agent",
                   choices = getNames()
                 ),
                 
                 plotOutput("agentPlot"),
                 
                 htmlOutput(
                   outputId = "noPlot"
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })

  observeEvent(input$addAgent, {
    addAgent(input$agentName)
    updateAgents(input, output)
  })
  
  observeEvent(input$addInfraction, {
    addInfraction(input$infractionType, input$date, input$agentSelect)
    updateData(input, output)
  })
  
  observeEvent(input$deletePressed, {
    rownum <- parseDeleteEvent(input$deletePressed)
    deleteInfraction(rownum)
    updateData(input, output)
  })
  
  observeEvent(input$deleteAgent, {
    deleteAgent(input$agentSelectDel)
    updateAgents(input, output, session)
    updateData(input, output)
  })
  
  observeEvent(input$agentInfractionsSelect, {
    if (input$agentInfractionsSelect == "All") {
      output$data <- DT::renderDataTable({
        deleteButtonColumn(getAllInfractions(), "del")
      })
    }
    else {
      output$data <- DT::renderDataTable({
        deleteButtonColumn(getInfractions(input$agentInfractionsSelect), "del")
      })
    }
  })
  
  observeEvent(input$agentSelectPlot, {
    updatePlot(input, output)
  })
  
  updateData(input, output)
  updateAgents(input, output, session)
  output$delAgentText <- renderText({"<h3>DELETE AGENT</h3><br><br>"})
}


shinyApp(ui, server)