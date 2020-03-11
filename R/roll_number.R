library(shiny)

ui <- fluidPage(h1("Roll a number between 0-10"),
    numericInput(inputId = "i", label = "number", width = "10%", min = 0, max = 1, 
                 value = 0),
    actionButton(inputId = "startit", label = "Start"),
    actionButton(inputId = "stopit", label = "Stop")

)

server <- function(input, output, session) {
    stop_flag <- reactiveVal(FALSE)
    observeEvent(input$stopit,  {
        stop_flag(TRUE)
    })
    observeEvent(input$startit, {
        stop_flag(FALSE)
    })
    observeEvent({input$i; input$startit},{
        if (!stop_flag()) {
            session$sendInputMessage("i", list(value = if (input$i < 10) input$i + 1 else 0))
        }
    })
}

shinyApp(ui, server)
