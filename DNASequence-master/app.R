#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
options(repos = BiocManager::repositories()) # run this if you want to deploy to shinyapps.io
library(shiny)
library(ORFik)
library(GenomicFeatures)
library(magrittr)


ui <- fluidPage(
    title =  "A simple app to find DNA ORFs",
    h1("A simple app to find DNA ORFs"),
    # Application title
    h3("Enter your DNA sequence below"),
    textInput("seq_in", label = NULL, value = "ATGAAATGAAGTAAATCAAAACAT"),
    h4("The complementary strand is"),
    verbatimTextOutput(outputId = "seq_com"), 
    h4("The ORF on positive strand is"),
    verbatimTextOutput(outputId = "orf_pos"),    
    h4("The ORF on negative strand is"),
    verbatimTextOutput(outputId = "orf_neg")
)

server <- function(input, output) {

    seq <- reactive({
        DNAStringSet(input$seq_in)
    }) %>% debounce(millis = 1000)
    
    observeEvent(seq, {
        output$seq_com <- renderPrint({
            reverseComplement(seq())[[1]] %>% as.character() %>% cat()
        })      
        output$orf_pos <- renderPrint({
            findORFs(seq(), startCodon = "ATG", minimumLength = 0) %>% 
                {extractTranscriptSeqs(x = seq()[[1]], .)} %>% as.character()%>% {
                    if(length(.) < 1){
                        cat("not found")
                    } else {
                        names(.) <- NULL
                        cat(.)
                    }
                }
        })        
        output$orf_neg <- renderPrint({
            findORFs(reverseComplement(seq()),
                     startCodon = "ATG", minimumLength = 0)%>% 
                {extractTranscriptSeqs(x = seq()[[1]], .)} %>% as.character() %>% {
                    if(length(.) < 1){
                        cat("not found")
                    } else {
                        names(.) <- NULL
                        cat(.)
                    }
                }
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
