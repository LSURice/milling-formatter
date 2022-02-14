
library(shiny)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Milling Data Formatter"),
    fluidRow(
        column(8,
               fileInput("upload", "Upload a file", accept="csv"),
               downloadButton("download")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        req(input$upload)
        infile <- read.csv(input$upload$datapath, header=F)
        new <- c()
        for(ent in infile[,1]){
            if(ent == "NO SEED"){
                new <- c(new, c("NO SEED", "NO SEED"))
            } else{
                new <- c(new, ent)
            }
        }
        dataMat <- matrix(new, ncol=3, byrow=T)
        colnames(dataMat) <- c("plot_number", "Total", "Whole")
        dataMat[,2] <- str_replace_all(dataMat[,2], "[:alpha:]", "")
        dataMat[,3] <- str_replace_all(dataMat[,3], "[:alpha:]", "")
        dataMat[,2] <- str_replace_all(dataMat[,2], "\\?", "")
        dataMat[,3] <- str_replace_all(dataMat[,3], "\\?", "")
        dataMat[,2][dataMat[,2] == " "] <- "NO SEED"
        dataMat[,3][dataMat[,3] == " "] <- "NO SEED"
        dataMat
    })
    output$download <- downloadHandler(
        filename = function(){
            paste0("formatted_",input$upload$name)
        },
        content = function(file){
            write.csv(data(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
