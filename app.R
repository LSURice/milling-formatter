
library(shiny)
library(stringr)
library(readxl)
library(xlsx)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Milling Data Formatter"),
    fluidRow(
        column(8,
               fileInput("upload", "Upload a file", accept="xlsx"),
               downloadButton("download")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        req(input$upload)
        infile <- as.data.frame(read_excel(input$upload$datapath, col_names=F))
        new <- c()
	print(infile[,1])
        for(ent in infile[,1]){
            if(ent == "NO SEED"){
                new <- c(new, c("NO SEED", "NO SEED"))
            } else{
                new <- c(new, ent)
            }
        }
        dataMat <- matrix(new, ncol=3, byrow=T)
        colnames(dataMat) <- c("observationunit_name", "LSU01:0000101",
			      "LSU01:0000100")
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
            paste0("formatted_",input$upload$name, ".xls")
        },
        content = function(file){
            write.xlsx(data(), file, row.names=F)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
