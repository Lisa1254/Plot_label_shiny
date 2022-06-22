library(shiny)

list_split <- function(in_var){
  if(in_var == "") {return(NULL)}
  
  s <- unlist(strsplit(
    unlist(strsplit(
      unlist(strsplit(
        unlist(strsplit(toupper(in_var), 
                        c("[\n]"))),
        c("[,]"))),
      c("[:space:]"))),
    c("\\s")))
  s <- s[s != ""]
  
  return(s)
}

ui <- fluidPage(
  textAreaInput("genes1", "Genes list:", "", width="200px", height="240px"),
  verbatimTextOutput("parsed1"),
  textAreaInput("genes2", "Genes list:", "", width="200px", height="240px"),
  verbatimTextOutput("parsed2")
)

server <- function(input, output, session) {
  #Function for splitting list of genes into individual items
  list_genes1 <- reactive({
    list_split(input$genes1)
  })
  
  output$parsed1 <- renderPrint(list_genes1())
  output$parsed2 <- renderPrint(list_split(input$genes2))
}

shinyApp(ui, server)