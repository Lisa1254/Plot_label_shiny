## Libraries and Environment ---- 

library(shiny)
library(shinyBS)
library(purrr)
library(ggplot2)
library(ggrepel)

# Colour Choices and corresponding hex or R names
col_hex <- setNames(c("lightgray", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 
                    c("Light Gray", "Black", "Orange", "Sky Blue", "Bluish Green", "Yellow", "Blue", "Red", "Purple"))

#Reusable conditional for Highlight Groups
condPan01 <- function(number) {
  conditionalPanel(paste0("input.num_gps >=", as.numeric(number)),
                   bsCollapse(id = paste0("collapse_gp", number),
                              bsCollapsePanel(paste0("Attributes for Highlight Group ", number),
                                              textInput(paste0("gp", number), label = paste0("Group Name: ")),
                                              selectInput(paste0("col", number), label = paste0("Colour for Group ", number),
                                                          choices = col_hex),
                                              radioButtons(paste0("type", number), label = paste0("Input Type for Group ", number), choices = c("Plot Interaction", "Specified Values", "Gene Input")),
                                              conditionalPanel(paste0("input.type", number, " == `Specified Values`"),
                                                               numericInput(paste0("minX", number), "X Value Minumum", value = 0),
                                                               numericInput(paste0("maxX", number), "X Value Maximum", value = 0),
                                                               numericInput(paste0("minY", number), "Y Value Minumum", value = 0),
                                                               numericInput(paste0("maxY", number), "Y Value Maximum", value = 0)
                                                               
                                              ), #conditional panel for specified vales end
                                              conditionalPanel(paste0("input.type", number, " == `Gene Input`"),
                                                               textAreaInput(paste0("genes", number), "Genes list:", "", width="200px", height="240px")
                                              ), #Conditional panel  for gene input end
                                              style = "info"
                                              ) #End collapse panel
                              ) #End bsCollapse
                   
                   
                   
                  
  ) #end for group's conditional panel
}

#Apply conditional panel for 3 possible group inputs
map_conds <- map(c(1,2,3), condPan01)

#Function to parse gene list input 
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

#Function to return genes within desired range
subset_genes <- function(df, colx, minx, maxx, all_y, miny, maxy, vecN){
  genes_ind <- which((all_y >= miny) & (all_y <= maxy) 
                     & (df[,colx] >= minx) & (df[,colx] <= maxx))
  genes_sub <- vecN[genes_ind]
  return(genes_sub)
}

## UI ----
ui <- fluidPage(
  #Title
  titlePanel("Scatterplot with Custom Labels"),
  
  #Sidebar Panel for data inputs & customization, Main Panel for plot
  sidebarLayout(
    #---------------------------SideBar----------------
    
    sidebarPanel(
      
      bsCollapse(id = "collapseData", open = "Source Data",
                 bsCollapsePanel("Source Data",
                                 #Source data
                                 fileInput("txt_data", "Supply tab delimited .txt file", width = "100%"),
                                 #Option to preview data two ways
                                 checkboxInput("preview", "Show preview of data"),
                                 checkboxInput("summary", "Show summary of data"),
                                 style = "success"
                                 ), #end collapse panel Source Data
                 bsCollapsePanel("Data Attributes",
                                 #Select data point labels
                                 selectInput("name", "Labels for data points", choices = NULL),
                                 #Colour data points
                                 selectInput("col_m", "Main colour", choices = col_hex),
                                 style = "success") #end Data Attributes collapse panel
                 ), #end collapseData bsCollapse

      
      #Select and label X-values
      bsCollapse(id = "collapseXs",
                 bsCollapsePanel("X-axis Attributes",
                                 selectInput("x", "X values", choices = NULL),
                                 textInput("xlab", "Attribute name for X values", placeholder = "e.g. LFC"),
                                 style = "success"
                                 ) #end X-axis attribtes collapse panel
                 ), #end collapseXs bsCollapse
      
      #Select and label Y-values
      bsCollapse(id = "collapseYs",
                 bsCollapsePanel("Y-axis Attributes",
                                 radioButtons("num_y", "Number of Inputs for Y-axis", choices = c("1", "2"), inline = TRUE),
                                 conditionalPanel(condition = "input.num_y == 1",
                                                  selectInput("y1", "Y values", choices = NULL)),
                                 conditionalPanel(condition = "input.num_y == 2",
                                                  column(6, selectInput("yn", "Negative Score", choices = NULL)),
                                                  column(6, selectInput("yp", "Positive Score", choices = NULL))),
                                 textInput("ylab", "Attribute name for y values", placeholder = "e.g. Score (log10)"),
                                 checkboxGroupInput("y_trans", "Transformations for y-axis:", c("log10", "reverse")),
                                 style = "success"
                 ) #end Y attributes
      ), #end collapseYs
      
      bsCollapse(id = "all_highlight",
                 bsCollapsePanel("Gene highlight groups",
                                 sliderInput("num_gps", "How many highlighted groups?", min=0, max=3, step=1, value=0),
                                 map_conds,
                                 style = "success"
                                 ) #end gene highlight groups collapse panel
                 ) #end all_highlight collapse panel
      
      
      
    ),
    
    #---------------------------MainPanel-------------------
    mainPanel(
      #If requested, show data preview - useful for helping choose which columns will be used for each axis, and verifying correct data was selected
      conditionalPanel(condition = "input.preview == true", tableOutput("head_data")),
      #If requested, show data summary - useful for seeing value ranges when selecting data highlight on plot
      conditionalPanel(condition = "input.summary == true", verbatimTextOutput("summary_data")),
      
      #Click to plot or reset plot
      actionButton("plot", "Generate/Reset Scatterplot"),
      conditionalPanel("input.num_gps >=1",
                       radioButtons("current_gp", "Current group for labelling", 
                                    choices = "none", inline = T)),
      fluidRow(column(9, plotOutput("scatter", click = "plot_click", hover = "plot_hover", brush = "plot_brush")),
               column(3, tableOutput("nT_hover"))),
      
      #Display warnings for unlabelled points
      verbatimTextOutput(outputId='ggplot_warnings'),
      
      #Download
      fluidRow(column(4, downloadButton("dl_plot", "Save plot as pdf")),
               column(4, actionButton("dl_genes", "Save selected genes"))),
               #column(4, downloadButton("dl_genes", "Save selected genes"))),
      tags$br(),
      verbatimTextOutput("test_df")
    ) #end MainPanel
  ) #end sidebarLayout
  
  
) #end ui

