source("env.R")

## UI ----
shinyUI(navbarPage("Custom Scatterplots", id = "tabs",
                 tabPanel("Plot",
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
                                                                          selectInput("yn", "Negative Score", choices = NULL),
                                                                          selectInput("yp", "Positive Score", choices = NULL)),
                                                         
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
                              ), #end all_highlight collapse panel
                              
                              #Click to plot or reset plot
                              actionButton("plot", "Construct Plot", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                              
                              
                              
                            ),
                            
                            #---------------------------MainPanel-------------------
                            mainPanel(
                              #If requested, show data preview - useful for helping choose which columns will be used for each axis, and verifying correct data was selected
                              conditionalPanel(condition = "input.preview == true", tableOutput("head_data")),
                              #If requested, show data summary - useful for seeing value ranges when selecting data highlight on plot
                              conditionalPanel(condition = "input.summary == true", verbatimTextOutput("summary_data")),

                              conditionalPanel("input.num_gps >=1 & 
                       (input.type1 == `Plot Click` | input.type2 == `Plot Click` | input.type3 == `Plot Click`)",
                                               radioButtons("current_gp", "Current group for labelling", 
                                                            choices = "none", inline = T)),
                              fluidRow(column(8, plotOutput("scatter", click = "plot_click", hover = "plot_hover", brush = "plot_brush")),
                                       column(3, tableOutput("nT_hover"))),
                              
                              #Display warnings for unlabelled points
                              verbatimTextOutput(outputId='ggplot_warnings'),
                              
                              #Download
                              fluidRow(column(4, textInput("dl_plot_name", "Save plot image as...", value = "plotImageScatter")),
                                       column(4, tags$br(), downloadButton("dl_plot", "Save plot (.pdf)", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              fluidRow(column(4, textInput("dl_genes_name", "Save selected genes as...", value = "selectedGenes")),
                                       column(4, tags$br(), downloadButton("dl_genes", "Save genes (.txt)", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                       ),
                              radioButtons("dl_genes_order", "Order genes table by...", choices = c("Name", "Group", "X-Value", "Y-Value"), inline = TRUE),
                              tags$br()
                              
                            ) #end MainPanel
                          ) #end sidebarLayout
                          
                 ), #end Plot tab
                 
                 tabPanel("Help", 
                          titlePanel("Help Documentation"),
                          htmlOutput("help"))
  
) #end navbar Page
) #end ui
