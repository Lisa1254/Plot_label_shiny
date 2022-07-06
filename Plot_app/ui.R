source("env.R")

## UI ----
shinyUI(navbarPage("Custom Scatterplots", id = "tabs",
                   #--------PlotPanel-------------------
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
                                                         radioButtons("srctype", "Data", choices = c("Input", "FDR Example", "Volcano Example")),
                                                         conditionalPanel("input.srctype == `Input`",
                                                                          fileInput("txt_data", "Tab-delimited file (.txt or .tsv), or .csv file", width = "100%", accept = c(".csv", ".txt", ".tsv")),
                                                                          bsAlert("data_alert")),
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
                                                         style = "success"), #end Data Attributes collapse panel
                                         bsCollapsePanel("X-axis Attributes",
                                                         selectInput("x", "X values", choices = NULL),
                                                         textInput("xlab", "Attribute name for X values", placeholder = "e.g. LFC"),
                                                         style = "success"
                                         ), #end X-axis attribtes collapse panel
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
                                         ), #end Y attributes
                                         bsCollapsePanel("Gene highlight groups",
                                                         #Use button input to add groups
                                                         actionButton("add_gp", "Add group", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                         #Alert for too many groups
                                                         bsAlert("gp_alert"),
                                                         tags$br(),
                                                         #Use hidden slider to allow conditional groups
                                                         conditionalPanel("input.num_gps >=8",
                                                                          sliderInput("num_gps", "How many highlighted groups?", min=0, max=7, step=1, value=0)),
                                                         #Display number of groups selected
                                                         map_conds,
                                                         style = "info"
                                         ) #end gene highlight groups collapse panel
                              ), #end collapseData bsCollapse

                              
                              #bsCollapse(id = "all_highlight",
                                         
                              #), #end all_highlight collapse panel
                              
                              #Click to plot or reset plot
                              actionButton("plot", "Construct Plot", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                              
                              
                              
                            ),
                            
                            #---------------------------MainPanel-------------------
                            mainPanel(
                              textOutput("file_name"),
                              #If requested, show data preview - useful for helping choose which columns will be used for each axis, and verifying correct data was selected
                              conditionalPanel(condition = "input.preview == true", tableOutput("head_data")),
                              #If requested, show data summary - useful for seeing value ranges when selecting data highlight on plot
                              conditionalPanel(condition = "input.summary == true", verbatimTextOutput("summary_data")),
                              
                              checkboxGroupInput("inc_groups", "Groups to include in plot/save", choices = NULL, inline = TRUE),
                              conditionalPanel("input.num_gps >=1",
                                               radioButtons("current_gp", "Current group for labelling", 
                                                            choices = "none", inline = T)),

                              fluidRow(column(8, plotOutput("scatter", click = "plot_click", hover = "plot_hover", brush = "plot_brush")),
                                       column(3, tableOutput("nT_hover"))),
                              
                              #Give option to show all labels, and display warnings for unlabelled points
                              checkboxInput("inf_over", "Show all labels"),
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
            
          #---------------OtherPanels----------------
                 
                 tabPanel("Help", 
                          titlePanel("Help Documentation"),
                          htmlOutput("help")),
                 
                 tabPanel("Example 1",
                          titlePanel("Example for 2 Screen Comparison"),
                          imageOutput("fdr_image"),
                          htmlOutput("fdr_desc"),
                          tableOutput("fdr_table")),
                 tabPanel("Example 2",
                          titlePanel("Example of Volcano Plot from MAGeCK Data"),
                          imageOutput("volcano_image"),
                          htmlOutput("volcano_desc"),
                          tableOutput("volcano_table")),
                  tabPanel("Example 3",
                          titlePanel("Example of Non-Numeric X-Input"),
                          imageOutput("nnx_image1"),
                          htmlOutput("nnx_desc1"),
                          imageOutput("nnx_image2"),
                          htmlOutput("nnx_desc2"),
                          tableOutput("nnx_table"))
  
) #end navbar Page
) #end ui
