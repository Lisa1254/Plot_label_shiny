library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title="Scatterplot with Custom Labels"),
  ###point 1.
  dashboardSidebar(sidebarMenuOutput("Semi_collapsible_sidebar")),
  ###point 2.
  dashboardBody(tags$head(tags$link(rel = "stylesheet",
                                    type = "text/css", href = "style.css")))
)