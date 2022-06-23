library(shiny)
library(shinydashboard)
shinyServer(
  function(input,output){
  
  output$Semi_collapsible_sidebar=renderMenu({
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Charts", icon = icon("bar-chart-o"),
               menuSubItem("Sub-item 1", tabName = "subitem1"),
               menuSubItem("Sub-item 2", tabName = "subitem2")
      ))
    
  })
  })