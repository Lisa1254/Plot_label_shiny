## Libraries and Environment ---- 

library(shiny)
library(shinyBS)
library(purrr)
library(ggplot2)
library(ggrepel)

# Colour Choices and corresponding hex or R names
col_hex <- setNames(c("lightgray", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"), 
                    c("Light Gray", "Sky Blue", "Orange", "Bluish Green", "Yellow", "Blue", "Red", "Purple", "Black"))

#Reusable conditional for Highlight Groups
condPan01 <- function(number) {
  conditionalPanel(paste0("input.num_gps >=", as.numeric(number)),
                   bsCollapse(id = paste0("collapse_gp", number),
                              bsCollapsePanel(paste0("Attributes for Highlight Group ", number),
                                              textInput(paste0("gp", number), label = paste0("Group Name: ")),
                                              selectInput(paste0("col", number), label = paste0("Colour for Group ", number),
                                                          choices = col_hex, selected = col_hex[as.numeric(number)+1]),
                                              radioButtons(paste0("type", number), label = paste0("Input Type for Group ", number), choices = c("Plot Interaction", "Specified Values", "Gene Input")),
                                              conditionalPanel(paste0("input.type", number, " == `Specified Values`"),
                                                               textInput(paste0("minX", number), "X Value Minumum", value = 0),
                                                               textInput(paste0("maxX", number), "X Value Maximum", value = 0),
                                                               textInput(paste0("minY", number), "Y Value Minumum", value = 0),
                                                               textInput(paste0("maxY", number), "Y Value Maximum", value = 0)
                                                               
                                              ), #conditional panel for specified vales end
                                              conditionalPanel(paste0("input.type", number, " == `Gene Input`"),
                                                               textAreaInput(paste0("genes", number), "Genes list:", "", width="200px", height="240px")
                                              ), #Conditional panel  for gene input end
                                              conditionalPanel(paste0("input.type", number, "== `Plot Interaction`"),
                                                               actionButton(paste0("reset_interact", number), 
                                                                            paste0("Reset gene selection"),
                                                                            style = "color: #fff; background-color: #630101; border-color: #260000")),
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
