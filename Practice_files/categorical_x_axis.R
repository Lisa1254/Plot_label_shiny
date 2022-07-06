#Practice for categorical x-axis
#as per TC request
library(ggplot2)
library(ggrepel)


df <- read.delim("/Users/lhoeg/Durocher Lab Dropbox/lisahoeg@gmail.com/Projects/3_TC_CRISPR_220601/TAK981_Jan20_DrugZ.txt")

y_vals <- df$normZ

data_names <- reactive({
  req(input$plot)
  if (input$name == "Rownames") {
    rownames(data())
  } else {
    data()[,input$name]
  }
})
x_in <- "GENE"
if (class(df[,x_in]) == "numeric"){
  x_vals <- df[,x_in]
} else {
  order <- order(df[,x_in])
  x_vals <- seq(1,nrow(df))
  for (i in 1:length(order)){
    x_vals[order[i]] <- i
  }
}

plot_data <- data.frame(ID = df$GENE, 
                        X_Value = x_vals,
                        Y_Value = y_vals)

subset_genes <- function(df, all_x, minx, maxx, all_y, miny, maxy, vecN){
  genes_ind <- which((all_y >= miny) & (all_y <= maxy) 
                     & (all_x >= minx) & (all_x <= maxx))
  genes_sub <- vecN[genes_ind]
  return(genes_sub)
}

minX1 <- "-Inf"
maxX1 <- "Inf"
minY1 <- "-99"
maxY1 <- "-4"
inf_over <- FALSE

genes_in1 <- subset_genes(df, x_in, as.numeric(minX1), as.numeric(maxX1), 
                          y_vals, as.numeric(minY1), as.numeric(maxY1), df$GENE)

if (inf_over == TRUE) {
  set_ovr <- Inf
} else {
  set_ovr <- 15
}

plot_gp_data <- plot_data[complete.cases(plot_data),]
plot_gp_data$Gp <- rep_len("Main", nrow(plot_gp_data))
plot_gp_data$Mult <- rep_len(0, nrow(plot_gp_data))

plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% genes_in1, "Gp1", plot_gp_data$Gp)
plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% genes_in1, plot_gp_data$Mult+1, plot_gp_data$Mult)

plot_gp_data$Gp <- factor(plot_gp_data$Gp)
plot_gp_data[which(plot_gp_data$Mult == 1),"Mult"] <- 0
plot_gp_data[which(plot_gp_data$Mult > 3), "Mult"] <- 3
plot_gp_data$Mult <- factor(plot_gp_data$Mult)

ind_mains <- which(plot_gp_data$Gp == "Main")
plot_gp_data_ord <- rbind(plot_gp_data[ind_mains,],
                          plot_gp_data[-ind_mains,])

cols <- c("Main" = "lightgray", "Gp1" = "darkblue")

g <- ggplot(data=plot_gp_data_ord) +
  geom_point(aes(x=X_Value, y=Y_Value, color = Gp, shape = Mult), size = 3) +
  scale_color_manual(name = "Groups", labels = c("NA", "Gp1"), values = cols) +
  geom_text_repel(aes(x=X_Value, y=Y_Value, label=ifelse(Gp=="Main", '', ID)), 
                  min.segment.length = 0, size = 3, max.overlaps = set_ovr)

g <- g + scale_shape_manual(values = c(16), guide = "none")

g <- g +
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_blank(), axis.line = element_line()) +
  labs(y = "normZ", x = "Names")


g




