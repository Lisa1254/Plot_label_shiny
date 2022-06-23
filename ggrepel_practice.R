data <- read.delim(file ="example_mageck_output.txt")

y_vals <- ifelse(data[,8] < 0, data[,3], data[,9])

plot_LFC_data <- data.frame(ID = data[,1], 
                            LFC = data[,8],
                            Score = log10(y_vals))


#Define genes to be highlighted
all_labels <- c("Group1", "Group2", "Group3")
all_genes_1 <- sample(plot_LFC_data$ID, 25)
all_genes_2 <- sample(plot_LFC_data$ID, 25)
all_genes_3 <- sample(plot_LFC_data$ID, 25)

#Set up plotting dataframe
plot_gp_data <- plot_LFC_data
plot_gp_data$Gp <- rep_len("Main", nrow(plot_gp_data))
plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_1, "Gp1", plot_gp_data$Gp)
plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_2, "Gp2", plot_gp_data$Gp)
plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_3, "Gp3", plot_gp_data$Gp)
plot_gp_data$Gp <- factor(plot_gp_data$Gp)

ind_mains <- which(plot_gp_data$Gp == "Main")
plot_gp_data_ord <- rbind(plot_gp_data[ind_mains,],
                          plot_gp_data[-ind_mains,])

#Define colours
cols <- c("Main" = "lightgray", "Gp1" = "#56B4E9", "Gp2" = "#009E73", "Gp3" = "#CC79A7")

ggplot(data=plot_gp_data_ord) +
  geom_point(aes(x=LFC, y=Score, color = Gp), shape = 16, size = 3) +
  scale_color_manual(name = "Groups", labels = c("NA", all_labels), values = cols) +
  geom_text_repel(aes(x=LFC, y=Score, label=ifelse(Gp=="Main", '', ID)), 
                  min.segment.length = 0, size = 3, max.overlaps = 5,
                  nudge_x = -0.2, direction = "y", hjust = "right") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_blank(), axis.line = element_line()) +
  labs(y = "Score", x = "LFC") +
  scale_y_continuous(trans = "reverse")
