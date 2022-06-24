source("env.R")

## SERVER ----


server <- function(input, output, session) {
  
  #-----------------------Data-------------------------
  
  # Upload data and define parameters of scatter plot
  
  #Receive & verify data
  data <- reactive({
    req(input$txt_data)
    read.delim(file =input$txt_data$datapath)
  })
  
  output$head_data <- renderTable(head(data()), rownames = TRUE)
  output$summary_data <- renderPrint(summary(data()))
  
  #Update plotting options by data columns
  observeEvent(data(), {
    choices <- colnames(data())
    updateSelectInput(inputId = "name", choices = c("Choose one" = "", 
                                                    list(`By rownames` = "Rownames", 
                                                         `By column` = choices)))
    updateSelectInput(inputId = "x", choices = c("columns" = "", choices))
    if (input$num_y == "1") {
      updateSelectInput(inputId = "y1", choices = c("columns" = "", choices))
    } else if (input$num_y == "2") {
      updateSelectInput(inputId = "yn", choices = c("columns" = "", choices))
      updateSelectInput(inputId = "yp", choices = c("columns" = "", choices))
    }
  })
  
  #Update y options if number of y inputs changed
  observeEvent(input$num_y, {
    req(input$txt_data)
    choices <- colnames(data())
    if (input$num_y == "1") {
      updateSelectInput(inputId = "y1", choices = c("columns" = "", choices))
      updateTextInput(inputId = "ylab", value = input$y1)
    } else if (input$num_y == "2") {
      updateSelectInput(inputId = "yn", choices = c("columns" = "", choices))
      updateSelectInput(inputId = "yp", choices = c("columns" = "", choices))
      updateTextInput(inputId = "ylab", value ="MAGeCK Score")
    }
  })
  
  #Use column names as default label until a custom input is used (only for 1 input for y values)
  observeEvent(input$x, {
    updateTextInput(inputId = "xlab", value = input$x)
  })
  
  observeEvent(input$y1, {
    updateTextInput(inputId = "ylab", value = input$y1)
  })
  

  
  #Define y values based on 1 or 2 inputs
  y_vals <- reactive({
    req(input$plot)
    if (input$num_y == "1") {
      data()[,input$y1]
    } else if (input$num_y == "2"){
      ifelse(data()[,input$x] < 0, data()[,input$yn], data()[,input$yp])
    }
  })
  
  #Define point ID names based on column or rownames
  data_names <- reactive({
    req(input$plot)
    if (input$name == "Rownames") {
      rownames(data())
    } else {
      data()[,input$name]
    }
  })
  
  #Construct basic dataframe for plotting
  plot_LFC_data <- reactive({
    req(input$plot)
    if ("log10" %in% input$y_trans){
      data.frame(ID = data_names(), 
                 LFC = data()[,input$x],
                 Score = log10(y_vals()))
    } else {
      data.frame(ID = data_names(), 
                 LFC = data()[,input$x],
                 Score = y_vals())
    }
    
  })
  
  
  #-----------------------Group Vars-------------------------
  
  # Set up graphical parameters for genes of interest, up to three groups
  all_labels <- reactive(c(input$gp1, input$gp2, input$gp3)[1:input$num_gps])
  #all_labels <- reactive({
  #  disp_labs <- vector()
  #  if (input$type1 == "Plot Interaction") {
  #    disp_labs <- c(disp_labs, input$gp1)
  #  }
  #  if ((input$type2 == "Plot Interaction") ) { #& (input$num_gps >= 2)
  #    disp_labs <- c(disp_labs, input$gp2)
  #  }
  #  if ((input$type3 == "Plot Interaction") ) { #& (input$num_gps == 3)
  #    disp_labs <- c(disp_labs, input$gp3)
  #  }
  #  return(disp_labs)
  #  })
  
  #Need to figure out how to better iterate or make a module/function for this
  observeEvent(input$gp1, {
    updateSelectInput(inputId = "col1", label = paste0("Colour for ", input$gp1))
    updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
    updateRadioButtons(inputId = "type1", label = paste0("Input Type for ", input$gp1))
  })
  
#  observeEvent(input$type1, {
#    updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
#  })
#  observeEvent(input$type2, {
#    updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
#  })
#  observeEvent(input$type3, {
#    updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
#  })
  
  #observeEvent(input$num_gps, {
  #  updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
  #})
  
  observeEvent(input$gp2,{
    updateSelectInput(inputId = "col2", label = paste0("Colour for ", input$gp2))
    updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
    updateRadioButtons(inputId = "type2", label = paste0("Input Type for ", input$gp2))
  })
  observeEvent(input$gp3,{
    updateSelectInput(inputId = "col3", label = paste0("Colour for ", input$gp3))
    updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
    updateRadioButtons(inputId = "type3", label = paste0("Input Type for ", input$gp3))
  })
  
  
  #If specific input list of genes:
  genes_in1 <- reactive({
    if (input$type1 == "Gene Input") {
      list_split(input$genes1)[which(list_split(input$genes1) %in% data_names())]
    } else {
      vector()
    }
  })
  
  genes_in2 <- reactive({
    if (input$type2 == "Gene Input") {
      list_split(input$genes2)[which(list_split(input$genes2) %in% data_names())]
    } else {
      vector()
    }
  })
  
  genes_in3 <- reactive({
    if (input$type3 == "Gene Input") {
      list_split(input$genes3)[which(list_split(input$genes3) %in% data_names())]
    } else {
      vector()
    }
  })
  
  #If range values specified for genes
  gene_sub1 <- reactive({
    if (input$type1 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX1), as.numeric(input$maxX1), 
                   y_vals(), as.numeric(input$minY1), as.numeric(input$maxY1), data_names())
    } else {
      vector()
    }
  })
  
  gene_sub2 <- reactive({
    if (input$type2 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX2), as.numeric(input$maxX2), 
                   y_vals(), as.numeric(input$minY2), as.numeric(input$maxY2), data_names())
    } else {
      vector()
    }
  })
  
  gene_sub3 <- reactive({
    if (input$type3 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX3), as.numeric(input$maxX3), 
                   y_vals(), as.numeric(input$minY3), as.numeric(input$maxY3), data_names())
    } else {
      vector()
    }
  })
  
  #-----------------------Scatter-------------------------
  
  # Construct scatter plot
  
  make_scatter <- reactive({
    #Construct on hitting the plot button
    input$plot
    
    
    
    #Set up plotting dataframe
    plot_gp_data <- plot_LFC_data()
    plot_gp_data$Gp <- rep_len("Main", nrow(plot_gp_data))
    ##Define genes to be highlighted
    if (input$num_gps  >= 1) {
      all_genes_1 <- c(gene_sub1(), genes_in1(), selected1())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_1, "Gp1", plot_gp_data$Gp)
    }
    if (input$num_gps >=2) {
      all_genes_2 <- c(gene_sub2(), genes_in2(), selected2())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_2, "Gp2", plot_gp_data$Gp)
    }
    if (input$num_gps == 3) {
      all_genes_3 <- c(gene_sub3(), genes_in3(), selected3())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_3, "Gp3", plot_gp_data$Gp)
    }
    plot_gp_data$Gp <- factor(plot_gp_data$Gp)
    
    #To get input genes plotted last, and therefore with the colour displaying when cluttered, organize those points to the bottom. For now, will just put all "Main" at the top, but consider adding feature to check for if input type is a gene list, as that gets priority to the bottom rather than a selection type
    ind_mains <- which(plot_gp_data$Gp == "Main")
    plot_gp_data_ord <- rbind(plot_gp_data[ind_mains,],
                              plot_gp_data[-ind_mains,])
    
    #Define colours
    cols <- c("Main" = input$col_m, "Gp1" = input$col1, "Gp2" = input$col2, "Gp3" = input$col3)[1:(input$num_gps+1)]
    
    #Set up scatter plot with or without colour for input groups
    if (input$num_gps == 0){
      g <- ggplot(data=plot_gp_data_ord) +
        geom_point(aes(x=LFC, y=Score), shape = 16, size = 3, color = input$col_m) +
        theme(legend.position = "none")
        
    } else {
      g <- ggplot(data=plot_gp_data_ord) +
        geom_point(aes(x=LFC, y=Score, color = Gp), shape = 16, size = 3) +
        scale_color_manual(name = "Groups", labels = c("NA", all_labels()), values = cols) +
        geom_text_repel(aes(x=LFC, y=Score, label=ifelse(Gp=="Main", '', ID)), 
                        min.segment.length = 0, size = 3, max.overlaps = 15)

    }
    
    #Add common elements to plot types with or without coloured points
    g <- g +
      theme(panel.background = element_rect(fill = "white"), 
            panel.border = element_blank(), axis.line = element_line()) +
      labs(y = input$ylab, x = input$xlab)
    
    #Reverse y-axis if requested
    if ("reverse" %in% input$y_trans) {
      g <- g + scale_y_continuous(trans = "reverse")
    }

    #Output constructed plot to reactive
    g
  })
  
  output$scatter <- renderPlot({
    make_scatter()
  })
  
  dataerrors <- reactive({
    tryCatch({
      print(make_scatter())
    }, message = function(e) {
      return(e$message)
    }, warning = function(e) {
      return(e$message)
    }, error = function(e) {
      return(e$message)
    })
  })
  
  output$ggplot_warnings <- renderPrint({
    dataerrors()
  })
  
  #-----------------------Mouse Input-------------------------
  
  # Use mouse input to select points of interest
  
  #On mouse hover, display Near Point data
  nearTable_h <- reactive({
    req(input$plot_hover)
    nearPoints(plot_LFC_data(), input$plot_hover)
  })
  
  output$nT_hover <- renderTable(nearTable_h())
  
  #Use NearPoints to add labels to plot on click
  
  id_lab <- reactive({
    req(input$plot_click)
    if ((input$current_gp == input$gp1) & (input$type1 == "Plot Interaction")) {
      nearPoints(plot_LFC_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp2) & (input$type2 == "Plot Interaction")) {
      nearPoints(plot_LFC_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp3) & (input$type3 == "Plot Interaction")) {
      nearPoints(plot_LFC_data(), input$plot_click)[,1]
    } 
    
  })
  
  selected1 <- reactiveVal({
    vector()
  })
  selected2 <- reactiveVal({
    vector()
  })
  selected3 <- reactiveVal({
    vector()
  })
  
  observeEvent(input$plot_click, {
    gp_sel <- paste0("gp", as.character(which(all_labels() == input$current_gp)))
    switch(gp_sel,
           gp1 = selected1(unique(c(id_lab(), selected1()))),
           gp2 = selected2(unique(c(id_lab(), selected2()))),
           gp3 = selected3(unique(c(id_lab(), selected3())))
    )
  })
  
  #Add brushed points to selected
  brush_sel <- reactive({
    req(input$plot_brush)
    if ((input$current_gp == input$gp1) & (input$type1 == "Plot Interaction")) {
      brushedPoints(plot_LFC_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp2) & (input$type2 == "Plot Interaction")) {
      brushedPoints(plot_LFC_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp3) & (input$type3 == "Plot Interaction")) {
      brushedPoints(plot_LFC_data(), input$plot_brush)[,1]
    }
    
  })
  observeEvent(input$plot_brush,{
    gp_sel <- paste0("gp", as.character(which(all_labels() == input$current_gp)))
    switch(gp_sel,
           gp1 = selected1(unique(c(brush_sel(), selected1()))),
           gp2 = selected2(unique(c(brush_sel(), selected2()))),
           gp3 = selected3(unique(c(brush_sel(), selected3())))
    )
  })
  
  #If reset group button clicked, remove all values in selection
  observeEvent(input$reset_interact1, {
    selected1(vector())
  })
  observeEvent(input$reset_interact2, {
    selected2(vector())
  })
  observeEvent(input$reset_interact3, {
    selected3(vector())
  })
  
  #If new plot generated, reset selected info
  observeEvent(input$plot, {
    selected1(vector())
    selected2(vector())
    selected3(vector())
  })
  
  #-----------------------Download-------------------------
  
  # Download plot and gene data
  
  #Download plot
  output$dl_plot <- downloadHandler(
    filename = "plotImageScatter.pdf",
    content = function(file) {
      ggsave(make_scatter(), 
             file = file, 
             device = "pdf", width = 35, height = 25, units = "cm")
    }
  )
  
  
  #Download selected genes
 
  output$dl_genes <- downloadHandler(
    filename = "selectedGenes.txt",
    content = function(file) {
      #Define genes to be highlighted
      all_genes_1 <- c(gene_sub1(), genes_in1(), selected1())
      all_genes_1_ind <- which(data_names() %in% all_genes_1)
      dl_gene_df <- data.frame(Gene = all_genes_1, 
                               Xval = data()[all_genes_1_ind,input$x],
                               Yval = y_vals()[all_genes_1_ind],
                               Group = rep(input$gp1, length(all_genes_1)))
      
      if (input$num_gps >= 2) {
        all_genes_2 <- c(gene_sub2(), genes_in2(), selected2())
        all_genes_2_ind <- which(data_names() %in% all_genes_2)
        dl_gene_df2 <- data.frame(Gene = all_genes_2, 
                                  Xval = data()[all_genes_2_ind,input$x],
                                  Yval = y_vals()[all_genes_2_ind],
                                  Group = rep(input$gp2, length(all_genes_2)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_df2)
        
      }
      if (input$num_gps == 3) {
        all_genes_3 <- c(gene_sub3(), genes_in3(), selected3())
        all_genes_3_ind <- which(data_names() %in% all_genes_3)
        dl_gene_df3 <- data.frame(Gene = all_genes_3, 
                                  Xval = data()[all_genes_3_ind,input$x],
                                  Yval = y_vals()[all_genes_3_ind],
                                  Group = rep(input$gp3, length(all_genes_3)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_df3)
        
      }
      colnames(dl_gene_df)[c(2,3)] <- c(input$xlab, input$ylab)
      write.table(dl_gene_df, file, row.names = F, quote = F)
    }
  )
  
  
}

