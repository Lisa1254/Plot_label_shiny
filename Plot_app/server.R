## SERVER ----

server <- function(input, output, session) {
  
  #------------------------------------------------
  
  # Upload data and define parameters of scatter plot
  
  data <- reactive({
    req(input$txt_data)
    read.delim(file =input$txt_data$datapath)
  })
  
  output$head_data <- renderTable(head(data()), rownames = TRUE)
  output$summary_data <- renderPrint(summary(data()))
  
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
  
  observeEvent(input$num_y, {
    req(input$txt_data)
    choices <- colnames(data())
    if (input$num_y == "1") {
      updateSelectInput(inputId = "y1", choices = c("columns" = "", choices))
    } else if (input$num_y == "2") {
      updateSelectInput(inputId = "yn", choices = c("columns" = "", choices))
      updateSelectInput(inputId = "yp", choices = c("columns" = "", choices))
    }
  })
  
  y_vals <- reactive({
    req(input$plot)
    if (input$num_y == "1") {
      data()[,input$y1]
    } else if (input$num_y == "2"){
      ifelse(data()[,input$x] < 0, data()[,input$yn], data()[,input$yp])
    }
  })
  
  
  data_names <- reactive({
    req(input$plot)
    if (input$name == "Rownames") {
      rownames(data())
    } else {
      data()[,input$name]
    }
  })
  
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
  
  
  #------------------------------------------------
  
  # Set up graphical parameters for genes of interest, up to three groups
  
  all_labels <- reactive(c(input$gp1, input$gp2, input$gp3)[1:input$num_gps])
  
  #Need to figure out how to better iterate or make a module/function for this
  observeEvent(input$gp1, {
    updateSelectInput(inputId = "col1", label = paste0("Colour for ", input$gp1))
    updateRadioButtons(inputId = "current_gp", choices =all_labels(), inline = T)
    updateRadioButtons(inputId = "type1", label = paste0("Input Type for ", input$gp1))
  })
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
  
  genes_in1 <- reactive({
    if (input$type1 == "Gene Input") {
      list_split(input$genes1)
    } else {
      vector()
    }
  })
  
  genes_in2 <- reactive({
    if (input$type2 == "Gene Input") {
      list_split(input$genes2)
    } else {
      vector()
    }
  })
  
  genes_in3 <- reactive({
    if (input$type3 == "Gene Input") {
      list_split(input$genes3)
    } else {
      vector()
    }
  })
  
  gene_sub1 <- reactive({
    if (input$type1 == "Specified Values") {
      subset_genes(data(), input$x, input$minX1, input$maxX1, y_vals(), input$minY1, input$maxY1, data_names())
    } else {
      vector()
    }
  })
  
  gene_sub2 <- reactive({
    if (input$type2 == "Specified Values") {
      subset_genes(data(), input$x, input$minX2, input$maxX2, y_vals(), input$minY2, input$maxY2, data_names())
    } else {
      vector()
    }
  })
  
  gene_sub3 <- reactive({
    if (input$type3 == "Specified Values") {
      subset_genes(data(), input$x, input$minX3, input$maxX3, y_vals(), input$minY3, input$maxY3, data_names())
    } else {
      vector()
    }
  })
  
  #------------------------------------------------
  
  # Construct scatter plot
  
  make_scatter <- reactive({
    #Construct on hitting the plot button
    input$plot
    
    #Define genes to be highlighted
    all_genes_1 <- c(gene_sub1(), genes_in1(), selected1())
    all_genes_2 <- c(gene_sub2(), genes_in2(), selected2())
    all_genes_3 <- c(gene_sub3(), genes_in3(), selected3())
    
    #Set up plotting dataframe
    plot_gp_data <- plot_LFC_data()
    plot_gp_data$Gp <- rep_len("Main", nrow(plot_gp_data))
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_1, "Gp1", plot_gp_data$Gp)
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_2, "Gp2", plot_gp_data$Gp)
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_3, "Gp3", plot_gp_data$Gp)
    plot_gp_data$Gp <- factor(plot_gp_data$Gp)
    
    #To get input genes plotted last, and therefore with the colour displaying when cluttered, organize those points to the bottom. For now, will just put all "Main" at the top, but consider adding feature to check for if input type is a gene list, as that gets priority to the bottom rather than a selection type
    ind_mains <- which(plot_gp_data$Gp == "Main")
    plot_gp_data_ord <- rbind(plot_gp_data[ind_mains,],
                              plot_gp_data[-ind_mains,])
    
    #Define colours
    cols <- c("Main" = input$col_m, "Gp1" = input$col1, "Gp2" = input$col2, "Gp3" = input$col3)[1:(input$num_gps+1)]
    
    #Set up scatter plot
    g <- ggplot(data=plot_gp_data_ord) +
      geom_point(aes(x=LFC, y=Score, color = Gp), shape = 16, size = 3) +
      scale_color_manual(name = "Groups", labels = c("NA", all_labels()), values = cols) +
      geom_text_repel(aes(x=LFC, y=Score, label=ifelse(Gp=="Main", '', ID)), 
                      min.segment.length = 0, size = 3, max.overlaps = 15) +
      theme(panel.background = element_rect(fill = "white"), 
            panel.border = element_blank(), axis.line = element_line()) +
      labs(y = input$ylab, x = input$xlab)
    
    #Reverse y-axis if requested
    if ("reverse" %in% input$y_trans) {
      g + scale_y_continuous(trans = "reverse")
    } else {
      g
    }
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
  
  #------------------------------------------------
  
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
  
  #If new plot generated, reset selected info
  observeEvent(input$plot, {
    selected1(vector())
    selected2(vector())
    selected3(vector())
  })
  
  #------------------------------------------------
  
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

