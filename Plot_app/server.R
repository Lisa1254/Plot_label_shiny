source("env.R")




shinyServer(function(input, output, session) {
  
  #-----------------------Data-------------------------
  
  # Upload data and define parameters of scatter plot
  
  #Receive & verify data
  data <- reactive({
    if(input$srctype == "Input"){
      req(input$txt_data)
      ext <- tools::file_ext(input$txt_data$name)
      if (ext %in% c("csv", "txt", "tsv")) {
        switch(ext,
               txt = read.delim(file =input$txt_data$datapath),
               tsv = read.delim(file =input$txt_data$datapath),
               csv = read.csv(input$txt_data$datapath))
      } else {
        createAlert(session, "data_alert", title = "Oops", content = "Upload filetype needs to be either a .csv file, or tab-delimited file as .txt or .tsv", append = FALSE, style = "danger")
      }
      
      
    } else if (input$srctype == "FDR Example") {
      read.delim(file = "Ex/sample_drugZ_fdrSynth.txt")
    } else if (input$srctype == "Volcano Example") {
      read.delim(file = "Ex/sample_mageck.txt")
    }
    
  })
  
  output$file_name <- renderText(paste("Source data:", input$txt_data$name))
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
    choices <- colnames(data())
    if (input$num_y == "1") {
      updateSelectInput(inputId = "y1", choices = c("columns" = "", choices))
      updateTextInput(inputId = "ylab", value = input$y1)
    } else if (input$num_y == "2") {
      updateSelectInput(inputId = "yn", choices = c("columns" = "", choices))
      updateSelectInput(inputId = "yp", choices = c("columns" = "", choices))
      updateTextInput(inputId = "ylab", value ="Score")
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
  plot_data <- reactive({
    req(input$plot)
    if ("log10" %in% input$y_trans){
      data.frame(ID = data_names(), 
                 X_Value = data()[,input$x],
                 Y_Value = log10(y_vals()))
    } else {
      data.frame(ID = data_names(), 
                 X_Value = data()[,input$x],
                 Y_Value = y_vals())
    }
    
  })
  
  
  #-----------------------Group Vars-------------------------
  
  #Using a button to add groups. Can remove from plot using checkbox at top of main panel.
  counter <- reactiveVal(0)
  
  observeEvent(input$add_gp,{
    if (counter() < 7) {
      counter(counter() + 1)
      updateSliderInput(inputId = "num_gps", value = counter())
      prev_sel <- input$inc_groups
      updateCheckboxGroupInput(inputId = "inc_groups", inline = TRUE,
                               choices = paste0("Group ", seq(1,input$num_gps+1)),
                               selected = c(prev_sel, paste0("Group ", input$num_gps+1)))
    } else {
      createAlert(session, "gp_alert", title = "Oops", content = "Maximum group number is seven", append = FALSE)
    }
    
  })
  
  # Set up graphical parameters for genes of interest, up to three groups
  all_labels <- reactive(c(input$gp1, input$gp2, input$gp3, input$gp4, input$gp5, input$gp6, input$gp7)[1:input$num_gps])
 
  observeEvent(all_labels(), {
    updateRadioButtons(inputId = "current_gp", choices = all_labels(), inline = T)
  })
  
 
  
  #Set up specified gene inputs:
  #Can't seem to figure out lapply or otherwise reactive use within a loop. Try to clean code later
  genes_in1 <- reactive({
    if (input$type1 == "Gene Input") {
      list_split(input$genes1)[which(list_split(input$genes1) %in% data_names())]
    } else if (input$type1 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX1), as.numeric(input$maxX1), 
                   y_vals(), as.numeric(input$minY1), as.numeric(input$maxY1), data_names())
    } else {
      vector()
    }
  })
  
  genes_in2 <- reactive({
    if (input$type2 == "Gene Input") {
      list_split(input$genes2)[which(list_split(input$genes2) %in% data_names())]
    } else if (input$type2 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX2), as.numeric(input$maxX2), 
                   y_vals(), as.numeric(input$minY2), as.numeric(input$maxY2), data_names())
    } else {
      vector()
    }
  })
  
  genes_in3 <- reactive({
    if (input$type3 == "Gene Input") {
      list_split(input$genes3)[which(list_split(input$genes3) %in% data_names())]
    } else if (input$type3 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX3), as.numeric(input$maxX3), 
                   y_vals(), as.numeric(input$minY3), as.numeric(input$maxY3), data_names())
    } else {
      vector()
    }
  })
  
  genes_in4 <- reactive({
    if (input$type4 == "Gene Input") {
      list_split(input$genes4)[which(list_split(input$genes4) %in% data_names())]
    } else if (input$type4 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX4), as.numeric(input$maxX4), 
                   y_vals(), as.numeric(input$minY4), as.numeric(input$maxY4), data_names())
    } else {
      vector()
    }
  })
  
  genes_in5 <- reactive({
    if (input$type5 == "Gene Input") {
      list_split(input$genes5)[which(list_split(input$genes5) %in% data_names())]
    } else if (input$type5 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX5), as.numeric(input$maxX5), 
                   y_vals(), as.numeric(input$minY5), as.numeric(input$maxY5), data_names())
    } else {
      vector()
    }
  })
  
  genes_in6 <- reactive({
    if (input$type6 == "Gene Input") {
      list_split(input$genes6)[which(list_split(input$genes6) %in% data_names())]
    } else if (input$type6 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX6), as.numeric(input$maxX6), 
                   y_vals(), as.numeric(input$minY6), as.numeric(input$maxY6), data_names())
    } else {
      vector()
    }
  })
  
  genes_in7 <- reactive({
    if (input$type7 == "Gene Input") {
      list_split(input$genes7)[which(list_split(input$genes7) %in% data_names())]
    } else if (input$type7 == "Specified Values") {
      subset_genes(data(), input$x, as.numeric(input$minX7), as.numeric(input$maxX7), 
                   y_vals(), as.numeric(input$minY7), as.numeric(input$maxY7), data_names())
    } else {
      vector()
    }
  })
  
  #
  #-----------------------Scatter-------------------------
  
  # Construct scatter plot
  
  make_scatter <- reactive({
    #Construct on hitting the plot button
    input$plot
    
    #Define whether to show all labels in dense areas of plot
    if (input$inf_over == TRUE) {
      set_ovr <- Inf
    } else {
      set_ovr <- 15
    }
    
    #Set up plotting dataframe
    plot_gp_data <- plot_data()[complete.cases(plot_data()),]
    plot_gp_data$Gp <- rep_len("Main", nrow(plot_gp_data))
    plot_gp_data$Mult <- rep_len(0, nrow(plot_gp_data))
    ##Define genes to be highlighted
    if ((input$num_gps  >= 1) & ("Group 1" %in% input$inc_groups)) {
      all_genes_1 <- c(genes_in1(), selected1())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_1, "Gp1", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_1, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if ((input$num_gps >=2) & ("Group 2" %in% input$inc_groups)) {
      all_genes_2 <- c(genes_in2(), selected2())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_2, "Gp2", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_2, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if ((input$num_gps >= 3) & ("Group 3" %in% input$inc_groups)) {
      all_genes_3 <- c(genes_in3(), selected3())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_3, "Gp3", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_3, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if ((input$num_gps >= 4) & ("Group 4" %in% input$inc_groups)) {
      all_genes_4 <- c(genes_in4(), selected4())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_4, "Gp4", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_4, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if ((input$num_gps >= 5) & ("Group 5" %in% input$inc_groups)) {
      all_genes_5 <- c(genes_in5(), selected5())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_5, "Gp5", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_5, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if ((input$num_gps >= 6) & ("Group 6" %in% input$inc_groups)) {
      all_genes_6 <- c(genes_in6(), selected6())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_6, "Gp6", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_6, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if ((input$num_gps == 7) & ("Group 7" %in% input$inc_groups)) {
      all_genes_7 <- c(genes_in7(), selected7())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_7, "Gp7", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_7, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    
    plot_gp_data$Gp <- factor(plot_gp_data$Gp)
    plot_gp_data[which(plot_gp_data$Mult == 1),"Mult"] <- 0
    plot_gp_data[which(plot_gp_data$Mult > 3), "Mult"] <- 3
    plot_gp_data$Mult <- factor(plot_gp_data$Mult)
    
    #To get input genes plotted last, and therefore with the colour displaying when cluttered, organize those points to the bottom. For now, will just put all "Main" at the top, but consider adding feature to check for if input type is a gene list, as that gets priority to the bottom rather than a selection type
    ind_mains <- which(plot_gp_data$Gp == "Main")
    plot_gp_data_ord <- rbind(plot_gp_data[ind_mains,],
                              plot_gp_data[-ind_mains,])
    
    #Define colours
    cols <- c("Main" = input$col_m, "Gp1" = input$col1, "Gp2" = input$col2, "Gp3" = input$col3, "Gp4" = input$col4, "Gp5" = input$col5, "Gp6" = input$col6, "Gp7" = input$col7)[1:(input$num_gps+1)]
    
    #Set up scatter plot with or without colour for input groups
    if ((input$num_gps == 0) | (is.null(input$inc_groups))){
      g <- ggplot(data=plot_gp_data_ord) +
        geom_point(aes(x=X_Value, y=Y_Value), shape = 16, size = 3, color = input$col_m) +
        theme(legend.position = "none")
        
    } else {
      #Define index of included groups:
      inc_ind <- vector()
      for (i in 1:input$num_gps){
        if (sum(grepl(as.character(i), input$inc_groups))>0){
          inc_ind <- c(inc_ind, i)
        }
        }
      #Plot with included groups
      g <- ggplot(data=plot_gp_data_ord) +
        geom_point(aes(x=X_Value, y=Y_Value, color = Gp, shape = Mult), size = 3) +
        scale_color_manual(name = "Groups", labels = c("NA", all_labels()[inc_ind]), values = cols[c(1,1+inc_ind)]) +
        geom_text_repel(aes(x=X_Value, y=Y_Value, label=ifelse(Gp=="Main", '', ID)), 
                        min.segment.length = 0, size = 3, max.overlaps = set_ovr)

    }
    
    if ((input$num_gps != 0) & (max(as.numeric(plot_gp_data$Mult)) == 1)) {
      g <- g + scale_shape_manual(values = c(16), guide = "none")
    } else if (max(as.numeric(plot_gp_data$Mult)) == 2) {
      g <- g + scale_shape_manual(name = "Number of groups", labels = c("0-1 groups", "2 groups"), values = c(16,17))
    } else if (max(as.numeric(plot_gp_data$Mult)) == 3) {
      g <- g + scale_shape_manual(name = "Number of groups", labels = c("0-1 groups", "2 groups", "3 or more groups"), values = c(16,17,15))
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
  
  #----------------------------Hover----
  # Use mouse input to select points of interest
  
  #On mouse hover, display Near Point data
  nearTable_h <- reactive({
    req(input$plot_hover)
    nearPoints(plot_data(), input$plot_hover)
  })
  
  output$nT_hover <- renderTable(nearTable_h())
  
  #----------------------------PlotClickId----
  #Use NearPoints to add labels to plot on click
  id_lab <- reactive({
    req(input$plot_click)
    if ((input$current_gp == input$gp1) & (input$type1 == "Plot Click") & ("Group 1" %in% input$inc_groups)) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp2) & (input$type2 == "Plot Click") & ("Group 2" %in% input$inc_groups)) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp3) & (input$type3 == "Plot Click") & ("Group 3" %in% input$inc_groups)) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp4) & (input$type4 == "Plot Click") & ("Group 4" %in% input$inc_groups)) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp5) & (input$type5 == "Plot Click") & ("Group 5" %in% input$inc_groups)) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp6) & (input$type6 == "Plot Click") & ("Group 6" %in% input$inc_groups)) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp7) & (input$type7 == "Plot Click") & ("Group 7" %in% input$inc_groups)) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } 
    
  })
  #----------------------------reactiveVal----
  selected1 <- reactiveVal({
    vector()
  })
  selected2 <- reactiveVal({
    vector()
  })
  selected3 <- reactiveVal({
    vector()
  })
  selected4 <- reactiveVal({
    vector()
  })
  selected5 <- reactiveVal({
    vector()
  })
  selected6 <- reactiveVal({
    vector()
  })
  selected7 <- reactiveVal({
    vector()
  })
  
  #----------------------------observePlotClick----
  observeEvent(input$plot_click, {
    gp_sel <- paste0("gp", as.character(which(all_labels() == input$current_gp)))
    switch(gp_sel,
           gp1 = selected1(unique(c(id_lab(), selected1()))),
           gp2 = selected2(unique(c(id_lab(), selected2()))),
           gp3 = selected3(unique(c(id_lab(), selected3()))),
           gp4 = selected4(unique(c(id_lab(), selected4()))),
           gp5 = selected5(unique(c(id_lab(), selected5()))),
           gp6 = selected6(unique(c(id_lab(), selected6()))),
           gp7 = selected7(unique(c(id_lab(), selected7())))
    )
  })
  
  #----------------------------brushSelectId----
  #Add brushed points to selected
  brush_sel <- reactive({
    req(input$plot_brush)
    if ((input$current_gp == input$gp1) & (input$type1 == "Plot Click") & ("Group 1" %in% input$inc_groups)) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp2) & (input$type2 == "Plot Click") & ("Group 2" %in% input$inc_groups)) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp3) & (input$type3 == "Plot Click") & ("Group 3" %in% input$inc_groups)) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp4) & (input$type4 == "Plot Click") & ("Group 4" %in% input$inc_groups)) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp5) & (input$type5 == "Plot Click") & ("Group 5" %in% input$inc_groups)) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp6) & (input$type6 == "Plot Click") & ("Group 6" %in% input$inc_groups)) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp7) & (input$type7 == "Plot Click") & ("Group 7" %in% input$inc_groups)) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    }
    
  })
  
  #----------------------------observeBrush----
  observeEvent(input$plot_brush,{
    gp_sel <- paste0("gp", as.character(which(all_labels() == input$current_gp)))
    switch(gp_sel,
           gp1 = selected1(unique(c(brush_sel(), selected1()))),
           gp2 = selected2(unique(c(brush_sel(), selected2()))),
           gp3 = selected3(unique(c(brush_sel(), selected3()))),
           gp4 = selected4(unique(c(brush_sel(), selected4()))),
           gp5 = selected5(unique(c(brush_sel(), selected5()))),
           gp6 = selected6(unique(c(brush_sel(), selected6()))),
           gp7 = selected7(unique(c(brush_sel(), selected7())))
    )
  })
  
  #----------------------------resets----
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
  observeEvent(input$reset_interact4, {
    selected4(vector())
  })
  observeEvent(input$reset_interact5, {
    selected5(vector())
  })
  observeEvent(input$reset_interact6, {
    selected6(vector())
  })
  observeEvent(input$reset_interact7, {
    selected7(vector())
  })
  
  #If new plot generated, reset selected info
  observeEvent(input$plot, {
    selected1(vector())
    selected2(vector())
    selected3(vector())
    selected4(vector())
    selected5(vector())
    selected6(vector())
    selected7(vector())
  })
  
  #-----------------------Download-------------------------
  
  # Download plot and gene data
  
  #Download plot
  output$dl_plot <- downloadHandler(
    filename = function(){
      paste0(input$dl_plot_name, ".pdf")},
    content = function(file) {
      ggsave(make_scatter(), 
             file = file, 
             device = "pdf", width = 35, height = 25, units = "cm")
    }
  )
  
  
  #Download selected genes
 
  output$dl_genes <- downloadHandler(
    filename = function(){
      paste0(input$dl_genes_name, ".txt")},
    content = function(file) {
      #Initialize dataframe
      dl_gene_df <- data.frame(Gene = NA, Xval = NA, Yval = NA, Group = NA)
      #Define genes to be highlighted
      if ((input$num_gps >=1) & ("Group 1" %in% input$inc_groups)){
        all_genes_t <- c(genes_in1(), selected1())
        all_genes_t_ind <- which(data_names() %in% all_genes_t)
        dl_gene_dft <- data.frame(Gene = all_genes_t, 
                                 Xval = data()[all_genes_t_ind,input$x],
                                 Yval = y_vals()[all_genes_t_ind],
                                 Group = rep(input$gp1, length(all_genes_t)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_dft)
      }
      if ((input$num_gps >= 2) & ("Group 2" %in% input$inc_groups)) {
        all_genes_t <- c(genes_in2(), selected2())
        all_genes_t_ind <- which(data_names() %in% all_genes_t)
        dl_gene_dft <- data.frame(Gene = all_genes_t, 
                                  Xval = data()[all_genes_t_ind,input$x],
                                  Yval = y_vals()[all_genes_t_ind],
                                  Group = rep(input$gp2, length(all_genes_t)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_dft)
      }
      if ((input$num_gps >= 3) & ("Group 3" %in% input$inc_groups)) {
        all_genes_t <- c(genes_in3(), selected3())
        all_genes_t_ind <- which(data_names() %in% all_genes_t)
        dl_gene_dft <- data.frame(Gene = all_genes_t, 
                                  Xval = data()[all_genes_t_ind,input$x],
                                  Yval = y_vals()[all_genes_t_ind],
                                  Group = rep(input$gp3, length(all_genes_t)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_dft)
      }
      if ((input$num_gps >= 4) & ("Group 4" %in% input$inc_groups)) {
        all_genes_t <- c(genes_in4(), selected4())
        all_genes_t_ind <- which(data_names() %in% all_genes_t)
        dl_gene_dft <- data.frame(Gene = all_genes_t, 
                                  Xval = data()[all_genes_t_ind,input$x],
                                  Yval = y_vals()[all_genes_t_ind],
                                  Group = rep(input$gp4, length(all_genes_t)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_dft)
      }
      if ((input$num_gps >= 5) & ("Group 5" %in% input$inc_groups)) {
        all_genes_t <- c(genes_in5(), selected5())
        all_genes_t_ind <- which(data_names() %in% all_genes_t)
        dl_gene_dft <- data.frame(Gene = all_genes_t, 
                                  Xval = data()[all_genes_t_ind,input$x],
                                  Yval = y_vals()[all_genes_t_ind],
                                  Group = rep(input$gp5, length(all_genes_t)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_dft)
      }
      if ((input$num_gps >= 6) & ("Group 6" %in% input$inc_groups)) {
        all_genes_t <- c(genes_in6(), selected6())
        all_genes_t_ind <- which(data_names() %in% all_genes_t)
        dl_gene_dft <- data.frame(Gene = all_genes_t, 
                                  Xval = data()[all_genes_t_ind,input$x],
                                  Yval = y_vals()[all_genes_t_ind],
                                  Group = rep(input$gp6, length(all_genes_t)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_dft)
      }
      if ((input$num_gps == 7) & ("Group 7" %in% input$inc_groups)) {
        all_genes_t <- c(genes_in7(), selected7())
        all_genes_t_ind <- which(data_names() %in% all_genes_t)
        dl_gene_dft <- data.frame(Gene = all_genes_t, 
                                  Xval = data()[all_genes_t_ind,input$x],
                                  Yval = y_vals()[all_genes_t_ind],
                                  Group = rep(input$gp7, length(all_genes_t)))
        dl_gene_df <- rbind(dl_gene_df, dl_gene_dft)
      }
      #Remove initializing row if dataframe has content. Otherwise, return frame as is with only NA values
      if (nrow(dl_gene_df) > 1){
        dl_gene_df <- dl_gene_df[-1,]
      }
      
      if (input$dl_genes_order == "Name") {
        dl_gene_df <- dl_gene_df[order(dl_gene_df$Gene),]
      } else if (input$dl_genes_order == "X-Value") {
        dl_gene_df <- dl_gene_df[order(dl_gene_df$Xval),]
      } else if (input$dl_genes_order == "Y-Value") {
        dl_gene_df <- dl_gene_df[order(dl_gene_df$Yval),]
      }
      colnames(dl_gene_df)[c(2,3)] <- c(input$xlab, input$ylab)
      write.table(dl_gene_df, file, row.names = F, quote = F, sep = "\t")
    }
  )
  
  #-----------------------Help-------------------------
  
  output$help <- renderUI({
    HTML(" This Shiny application makes a scatterplot from the X & Y values as supplied in a data table uploaded in a tab delimited .txt or .tsv format, or in .csv format. Application currently supports up to seven different groups of highlighting labels with accepted input for gene choice for each group including plot interaction (click or drag select), range of specified values, or input gene list.<br/>
    <br/>
    <br/>
    
              <b>Source Data:</b><br/>
              <b><i>Data:</b></i> Options inlcude \"Input\" which should be either tab delimited file (.txt or .tsv) or a .csv file that includes data to be used in construction of scatterplot; \"FDR Example\" which is the set of fdr_synth values from a selection of 1000 genes and 4 CRISPR screens as calculated with drugZ; and \"Volcano Example\" which includes a selection of 1000 genes from a single screen's MAGeCK analysis. Example files are provided for practicing the functionality of the app, and have been constructed from the same data used to create the provided examples.<br/>
               &emsp;<i>Show preview of data:</i> First six rows of data are displayed. This feature can help ensure that the correct file was selected and has uploaded correctly, and which column header is being used for which attribute.<br/>
               &emsp;<i>Show summary of data:</i> Shows R-console output of summary command: this includes quartiles and mean for numeric inputs. This can be useful in seeing the distribution of data and helping decide cutoff values for adding colour highlight to a specific range of input values.<br/>
               <br/>
               <b>Data Attributes:</b><br/>
               Labels for data points can be extracted from either rownames of the imported data, or a specific column. Default colour for points is light gray.<br/>
               <br/>
               <b>X-axis Attributes:</b><br/>
               <b><i>X values:</i></b> Select a column from uploaded data to provide values for X-axis<br/>
               <b><i>Attribute name for X values:</i></b> Default name is column name selected for values. This name will be used as the plot's x-axis label, as well as for column header when saving information about selected genes.<br/>
               <br/>
               <b>Y-axis Attributes:</b><br/>
               <i><b>Number of Inputs for Y-axis:</i></b> Can input single column, or two. Two column input is set up such that the \"Score\" for the y-value is the negative score if  X-value < 0, or the positive score if X-value > 0. This is useful when using MAGeCK or drugZ output that separates relevant values from the sensitizer vs. resistance statistical tests. If your data requires a different manipulation, it is recommended to perform the transformations externally prior to importing the values to the Shiny.<br/>
               <i><b>Y values:</i></b> If 1 column is selected for number of y-inputs, choose data column here.<br/>
               <i><b>Negative Score:</i></b> If 2 column input is selected for y-values, these values will be used for the case when X-value < 0<br/>
               <i><b>Positive Score:</i></b> If 2 column input is selected for y-values, these values will be used for the case when X-value > 0<br/>
               <i><b>Attribute name for y values:</i></b> Default name is column name when 1 column input is selected for y-values, or \"Score\" when 2 column input is selected for y-values. This name will be used as the plot's x-axis label, as well as for column header when saving information about selected genes.<br/>
               <i><b>Transformations for y-axis:</i></b> Values for y-axis can be log transformed, reversed, or both.<br/>
               <br/>
               <b>Gene highlight groups:</b><br/>
               <i><b>Add highlight group:</i></b> Currently app functionality supports up to 7 groups of gene selection for highlighting on the plot. Currently, if a gene belongs to more than one group, only the colour of the highest group number will be displayed, but both groups will be included in the download table of selected genes. The shape of the point will change if a gene belongs to more than one group. Whether or not to inlude the group in the plot and download list can be toggled above the plot. Default state after adding a group is to include. <br/>
               <i><b>Group Name:</i></b> Custom name input for each group to highlight in plot. The name supplied will be used as group identity on the plot legend as well as in the group variable column when downloading the selected gene list. <br/>
               <i><b>Colour for Group:</i></b> Available colours represent the Wong colour palette that has been optimized for colour blind individuals. See <a href=\"https://www.nature.com/articles/nmeth.1618\">Points of view: Color blindness</a> <br/>
               <i><b>Input Type for Group:</i></b> Options include:<br/> 
               &emsp;<i>\"Plot Click\"</i> which allows users to click points on the figure to include in the group, or to select a group of points at the same time by holding the mouse click and dragging to expand. This type includes a button to <i>Reset gene selection,</i> which will clear all points selected in the group by clicking the plot.<br/>
               <p style=\"background-color:darksalmon;\">
    <b>NOTE:</b> The plot can lag a bit as it adds new points and features. Be patient. Trying to click points or change attributes while the app is still thinking can cause unexpected behaviour, like assigning a point to the wrong group.
    </p>
               &emsp;<i>\"Specified Values\"</i> which provides input boxes for numeric values that describe maximum and minimum for each of X and Y values. All 4 values are taken into account when determining which points should be labelled, and should be updated if using this feature. If log10 transformation has been selected for Y-values, input into Y max/min does not take into account the transformation, and original data value should be used.<br/>
               &emsp;<i>\"Gene Input\"</i> which provides a text box to input genes desired to highlight on plot. Genes can be separated by a comma, space, or newline characater. The input is case-sensitive for exact match in gene list as indicated in the \"Labels for data points\" attribute specified. If a gene is not recognized as being in the set of plotted datapoints, it will be ignored.<br/>
               <br/>
               <b>Construct Plot:</b><br/>
               Figure will not be constructed until the button is clicked, but once constructed will respond to adjustments in real time. Clicking \"Construct Plot\" when a plot is already active will remove all points selected by clicking the plot. When hovering over datapoints on the plot, the associated point information will be displayed to the right of the plot. If log10 transformation has been selected, the Y-value displayed will reflect that transformation.
               <br/>
               <b>Groups to include in plot/save:</b> Different parameters can be tested by adding new groups in the sidebar, then included or removed from the output with the checkbox.<br/>
               <b>Current Group:</b><br/>
               Groups will be identified by label provided by user. Although current app functionality will display all groups described, only groups with input type as \"Plot Click\" will allow selection by clicking points on the plot.<br/>
               <br/>
               <b>Warnings</b><br/>
               Below the plot, any warning from ggplot for unlabelled data points will be displayed here. Any points that are not labelled on the plot for having too many overlaps with other data points will still be saved within the selected gene lists for download.<br/>
               To override the aesthetic of not labelling points in areas of the plot that are too dense, click the \"Show all labels\" button.<br/>
               <br/>
               <b>Save</b><br/>
               <i><b>Save plot:</b></i> Save pdf image of figure as constructed.<br/>
               <i><b>Save genes:</b></i> All selected genes will be saved as a table with tab separated values. Table will include gene ID, X value, Y value (original, not log10 transformed), and group. Option to order saved table by name, group, x-value, or y-value. <br/>
               <br/>
               <br/>
               <p style=\"background-color:powderblue;\">
    <b>NOTE:</b> This Shiny is still in progress, but should be functional for most purposes. Please see associated <a href=\"https://github.com/Lisa1254/Plot_label_shiny\"> GitHub</a> for current issues being worked on, or to submit a feature request.<br/>
    </p><br/>
    <b>Updates since initial publish:</b>
    <ul style=\"list-style-type:disc\">
    <li><b>July 5, 2022:</b> added additional filetype input options and warning for unsupported type</li>
         ")
  })
  
  #
  #-----------------------Example 1------------------
  
  output$fdr_image <- renderImage({
    return(list(
      src = "Ex/fdr_ex_screenshot.png",
      filetype = "image/png",
      alt = "Screenshot of 2 screen comparison",
      width = "550",
      height = "400"
    ))
  }, deleteFile = FALSE)
  
  output$fdr_desc <- renderUI({
    HTML("<br/><p style=\"background-color:powderblue;\">
    To practice with this type of input data, select \"FDR Example\" in the Source Data section. A selection of 1000 genes and 4 screens from drugZ output have their fdr_synth values included.
    </p>
    Pictured above is a screenshot of using the app to compare the fdr_synth output of drugZ analysis for two screens. When saving the image using the button in the Shiny, only the plot will be saved, not the entire screenshot.<br/><br/>
         For the purposes of the example, genes are only identified by index number instead of name. Three groups were selected for adding highligh colours on the plot. CPT & Bleomycin were input using range data to highlight genes with FDR <= 0.01. Notice how gene \"1148\" has a triangle shape on the plot to indicate that it is included in more than one group. The green points for \"Other\" group were labelled by clicking the points on the plot to select. The \"Current group for labelling\" at the top is selected to the \"Other\" group to indicate that points clicked in the plot get assigned to the correct group. Group 3, corresponding to the \"Test\" label has been unclicked at the section for groups to include, so that any genes within that group are not shown in the image or returned in the download of selected genes.<br/><br/>
         The small table at the right of the image shows the gene that the mouse was hovering over at the time of the screenshot.<br/><br/>
         The bottom of the image shows the warning from the ggplot labelling algorithm that was unable to label 21 selected points in the image. As you can see, there are a series of unlabelled points in the upper left corner for the CPT group that are too close together for the program to be able to properly label them in the image. These genes are still included in the saved genes list.<br/><br/>
         Shown below are the first 13 rows of the datatable saved by downloading the genes selected to make the shown image. The display here has rounded to 6 digits, but the saved table will include all digits present in the source data. They were saved in Name order.<br/>"
    )
  })
  
  output$fdr_table <- renderTable({
    head(read.delim("Ex/fdr_ex_genes.txt"), 13)
  }, digits = 6)
  
  #
  #-----------------------Example 2------------------
  
  output$volcano_image <- renderImage({
    return(list(
      src = "Ex/volcano_ex_screenshot.png",
      filetype = "image/png",
      alt = "Screenshot of volcano plot constructedfrom MAGeCK data",
      width = "550",
      height = "400"
    ))
  }, deleteFile = FALSE)
  
  output$volcano_desc <- renderUI({
    HTML("<br/><p style=\"background-color:powderblue;\">
    To practice with this type of input data, select \"Volcano Example\" in the Source Data section. A selection of 1000 genes have been selected from MAGeCK analysis gene summary output file. X-values have been selected from \"neg.lfc\" since \"neg.lfc\" and \"pos.lfc\" have identical values, and 2-value Y input was selected.
    </p>
    Pictured above is a screenshot of using the app to compare the LFC and Score for each gene from the output of MAGeCK analysis for a single screen. When saving the image using the button in the Shiny, only the plot will be saved, not the entire screenshot.<br/><br/>
    In this example, 2 columns were selected for the Y-input in order to account for the separation of positive and negative score in MAGeCK output. Log10 transformation and reverse y-axis were also selected. For the purposes of the example, genes are only identified by index number instead of name.<br/><br/>
    Three groups were selected for adding highligh colours on the plot. Sensitizer & Resistance were highlighted by using the mouse to click and drag over the sections desired for labelling while the correct group was indicated in the \"Current group for labelling\" section. Notice how gene \"1\" has a triangle shape on the plot to indicate that it is included in more than one group. The green points for \"Glycosylase\" group were labelled by inputing a list of gene names into the text box with \"Gene Input\" indicated as group type. When displaying information about a point that the mouse hovers over, the displayed Y-value will be the value as described in the plot, which includes the log10 transformation if selected.<br/><br/>
         The bottom of the image shows the warning from the ggplot labelling algorithm that was unable to label 9 selected points in the image. As you can see, there are a series of unlabelled points in the in the middle of the plot from the \"Glycosylase\" group that are too close together for the program to be able to properly label them in the image. These genes are still included in the saved genes list.<br/><br/>
         Shown below are the first 6 rows of the datatable saved by downloading the genes selected to make the shown image. The display here has rounded to 4 digits, but the saved table will include all digits present in the source data. They were saved in Name order. The saved Y-values will reflect the original data value from the appropriate Y-input column, not the log10 scaled value.<br/>"
    )
  })
  
  output$volcano_table <- renderTable({
    head(read.delim("Ex/volcano_ex_genes.txt"), 6)
  }, digits = 4)
  
  #
  #-----------------------Testing Section-------------

  #

  
})

