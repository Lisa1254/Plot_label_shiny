source("env.R")




shinyServer(function(input, output, session) {
  
  #-----------------------Data-------------------------
  
  # Upload data and define parameters of scatter plot
  
  #Receive & verify data
  data <- reactive({
    req(input$txt_data)
    read.delim(file =input$txt_data$datapath)
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
    req(input$txt_data)
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
  
  # Set up graphical parameters for genes of interest, up to three groups
  all_labels <- reactive(c(input$gp1, input$gp2, input$gp3)[1:input$num_gps])
  #all_labels <- reactive({
  #  disp_labs <- vector()
  #  if (input$type1 == "Plot Click") {
  #    disp_labs <- c(disp_labs, input$gp1)
  #  }
  #  if ((input$type2 == "Plot Click") ) { #& (input$num_gps >= 2)
  #    disp_labs <- c(disp_labs, input$gp2)
  #  }
  #  if ((input$type3 == "Plot Click") ) { #& (input$num_gps == 3)
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
    plot_gp_data <- plot_data()[complete.cases(plot_data()),]
    plot_gp_data$Gp <- rep_len("Main", nrow(plot_gp_data))
    plot_gp_data$Mult <- rep_len(0, nrow(plot_gp_data))
    ##Define genes to be highlighted
    if (input$num_gps  >= 1) {
      all_genes_1 <- c(gene_sub1(), genes_in1(), selected1())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_1, "Gp1", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_1, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if (input$num_gps >=2) {
      all_genes_2 <- c(gene_sub2(), genes_in2(), selected2())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_2, "Gp2", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_2, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    if (input$num_gps == 3) {
      all_genes_3 <- c(gene_sub3(), genes_in3(), selected3())
      plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% all_genes_3, "Gp3", plot_gp_data$Gp)
      plot_gp_data$Mult <- ifelse(plot_gp_data$ID %in% all_genes_3, plot_gp_data$Mult+1, plot_gp_data$Mult)
    }
    
    plot_gp_data$Gp <- factor(plot_gp_data$Gp)
    plot_gp_data[which(plot_gp_data$Mult == 1),"Mult"] <- 0
    plot_gp_data$Mult <- factor(plot_gp_data$Mult)
    
    #To get input genes plotted last, and therefore with the colour displaying when cluttered, organize those points to the bottom. For now, will just put all "Main" at the top, but consider adding feature to check for if input type is a gene list, as that gets priority to the bottom rather than a selection type
    ind_mains <- which(plot_gp_data$Gp == "Main")
    plot_gp_data_ord <- rbind(plot_gp_data[ind_mains,],
                              plot_gp_data[-ind_mains,])
    
    #Define colours
    cols <- c("Main" = input$col_m, "Gp1" = input$col1, "Gp2" = input$col2, "Gp3" = input$col3)[1:(input$num_gps+1)]
    
    #Set up scatter plot with or without colour for input groups
    if (input$num_gps == 0){
      g <- ggplot(data=plot_gp_data_ord) +
        geom_point(aes(x=X_Value, y=Y_Value), shape = 16, size = 3, color = input$col_m) +
        theme(legend.position = "none")
        
    } else {
      g <- ggplot(data=plot_gp_data_ord) +
        geom_point(aes(x=X_Value, y=Y_Value, color = Gp, shape = Mult), size = 3) +
        scale_color_manual(name = "Groups", labels = c("NA", all_labels()), values = cols) +
        geom_text_repel(aes(x=X_Value, y=Y_Value, label=ifelse(Gp=="Main", '', ID)), 
                        min.segment.length = 0, size = 3, max.overlaps = 15)

    }
    
    if ((input$num_gps != 0) & (max(as.numeric(plot_gp_data$Mult)) == 1)) {
      g <- g + scale_shape_manual(values = c(16), guide = "none")
    } else if (max(as.numeric(plot_gp_data$Mult)) == 2) {
      g <- g + scale_shape_manual(name = "Number of groups", labels = c("0-1 groups", "2 groups"), values = c(16,17))
    } else if (max(as.numeric(plot_gp_data$Mult)) == 3) {
      g <- g + scale_shape_manual(name = "Number of groups", labels = c("0-1 groups", "2 groups", "3 groups"), values = c(16,17,15))
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
    nearPoints(plot_data(), input$plot_hover)
  })
  
  output$nT_hover <- renderTable(nearTable_h())
  
  #Use NearPoints to add labels to plot on click
  
  id_lab <- reactive({
    req(input$plot_click)
    if ((input$current_gp == input$gp1) & (input$type1 == "Plot Click")) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp2) & (input$type2 == "Plot Click")) {
      nearPoints(plot_data(), input$plot_click)[,1]
    } else if ((input$current_gp == input$gp3) & (input$type3 == "Plot Click")) {
      nearPoints(plot_data(), input$plot_click)[,1]
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
    if ((input$current_gp == input$gp1) & (input$type1 == "Plot Click")) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp2) & (input$type2 == "Plot Click")) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
    } else if ((input$current_gp == input$gp3) & (input$type3 == "Plot Click")) {
      brushedPoints(plot_data(), input$plot_brush)[,1]
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
    HTML(" This Shiny application makes a scatterplot from the X & Y values as supplied in a data table uploaded in a tab delimited .txt format. Application currently supports three different groups of highlighting labels with accepted input for gene choice for each group including plot interaction (click or drag select), range of spcified values, or input gene list.<br/>
    <br/>
    <br/>
    <p style=\"background-color:powderblue;\">
    <b>NOTE:</b> This Shiny is still in progress, but should be functional for most purposes. Please see associated <a href=\"https://github.com/Lisa1254/Plot_label_shiny\"> GitHub</a> for current issues being worked on, or to submit a feature request.
    </p><br/>
              <b>Source Data:</b><br/>
               Input should be a tab delimited .txt file that includes data to be used in construction of scatterplot.<br/>
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
               <i><b>How many highlighted groups?:</i></b> Currently app functionality supports up to 3 groups of gene selection for highlighting on the plot. Currently, if a gene belongs to more than one group, only the colour of the highest group number will be displayed, but both groups will be included in the download table of selected genes. The shape of the point will change if a gene belongs to more than one group.<br/>
               <i><b>Group Name:</i></b> Custom name input for each group to highlight in plot. The name supplied will be used as group identity on the plot legend as well as in the group variable column when downloading the selected gene list. <br/>
               <i><b>Colour for Group:</i></b> Available colours represent the Wong colour palette that has been optimized for colour blind individuals. See <a href=\"https://www.nature.com/articles/nmeth.1618\">Points of view: Color blindness</a> <br/>
               <i><b>Input Type for Group:</i></b> Options include:<br/> 
               &emsp;<i>\"Plot Click\"</i> which allows users to click points on the figure to include in the group, or to select a group of points at the same time by holding the mouse click and dragging to expand. This type includes a button to <i>Reset gene selection,</i> which will clear all points selected in the group by clicking the plot.<br/>
               &emsp;<i>\"Specified Values\"</i> which provides input boxes for numeric values that describe maximum and minimum for each of X and Y values. If log10 transformation has been selected for Y-values, input into Y max/min does not take into account the transformation, and original data value should be used.<br/>
               &emsp;<i>\"Gene Input\"</i> which provides a text box to input genes desired to highlight on plot. Genes can be separated by a comma, space, or newline characater. If a gene is not recognized as being in the set of plotted datapoints, it will be ignored.<br/>
               <br/>
               <b>Construct Plot:</b><br/>
               Figure will not be constructed until the button is clicked, but once constructed will respond to adjustments in real time. When hovering over datapoints on the plot, the associated point information will be displayed to the right of the plot. If log10 transformation has been selected, the Y-value displayed will reflect that transformation.
               <br/>
               <b>Current Group:</b><br/>
               Groups will be identified by label provided by user. Although current app functionality will display all groups described, only groups with input type as \"Plot Click\" will allow selection by clicking points on the plot.<br/>
               <br/>
               <b>Warnings</b><br/>
               Below the plot, any warning from ggplot for unlabelled data points will be displayed here. Any points that are not labelled on the plot for having too many overlaps with other data points will still be saved within the selected gene lists for download.<br/>
               <br/>
               <b>Save</b><br/>
               <i><b>Save plot:</b></i> Save pdf image of figure as constructed.<br/>
               <i><b>Save genes:</b></i> All selected genes will be saved as a table with tab separated values. Table will include gene ID, X value, Y value (original, not log10 transformed), and group. Option to order saved table by name, group, x-value, or y-value. <br/>
               <br/>
         ")
  })
  
  #
  #-----------------------Example 1------------------
  
  output$fdr_image <- renderImage({
    return(list(
      src = "Ex/fdr_ex_screenshot.png",
      filetype = "image/png",
      alt = "Screenshot of 2 screen comparison",
      width = "600",
      height = "400"
    ))
  }, deleteFile = FALSE)
  
  output$fdr_desc <- renderUI({
    HTML("<br/>Pictured above is a screenshot of using the app to compare the fdr_synth output of drugZ analysis for two screens. When saving the image using the button in the Shiny, only the plot will be saved, not the entire screenshot.<br/><br/>
         Three groups were selected for adding highligh colours on the plot. CPT & Bleomycin were input using range data to highlight genes with FDR <= 0.01. Notice how ATM has a triangle shape on the plot to indicate that it is included in more than one group. The green points for \"Other\" group were labelled by clicking the points on the plot to select. The \"Current group for labelling\" at the top is selected to the \"Other\" group to indicate that points clicked in the plot get assigned to the correct group.<br/><br/>
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
      width = "600",
      height = "400"
    ))
  }, deleteFile = FALSE)
  
  output$volcano_desc <- renderUI({
    HTML("<br/>Pictured above is a screenshot of using the app to compare the LFC and Score for each gene from the output of MAGeCK analysis for a single screen. When saving the image using the button in the Shiny, only the plot will be saved, not the entire screenshot.<br/><br/>
    In this example, 2 columns were selected for the Y-input in order to account for the separation of positive and negative score in MAGeCK output. Log10 transformation and reverse y-axis were also selected to display the most significant genes at the top of the plot, and spaced for ease of viewing.<br/><br/>
    Three groups were selected for adding highligh colours on the plot. Sensitizer & Resistance were highlighted by using the mouse to click and drag over the sections desired for labelling while the correct group was indicated in the \"Current group for labelling\" section. Notice how NEIL1 has a triangle shape on the plot to indicate that it is included in more than one group. The green points for \"Glycosylase\" group were labelled by inputing a list of gene names into the text box with \"Gene Input\" indicated as group type.<br/><br/>
         The bottom of the image shows the warning from the ggplot labelling algorithm that was unable to label 8 selected points in the image. As you can see, there are a series of unlabelled points in the in the middle of the plot from the \"Glycosylase\" group that are too close together for the program to be able to properly label them in the image. These genes are still included in the saved genes list.<br/><br/>
         Shown below are the first 6 rows of the datatable saved by downloading the genes selected to make the shown image. The display here has rounded to 4 digits, but the saved table will include all digits present in the source data. They were saved in Name order.<br/>"
    )
  })
  
  output$volcano_table <- renderTable({
    head(read.delim("Ex/volcano_ex_genes.txt"), 6)
  }, digits = 4)
  
  #
  #-----------------------Testing Section-------------
  
  #Using a button to add groups. Will have remove group within each group's section so they don't have to be removed in the same order.
  counter <- reactiveVal(0)
  
  observeEvent(input$add_gp,{
    if (counter() < 7) {
      counter(counter() + 1)
    } else {
      createAlert(session, "gp_alert", "exampleAlert", title = "Oops", content = "Maximum group number is seven", append = FALSE)
    }
  })
  output$cnt_gps <- renderPrint({
    cat("The number: ", as.character(counter()))
  })
  
})

