server <- function(input, output, session) { 
  
  ### Data tab items #####
  
  # Get data
  df_original <- reactive({
    
    if (input$data_input == 1) {
      if (input$data_choice == 1) 
        data <- read_excel("data/mow data.xlsx")
      else if (input$data_choice == 2)
        data <- read_excel("data/dunes.xlsx")
      else if (input$data_choice == 3)
        data <- read_excel("data/Framingham subset.xlsx")
      else if (input$data_choice == 4)
          data <- read_excel("data/Hubbard brook tree surveys.xlsx")
      else
        data <- read_excel("data/CCASE saplings.xlsx")
    } 
    
    else if (input$data_input == 2) {
      file_in <- input$upload_excel
      if (is.null(input$upload_excel)) {
        return(data.frame(x = "Select your datafile"))
      } else {
        data <- read_excel(file_in$datapath, 
                           na=c("NA","na","n/a","N/A",""))
      }
    }
    else {
      file_in <- input$upload_csv
      if (is.null(input$upload_csv)) {
        return(data.frame(x = "Select your datafile"))
      } else {
        data <- read_csv(file_in$datapath, 
                             na=c("NA","na","n/a","N/A",""),
                             col_types = cols())
      }
    }
    
    data <- mutate_if(data, is.character, as.factor)
    return(data)
  })

  # Make lists of variables by type  
  observe({

    avail_num <- c(df_original() %>% 
                     select_if(~ is.numeric(.) || is.double(.) || is.integer(.)) %>% 
                     names
                   )
    if (identical(avail_num, character(0))) 
      avail_num <- c("no numeric variables available" = ".")
    
    avail_cat <- c(df_original() %>% 
                     select_if(is.factor) %>% 
                     names
                   )
    if (identical(avail_cat, character(0)))
      avail_cat <- c("no categorical variables available" = ".")

    avail_groups <- c("no groups" = ".", avail_cat)
    
    updateSelectInput(session, "x_var_num", choices = avail_num)
    updateSelectInput(session, "x_var_cat", choices = avail_cat)
    updateSelectInput(session, "y_var", choices = avail_num)
    updateSelectInput(session, "groups", choices = avail_groups)

    
  })
  
  # Summarize data
  
  cat_summary <- reactive({
    
    df.cat <- select_if(df_original(), is.factor)
    if (ncol(df.cat) > 0) {
      df.scr <- ds_screener(df.cat)
      df.cat.sum <- data.frame(variable = df.scr[["Variables"]],  
                               levels = sapply(df.scr[["levels"]],"toString")
      )
      return(df.cat.sum)
    } else {
      return(data.frame(x = "No categorical variables found."))
    }
    
  })
  
  output$cat_table <- renderTable(
    cat_summary()
  )
  
  num_summary <- reactive({
    
    df.num <- select_if(df_original(), is.numeric)
    if (ncol(df.num) > 0) {
      df.stats <- ds_tidy_stats(df.num)
      df.num.sum <- data.frame(df.stats)
      df.num.sum <- select(df.num.sum,
                           variable = vars,
                           max, Q3 = q3, mean, median, Q1 = q1, min, sd = stdev)
      return(df.num.sum)
    } else {
      return(data.frame(x = "No numeric variables found."))
    }
    
  })
  
  output$num_table <- renderTable(
    num_summary(), digits = 3
  )
  
  ### Graph tab items #####
  
  df_graph <- eventReactive(input$go_plot, {
    
    if (input$Type == "hist" || input$Type == "scatter") {
      v1 <- pull(df_original(),
                   as.character(input$x_var_num))
    } else {
      v1 <- pull(df_original(),
                   as.character(input$x_var_cat))
    }
    
    if (input$Type == "hist" || input$Type == "bar") {
      v2 <- rep(NA, length(v1))
    } else {
      v2 <- pull(df_original(),
                   as.character(input$y_var))
    }
    
    if (input$groups == ".") {
      v3 <- rep(NA, length(v1))
    } else {
      v3 <- pull(df_original(),
                   as.character(input$groups))
    }
    
    df_working <- data.frame(v1, v2, v3)
    names(df_working) <- c("x_var", "y_var", "group_var")
    
    if (input$reorder_x) {
      df_working[,1] <- factor(df_working[,1], 
                               levels = as.vector(input$x_sorted_order))
    }
    if (input$reorder_groups) {
      df_working[,3] <- factor(df_working[,3], 
                               levels = as.vector(input$groups_sorted_order))
    }
    
    df_working <- drop_na(df_working, x_var)
    if (input$Type != "hist" && input$Type != "bar") {
      df_working <- drop_na(df_working, y_var)
    }
    if (input$groups != ".") {
      df_working <- drop_na(df_working, group_var)
    }
    
    return(df_working)
    
  })
  
  output$change_x <- renderUI({
    
    orderInput("x_sorted", "",
               items = levels(df_graph()[, 1]),
               width = "100%",
               class = "ui-sortable"
    )
  })
  
  output$change_groups <- renderUI({
    
    orderInput("groups_sorted", "",
               items = levels(df_graph()[, 3]),
               width = "100%",
               class = "ui-sortable"
    )
  })

  # Create graph code
  string_code <- eventReactive(input$go_plot, {
    
    if (input$Type == "hist" || input$Type == "scatter") {
      name_x_var <- toString(input$x_var_num)
    } else {
      name_x_var <- toString(input$x_var_cat)
    }
    
    if (input$Type == "hist" || input$Type == "bar") {
      name_y_var <- "count"
    } else {
      name_y_var <- toString(input$y_var)
    }
    
    if (input$groups == ".") {
      name_group_var <- ""
    } else {
      name_group_var <- toString(input$groups)
    }
    
    # specify aesthetic
    p <- paste(
      "ggplot(df, aes(x = x_var",
      
      if (name_y_var != "count") ", y = y_var",
      
      if (input$groups != "." && input$Type != "scatter") 
        ", fill = group_var",
      if (input$groups != "." && input$Type == "scatter") 
        ", shape = group_var",
      
      ")) +",
      
      sep = " "
    )
    
    # specify geom + basic parameters
    
    if (input$Type == "hist" || input$Type == "bar" || input$Type == "box") {
      p <- paste(p,
                 if (input$Type == "hist") {
                   if(input$adj_bins) "geom_histogram(binwidth = input$bin_w, "
                   else "geom_histogram(nbins = 10, "
                 },
                 if (input$Type == "bar")  "geom_bar(",
                 if (input$Type == "box")  "geom_boxplot(",
                 "color = 'black'",
                 if (input$groups == ".") ", fill = 'white'"
                 else ", position = 'dodge'",
                 
                 ")",
                 sep = ""
      )
    }
    
    if(input$Type == "scatter") {
      p <- paste(p, "geom_point(fill = 'black', size = input$sym_size)", sep="")
    }
    
    if (input$Type == "bar_e") {
      p <- paste(p, 
                 
                 if (input$error_bars != "no bars" && input$bars_place == "back") {
                   paste ("stat_summary(fun.data = ", 
                          if (input$error_bars == "sd")
                            "'mean_sdl', fun.args = list(mult=1), ",
                          if (input$error_bars == "se")
                            "'mean_se'",
                          ", geom = 'errorbar', color = 'black', width = .3",
                          ", position = position_dodge(0.9)) +",
                          sep = "")
                 },
                 
                 "stat_summary(fun.y = 'mean', geom = 'bar', color = 'black'",
                 if (input$groups == ".") ", fill = 'white'"
                 else ", position = 'dodge'",
                 ")",
                 
                 if (input$error_bars != "no bars" && input$bars_place == "front") {
                   paste ("+ stat_summary(fun.data = ", 
                          if (input$error_bars == "sd")
                            "'mean_sdl', fun.args = list(mult=1), ",
                          if (input$error_bars == "se")
                            "'mean_se'",
                          ", geom = 'errorbar', color = 'black', width = .3",
                          ", position = position_dodge(0.9))",
                          sep = "") 
                 },
                 
                 sep=" ")
    }
    
    
    # add graph specific options
    
    if (input$Type != "scatter" && input$groups != ".") {
      p <- paste(p, "+ scale_fill_grey(start = 1, end = 0.8)", sep = " ")
    }
    
    # add regression line
    if (input$Type == "scatter" && input$line) 
      p <- paste(p, "+ geom_smooth(method = 'lm', se = F, color = 'black')", sep=" ")
    
    # axis labels
    p <- paste(p, "+ labs(x ='",
               if (input$label_x_axis) "input$lab_x" else name_x_var,
               "', y = '",
               if (input$label_y_axis) "input$lab_y" else name_y_var,
               "')", sep="")
    
    # adjust axes
    p <- paste(p, "+ scale_y_continuous(expand = c(0,0))")
    if(input$num_x_values) {
      p <- paste(p, "+ coord_cartesian(xlim = c(input$min_x,input$max_x)",
                 if (input$num_y_values)
                   ", ylim = c(input$min_y,input$max_y)",
                 ")",
                 sep = "")
    } else if (input$num_y_values) {
      p <- paste(p, "+ coord_cartesian(ylim = c(input$min_y,input$max_y))", 
                 sep = "")
    }
    
    # adjust legend title
    if (input$groups != ".") {
      p <- paste(p, "+ labs(",
                 if (input$Type == "scatter") "shape" else "fill",
                 if (input$label_legend) " = 'input$leg_ttl')"
                 else paste(" = '", name_group_var, "')"),
                 sep = "")
    }
    
    # adjust theme
    p <- paste(p, 
               " + theme(axis.title = element_text(size = input$fnt_sz_ttl),", 
               "axis.text = element_text(size = input$fnt_sz_ax),", 
               "legend.position = 'input$pos_leg',",
               "legend.text = element_text(size = input$fnt_sz_leg_ttl),",
               "legend.title = element_text(size = input$fnt_sz_leg),",
               "legend.key.size = unit(input$leg_key,'lines'),",
               "plot.title = element_blank(),",
               "plot.background = element_blank())",
               if (input$rot_txt)
                 "axis.text.x = element_text(angle = 45, hjust = 1),",
               sep = "")
    
    # Replace name of variables by values
    p <- str_replace_all(
      p,
      c("input\\$sym_size" = as.character(input$sym_size),
        "input\\$bin_w" = as.character(input$bin_w),
        "input\\$lab_x" = as.character(input$lab_x),
        "input\\$lab_y" = as.character(input$lab_y),
        "input\\$min_x" = as.character(input$min_x),
        "input\\$max_x" = as.character(input$max_x),
        "input\\$min_y" = as.character(input$min_y),
        "input\\$max_y" = as.character(input$max_y),
        "input\\$leg_ttl" = as.character(input$leg_ttl),
        "input\\$fnt_sz_ttl" = as.character(input$fnt_sz_ttl),
        "input\\$fnt_sz_ax" = as.character(input$fnt_sz_ax),
        "input\\$fnt_sz_leg_ttl" = as.character(input$fnt_sz_leg_ttl),
        "input\\$fnt_sz_leg_ax" = as.character(input$fnt_sz_leg_ax),
        "input\\$leg_key" = as.character(input$leg_key),
        "input\\$pos_leg" = as.character(input$pos_leg)
      )
    )
    
    p
    
  })
  
  # Output graph
  width.p <- reactive ({ input$fig_width })
  height.p <- reactive ({ input$fig_height })
  width_download <- reactive ({ input$fig_width_download })
  height_download <- reactive ({ input$fig_height_download })
  
  output$out_ggplot <- renderPlot(width = width.p,
                                  height = height.p, {
                                    # evaluate the string RCode as code
                                    df <- df_graph()
                                    p <- eval(parse(text = string_code()))
                                    p
                                  })
  
  # Reset graph
  observeEvent(input$reset_plot, {
    
    reset("graph_options_2")
    reset("graph_options_3")
    reset("graph_options_4")
    reset("graph_options_5")
    reset("graph_options_6")
    reset("graph_options_7")
    
  })
  
  # Download graph
  
  output$download_plot <- downloadHandler(
    filename <- function() {
      paste("Figure_", Sys.time(), ".png", sep = "")
    },
    content <- function(file) {
      df <- df_graph()
      p <- eval(parse(text = string_code()))
      save_plot(file, p, base_width = width_download(),
                base_height = height_download())
    }
  )
 
  ### Info tab items #####
  output$mow_metadata <- renderTable(
    mow.meta <- read_excel("data/mow data.xlsx", sheet = 2)
  )
  
  output$dune_metadata <- renderTable(
    dune.meta <- read_excel("data/dunes.xlsx", sheet = 2)
    )
  
  # End R-session when browser closed
  session$onSessionEnded(stopApp)
}
