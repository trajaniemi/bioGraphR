library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)
library(Hmisc)  
library(tidyverse)
library(readxl)
library(cowplot)
library(descriptr)

ui <- dashboardPage( 
  
  dashboardHeader(
    title = "bioGraphR"
  ),
  
  dashboardSidebar(
     
    sidebarMenu(
      menuItem("Data", tabName = "data_tab"),
      menuItem("Graphs", tabName = "graph_tab"),
      menuItem("Info", tabName = "info_tab")
    )
    
  ),
  
  dashboardBody(
    
    useShinyjs(),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
    ),
    
    tabItems(

      tabItem(tabName = "data_tab", 
      ### Data tab #####        
              fluidRow(
                
                column(4,
                       box(
                         title = "Select a dataset", 
                         width = NULL, status = "primary",
                         solidHeader = T,
                         
                         "You may use a practice dataset, or upload your own data.",
                         
                         radioButtons("data_input", "",
                                      choices = list("Use practice data" = 1,
                                                     "Upload an Excel file" = 2,
                                                     "Upload a text file (.csv)" = 3),
                                      selected = 1),

                         conditionalPanel("input.data_input == '1'",
                                          h5("Choose a practice dataset: "),
                                          selectInput("data_choice", "",
                                                      choices = list("Reduced mowing" = 1,
                                                                     "Dune environment" = 2)
                                          )
                         ),
                         
                         conditionalPanel("input.data_input == '2'",
                                          h5("Select a file: "),
                                          fileInput("upload_excel", "", 
                                                    multiple = FALSE,
                                                    accept=c('application/vnd.ms-excel',
                                                             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                             '.xls',
                                                             '.xlsx')
                                          )
                         ),
                       
                         conditionalPanel("input.data_input == '3'",
                                          h5("Select a file: "),
                                          fileInput("upload_csv", "", 
                                                    multiple = FALSE,
                                                    accept = c(".csv",
                                                               ".text/csv")
                                          )
                         )
                      )
                ),
                
                column(8,
                       box(
                         title = "Data summary", 
                         width = NULL, status = "primary", solidHeader = T,
                         strong("Categorical variables"),
                         tableOutput("cat_table"),
                         strong("Numeric variables"),
                         tableOutput("num_table")
                       )
                )
                
              ) # close row
              
      ),  
      ### close data tab #####
      
      tabItem(tabName = "graph_tab", 
              ### Graph tab #####       
              fluidRow(
                
                ### left column: graph type and tabbed options #####
                column(4,
                       box(
                         title = "Graph type", width = NULL, status = "primary",
                         solidHeader = T,
                         
                         selectInput("Type", "Select a graph type",
                                     choices = c("Histogram" = "hist",
                                                 "Bar graph (counts)" = "bar",
                                                 "Boxplot" = "box",
                                                 "Bar graph (means)" = "bar_e",
                                                 "Scatterplot" = "scatter")
                         )
                       ),
                       
                       box(
                         title = "Graph options", width = NULL, height = 25,
                         status = "primary",
                         solidHeader = T
                       ),
                       
                       tabBox(
                         width = NULL,
                         
                         tabPanel("Variables",
 
                                  div(id = "graph_options_1",
                                      
                                      conditionalPanel("input.Type=='hist' || 
                                                       input.Type=='scatter'",
                                                       selectInput("x_var_num", 
                                                                   "X Variable", 
                                                                   choices = "")
                                      ),
                                      conditionalPanel("input.Type=='box' || 
                                                       input.Type=='bar_e' || 
                                                       input.Type=='bar'",
                                                       selectInput("x_var_cat", 
                                                                   "X Variable", 
                                                                   choices = "")
                                      ),
                                      conditionalPanel("input.Type=='box' || 
                                                       input.Type=='bar_e' || 
                                                       input.Type=='scatter'",
                                                       selectInput("y_var", 
                                                                   "Y Variable", 
                                                                   choices = "")
                                      ),
                                      selectInput("groups", 
                                                  "Grouping variable", 
                                                  choices = "")
                                  )
                         ),
                         
                         tabPanel("X axis",

                                  div(id = "graph_options_2",
                                      
                                      checkboxInput("label_x_axis", 
                                                    strong("Change axis label"), 
                                                    F),
                                      conditionalPanel("input.label_x_axis",
                                                       textInput("lab_x", 
                                                                 "X axis label", 
                                                                 "label x-axis")
                                      ),
                                      
                                      conditionalPanel("input.Type=='hist'",
                                                       checkboxInput("adj_bins",
                                                                     strong("Change bin width"),
                                                                     F),
                                                       conditionalPanel("input.adj_bins", 
                                                                        textInput("bin_w","Bin width:", 5)
                                                       )
                                      ), 
                                      
                                      conditionalPanel("input.Type == 'scatter' 
                                                       || input.Type == 'hist'",
                                                       checkboxInput("num_x_values", 
                                                                     strong("Change value range"), 
                                                                     F),
                                                       conditionalPanel("input.num_x_values",
                                                                        textInput("min_x", "minimum x", 0),
                                                                        textInput("max_x", "maximum x", 100)
                                                       )
                                      ),
                                      conditionalPanel("input.Type == 'bar' || 
                                                       input.Type == 'box' || 
                                                       input.Type == 'bar_e'",
                                                       checkboxInput("reorder_x",
                                                                     strong("Change order of values"),
                                                                     F),
                                                       conditionalPanel("input.reorder_x",
                                                                        "Drag and drop values to change order",
                                                                        uiOutput("change_x")
                                                       )
                                      ),
                                      checkboxInput("rot_txt", 
                                                    strong("Rotate x-axis text"), 
                                                    F)
                                  )
                         ),
                         
                         tabPanel("Y axis",

                                  div(id = "graph_options_3",
                                      
                                      checkboxInput("label_y_axis", 
                                                    strong("Change axis label"), 
                                                    F),
                                      conditionalPanel("input.label_y_axis",
                                                       textInput("lab_y", "
                                                                 Y-axis label", 
                                                                 "label x-axis")
                                      ),
                                      conditionalPanel("input.Type == 'scatter' || 
                                                       input.Type == 'bar_e' || 
                                                       input.Type == 'box'",
                                                       checkboxInput("num_y_values", 
                                                                     strong("Change axis value range"), 
                                                                     F),
                                                       conditionalPanel("input.num_y_values",
                                                                        textInput("min_y", "minimum y", 0),
                                                                        textInput("max_y", "maximum y", 100)
                                                       )
                                      )
                                  )
                         ),
                         
                         tabPanel("Legend",
                                  
                                  div(id = "graph_options_4",
                                      
                                      conditionalPanel("input.groups != '.'",
                                                       checkboxInput("label_legend",
                                                                     strong("Change legend title"),
                                                                     F),
                                                       conditionalPanel("input.label_legend",
                                                                        textInput("leg_ttl", 
                                                                                  "Legend title", 
                                                                                  "legend title")
                                                       ),
                                                       checkboxInput("reorder_groups",
                                                                     strong("Change order of values"),
                                                                     F),
                                                       conditionalPanel("input.reorder_groups",
                                                                        "Drag and drop values to change order",
                                                                        uiOutput("change_groups")
                                                       ),
                                                       radioButtons("pos_leg", 
                                                                    strong("Legend position"),
                                                                    choices = c("right", "left", "top", "bottom"),
                                                                    selected = "right"),
                                                       
                                                       numericInput("leg_key",
                                                                    "Size of legend key:",
                                                                    1)
                                      )
                                  )
                         ),
                         
                         tabPanel("Other",
                                
                                  div(id = "graph_options_5",
                                      
                                      conditionalPanel("input.Type == 'scatter'",
                                                       numericInput("sym_size", 
                                                                    "Symbol size", 
                                                                    1),
                                                       checkboxInput("line", 
                                                                     strong("Show regression line"), 
                                                                     F)
                                      ),
                                      
                                      conditionalPanel("input.Type == 'bar_e'",
                                                       radioButtons("error_bars", 
                                                                    strong("Type"),
                                                                    choices = list("no error bars" = "no bars",
                                                                                   "standard deviation" = "sd",
                                                                                   "standard error" = "se"),
                                                                    selected = "no bars")
                                      ),
                                      conditionalPanel("input.error_bars != 'no bars'",
                                                       radioButtons("bars_place",
                                                                    strong("Direction"),
                                                                    choices = list("positive only" = "back",
                                                                                   "positive and negative" = "front"),
                                                                    selected = "front"
                                                       )
                                      )
                                  )
                         ),
                         
                         tabPanel("Text",

                                  div(id = "graph_options_6",
                                      
                                      strong("Font size"),
                                      numericInput("fnt_sz_ttl", "Axis labels", 12),
                                      numericInput("fnt_sz_ax", "Axis values", 12),
                                      numericInput("fnt_sz_leg_ttl", "Legend title", 12),
                                      numericInput("fnt_sz_leg", "Legend values", 12)
                                  )
                         ),
                         
                         tabPanel("Size",
                                  
                                  div(id = "graph_options_7",
                                      
                                      numericInput("fig_height", 
                                                   "Height on screen (px)", 
                                                   360),
                                      numericInput("fig_width", 
                                                   "Width on screen (px)", 
                                                   480),
                                      numericInput("fig_height_download", 
                                                   "Download height (in):", 
                                                   3),
                                      numericInput("fig_width_download", 
                                                   "Download width (in):", 
                                                   4)
                                  )
                                  
                         )
                         
                       ) 

                          
                ),
                
                ### right column: graph and buttons #####
                
                column(8,
                       
                       fluidRow(
                         column(4,
                                box(
                                  title = "Plot/update graph", 
                                  width = NULL, status = "primary", solidHeader = T,
                                  actionButton("go_plot", "GO")
                                )
                         ),
                         column(4,
                                box(
                                  title = "Reset graph options", 
                                  width = NULL, status = "primary", solidHeader = T,
                                  actionButton("reset_plot", "GO")
                                )
                         ),
                         column(4,
                                box(
                                  title = "Download graph", solidHeader = T,
                                  width = NULL,
                                  status = "primary",
                                  
                                  downloadButton("download_plot","GO")
                                )
                         )
                       ),
                       
                       fluidRow(
                         
                         column(12,
                                box(
                                  title = "Graph output",
                                  width = NULL, status = "primary",
                                  solidHeader = T, height = 480,
                                  plotOutput("out_ggplot")  
                                )
                         )
                       )
                ) # close column 2
              ) # close row
      ),
      ### close graph tab #####

      ### Info tab #####
      tabItem(tabName = "info_tab", 
              
              fluidRow(
                
                column(8,
                       
                       box(
                         title = "App information", width = NULL, height = 25,
                         status = "primary",
                         solidHeader = T
                       ),
                       
                       tabBox(width = NULL,
                              
                              tabPanel("Practice datasets",
                                       
                                       h3("Reduced mowing"), 
                                       "These data are from reduced-mowing pilot plots in summer and fall 2018. ",
                                       br(),"Data collected by Dr. Jennifer Koop, Dr. Diana Barrett, Andrea Bickford, Steve Parks, ",
                                       "and Fall 2018 EEE lab students.",
                                       br(),
                                       strong("Metadata:"),
                                       tableOutput("mow_metadata"),
                                       br(),br(),
                                       h3("Dune environment"),
                                       "These data are from the coastal dunes at Waquoit Bay Estuarine Research Reserve. ",
                                       br(),
                                       "Data collected by Dr. Tara Rajaniemi in 2006 and 2007.",
                                       br(),
                                       strong("Metadata:"),
                                       tableOutput("dune_metadata")
                                       ),
                              
                              tabPanel("Data input",
                                       
                                       "Data files may be uploaded as an Excel file (.xls or .xlsx) ",
                                       "or comma-delimited text (.csv).",
                                       br(),br(),
                                       "Files should be formatted as follows:",
                                       tags$ul(
                                         tags$li("In Excel files, the data are on the first worksheet."),
                                         tags$li("Each column is a single variable, and each row is a single observational unit."),
                                         tags$li("The first row contains variable names. Variable names begin with a letter and do not contain spaces."),
                                         tags$li("Missing values are indicated by an empty cell or a cell containing NA.")
                                       )
                                       
                              ),
                              
                              tabPanel("Variable types",
                                       
                                       "Variables are automatically classified as categorical or numeric.",
                                       br(),br(),
                                       paste("If your variable should be numeric, but is not ",
                                             "recognized as numeric, check your Excel file to make ",
                                             "sure the values in the column contain only numbers, ",
                                             "and no letters or other text. (Cells with missing ",
                                             "values may have NA in them.)"
                                       ),
                                       br(),br(),
                                       paste("If your variable should be categorical, but is classified ",
                                             "as numeric, use letters (or a combination of letters and ",
                                             "numbers) instead of numbers to code the values."
                                       ),
                                       br(),br(),
                                       "If variables continue to be assigned incorrectly, try saving the file in .csv format."
                                       
                              ),
                              
                              tabPanel("Data in graphs",
                                       
                                       "Variable choices are based on the type of graph selected.",
                                       tags$ul(
                                         tags$li(paste("The X variable must be numeric for histograms ",
                                                       "and scatterplots, and categorical for bar ",
                                                       "graphs and boxplots.")),
                                         tags$li(paste("Histograms and bar graphs show frequency on ",
                                                       "the Y axis. For other graphs, the Y variable ",
                                                       "must be numeric.")),
                                         tags$li("Any graph may have a categorical grouping variable.")
                                       ),
                                       br(),
                                       "Observations with missing values of any of the selected ",
                                       "variables are excluded from the graph."
                              ),
                              
                              tabPanel("Graph action buttons",
                                       
                                       "Use the 'GO' button under 'Plot/update graph' to create a ",
                                       "graph, and to apply any changes to graph options.",
                                       br(), br(),
                                       "The 'GO' button under 'Reset plot options' will reset all ",
                                       "selections made in the 'Graph options' box.",
                                       br(), br(),
                                       "Graphs can be downloaded as .png files. The size of the ",
                                       "downloaded image can be changed in the 'Size' options tab. ",
                                       "A font size of 10 is recommended for download. The font ",
                                       "size can be changed in the 'Text' options tab."
                                       
                              ),
                              
                              tabPanel("About",
                                       
                                       "bioGraphR is designed to produce graphs needed by biology ",
                                       "undergraduates in lab courses.",
                                       br(),br(),
                                       "This app is modified from ",
                                       tags$a(href = "https://www.showmeshiny.com/ggplot-gui/", 
                                              "ggplot GUI"),
                                       ", written by Gert Stulp.",
                                       br(),br(),
                                       "This app was written in R version 3.6.0 and uses these packages: ",
                                       tags$ul(
                                         tags$li("shiny"),
                                         tags$li("shinydashboard"),
                                         tags$li("shinyjs"), 
                                         tags$li("shinyjqui"),
                                         tags$li("readxl"), 
                                         tags$li("tidyverse"),
                                         tags$li("cowplot"),
                                         tags$li("Hmisc"),
                                         tags$li("descriptr")
                                       ),
                                       br(),br(),
                                       "This version created 07/2019 by Tara K. Rajaniemi."
                              )
                       ) # close tabBox
                       
                ) # close column
              ) # close row
              
      ) ### close info tab #####
      
    ) 
    ### close tab items #####
    
  )
  ### close dashboard body #####

)
### close UI #####                                     

server <- function(input, output, session) { 
  
  ### Data tab items #####
  
  # Get data
  df_original <- reactive({
    
    if (input$data_input == 1) {
      if (input$data_choice == 1) 
        data <- read_excel("data/mow data.xlsx")
      else 
        data <- read_excel("data/dunes.xlsx")
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
shinyApp(ui, server)
