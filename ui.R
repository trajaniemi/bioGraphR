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
                                                                     "Dune environment" = 2,
                                                                     "Framingham heart study" = 3,
                                                                     "Hubbard Brook tree census" = 4,
                                                                     "Sapling warming expt" = 5)
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
