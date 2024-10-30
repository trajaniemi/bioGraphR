bgr_pkgs <- c('shiny','shinydashboard','shinyjs','shinyjqui','car',
              'tidyverse','gridExtra','readxl','Hmisc','DT')

need_pkgs <- bgr_pkgs[!(bgr_pkgs %in% installed.packages()[,"Package"])]

if (length(need_pkgs) > 0) {
  install.packages(need_pkgs, repos = "https://cloud.r-project.org")
}



library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)
library(car)
library(tidyverse)
library(gridExtra)
library(readxl)
library(DT)
library(Hmisc)

# --- # --- # --- # User Interface # --- # --- # --- #

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- dashboardPage(

	dashboardHeader(
		title = "bioGraphR"
	),
		
	dashboardSidebar(
		sidebarMenu(
			menuItem("Data", 		tabName = "data_tab"),
			menuItem("Graphs", 		tabName = "graph_tab"),
			menuItem("Stats",  		tabName = "stats_tab"),
			menuItem("Info",   		tabName = "info_tab"),
			actionButton("exit_app", "Exit"),
			textOutput("exit_message")
		)
	),
	
	dashboardBody(
	
		useShinyjs(),
		extendShinyjs(text = jscode, functions = c("closeWindow")),
	
		tags$head(tags$style(HTML('
			.skin-blue .main-header .logo {
			  background-color: #003764;
			  
			}

			.skin-blue .main-header .navbar {
			  background-color: #FEC24D;
			  
			}       

			.skin-blue .sidebar-menu > li.active > a,
			.skin-blue .sidebar-menu > li:hover > a {
			  border-left-color: #FEC24D;
			  }

			.box.box-primary >.box-header {
			  color:white;
			  background:#003764;
			  
			}
								
			.box.box-primary {
			  border-bottom-color:white;
			  border-left-color:white;
			  border-right-color:white;
			  border-top-color:#003764;
			  
			}


			.box.box-solid.box-primary >.box-header {
			  color:white;
			  background:#003764;
			  
			}
								
			.box.box-solid.box-primary {
			  border-bottom-color:white;
			  border-left-color:white;
			  border-right-color:white;
			  border-top-color:#003764;
			  
			}

			.box.box-info >.box-header {
			  color:white;
			  background:#FEC24D;
			  
			}
								
			.box.box-info {
			  border-bottom-color:white;
			  border-left-color:white;
			  border-right-color:white;
			  border-top-color:#FEC24D;
			  
			}

			.box.box-solid.box-info >.box-header {
			  color:white;
			  background:#FEC24D;
			  
			}
								
			.box.box-solid.box-info {
			  border-bottom-color:white;
			  border-left-color:white;
			  border-right-color:white;
			  border-top-color:#FEC24D;
			  
			}                              

			.nav-tabs-custom .nav-tabs li.active { 
			  border-top-color: #003764; 
			  
			}
										  
			.btn,
			.btn:focus{
			  color:black;
			  background:#FEC24D;
			  border-bottom-color:#FEC24D;
			  border-left-color:#FEC24D;
			  border-right-color:#FEC24D;
			  border-top-color:#FEC24D;
			}

			.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
			  background: #003764 ;
			  border-top: 1px solid #003764 ;
			  border-bottom: 1px solid #003764 ;
			}
			.irs-from, .irs-to, .irs-single { 
			  background: #003764; 
			}

		'))),
	
		tabItems(
		
			# --- # --- # Data tab # --- # --- # 
			tabItem(tabName = "data_tab",
				fluidRow(
					column(4, 
						box(title = "Open a dataset", status = "primary",
							solidHeader = TRUE, width = NULL,
						 							
							"See the Info tab for help formatting your data file.",
							br(),br(),
							h5("Select a file: "),
							radioButtons(
								"data_input",
								"",
								choices = list(
									"Upload an Excel file" = 1,
									"Upload a text file (.csv)" = 2
								),
								selected = 1
							),
							br(),br(),
							
							fileInput(
								"upload_datafile", 
								"", 
								multiple = FALSE,
								accept = c(
									"application/vnd.ms-excel","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
									".xls", ".xlsx", ".csv", ".text/csv"
								)
							),
							
							actionButton("go_data", "LOAD DATA", icon = icon("file-upload"), width = "50%")
						),
						
					box(title = "Filter data", status = "primary",
							solidHeader = TRUE, width = NULL,
						 	
							div(id = "data_filter_options",							
							
								"You can work with a subset of the data, based on the values of selected variables.",
								br(), br(),
								"Make selections below, then click FILTER to apply your selections. You may apply changes to multiple variables.",
								br(),br(),
								"Click RESET to reset all changes.",
								br(),br(),
								selectInput(
									"filter_variable",
									"Variable",
									choices = "",
									selected = NULL
								),
								uiOutput("filter_data_widgets"),
								br(),br(),
								fluidRow(
									column(6, align = "center",
										actionButton("apply_filters", "FILTER", icon = icon("filter"), width = "75%")
									),
									column(6, align = "center",
										actionButton("reset_filters", "RESET", icon = icon("undo"), width = "75%")
									)
								)
							)
						)
					),
					column(6, 
						box(title = "Data summary", status = "primary",
							solidHeader = TRUE, width = NULL,
						 	
							uiOutput("data_summ")
						)
					)
				)
			),

			# --- # --- # Graph tab # --- # --- # 
			tabItem(tabName = "graph_tab", 
				fluidRow(
					column(4,
						box(title = "Create a graph", width = NULL, 
							status = "primary", solidHeader = TRUE,
							
							radioButtons(
								"g_type", 
								"Select a graph type",
								choices = list(
									"histogram" = "hist",
									"frequency plot" = "freq",
									"boxplot" = "box",
									"bar graph" = "bar",
									"scatterplot" = "scatter"
								),
								selected = NULL
							),
							"Choose variables and specify options below. Then click 'PLOT' to plot or update the graph, or 'RESET' to reset all options.",
							br(), br(),
							
							fluidRow(
								column(6, align = "center",
									actionButton("go_plot", "PLOT", icon = icon("chart-bar"), width = "75%")
								),
								column(6,align = "center",
									actionButton("reset_plot", "RESET", icon = icon("undo"), width = "75%")
								)
							)
						),
							
						tabBox(width = NULL,
							tabPanel("Variables",
								div(id = "graph_options_v",
									
									selectInput(
										"x_var", 
										"X Variable", 
										choices = "",
										selected = NULL
									),
									conditionalPanel(
										"input.g_type == 'box' || input.g_type == 'bar' || input.g_type == 'scatter'",
										 selectInput(
											"y_var", 
											"Y Variable", 
											choices = "",
											selected = NULL
										)
									),
									conditionalPanel(
										"input.g_type == 'bar'",
										radioButtons(
											"error_bars", 
											"Error bars",
											choices = list(
												"none" = 1,
												"standard deviation" = 2,
												"standard error" = 3
											),
											selected = 1
										)
									),
									radioButtons(
										"groups",
										"Plot groups",
										choices = list(
											"no groups" = "none",
											"in one panel with a legend" = "legend",
											"in separate panels" = "panel"
										),
										selected = "none"
									),
									conditionalPanel(
										"input.groups != 'none'",
										selectInput(
											"z_var",
											"Grouping variable",
											choices = "",
											selected = NULL
										)
									),
									conditionalPanel(
										"input.g_type == 'scatter'",
										numericInput("sym_size", "Symbol size", 1),
										checkboxInput(
											"rline", 
											strong("Show regression line"), 
											FALSE
										)
									)
								)
							),	
								
							tabPanel("X axis",
								div(id = "graph_options_x",
								
									textInput("lab_x", "X axis label", ""),
									conditionalPanel(
										"input.g_type == 'hist'",
										numericInput("bins","# of bins", 10)
									),
									conditionalPanel(
										"input.g_type == 'scatter'",
										checkboxInput(
											"set_x_limits",
											strong("adjust x range"),
											FALSE
										),
										conditionalPanel(
											"input.set_x_limits",
											numericInput("xaxis_min", "Minimum x", 0),
											numericInput("xaxis_max", "Maximum x", 100)
										)
									),
									conditionalPanel(
										"input.g_type == 'freq' || input.g_type == 'box' || input.g_type == 'bar'",
										strong("drag and drop values to change order"),
										uiOutput("change_x")
									)
								)
							),
							 
							tabPanel("Y axis",
								div(id = "graph_options_y",
								
									textInput("lab_y", "Y axis label", "label y-axis"),
									conditionalPanel(
										"input.g_type == 'scatter' || input.g_type == 'box' || input.g_type == 'bar'",
										checkboxInput(
											"set_y_limits",
											strong("adjust y range"),
											FALSE
										),
										conditionalPanel(
											"input.set_y_limits",
											numericInput("yaxis_min", "Minimum y", 0),
											numericInput("yaxis_max", "Maximum y", 100)
										)
									)
								)
							),
							
							tabPanel("Groups",
								div(id = "graph_options_g",
									
									conditionalPanel(
										"input.groups == 'none'",
										"No groups selected"
									),
									conditionalPanel(
										"input.groups == 'legend'",
										textInput("lab_z", "Legend label", ""),
										radioButtons(
											"pos_leg",
											strong("Legend position"),
											choices = c("right", "left", "top", "bottom"),
											selected = "right"
										),
										numericInput(
											"leg_key",
											"Size of legend key",
											1
										)
									),
									conditionalPanel(
										"input.groups != 'none'",
										strong("drag and drop values to change order"),
										uiOutput("change_z")
									)
								)
							),
							
							tabPanel("Font",
								div(
									id = "graph_options_f",
									
									strong("Font size"),
									numericInput("font_labs", "Axis labels", 12),
									numericInput("font_vals", "Axis values", 12),
									conditionalPanel(
										"input.groups == 'legend'",
										numericInput("font_llab", "Legend title", 12),
										numericInput("font_lval", "Legend values", 12)
									)
								)
							)
						),
						
						box(width = NULL,
							status = "primary",
							
							radioButtons("file_type", "Download graph as", choices = c("png","pdf","jpg"), inline = T),
							numericInput("plot_w", "Image width (in)", 4),
							numericInput("plot_h", "Image height (in)", 3),
							column(
								width = 12, align = "center",
								shinyjs::hidden(downloadButton("download_plot","Download", width = "50%"))
							)
						)
					),
					
					column(8,
						box(
							title = "Graph output",
							width = NULL, 
							status = "primary",
							solidHeader = T, 
							
							plotOutput("out_ggplot")  
						)  
					)
				)
			),

			# --- # --- # Stats tab # --- # --- # 
			tabItem(
				tabName = "stats_tab",
				
				fluidRow(
				
					column(
						4,
						box(
							title = "Analysis options", 
							width = NULL, 
							status = "primary",
							solidHeader = TRUE,
							
							radioButtons(
								"stat_type",
								"Select an analysis type",
								choices = list(
									"summarize data (descriptives, frequency tables)" = "d",
									"compare means (t-tests, ANOVA)" = "t",
									"compare frequencies (chi square tests)" = "x",
									"test associations (correlation and regression)" = "r"
								)
							),
							radioButtons(
								"stat", 
								"Choose a specific analysis",
								choices = ""
							),
							"Choose variables and specify options below. Then click 'CALCULATE' to get results, or 'RESET' to reset all options.",
							br(), br(),
							fluidRow(
							
								column(
									width = 6,
									align = "center",
									actionButton("go_stats", "CALCULATE", icon = icon("calculator"), width = "75%")
								),
								column(
									width = 6,
									align = "center",
									actionButton("reset_stats", "RESET", icon = icon("undo"), width = "75%")
								)
							)
						),
						box(
							width = NULL, 
							status = "primary",
							
							div(
								id = "s_options",
								conditionalPanel(
									"input.stat != 3 && input.stat != 7 && input.stat != 1",
									selectInput(
										"stat_x",
										label = "",
										choices = ""
									)
								), 
								conditionalPanel(
									"input.stat == 1",
									selectInput(
										"stat_xm",
										"Choose variables to summarize",
										choices = "",
										multiple = TRUE
									)
								),
								conditionalPanel(
									"input.stat == 1 || input.stat == 2",
									checkboxInput("desc_groups_yn", "Add a grouping variable?", FALSE),
									conditionalPanel(
										"input.desc_groups_yn",
										selectInput(
											"stat_y_d",
											label = "",
											choices = ""
										)
									)
								),
								conditionalPanel(
									"input.stat > 2",
									selectInput(
										"stat_y",
										label = "",
										choices = ""
									)
								),
								conditionalPanel(
									"input.stat == 6 || input.stat == 10",
									checkboxInput("log_y", "transform y -> ln(y)", FALSE)
								),
								conditionalPanel(
									"input.stat == 1",
									numericInput("sigfigs","Significant figures to report",3),
									checkboxGroupInput("stats_list","Select statistics",
										choices = list(
											"sample size" = "n",
											"mean" = "m",
											"median" = "md",
											"standard deviation" = "s",
											"variance" = "v",
											"standard error" = "se",
											"coefficient of variation" = "cv",
											"minimum" = "min",
											"maximum" = "max",
											"range" = "r",
											"1st quartile" = "q1",
											"3rd quartile" = "q3",
											"interquartile range" = "iqr"),
										selected = "none")			
								),
								conditionalPanel(
									"input.stat == 3",
									numericInput(
										"mu_null",
										HTML("What is the hypothesized value of &mu; ?"),
										0
									)
								),
								conditionalPanel(
									"input.stat == 6",
									checkboxInput("tukey", "Tukey post-hoc tests", FALSE)
								),
								conditionalPanel(
									"input.stat == 6 || input.stat == 10",
									checkboxInput("assumptions", "residuals plots", FALSE)
								),
								conditionalPanel(
									"input.stat == 7",
									"Enter hypothesized relative frequencies. Values should sum to 1.",
									uiOutput("chi_exp_wid")
								)
							)
						)
					),
					
					column(
						8,
						box(
							title = "Statistics results", 
							width = NULL, 
							status = "primary",
							solidHeader = TRUE,
							
							uiOutput("stats_results")
							
						)
					)
				)
			),


			# --- # --- # Info tab # --- # --- # 
			tabItem(
				tabName = "info_tab", 
				  
				fluidRow(
				
					column(
						12,
						   
						box(
							title = "App information", 
							width = NULL, 
							height = 25,
							status = "primary",
							solidHeader = T
						),
						   
						tabBox(
							width = NULL,
								  
							tabPanel(
								"Data input",
								"Data files may be uploaded as an Excel file (.xls or .xlsx) or comma-delimited text (.csv).",
								br(),br(),
								"Files should be formatted as follows:",
								tags$ul(
									tags$li("In Excel files, the data are on the first worksheet."),
									tags$li("Each column is a single variable, and each row is a single case (observational unit.)"),
									tags$li("The first row contains variable names. Variable names begin with a letter and do not contain spaces."),
									tags$li("Missing values are indicated by an empty cell or a cell containing NA.")
								)
							),
								  
							tabPanel(
								"Variable types",
								"Variables are automatically classified as categorical or numeric.",
								br(),br(),
								"If your variable should be numeric, but is not recognized as numeric, check your Excel file to make sure the values in the column contain only numbers, and no letters or other text. (Cells with missing values may have NA in them.)",
								br(),br(),
								"If your variable should be categorical, but is classified as numeric, use letters (or a combination of letters and numbers) instead of numbers to code the values.",
								br(),br(), 
								"If variables continue to be assigned incorrectly, try saving the file in .csv format."
							),
								  
							tabPanel(
								"Data choices",
								"Variable choices are based on the type of graph or statistical analysis selected.",
								br(),br(),
								"Observations with missing values of any of the selected variables are excluded from the graph or analysis."
							),
								  
							tabPanel(
								"Downloads",
								"Graphs can be downloaded as .png, .pdf, or .jpg files. A font size of 10 is recommended for download. The font size can be changed in the 'Font' options tab."
							),
								  
							tabPanel(
								"About",
								"BioGraphR is designed to produce graphs and statistical analyses needed by biology undergraduates in lab courses.",
								br(),br(),
								"This app was written in R version 4.4.1 and uses these packages: ",
								tags$ul(
									tags$li("shiny"),
									tags$li("shinydashboard"),
									tags$li("shinyjs"), 
									tags$li("shinyjqui"),
									tags$li("dplyr"),
									tags$li("tidyr"),
									tags$li("tibble"),
									tags$li("stringr"),
									tags$li("ggplot2"),
									tags$li("readxl"), 
									tags$li("gridExtra")
								),
								br(),br(),
								"This version created in 2024 by Tara K. Rajaniemi."
								)
								
						) 	# 	close tabBox
						   
					)
				)	## 	close row  
				
			)

		)
	
	)

)

server <- function(input, output, session) { 
		
# --- # --- # --- # Data # --- # --- # --- # 

# --- #--- # Get data from a file # --- # --- #

	values <- reactiveValues()

	observeEvent(input$go_data, {
	
		file_in <- input$upload_datafile
		if (is.null(input$upload_datafile)) {
			return(data.frame(x = "Select your datafile"))
		} else {
			if (input$data_input == 1) {
				d <- read_excel(
					file_in$datapath, 
					na = c("NA","na","n/a","N/A",""),
					.name_repair = "universal"
				)
			} else {
				d <- read_csv(
					file_in$datapath, 
					na = c("NA","na","n/a","N/A",""),
					col_types = cols()
				)
			}
		}
		
		d <- d %>% mutate(across(where(is.character), as.factor))
		d_c <- d %>% select(where(is.factor))
		d_n <- d %>% select(where(is.numeric))
		
		values$a_data <- d
		values$a_num  <- ncol(d)
		values$c_num  <- ncol(d_c)
		values$n_num  <- ncol(d_n)
		values$a_vars <- names(d)
		values$c_vars <- names(d_c)
		values$n_vars <- names(d_n)
		
		values$w_data <- d
		
		reset("data_filter_options")
		reset("graph_options_v")
		reset("graph_options_x")
		reset("graph_options_y")
		reset("graph_options_g")
		reset("graph_options_f")
		reset("s_options")

	})

	
# --- # --- # Filter data # --- # --- #

	observe({
	
		req(values$a_data)
		updateSelectInput(session, "filter_variable", choices = values$a_vars)
		
	})
	
	output$filter_data_widgets <- renderUI({
	
		req(values$w_data)
		v_name <- input$filter_variable
			
		if (v_name %in% values$a_vars) {
		
			w <- values$w_data %>% pull(all_of(v_name))
			if (v_name %in% values$c_vars) {
				w <- factor(w)
				widget1 <- checkboxGroupInput("filter_widget", "Check values to include in the dataset. Uncheck values to be filtered out.", choices = levels(w), selected = levels(w))
			} else {
				w_range <- range(w, na.rm = TRUE)
				widget1 <- sliderInput("filter_widget", "Use the sliders to select values to include in the dataset.", min = w_range[1], max = w_range[2], value = w_range)
			}
			
			d_out <- list(widget1)
			
		} else {
			filter_message <- "Select a variable to filter."
			d_out <- list(renderText(filter_message))
		}

		do.call(tagList, d_out)
		
		
	})
	
	observeEvent(input$apply_filters, {
	
		req(values$w_data)
		
		v_name <- input$filter_variable
		
		if (v_name %in% values$c_vars) {
			df.temp <- values$w_data %>%
				filter(.data[[v_name]] %in% input$filter_widget) %>%
				droplevels()
		} else {
			v_range_new <- input$filter_widget
			df.temp <- values$w_data %>% 
				drop_na(.data[[v_name]]) %>%
				filter(between(.data[[v_name]], v_range_new[1], v_range_new[2]))
		}
		
		values$w_data <- df.temp
	
	})
	
	
	observeEvent(input$reset_filters, {
		
		values$w_data <- values$a_data
		reset("data_filter_options")
		
	})
		
# --- # --- # Summarize data # --- # --- # 
		
	data_summary <- reactive({
	
		req(values$w_data)
		ncases <- nrow(values$w_data)
		d_head <- paste("<p>", ncases, " observations of ", values$a_num, " variables</p>", sep = "")
		d_sub1 <- "<p><b>Categorical variables</b></p>"
		d_sub2 <- "<p><b>Numeric variables</b></p>"
		
		df.cat <- values$w_data %>% select(where(is.factor))
		
		if (values$c_num > 0) {
			v <- lapply(df.cat, levels)
			a <- names(v)
			b <- rep("x", length(a))
			for (i in 1:length(v)) { 
				b[i] <- paste(v[[i]], collapse = ", ")
			}
			c.na   <- df.cat %>% summarise(across(everything(), ~ sum(is.na(.x))))
			d_cat <- data.frame("variable" = a, "levels" = b)
		} else {
			d_cat <- (data.frame(x = "No categorical variables found."))
		}
		
		df.num <- values$w_data %>% select(where(is.numeric))
	
		if (values$n_num > 0) {
			d.min  <- df.num %>% summarise(across(everything(), ~ min(.x, na.rm = TRUE)))
			d.max  <- df.num %>% summarise(across(everything(), ~ max(.x, na.rm = TRUE)))
			d.mean <- df.num %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
			d.sd   <- df.num %>% summarise(across(everything(), ~ sd(.x, na.rm = TRUE)))
			d.na   <- df.num %>% summarise(across(everything(), ~ sum(is.na(.x))))
			d_num <- data.frame("variable" = values$n_vars, "min" = t(d.min), "max" = t(d.max), "mean" = t(d.mean), "sd" = t(d.sd))
		} else {
			d_num <- (data.frame(x = "No numeric variables found."))
		}
		
		d_out <- list(
			HTML(d_head), HTML(d_sub1), renderTable(d_cat), HTML(d_sub2), renderTable(d_num)
			)
		do.call(tagList, d_out)
	})

	output$data_summ <- renderUI(data_summary())


		
# --- # --- # --- # Graphs # --- # --- # --- #

# --- # --- # Update options # --- # --- #

	observe({
	
		req(values$w_data)
		
		if (input$g_type == "scatter" || input$g_type == "hist")
			updateSelectInput(session, "x_var", choices = values$n_vars)
		else 
			updateSelectInput(session, "x_var", choices = values$c_vars)

		updateSelectInput(session, "y_var", choices = values$n_vars)
		updateSelectInput(session, "z_var", choices = values$c_vars)
		
	})
	
	observe({
	
		req(input$x_var)
		req(input$y_var)
		
		updateTextInput(session, "lab_x", value = input$x_var)
		if (input$g_type == "hist" || input$g_type == "freq") {
			updateTextInput(session, "lab_y", value = "frequency")
		} else {
			updateTextInput(session, "lab_y", value = input$y_var)
			updateNumericInput(session, "yaxis_min", value = min(values$w_data[[input$y_var]]))
			updateNumericInput(session, "yaxis_max", value = max(values$w_data[[input$y_var]]))
		}
		
		if (input$groups == "legend")
			updateTextInput(session, "lab_z", value = input$z_var)

	})

	output$change_x <- renderUI({
	
		orderInput(
			"x_sorted", 
			"",
			items = levels(values$w_data[[input$x_var]]),
			width = "100%",
			class = "ui-sortable"
			)
	
	})
  
	output$change_z <- renderUI({
	
		orderInput(
			"z_sorted", 
			"",
			items = levels(values$w_data[[input$z_var]]),
			width = "100%",
			class = "ui-sortable"
			)
	
	})	
	
	observe ({
	
		if(is.null(values$ggcode))
			shinyjs::hide("download_plot") 
		else
			shinyjs::show("download_plot")
		
	})
			
# --- # --- # Select data for graphs # --- # --- #
	
	g.data <- eventReactive(input$go_plot, {
	
		d1 <- values$w_data %>% select(all_of(input$x_var))
		
		if (input$g_type == "box" || input$g_type == "bar" || input$g_type == "scatter")
			d2 <- values$w_data %>% select(all_of(input$y_var))
			
		if (input$groups != "none") 
			d3 <- values$w_data %>% select(all_of(input$z_var))
		
		if (input$g_type == "hist" || input$g_type == "freq") {
			if (input$groups == "none") {
				d <- d1
				names(d) <- "xvar" 
			} else {
				d <- cbind(d1, d3)
				names(d) <- c("xvar", "zvar")
			}
		} else {
			if (input$groups == "none") {
				d <- cbind(d1, d2)
				names(d) <- c("xvar", "yvar") 
			} else {
				d <- cbind(d1, d2, d3)
				names(d) <- c("xvar", "yvar", "zvar")
			}
		}
		
		if (input$x_var %in% values$c_vars & !is.null(input$x_sorted)) 
			d$xvar <- factor(d$xvar, levels = input$x_sorted)
		if (input$groups != "none" & !is.null(input$z_sorted)) 
			d$zvar <- factor(d$zvar, levels = input$z_sorted)
		d <- drop_na(d)	
		return(d)
	
	})

# --- # --- # Create ggplot code # --- # --- #
	
	observeEvent(input$go_plot, {
	
		label = "ggplot code"
	
		isolate ({
		
			# aes
			if (input$groups == "legend") {			
				if (input$g_type == "hist" || input$g_type == "freq") 
					p_aes <- ggplot(g.data(), aes(x = xvar, fill = zvar))
				if (input$g_type == "scatter") 
					p_aes <- ggplot(g.data(), aes(x = xvar, y = yvar, fill = zvar))
				if (input$g_type == "box" || input$g_type == "bar")
					p_aes <- ggplot(g.data(), aes(x = xvar, y = yvar, fill = zvar))
			} else {
				if (input$g_type == "hist" || input$g_type == "freq") 
					p_aes <- ggplot(g.data(), aes(x = xvar))
				if (input$g_type == "box" || input$g_type == "bar" || input$g_type == "scatter")
					p_aes <- ggplot(g.data(), aes(x = xvar, y = yvar))
			}

			# geom
			if (input$groups == "legend") {
				if (input$g_type == "hist")
					p_geom <- geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black", position = "dodge", width = 1)
				if (input$g_type == "freq")
					p_geom <- geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black", position = "dodge")
				if (input$g_type == "box")
					p_geom <- geom_boxplot(color = "black", position = "dodge")
				if (input$g_type == "bar")
					p_geom <- stat_summary(fun = "mean", geom = "bar", color = "black", position = "dodge")
				if (input$g_type == "scatter")
					p_geom <- geom_point(color = "black", shape = 16, size = input$sym_size)
			} else {
				if (input$g_type == "hist")
					p_geom <- geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black", fill = "white", width = 1)
				if (input$g_type == "freq")
					p_geom <- geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black", fill = "white")
				if (input$g_type == "box")
					p_geom <- geom_boxplot(color = "black", fill = "white")
				if (input$g_type == "bar")
					p_geom <- stat_summary(fun = "mean", geom = "bar", color = "black", fill = "white")
				if (input$g_type == "scatter")
					p_geom <- geom_point(color = "black", shape = 16, size = input$sym_size)
			}
			
			# add geoms
			if (input$g_type == "bar" && input$error_bars == 2) {
				p_error <- stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1),
					geom = "errorbar", color = "black", width = 0.3, position = position_dodge(0.9))
			} else if (input$g_type == "bar" && input$error_bars == 3) {
				p_error <- stat_summary(fun.data = "mean_se", geom = "errorbar", 
					color = "black", width = 0.3, position = position_dodge(0.9))
			} else {
				p_error <- NULL
			}
					
			if (input$g_type == "scatter" && input$rline)
				p_line <- geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) 
			else
				p_line <- NULL
					
			# coordinates and limits
			if (input$set_x_limits & input$set_y_limits) {
				p_coord <- coord_cartesian(
					xlim = c(input$xaxis_min, input$xaxis_max),
					ylim = c(input$yaxis_min, input$yaxis_max),
					expand = FALSE
				)
			} else if (input$set_x_limits) {
				p_coord <- coord_cartesian(
					xlim = c(input$xaxis_min, input$xaxis_max)
				)
			} else if (input$set_y_limits) {
				p_coord <- coord_cartesian(
					ylim = c(input$yaxis_min, input$yaxis_max)
				)
			} else {
				p_coord <- coord_cartesian()
			}
			
			# x axis
			if (input$g_type == "hist") {
				p_xaxis <- scale_x_binned(
					n.breaks = input$bins, 
					name = input$lab_x
				)
			} else if (input$g_type == "scatter") {
				p_xaxis <- scale_x_continuous(
						name = input$lab_x
					)
			} else {
				p_xaxis <- scale_x_discrete(name = input$lab_x)
			}
			
			# y axis
			p_yaxis <- scale_y_continuous(
					name = input$lab_y,
					expand = expansion(mult = c(0, 0.1))
					)
			
			# legend variable
			if (input$groups == "legend") {
				p_fill <- scale_fill_grey(start = 1, end = 0.3, name = input$lab_z)
			} else {
				p_fill <- NULL
			}
			
			p <- p_aes + p_geom + p_error + p_line + p_coord + p_xaxis + p_yaxis + p_fill +
				theme_classic(base_size = 12) +
				theme(
					axis.title = element_text(size = input$font_labs, face = "plain", color = "black"),
					axis.text = element_text(size = input$font_vals, color = "black"),
					axis.ticks = element_line(color = "black")
				)
					
			if (input$groups == "legend") {
				p <- p + theme(
					legend.background = element_rect(color = NA, fill = NA),
					legend.text = element_text(size = input$font_lval),
					legend.title = element_text(size = input$font_llab),
					legend.position = input$pos_leg,
					legend.key.size = unit(input$leg_key,"cm")
				)
			}
					
			if (input$groups == "panel") {
				p <- p + facet_wrap(~ zvar, scales = "free", nrow = 1) +
					theme(
						strip.background = element_blank(),
						strip.text = element_text(colour = "black", size = rel(1))
					)
			}
		
			values$ggcode <- p
		
		})
	})

# --- # --- # Plot graph on screen # --- # --- #

	output$out_ggplot <- renderPlot(
		width = 480,
		height = 360,
			{ values$ggcode }
	)
	
# --- # --- # Download graph # --- # --- #
	
	output$download_plot <- downloadHandler(
		filename = function(){
			fn <- paste("plot ", Sys.time(), ".", input$file_type, sep = "")
			str_replace_all(fn, ":", "_")
		},
		content = function(file){
			ggsave(
				file, 
				plot = values$ggcode, 
				width = input$plot_w, 
				height = input$plot_h,
				units = "in", 
				device = input$file_type)
		}
	)
	
# --- # --- # Reset graph # --- # --- #
	
	observeEvent(input$reset_plot, {
		label = "reset graph options"
		reset("graph_options_v")
		reset("graph_options_x")
		reset("graph_options_y")
		reset("graph_options_g")
		reset("graph_options_f")
		})


# --- # --- # --- # Statistics # --- # --- # --- #

# --- # --- # Update options # --- # --- #
	
	observe({
	
		label = "update analysis choices"
		if (input$stat_type == "d") {
			updateRadioButtons(session, "stat",
				choices = list("descriptive statistics" = 1,
					"frequency tables" = 2))
		} else if (input$stat_type == "t") {
			updateRadioButtons(session, "stat",
				choices = list("one-sample t test" = 3,
					"paired samples t test" = 4,
					"two-sample t test" = 5,
					"ANOVA" = 6))
		} else if (input$stat_type == "x") {
			updateRadioButtons(session, "stat",
				choices = list("chi-square goodness of fit test" = 7,
					"chi-square test of association" = 8))
		} else {
			updateRadioButtons(session, "stat",
				choices = list("correlation" = 9,
					"regression" = 10))
		}
	})
	
	
	observe({
		
		label = "update stats variables"
		input$go_data
		input$apply_filters
		input$reset_filters
		input$stat

		isolate({
		
			# - # x variable
			
			if (input$stat == 2 || input$stat == 5 || input$stat == 6 || input$stat == 8) {
				x_type <- "c"
			} else {
				x_type <- "n"
			}
			
			# - # y variable
			if (input$stat == 1 || input$stat == 2) {
				y_type <- "c0"
			} else if (input$stat == 7 || input$stat == 8) {
				y_type <- "c"
			} else {
				y_type <- "n"
			}
			
			# - # x prompt
			if (input$stat == 2) {
				x_name <- "Choose a variable to summarize"
			} else if (input$stat == 4) {
				x_name <- "Choose Group 1 values"
			} else if (input$stat == 5 || input$stat == 6) {
				x_name <- "Choose an independent (grouping) variable"
			} else if (input$stat == 8 || input$stat == 9) {
				x_name <- "Choose variable 1"
			} else {
				x_name <- "Choose an independent variable"
			}
				
			# - # y prompt
			if (input$stat == 1 || input$stat == 2) {
				y_name <- "Choose a grouping variable"
			} else if (input$stat == 3 || input$stat == 7) {
				y_name <- "Choose a variable"
			} else if (input$stat == 4) {
				y_name <- "Choose Group 2 values"
			} else if (input$stat == 8 || input$stat == 9) {
				y_name <- "Choose variable 2"
			} else {
				y_name <- "Choose a dependent variable"
			}
			
			if (x_type == "c") {
				updateSelectInput(session, "stat_x", label = x_name, choices = values$c_vars)
			} else {
				updateSelectInput(session, "stat_x", label = x_name, choices = values$n_vars)
			}
			updateSelectInput(session, "stat_xm", choices = values$n_vars)
			
			if (y_type == "c0") {
				updateSelectInput(session, "stat_y", label = y_name, choices = c("no groups" = ".", values$c_vars, selected = "no groups"))
			} else if (y_type == "c") {
				updateSelectInput(session, "stat_y", label = y_name, choices = values$c_vars)
			} else { 
				updateSelectInput(session, "stat_y", label = y_name, choices = values$n_vars)
			}
			updateSelectInput(session, "stat_y_d", choices = values$c_vars)
		})
	})
	
	chi_wid <- reactive({
	
		req(input$stat_y)
		if (input$stat == 7) {
			levs <- levels(values$w_data[[input$stat_y]])
			val <- round(1/length(levs),2)
			cw <- lapply(1:length(levs), function (i) {
				ui_name <- paste("lev", i, "exp", sep = "_")
				numericInput(ui_name, levs[i], val) 
			})
		do.call(tagList, cw)
		
		}
		
	})

	output$chi_exp_wid <- renderUI({ chi_wid() })
	
	observeEvent(input$reset_stats, {
		reset("s_options")
	})
	
	observe ({
	
		if(is.null(values$r_plot))
			shinyjs::hide("download_r_plot") 
		else
			shinyjs::show("download_r_plot")
		
	})
	
	observe ({
	
		if(is.null(values$r_post))
			shinyjs::hide("download_posthoc") 
		else
			shinyjs::show("download_posthoc")
		
	})
	
	observe ({
	
		if(is.null(values$stats_table))
			shinyjs::hide("download_stats") 
		else
			shinyjs::show("download_stats")
		
	})
	
# --- # --- # Stats data # --- # --- #

	s_data <- eventReactive(input$go_stats, {
		
		if (input$stat == 1) { 
			v <- as.vector(input$stat_xm)  
			if (input$desc_groups_yn) {
				d <- values$w_data %>% select(any_of(c(v, input$stat_y_d))) 
			} else {
				d <- values$w_data %>% select(any_of(v))
			}
		} else if (input$stat == 2) { 
			if (input$desc_groups_yn) {
				d <- values$w_data %>% select(any_of(c(input$stat_x, input$stat_y_d)))
			} else {
				d <- values$w_data %>% select(all_of(input$stat_x))
			}
		} else {
			d <- values$w_data %>% select(any_of(c(input$stat_x, input$stat_y)))
		}
		d <- d %>% 
			mutate(across(where(is.character), as.factor)) %>% 
			drop_na()
	})
	
# --- # --- # Calculate stats # --- # --- #

	
	stats_out <- eventReactive(input$go_stats, {
		
		if (input$stat == 1) {
		# - # descriptives
			r_head <- HTML(
				"<p><h4>Descriptive statistics</h4></p>
				 <p>missing values have been removed</p>")
			s1 <- s_data()
			s_list_full <- data.frame(
				stat_name = c("sample size", "mean", "median", "standard deviation", "variance",
								"standard error", "coefficient of variation", "minimum", "maximum",
								"range", "1st quartile", "3rd quartile", "interquartile range"),
				stat_symbol = c("n", "m", "md", "s", "v", "se", "cv", "min", "max", "r", "q1", "q3", "iqr"))
			s_list <- as.vector(input$stats_list)
			s_names <- s_list_full %>%
				filter(stat_symbol %in% s_list) %>%
				pull(stat_name) %>%
				as.vector()
			v_list <- as.vector(input$stat_xm)
			v_edit <- str_replace_all(v_list, "_", ".")
			if (input$desc_groups_yn) {
				names(s1) <- c(v_edit, "yvar")
				s1 <- s1 %>% group_by(yvar)				
			} else {
				names(s1) <- v_edit 
			}

			s2 <- s1 %>%
				summarise(
					across(all_of(v_edit), 
						list(
							n = ~sum(!is.na(.x)), 
							m = ~mean(.x, na.rm = TRUE),
							s = ~sd(.x, na.rm = TRUE),
							min = ~min(.x, na.rm = TRUE), 
							max = ~max(.x, na.rm = TRUE), 
							q1 = unlist(~quantile(.x, probs = (.25), na.rm = TRUE)), 
							md = ~median(.x, na.rm = TRUE),
							q3 = unlist(~quantile(.x, probs = (.75), na.rm = TRUE)), 
							iqr = ~IQR(.x, na.rm = TRUE)
						)
					)
				) %>%
				pivot_longer(
					cols = starts_with(v_edit),
					names_to = c("var", ".value"),
					names_sep = "_"
				)

			if (input$desc_groups_yn) {
				s2 <- s2 %>% unite(rname, yvar, var, sep = "_")
			} else {
				s2 <- s2 %>% rename(rname = var)				
			}	
			
			s3 <- s2 %>% 
				mutate(
					r = max - min,
					v = s^2,
					se = s/sqrt(n),
					cv = s/m*100
				) %>%
				relocate(
					rname, n, m, md, s, v, se, cv, min, max, r, q1, q3, iqr
				) 

			s3[2:13] <- signif(s3[2:13], input$sigfigs)
			s3 <- s3 %>% 
				mutate(across(where(is.numeric), as.character)) %>% 
				select(rname, all_of(s_list))
			
			r_tab <- s3 %>%
				pivot_longer(
					cols = (-rname),
					names_to = "stat",
					values_to = "val"
				) %>%
				pivot_wider(
					names_from = rname,
					values_from = val
				) %>%
				mutate(stat = s_names) %>%
				column_to_rownames("stat")
				
			r_out <- list(r_head, renderTable(r_tab, align = "r", rownames = TRUE))
		
		} else if (input$stat == 2) {
		# - # frequency table
			r_head <- HTML(
				"<p><h4>Frequency</h4></p>
				 <p>number of observations</p>")
			if (input$desc_groups_yn) {
				s1 <- data.frame(table(s_data()[[input$stat_x]], s_data()[[input$stat_y_d]]))
				names(s1) <- c(input$stat_x, "yvar", "frequency")
				r_tab <- pivot_wider(
					s1,
					names_from = "yvar",
					values_from = "frequency"
				)
			} else {
			# - # groups
				
				# - # no groups
				r_tab <- data.frame(table(s_data()[[input$stat_x]]))
				names(r_tab) <- c(input$stat_x, "frequency")
			}
		
			r_out <- list(r_head, renderTable(r_tab))
		
		} else if (input$stat == 3) {
		# - # 1-sample t
			tx <- t.test(s_data()[[input$stat_y]], mu = input$mu_null)
			
			r_head <- "<p><h4>One sample t test</h4></p>"
			r_var <- paste("<p>y variable = ", input$stat_y, "</p>", sep = "")
			r_null <- paste("<p>H<sub>0</sub>: &mu;<sub>", input$stat_y, "</sub> = ", input$mu_null, "</p>", sep = "")
			r_st <- paste("<p>x&#773;<sub>", input$stat_y, "</sub> = ", signif(tx$estimate, 5), "</p>", sep = "")
		
			if (tx$p.value < 0.0005 ) pval <- "< 0.001"
				else pval <- round(tx$p.value,4)
			r_tab <- data.frame("stat" = c("t", "df", "p"), 
				"value" = c(round(tx$statistic, 3), round(tx$parameter, 1), pval))
			
			r_out <- list(HTML(r_head), HTML(r_var), HTML(r_null), HTML(r_st),
				renderTable(r_tab, colnames = FALSE, align = "lr", digits = 3))
				
		} else if (input$stat == 4) {
		# - # paired t
			tx <- t.test(s_data()[[input$stat_x]] ,s_data()[[input$stat_y]], paired = TRUE)				
			r_head <- "<p><h4>Paired sample t test</h4></p>"
			r_var <- paste("<p>y1 = ", input$stat_x, "<br>y2 = ", input$stat_y, "</p>", sep = "")
			r_null <- paste("<p>H<sub>0</sub>: &mu;<sub>", input$stat_x, "-", input$stat_y, "</sub> = 0</p>", sep = "")
			r_st <- paste("<p>x&#773;<sub>", input$stat_x, "-", input$stat_y, "</sub> = ", signif(tx$estimate,5), "</p>", sep = "")
				
			if (tx$p.value < 0.0005 ) pval <- "< 0.001"
			else pval <- round(tx$p.value,4)
			r_tab <- data.frame("stat" = c("t", "df", "p"), 
				"value" = c(round(tx$statistic, 3), round(tx$parameter, 1), pval))
			
			r_out <- list(HTML(r_head), HTML(r_var), HTML(r_null), HTML(r_st),
				renderTable(r_tab, colnames = FALSE, align = "lr", digits = 3))
				
		} else if (input$stat == 5) {
		# - # 2-sample t
			r_head <- "<p><h4>Two sample t test</h4></p>"
			r_var <- paste("<p>y = ", input$stat_y, "<br>x = ", input$stat_x, sep = "")
			grps <- levels(s_data()[[input$stat_x]])
			
			if (length(grps) == 2) {
				tx <- t.test(s_data()[[input$stat_y]] ~ s_data()[[input$stat_x]])

				r_null <- paste("<p>H<sub>0</sub>: &mu;<sub>", grps[1], "</sub> = &mu;<sub>", grps[2], "</sub></p>", sep = "")
				r_st <- paste("<p>x&#773;<sub>", grps[1], "</sub> = ", signif(tx$estimate[1],5), ", x&#773;<sub>", grps[2], "</sub> = ", signif(tx$estimate[2],5), "</p>", sep = "")
					
				if (tx$p.value < 0.00005 ) pval <- "< 0.0001"
				else pval <- round(tx$p.value, 4)
				r_tab <- data.frame(
					"stat" = c("t", "df", "p"), 
					"value" = c(round(tx$statistic, 3), round(tx$parameter, 1), pval)
				)
			
			} else {
				r_null <- paste("<p>This test requires an independent variable with 2 levels.</p><p>The variable ", input$stat_x, " has ", length(grps), " levels.", sep = "")
				r_st <- NULL
				r_tab <- NULL
			}
			r_out <- list(HTML(r_head), HTML(r_var), HTML(r_null), HTML(r_st),
				renderTable(r_tab, colnames = FALSE, align = "lr", digits = 3))		
		
		} else if (input$stat == 6 || input$stat == 10) {
		# - # ANOVA / regression
			
			d <- s_data()
			names(d) <- c("xvar", "yvar")
			
			if (input$log_y) {
				if (min(d$yvar) == 0) {
					d <- d %>% mutate("log1p_yvar" = log1p(yvar))
					r_var <- paste("<p>x = ", input$stat_x, "<br>y = ln(", input$stat_y, " + 1)</p>", sep = "")
					mod <- aov(d$log1p_yvar ~ d$xvar)
				} else if (min(d$yvar) > 0) {
					d <- d %>% mutate("log_yvar" = log(yvar))
					r_var <- paste("<p>x = ", input$stat_x, "<br>y = ln(", input$stat_y, ")</p>", sep = "")
					mod <- aov(d$log_yvar ~ d$xvar)
				} else {
					r_var <- paste("<p>x = ", input$stat_x, "<br>y = ", input$stat_y, 
						"</p><p>(y has negative values and was not transformed)</p>", sep = "")
					mod <- aov(d$yvar ~ d$xvar)
				}
			} else {
				r_var <- paste("<p>x = ", input$stat_x, "<br>y = ", input$stat_y, "</p>", sep = "")
				mod <- aov(d$yvar ~ d$xvar)
			}
			
			# ANOVA table
			r_tab <- summary(mod)[[1]] %>% 
				rownames_to_column() 
			names(r_tab) <- c("source","df","SS","MS","F","p")
			r_tab <- r_tab %>% relocate("source", "SS", "df", "MS","F","p")
			r_tab$source <- c(input$stat_x, "residual")
			if (r_tab[1,6] < 0.0005) r_tab[1,6] <- "< 0.001"
			
			# regression coeff
			if (input$stat == 10) {
				r_sq <- round(r_tab$SS[1]/r_tab$SS[2], 3)
				r_rgr <- paste("<p>slope = ", round(mod$coefficients[2], 2),
					"</p><p>intercept = ", round(mod$coefficients[1], 2),
					"</p><p>r<sup>2</sup>= ", r_sq,
					"</p>", sep = "")
			} else {
				r_rgr <- NULL
			}
			
			# post hoc
			if (input$tukey) {
				r_post <- TukeyHSD(mod)
				r_post <- data.frame(r_post[[1]])
				r_post$p.adj <- round(r_post$p.adj, 3)
				r_post$p.adj[r_post$p.adj < 0.0005] <- "< 0.001"
				names(r_post) <- c("difference", "lower", "upper", "p (adjusted)")
				r_post <- rownames_to_column(r_post, var = "levels")
			} else {
				r_post <- NULL
			}
			
			# assumptions
			
			if (input$assumptions) {
			
				if (input$stat == 6) {
				
					lv <- leveneTest(mod)
					names(lv) <- c("df", "F", "p")
					if (lv$p[1] < 0.001) {
						lvp <- "p < 0.001"					
					} else {
						lvp <- paste0("p = ", round(lv$p[1],3))
					}
					r_lev <- paste0("<p><h4>Levene's test for homogeneity of variance</h4></p><p>F = ", 
						round(lv$F[1],3), ", df = ", lv$df[1],", ", lv$df[2], ", ", lvp, "</p>")
				} else {
					r_lev <- NULL
				}
			
				res <- unique(mod1$residuals)
				ks <- ks.test(res, "pnorm", mean = mean(res), sd = sd(res))
				if (ks$p.value < 0.001) {
						ksp <- "p < 0.001"					
					} else {
						ksp <- paste0("p = ", round(ks$p.value,3))
					}
				r_ks <- paste0("<p><h4>Kolmogorov-Smirnov test for normality of residuals</h4></p><p>D = ",
					round(ks$statistic,3), ", ", ksp, "</p>")
				
				tq <- qqnorm(mod$residuals, plot.it = FALSE)
				tq.data <- data.frame("theoretical" = tq$x, "sample" = tq$y, "fitted" = mod$fitted.values, "residual" = mod$residuals)
				x <- quantile(tq.data$theoretical, c(0.25, 0.75), type = 5)
				y <- quantile(tq.data$sample, c(0.25, 0.75), type = 5)
				q.slope <- diff(y) / diff(x)
				q.int   <- y[1] - q.slope * x[1]

				r_qq.plot <- ggplot(tq.data, aes(x = theoretical, y = sample)) +
					geom_point(shape = 1) +
					geom_abline(slope = q.slope, intercept = q.int) +
					ggtitle("Normality") +
					theme_classic()
					
				
				r_var.plot <- ggplot(tq.data, aes(x = fitted, y = residual)) +
					geom_point(shape = 1, position = position_jitter(height = 0, width = .01)) +
					ggtitle("Homogeneity of Variance") +
					theme_classic()
					
			} else {
				r_ks <- NULL
				r_qq.plot <- NULL
				r_var.plot <- NULL
			}
			
			
			if (input$stat == 6) {
				r_head <- "<p><h4>Analysis Of Variance</h4></p>"
				r_null <- paste("<p>H<sub>0</sub>: &mu;<sub>", input$stat_y, "</sub> equal at all levels of ", input$stat_x, sep = "")
			} else {
				r_head <- "<p><h4>Regression</h4></p>"
				r_null <- "<p>H<sub>0</sub>: slope = 0</p>"
			}
			
			r_out <- list(HTML(r_head), HTML(r_var), HTML(r_null), 
						renderTable(r_tab, na = "", align = "lrrrrr", digits = 3),
						HTML(r_rgr), renderTable(r_post, rownames = TRUE),
						renderPlot(r_qq.plot), renderPlot(r_var.plot))	
				

		} else if (input$stat == 7) {
		# - # chi-square goodness of fit
			levs <- levels(s_data()[[input$stat_y]])
			r_head <- "<p><h4>&Chi;<sup>2</sup> goodness-of-fit test</h4></p>"
				
			if (length(levs) > 1) {
			
				y_exp <- rep(1, length(levs))
				for (i in 1:length(levs)) {
					ui_name <- paste("lev", i, "exp", sep = "_")
					y_exp[i] <- input[[ui_name]]
				}
				
				xsqt <- chisq.test(table(s_data()[[input$stat_y]]), p = y_exp, rescale.p = TRUE)
				r_null <- paste(
					"<p>H<sub>0</sub>: the population frequency distribution of ", input$stat_y,
					" is equal to the hypothesized distribution</p>", sep = ""
				)
				r_tab <- data.frame(xsqt$observed)
				names(r_tab) <- c(input$stat_y,"observed")
				r_tab$expected <- xsqt$expected

				if (xsqt$p.value < 0.0005)  pval <- "< 0.001"
				else pval <- round(xsqt$p.value, 3)	
				r_test <- paste(	
					"<p>&Chi;<sup>2</sup> test statistic =  ", 
					round(xsqt$statistic, 3),
					"</p><p>d.f. =  ", xsqt$parameter,
					"</p><p>p value =  ", pval,
					"</p>", sep = ""
				)
			
			} else {
				r_null <- paste("<p>This test requires a variable with 2 or more levels.</p><p>The variable ", input$stat_y, " has ", length(levs), ".", sep = "")
				r_tab <- NULL
				r_test <- NULL
			}

			r_out <- list(HTML(r_head), HTML(r_null), renderTable(r_tab), HTML(r_test))
			
		} else if (input$stat == 8) {
		# - # chi-square association
			levs_x <- levels(s_data()[[input$stat_x]])
			levs_y <- levels(s_data()[[input$stat_y]])
			r_head <- "<p><h4>&Chi;<sup>2</sub> test of association</h4></p>"
				
			if (length(levs_x) > 1 & length(levs_y) > 1) {
			
				xsqt <- chisq.test(table(s_data()[[input$stat_x]], s_data()[[input$stat_y]]))
				r_null <- paste(
					"<p>H<sub>0</sub>: ", input$stat_x,
					" is independent of ",input$stat_y, 
					"</p>", sep = ""
				)
				r_obs <- data.frame(xsqt$observed)
				names(r_obs) <- c(input$stat_x, input$stat_y, "Freq")
				r_obs <- r_obs %>%
					pivot_wider(names_from = input$stat_y, values_from = Freq)
				r_exp <- data.frame(xsqt$expected) %>% 
					rownames_to_column(var = input$stat_x)
				names(r_exp) <- c(input$stat_x, levels(s_data()[[input$stat_y]]))

				if (xsqt$p.value < 0.0005)  pval <- "< 0.001"
					else pval <- round(xsqt$p.value, 3)
				r_sub1 <- "<p><b>Observed values</b></p>"
				r_sub2 <- "<p><b>Expected values</b></p>"
				r_sub3 <- "<p><b>Hypothesis test</b></p>"
				r_test <- paste(
					"<p>&Chi;<sup>2</sup> test statistic =  ", 
					round(xsqt$statistic, 3),
					"</p><p>d.f. =  ", xsqt$parameter,
					"</p><p>p value =  ", pval,
					"</p>", sep = ""
				)
			
			} else {
				r_null <- paste("<p>This test requires variables with 2 or more levels.</p><p>The variable ", 
					input$stat_x, " has ", length(levs_x), " and the variable ", 
					input$stat_y, " has ", length(levs_y), ".", sep = "")
				r_sub1 <- NULL
				r_obs <- NULL
				r_sub2 <- NULL
				r_exp <- NULL
				r_sub3 <- NULL
				r_test <- NULL
			}
			
			r_out <- list(HTML(r_head), HTML(r_null), HTML(r_sub1), renderTable(r_obs), HTML(r_sub2), renderTable(r_exp), HTML(r_sub3), HTML(r_test))
		
		} else if (input$stat == 9) {
		
			ctest <- cor.test(s_data()[[input$stat_x]], s_data()[[input$stat_y]])
			
			r_head <- "<p><h4>Correlation</h4></p>"
			r_var <- paste("<p>x = ", input$stat_x, ", y = ",input$stat_y, "</p>", sep = "")
			r_null <- "<p>H<sub>0</sub>: no correlation</p>"
			if (ctest$p.value < 0.0005)  pval <- "< 0.001"
				else pval <- round(ctest$p.value, 3)
			r_st <- paste("<p>r = ", round(ctest$estimate, 3), "</p><p>d.f. = ", ctest$parameter, "</p><p>p = ", pval,sep = "")
		
			r_out <- list(HTML(r_head), HTML(r_var), HTML(r_null), HTML(r_st))
		
		} 
		
		if (input$stat == 1) values$stats_table <- rownames_to_column(r_tab, var = "statistic")
		else if (input$stat == 7 | input$stat == 8 | input$stat == 9) values$stats_table <- NULL
		else values$stats_table <- r_tab
		
		if (input$tukey) values$r_post <- r_post
		else values$r_post <- NULL

		do.call(tagList, r_out)
	})

	output$stats_results <- renderUI( { stats_out() } )


	
# --- # --- # Reset stats # --- # --- #
	
observeEvent(input$reset_stats, {
		label = "reset stats options"
		reset("s_options")
		})	


# --- # --- # --- # Info tab items # --- # --- # --- #
	
	output$dune_metadata <- renderTable(
		dune.meta <- read_excel("data/dunes.xlsx", sheet = 2)	
	)
	
	output$fhs_metadata <- renderTable(
		dune.meta <- read_excel("data/heart.xlsx", sheet = 2)	
	)
	
# --- # --- # --- # Exit # --- # --- # --- #

	observeEvent(input$exit_app, {
		
		output$exit_message <- renderText("Exiting in 3 seconds...")
		delay(1000, output$exit_message <- renderText("Exiting in 2 seconds..."))
		delay(2000, output$exit_message <- renderText("Exiting in 1 second..."))
		delay(3000, js$closeWindow())
		delay(3050, stopApp())

	})
	
	session$onSessionEnded(stopApp)
	
}


shinyApp(ui, server)
