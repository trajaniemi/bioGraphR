library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)

# --- # --- # User Interface # --- # --- #

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
			tags$link(
				rel = "stylesheet", 
				type = "text/css", 
				href = "mystyle.css"
			),
		tags$link(
			rel = "icon",
			type = "image/x-icon",
			href = "scatter_plot.ico")
		),
	
		tabItems(

			### Data tab
			tabItem(
				tabName = "data_tab", 

				fluidRow(

					#	column 1: Selection
					column(
						4,
						box(
							title = "Open a dataset", 
							width = NULL, 
							status = "primary",
							solidHeader = TRUE,
						 
							"You may use a practice dataset, or upload your own data.",
						 
							radioButtons(
								"data_input",
								"",
								choices = list(
									"Use practice data" = 1,
									"Upload an Excel file" = 2,
									"Upload a text file (.csv)" = 3
								),
								selected = 1
							),

							conditionalPanel(
								"input.data_input == '1'",
								h5("Choose a practice dataset: "),
								selectInput(
									"data_choice", 
									"",
									choices = list(
										"Dune environment" = 1,
										"Reduced mowing" = 2,
										"Hubbard Brook tree census" = 3,
										"Sapling warming" = 4,
										"Framingham heart study" = 5,
										"Digoxin trial" = 6
									)
								)
							),

							conditionalPanel(
								"input.data_input == '2'",
								h5("Select a file: "),
								fileInput(
									"upload_excel", 
									"", 
									multiple = FALSE,
									accept = c(
										'application/vnd.ms-excel','application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
										'.xls',
										'.xlsx'
									)
								)
							),
					   
							conditionalPanel(
								"input.data_input == '3'",
								h5("Select a file: "),
								fileInput(
									"upload_csv",
									"", 
									multiple = FALSE,
									accept = c(".csv", ".text/csv")
								)
							)
						)
					
					),	# close column 1
					
					# column 2: summary
					column(
						8,
						box(
							title = "Data summary", 
							width = NULL, 
							status = "primary", 
							solidHeader = TRUE,
							
							strong("Categorical variables"),
							tableOutput("cat_table"),
							strong("Numeric variables"),
							tableOutput("num_table")
						)
					) 	# close column 2
			
				) # close row
			),  ### close Data tab
			
			tabItem(
				tabName = "graph_tab", 
	 
				## row 1
				fluidRow(
					
					# column 1: graph type and tabbed options
					column(
						3,
						
						box(
							title = "Graph basics", 
							width = NULL, 
							status = "primary",
							solidHeader = TRUE,
							selectInput(
								"type", 
								"Select a graph type",
								choices = c(
									"histogram" = "hist",
									"frequency plot" = "freq",
									"boxplot" = "box",
									"bar graph" = "bar",
									"scatterplot" = "scatter"
								),
								selected = "hist"
							),
								
							div(
								id = "graph_options_v",
								
								selectInput(
									"x_var", 
									"X Variable", 
									choices = ""
								),
								conditionalPanel(
									"input.type=='box' || input.type=='bar' || input.type=='scatter'",
									 selectInput(
										"y_var", 
										"Y Variable", 
										choices = ""
									)
								),
								conditionalPanel(
									"input.type == 'bar'",
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
									"gr_type",
									"Plot groups",
									choices = list(
										"no groups" = "none",
										"in one panel with a legend" = "legend",
										"in separate panels" = "panel"
									)
								),
								conditionalPanel(
									"input.gr_type != 'none'",
									selectInput(
										"gr_var",
										"Grouping variable",
										choices = ""
									)
								),
								conditionalPanel(
									"input.type == 'scatter'",
									numericInput("sym_size", "Symbol size", 1),
									checkboxInput(
										"rline", 
										strong("Show regression line"), 
										FALSE
									)
								)
							)
						),
						   
						box(
							title = "Graph options", 
							width = NULL, 
							height = 25,
							status = "primary",
							solidHeader = TRUE
						),
						   
						tabBox(
							width = NULL,
							 
							tabPanel(
								"X axis",
								div(
									id = "graph_options_x",
									textInput("lab_x", "X axis label", "label x-axis"),
									conditionalPanel(
										"input.type=='hist'",
										numericInput("bins","# of bins", 10)
									),
									conditionalPanel(
										"input.type == 'scatter' || input.type == 'hist'",
										checkboxInput(
											"num_x_values",
											strong("Change value range"),
											FALSE),
										conditionalPanel(
											"input.num_x_values",
											numericInput("min_x", "minimum x", 0),
											numericInput("max_x", "maximum x", 100)
										)
									),
									conditionalPanel(
										"input.type == 'freq' || input.type == 'box' || input.type == 'bar'",
										"Drag and drop values to change order",
										uiOutput("change_x")
									)	
								)
							),	#	close X axis tabPanel
							 
							tabPanel(
								"Y axis",
								div(
									id = "graph_options_y",
									textInput("lab_y", "Y axis label", "label y-axis"),
									checkboxInput(
										"num_y_values",
										strong("Change value range"),
										FALSE
									),
									conditionalPanel(
										"input.num_y_values",
										numericInput("min_y", "minimum y", 0),
										numericInput("max_y", "maximum y", 100)
									)
								)
							),	#	close Y axis tabPanel (graph_options_3)
							
							tabPanel(
								"Groups",
								div(
									id = "graph_options_g",
									conditionalPanel(
										"input.gr_type == 'none'",
										"No groups selected"
									),
									conditionalPanel(
										"input.gr_type == 'legend'",
										textInput("lab_z", "Legend label", "label legend"),
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
										"input.gr_type != 'none'",
										"Drag and drop values to change order",
										uiOutput("change_groups")
									)
								)
							), #	close Groups tabPanel
							
							tabPanel(
								"Font",
								
								div(
									id = "graph_options_f",
									
									strong("Font size"),
									numericInput("font_labs", "Axis labels", 12),
									numericInput("font_vals", "Axis values", 12),
									conditionalPanel(
										"input.gr_type == 'legend'",
										numericInput("font_llab", "Legend title", 12),
										numericInput("font_lval", "Legend values", 12)
									)
							
                                )
							)	# close Font tabPanel
								
						) 	# close tabBox

					),	# 	close options column
					
					# column 2: buttons
					column(
						2,
						
						box(
							title = "Plot/update graph", 
							width = NULL, 
							status = "primary", 
							solidHeader = TRUE,
							actionButton("go_plot", "GO", width = "100%")
						),
						box(
							title = "Reset graph options", 
							width = NULL, 
							status = "primary", 
							solidHeader = TRUE,
							actionButton("reset_plot", "GO", width = "100%")
						),
						box(
							title = "Download graph", 
							width = NULL, 
							status = "primary", 
							solidHeader = TRUE,
							
							div(
								id = "graph_options_d",
									
								textInput("plot_name","File name", "filename"),
								radioButtons("file_type","File type", choices = c("png","pdf","jpg")),
								numericInput("plot_w","Image width (in)",4),
								numericInput("plot_h","Image height (in)", 3),
								br(), br(),
								downloadButton("download_plot","Download", width = "100%")
							)
						)
					),
					
					# column 3: graph
					column(
						7,
						
						box(
							title = "Graph output",
							width = NULL, 
							status = "primary",
							solidHeader = T, 
							height = 480,
							plotOutput("out_ggplot")  
						)  
					) 	# 	close graph column 
					
				) 	## 	close row
		  
			),	### close graph tab
			
						### Info tab
			tabItem(
				tabName = "info_tab", 
				  
				fluidRow(
				
					column(
						4,
						   
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
								"Data in graphs",
								"Variable choices are based on the type of graph selected.",
								tags$ul(
									tags$li("The X variable must be numeric for histograms and scatterplots, and categorical for frequency plots, bar graphs and boxplots."),
									tags$li("Histograms and frequency plots show frequency (fraction of observations) on the Y axis. For other graphs, the Y variable must be numeric."),
									tags$li("Any graph may have a categorical grouping variable.")
								),
								br(),
								"Observations with missing values of any of the selected variables are excluded from the graph."
							),
								  
							tabPanel(
								"Graph action buttons",
								"Use the 'GO' button under 'Plot/update graph' to create a graph, and to apply any changes to graph options.",
								br(), br(),
								"The 'GO' button under 'Reset plot options' will reset all selections. If a graph fails to plot, try resetting.",
								br(), br(),
								"Graphs can be downloaded as .png, .pdf, or .jpg files. A font size of 10 is recommended for download. The font size can be changed in the 'Font' options tab."
							),
								  
							tabPanel(
								"About",
								"bioGraphR is designed to produce graphs needed by biology undergraduates in lab courses.",
								br(),br(),
								"This app was written in R version 4.0.3 and uses these packages: ",
								tags$ul(
									tags$li("shiny"),
									tags$li("shinydashboard"),
									tags$li("shinyjs"), 
									tags$li("shinyjqui"),
									tags$li("dplyr"),
									tags$li("tidyr"),
									tags$li("readxl"), 
									tags$li("ggplot2")
								),
								br(),br(),
								"This version created 1 / 2021 by Tara K. Rajaniemi."
								)
								
						) 	# 	close tabBox
						   
					), 	# 	close column 1: App information
					
					column(
						8,
						   
						box(
							title = "Practice datasets", 
							width = NULL, 
							height = 25,
							status = "primary",
							solidHeader = T
						),
						   
						tabBox(
							width = NULL,
								  
							tabPanel(
								"Dune environment",
								"These data are from the coastal dunes at Waquoit Bay Estuarine Research Reserve. Data collected by Dr. Tara Rajaniemi in 2006 and 2007.",
								br(),br(),
								strong("Metadata:"),
								tableOutput("dune_metadata")
							),
							tabPanel(
								"Reduced mowing",
								"These data are from reduced-mowing pilot plots in summer and fall 2018. Data collected by Dr. Jennifer Koop, Dr. Diana Barrett, Andrea Bickford, Steve Parks, and Fall 2018 EEE lab students.",
								br(),br(),
								strong("Metadata:"),
								tableOutput("mow_metadata")
							),
							tabPanel(
								"Hubbard Brook trees census",
								"Data from the 2007 inventory of Watershed 6 at Hubbard Brook Experimental Forest. Some variables have been removed or recoded.",
								br(),br(),
								"Battles, J., C. Johnson, S. Hamburg, T. Fahey, C. Driscoll, and G. Likens. 2019. Forest Inventory of a Northern Hardwood Forest: Watershed 6 2007, Hubbard Brook Experimental Forest ver 2. Environmental Data Initiative. https://doi.org/10.6073/pasta/6a1cd73c4e196f930eda7d8e25bb69d8",
								br(),br(),
								strong("Metadata:"),
								tableOutput("tree_metadata")
							),
							tabPanel(
								"Sapling warming experiment",
								"Data from the Climate Change Across Seasons Experiment (CCASE) at the Hubbard Brook Experimental Forest. Some variables have been removed or recoded.",
								br(),br(),
								"Templer, P., J. Harrison, A. Reinmann, N. Phillips, P. Sorensen, and R. Sanders-Demott. 2020. Climate Change Across Seasons Experiment (CCASE) at the Hubbard Brook Experimental Forest. Environmental Data Initiative. https://doi.org/10.6073/pasta/efef0e8cfb65463c153aa9d587004ec8.",
								br(),br(),
								strong("Metadata:"),
								tableOutput("warm_metadata")
							),
							tabPanel(
								"Framingham heart study",
								"Teaching dataset derived from the Framingham Heart Study, made available by the National Heart, Lung, and Blood Institute at https://biolincc.nhlbi.nih.gov/teaching/. Data here are a subset of the complete dataset (only the first observation period, for patients with no previous heart disease). Some variables have been removed or recoded.",
								br(),br(),
								strong("Metadata:"),
								tableOutput("fhs_metadata")
							),
							tabPanel(
								"Digoxin trial",
								"Teaching dataset derived from the Digitalis Investigation Group, made available by the National Heart, Lung, and Blood Institute at https://biolincc.nhlbi.nih.gov/teaching/. Some variables have been removed or recoded.",
								br(),br(),
								strong("Metadata:"),
								tableOutput("dig_metadata")
							)
						) 	# 	close tabBox
							
					) # close column 2: Practice datasets
					
				)	## 	close row  
				
			)	### close info tab 

			
		) 	### close tab items
	
	)	### close dashboard body 

)	# 	close dashboardPage


server <- function(input, output, session) { 
		
# --- # --- # Data # --- # --- #

# --- # Get data from a file

	all.data <- reactive({
	
		if (input$data_input == 1) {
			if (input$data_choice == 1)
				d <- read_excel("data/dunes.xlsx", na = c("","NA"))
			else if (input$data_choice == 2)
				d <- read_excel("data/mow.xlsx", na = c("","NA"))
			else if (input$data_choice == 3)
				d <- read_excel("data/trees.xlsx", na = c("","NA"))
			else if (input$data_choice == 4) 
				d <- read_excel("data/warming.xlsx", na = c("","NA"))
			else if (input$data_choice == 5)
				d <- read_excel("data/heart.xlsx", na = c("","NA"))
			else d <- read_excel("data/digoxin.xlsx", na = c("","NA"))
		}
		
		else if (input$data_input == 2) {
			file_in <- input$upload_excel
			if (is.null(input$upload_excel)) {
				return(data.frame(x = "Select your datafile"))
			} else {
				d <- read_excel(
					file_in$datapath, 
					na = c("NA","na","n/a","N/A","")
				)
			}
		}
		
		else {
			file_in <- input$upload_csv
			if (is.null(input$upload_csv)) {
				return(data.frame(x = "Select your datafile"))
			} else {
				d <- read_csv(
					file_in$datapath, 
					na = c("NA","na","n/a","N/A",""),
					col_types = cols()
				)
			}
		}
		
		d <- d %>% mutate(across(where(is.character), as.factor))
		
		d

	})
	
# --- # Summarize data by variable type

	observe({
		
		avail_num <- c(all.data() %>%  select(where(is.numeric)) %>% names)
		
		if (identical(avail_num, character(0))) 
			avail_num <- c("no numeric variables available" = ".")
		
		avail_cat <- c(all.data() %>% select(where(is.factor)) %>% names)
		
		if (identical(avail_cat, character(0)))
			avail_cat <- c("no categorical variables available" = ".")
	
		if (input$type == "scatter" || input$type == "hist")
			updateSelectInput(session, "x_var", choices = avail_num)
		else 
			updateSelectInput(session, "x_var", choices = avail_cat)

		updateSelectInput(session, "y_var", choices = avail_num)
		updateSelectInput(session, "gr_var", choices = avail_cat)
		
	})
	
	cat_summary <- reactive({
	
		df.cat <- all.data() %>% select(where(is.factor))
		
		if (ncol(df.cat) > 0) {
			v <- lapply(df.cat, levels)
			a <- names(v)
			b <- rep("x", length(a))
			for (i in 1:length(v)) { 
				level_string <- v[[i]][1]
				for (j in 2:length(v[[i]])) {
					level_string <- paste(level_string, v[[i]][j], sep = ", ")
				}
				b[i] <- level_string
			}
			d <- data.frame("variable" = a, "levels" = b)

		} else {
			d <- (data.frame(x = "No categorical variables found."))
		}
		
		return(d)
	
	})

	output$cat_table <- renderTable(cat_summary())

	num_summary <- reactive({
	
		d.min  <- all.data() %>% summarise(across(where(is.numeric), ~ min(.x, na.rm = TRUE)))
		d.max  <- all.data() %>% summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE)))
		d.mean <- all.data() %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
		d.sd   <- all.data() %>% summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
		a <- names(d.min)
		
		if (ncol(d.mean > 0))
			d <- data.frame("variable" = a, "min" = t(d.min), "max" = t(d.max), "mean" = t(d.mean), "sd" = t(d.sd))
		else
			d <- (data.frame(x = "No numeric variables found."))
		
		return(d)
		
	})
	
	output$num_table <- renderTable(num_summary(), digits = 3)

# --- # Select data for graphs

	g.data <- eventReactive(input$go_plot, {
	
		d1 <- all.data() %>% select(.data[[input$x_var]])
		
		
		if (input$type == "box" || input$type == "bar" || input$type == "scatter")
			d2 <- all.data() %>% select(.data[[input$y_var]])
		
		if (input$gr_type != "none") 
			d3 <- all.data() %>% select(.data[[input$gr_var]])
		
		if (input$type == "hist" || input$type == "freq") {
			if (input$gr_type == "none") {
				d <- d1
				names(d) <- "xvar" 
			} else {
				d <- cbind(d1, d3)
				names(d) <- c("xvar", "zvar")
			}
		} else {
			if (input$gr_type == "none") {
				d <- cbind(d1, d2)
				names(d) <- c("xvar", "yvar") 
			} else {
				d <- cbind(d1, d2, d3)
				names(d) <- c("xvar", "yvar", "zvar")
			}
		}
	
		d <- drop_na(d)
		d <- droplevels(d)

		if (is.factor(d1)) d1 <- factor(d1, levels = as.vector(input$x_sorted_order))
		if (input$gr_type != "none") d3 <- factor(d3, levels = as.vector(input$groups_sorted_order))
		
	
		return(d)
	
	})
	
	output$change_x <- renderUI({
	
		orderInput(
			"x_sorted", 
			"",
			items = levels(all.data()[[input$x_var]]),
			width = "100%",
			class = "ui-sortable"
			)
	
	})
  
	output$change_groups <- renderUI({
	
		orderInput(
			"gr_sorted", 
			"",
			items = levels(all.data()[[input$gr_var]]),
			width = "100%",
			class = "ui-sortable"
			)
	
	})
	
  
# --- # --- # Graphs # --- # --- #

# --- # Update options

	observe({
	
		# x-axis
		updateTextInput(session, "lab_x", value = input$x_var)
		
		# y-axis
		if (input$type == "hist" || input$type == "freq") {
			updateTextInput(session, "lab_y", value = "frequency")
		}
		else {
			updateTextInput(session, "lab_y", value = input$y_var)
		}
		
		# groups
		if (input$gr_type == "legend")
			updateTextInput(session, "lab_z", value = input$gr_var)
		
		
		
	
	})
	

# --- # Create ggplot code
	
	data <- reactiveValues()

	observeEvent(input$go_plot, {
	
		# aes
		if (input$gr_type == "legend") {			
			if (input$type == "hist" || input$type == "freq") 
				p_aes <- ggplot(g.data(), aes(x = xvar, fill = zvar))
			if (input$type == "scatter") 
				p_aes <- ggplot(g.data(), aes(x = xvar, y = yvar, fill = zvar))
			if (input$type == "box" || input$type == "bar")
				p_aes <- ggplot(g.data(), aes(x = xvar, y = yvar, fill = zvar))
		} else {
			if (input$type == "hist" || input$type == "freq") 
				p_aes <- ggplot(g.data(), aes(x = xvar))
			if (input$type == "box" || input$type == "bar" || input$type == "scatter")
				p_aes <- ggplot(g.data(), aes(x = xvar, y = yvar))
		}

		# geom
		if (input$gr_type == "legend") {
			if (input$type == "hist")
				p_geom <- geom_bar(aes(y = (..count..)/sum(..count..)), color = "black", position = "dodge", width = 1)
			if (input$type == "freq")
				p_geom <- geom_bar(aes(y = (..count..)/sum(..count..)), color = "black", position = "dodge")
			if (input$type == "box")
				p_geom <- geom_boxplot(color = "black", position = "dodge")
			if (input$type == "bar")
				p_geom <- stat_summary(fun = "mean", geom = "bar", color = "black", position = "dodge")
			if (input$type == "scatter")
				p_geom <- geom_point(color = "black", shape = 21, size = input$sym_size)
		} else {
			if (input$type == "hist")
				p_geom <- geom_bar(aes(y = (..count..)/sum(..count..)), color = "black", fill = "white", width = 1)
			if (input$type == "freq")
				p_geom <- geom_bar(aes(y = (..count..)/sum(..count..)), color = "black", fill = "white")
			if (input$type == "box")
				p_geom <- geom_boxplot(color = "black", fill = "white")
			if (input$type == "bar")
				p_geom <- stat_summary(fun = "mean", geom = "bar", color = "black", fill = "white")
			if (input$type == "scatter")
				p_geom <- geom_point(color = "black", shape = 21, fill = "white", size = input$sym_size)
		}
		
		# add geoms
		if (input$type == "bar" && input$error_bars == 2) {
			p_error <- stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1),
				geom = "errorbar", color = "black", width = 0.3, position = position_dodge(0.9))
		} else if (input$type == "bar" && input$error_bars == 3) {
			p_error <- stat_summary(fun.data = "mean_se", geom = "errorbar", 
				color = "black", width = 0.3, position = position_dodge(0.9))
		} else {
			p_error <- NULL
		}
				
		if (input$type == "scatter" && input$rline)
			p_line <- geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) 
		else
			p_line <- NULL
				
		# x axis
		if (input$type == "hist" || input$type == "scatter") {
			if(input$num_x_values)  
				x_lims <- c(input$min_x, input$max_x)
			else x_lims <- NULL
		}
		if (input$type == "hist") {
			p_xaxis <- scale_x_binned(
				n.breaks = input$bins, 
				name = input$lab_x,
				limits = x_lims
			)
		} else if (input$type == "scatter") {
			p_xaxis <- scale_x_continuous(
					name = input$lab_x,
					limits = x_lims
			)
		} else {
			p_xaxis <- scale_x_discrete(name = input$lab_x)
		}
		
		# y axis
		if(input$num_y_values) 
			y_lims <- c(input$min_y, input$max_y)
		else y_lims <- NULL
		
		p_yaxis <- scale_y_continuous(
			name = input$lab_y,
			limits = y_lims,
			expand = expansion(mult = c(0, 0.1))
			)
		
		# legend variable
		if (input$gr_type == "legend") {
			p_fill <- scale_fill_grey(start = 1, end = 0.2, name = input$lab_z)
		} else {
			p_fill <- NULL
		}
		
		p <- p_aes + p_geom + p_error + p_line + p_xaxis + p_yaxis + p_fill +
			theme_classic(base_size = 12) +
			theme(
				axis.title = element_text(size = input$font_labs, face = "plain",color = "black"),
				axis.text = element_text(size = input$font_vals, color = "black"),
				axis.ticks = element_line(color = "black")
			)
				
		if (input$gr_type == "legend") {
			p <- p + theme(
				legend.background = element_rect(color = NA, fill = NA),
				legend.text = element_text(size = input$font_lval),
				legend.title = element_text(size = input$font_llab),
				legend.position = input$pos_leg,
				legend.key.size = unit(input$leg_key,"cm")
			)
		}
				
		if (input$gr_type == "panel") {
			p <- p + facet_wrap(~ zvar, scales = "free", nrow = 1) +
				theme(
					strip.background = element_blank(),
					strip.text = element_text(colour = "black", size = rel(1))
				)
		}
		
		data$plot <- p
		
	})

# --- # Plot graph on screen

	output$out_ggplot <- renderPlot(
		width = 480,
		height = 360,
			{ data$plot }
	)
	
# --- # Download graph
	observe({
	
		data$p.name <- input$plot_name
		data$p.type <- input$file_type
		data$p.w    <- input$plot_w  
		data$p.h    <- input$plot_h
	
	})


	output$download_plot <- downloadHandler(
		filename = function(){
			paste(data$p.name, ".", data$p.type, sep = "")
		},
		content = function(file){
			ggsave(file, plot = data$plot, 
			width = data$p.w, height = data$p.h,
			units = "in")
		}
	)
	
# --- # Reset graph
	
	observeEvent(input$reset_plot, {
		reset("graph_options_v")
		reset("graph_options_x")
		reset("graph_options_y")
		reset("graph_options_g")
		reset("graph_options_f")
		reset("graph_options_d")
		})

# --- # --- # Info tab items # --- # --- #
	
	output$dune_metadata <- renderTable(
		dune.meta <- read_excel("data/dunes.xlsx", sheet = 2)	
	)
	
	output$mow_metadata <- renderTable(
		dune.meta <- read_excel("data/mow.xlsx", sheet = 2)	
	)
	
	output$tree_metadata <- renderTable(
		dune.meta <- read_excel("data/trees.xlsx", sheet = 2)	
	)
	
	output$warm_metadata <- renderTable(
		dune.meta <- read_excel("data/warming.xlsx", sheet = 2)	
	)
	
	output$fhs_metadata <- renderTable(
		dune.meta <- read_excel("data/heart.xlsx", sheet = 2)	
	)
	
	output$dig_metadata <- renderTable(
		dune.meta <- read_excel("data/digoxin.xlsx", sheet = 2)	
	)
	
# --- # --- # End R session when browser closed # --- # --- #
	if (!interactive()) {
		session$onSessionEnded(function() {
			stopApp()
			q("no")
		})
	}
}


shinyApp(ui, server)
