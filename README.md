# bioGraphR
a shiny app for graphing
created for BIO 225, Experiments in Ecology and Evolution, at UMass Dartmouth

BGRbasic.R is a single-file version of the app. It does not use the data and www files. Students with limited R experience can run BioGraphR from RStudio with the package 'shiny' installed.

The current version of BGRbasic has these functions:

Data
	Import data from Excel or comma-delimited text files.
 	Filter data based on values of variables.

Graphs
	Produce histograms, frequency plots, boxplots, bar graphs, and scatterplots
 	Add a grouping variable to a single graph or separate groups into panels
	Add error bars or trend lines
 	Edit axis labels, font sizes, and symbol sizes
 	Reorder values of categorical variables
	Download graphs as .png, .pdf, or .jpg

Statistics
	Create tables of descriptive statistics, with selection of which statistics and number of significant digits
 	Create frequency tables
	Conduct statistical hypothesis tests
 		One-sample, paired, and 2-sample t tests
 		One-way ANOVA with Tukey post-hoc tests, residual plots, and Levene's test
		Chi-square goodness-of-fit tests and tests of association
		Correlation
		Regression with residual plots
	Log-transform data for ANOVA and regression
 	Download output tables as .csv

 

 
	
