
.PHONY: all

all:
	Rscript -e "shiny::runApp('square_sales_analysis/app.R',port=1314)"
