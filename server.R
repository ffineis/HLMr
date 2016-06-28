library(shiny)
library(shinyBS)
library(foreign)
library(data.table)


shinyServer(function(input, output, session) {
	rVals <- reactiveValues(DT = NULL)

	## LOAD IN DATA FILE.
	observeEvent(input$upload_data, {

		if(grepl(".csv|.txt|.tsv", input$upload_data$name)){
			dt <- fread(input$upload_data$datapath)
			rVals$DT <- dt
		}
		if(grepl(".sav", input$upload_data$name)){
			dt <- as.data.table(read.spss(input$upload_data$datapath, to.data.frame = T))
			all_names <- names(DT)
			repl_NA <- function(x){
				if(x %in% c("", " ", "  ", "   ", "    ")){
					return(NA)
				} else{
					return(x)
				}
			}

			dt[, eval(all_names) := lapply(.SD, repl_NA), .SDcols = all_names]
			rVals$DT <- dt
		}
		if(any(is.na(dt))){
			output$na_opt <- radioButtons("handleNA",
				"Missing values are present. Choose one:",
				choices = c("Complete cases", "Median impute", "KNN impute", "MICE impute"),
				selected = "Complete cases",
				inline = F)
		} else{
			output$na_opt <- renderText({"No missing values were detected."})
		}

		## RENDER NUMERIC SUMMARY TABLE.
		output$numeric_summary_dt <- renderDataTable({
			numeric_vars <- names(which(unlist(lapply(dt, function(x){class(x) %in% c("numeric", "integer")}))))
			NA_cts <- lapply(dt[, .SD, .SDcols = numeric_vars], function(x){sum(is.na(x))})
			mn <- as.numeric(lapply(dt[, .SD, .SDcols = numeric_vars], min))
			q25 <- as.numeric(lapply(dt[, .SD, .SDcols = numeric_vars], function(x){quantile(x, 0.25)}))
			q50 <- as.numeric(lapply(dt[, .SD, .SDcols = numeric_vars], function(x){quantile(x, 0.5)}))
			q75 <- as.numeric(lapply(dt[, .SD, .SDcols = numeric_vars], function(x){quantile(x, 0.75)}))
			mx <- as.numeric(lapply(dt[, .SD, .SDcols = numeric_vars], max))

			numeric_summary_dt <- data.table(numeric_vars, NA_cts, mn, q25, q50, q75, mx)
			setnames(numeric_summary_dt, c("Variable", "NA count", "Min", "25% quantile", "Median", "75% quantile", "Max"))

			numeric_summary_dt
		}, options = list(searching = FALSE))
	})
  
  ## RENDER LEVEL IDENTIFIER VARS.
  output$levelIDs <- renderUI({
    lapply(1:input$hlm_k, function(i){
      selectizeInput(inputId = paste0("level_id_", i),
                     label = paste("Level", i, "ID variable"),
                     choices = names(rVals$DT),
                     multiple = F)
    })
  })

  ## RENDER 'SPECIFY MODEL' TABLE.
  # output$specify_model_dt <- renderDataTable({
  # 	data.table(model = withMathJax(sprintf("\\beta\\")))
  # }, options = list(searching = FALSE))
  output$ex3 <- renderUI({ ## NOTE: MUST USE withMathJax WITH renderUI!!! See http://shiny.rstudio.com/gallery/mathjax.html.
    withMathJax(
      helpText('The busy Cauchy distribution
               $$\\frac{1}{\\pi\\gamma\\,\\left[1 +
               \\left(\\frac{x-x_0}{\\gamma}\\right)^2\\right]}\\!$$'))
  })

})