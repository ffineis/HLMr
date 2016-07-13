library(shiny)
library(shinyBS)
library(foreign)
library(data.table)
library(DT)


shinyServer(function(input, output, session) {

	##------------------ INITIALIZE USEFUL STORAGE/FUNS -----------------##

	rVals <- reactiveValues(DT = NULL)

	shinyInput = function(FUN, len, id, ...) { 
      inputs = character(len) 
      for (i in seq_len(len)) { 
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
      } 
      inputs 
    } 

    shinyValue = function(id, len) { 
      unlist(lapply(seq_len(len), function(i) { 
        value = input[[paste0(id, i)]] 
        if (is.null(value)) NA else value 
      })) 
    } 

	##----------------------------- FILE/DATA ----------------------------##
	
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
			},
			options = list(dom = "t")
		)
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


	##------------------------------ SPECIFY MODEL --------------------------##
	obs <- observe({
		output$outcome <- renderUI({
			selectizeInput(inputId = "outcome_var",
							label = "",					## No widget label to avoid clutter (already using h3 header)
							choices = names(rVals$DT),
							selected = names(rVals$DT)[1],
							multiple = F)
		})

		output$select_level_1_vars <- renderUI({
			selectizeInput(inputId = "level_1_vars",
							label = "Covariates",
							choices = c("Intercept", setdiff(names(rVals$DT), input$outcome_var)), ## Disallow user from picking outcome variable as a predictor.
							selected = "Intercept",
							multiple = T)
		})
	})

	obs <- observeEvent(input$level_1_vars, {
 
		output$level_1_options <- renderUI({
			lapply(c(1:length(input$level_1_vars)), function(x){
				if(input$level_1_vars[x] == "Intercept"){
					NULL
				} else {
					radioButtons(paste0("level_1_centering_", x),
								label = input$level_1_vars[x],
								choices = c("uncentered", "group centered", "grand centered"),
								selected = "uncentered",
								inline = T)	
				}
			})
		})
	})

	## RENDER MIXED/COMBINED MODEL
	obs <- observe({

		if(!is.null(input$outcome_var)){
			outcome <- paste0(input$outcome_var, "_{ij} ")
		} else{
			outcome <- NULL
		}
		
		if("Intercept" %in% input$level_1_vars){
			intercept <- "\\beta_{0j}"
		} else{
			intercept <- NULL
		}

		output$MixedModel <- renderUI({ ## NOTE: MUST USE withMathJax WITH renderUI!!! See http://shiny.rstudio.com/gallery/mathjax.html.
			withMathJax(
			  helpText(paste0(
			           "$$", outcome, " = ", intercept, "$$"
			           	)
			  )
			 )
		})
	})
})