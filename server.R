library(shiny)
library(shinyBS)
library(foreign)
library(data.table)
library(DT)

## TODO:
## - organize level-1, centering, level-2 covariates, random effect selection into generalizable nested list
## - Ensure that mixed model prints as expected


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

			dat <- datatable(numeric_summary_dt
				,rownames = FALSE
				,options = list(dom = "t")) %>% formatStyle(names(numeric_summary_dt), target = "row", backgroundColor = "#222222")

			return(dat)
			}
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
 
 		## LEVEL 1 COVARIATE CENTERING OPTIONS
		output$level_1_options <- renderUI({
			lapply(c(1:length(input$level_1_vars)), function(x){
				if(input$level_1_vars[x] == "Intercept"){
					NULL
				} else {
					radioButtons(paste0("level_1_centering_", level_1_vars[x])
								,label = input$level_1_vars[x]
								,choices = c("uncentered", "group centered", "grand centered")
								,selected = "uncentered"
								,inline = T)	
				}
			})
		})

		## LEVEL 2 COVARIATE SELECTION
		output$level_2_covariates <- renderUI({
			if("Intercept" %in% input$level_1_vars){
				l1 <- lapply(c(1), function(x){

						selectizeInput("level_2_covariate_Intercept"
							,label = HTML("&beta;-0j covariates:")
							,choices = paste0(rep(names(rVals$DT), each = 2), c(": uncentered", ": grand centered"))
							,selected = NULL
							,multiple = T)
						})
			} else{
				l1 <- NULL
			}

			nonIntercept_vars <- setdiff(input$level_1_vars, "Intercept")
			if(length(nonIntercept_vars) > 0){
				l2 <- lapply(1:length(nonIntercept_vars), function(x){
						selectizeInput(paste0("level_2_covariate_", nonIntercept_vars[x])
							,label = HTML(paste0("&beta;-", x, "j covariates:"))
							,choices = paste0(rep(names(rVals$DT), each = 2), c(": uncentered", ": grand centered"))
							,selected = NULL
							,multiple = T)
						})
			} else{
				l2 <- NULL
			}

			l <- append(l1, l2)
			l
		})

		## LEVEL 2 RANDOM EFFECT CHOICE
		output$level_2_randomEffects <- renderUI({
			if("Intercept" %in% input$level_1_vars){
				l1 <- lapply(c(1), function(x){
						div(style="height: 65px;",
							checkboxInput("level_2_randomEffect_Intercept"
								,label = HTML("&beta;-0j:&nbsp;&nbsp; Has random effect?")
								,value = T)
							)
						})
			} else{
				l1 <- NULL
			}

			nonIntercept_vars <- setdiff(input$level_1_vars, "Intercept")
			if(length(nonIntercept_vars) > 0){
				l2 <- lapply(1:length(nonIntercept_vars), function(x){
						div(style="height: 65px;",
							checkboxInput(paste0("level_2_randomEffect_", nonIntercept_vars[x])
								,label = HTML(paste0("&beta;-", x, "j:&nbsp;&nbsp; Has random effect?"))
								,value = T)
							)
						})
			} else{
				l2 <- NULL
			}

			l <- append(l1, l2)
			l
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

		## Need to better organize a nested list of level-1 covariates, centering options, level-2 covariates, and rand effects
		nonIntercept_vars <- setdiff(input$level_1_vars, "Intercept")
		level_1_centeringVec <- input[[grep(names(input), "level_1_centering_", value = T)]]
		covars <- NULL
		if(length(nonIntercept_vars) > 0){
			for(i in 1:length(nonIntercept_vars)){
				# covars <- c(covars, paste0("\\beta_{", i, "j}")
				newCovar_name <- nonIntercept_vars[i]
				centering_opt <- grep(newCovar_name, level_1_centeringVec, value = T)
				if(grepl("uncentered", centering_opt)){
					newCovar_exp <- paste0("\\beta_{", i, "j}(", newCovar_name, "_{ij})")
				}
				if(grepl("group centered", centering_opt)){
					newCovar_exp <- paste0("\\beta_{", i, "j}(", newCovar_name, "- \\bar{", newCovar_name, "}_{\\cdot j})")
				}
				if(grepl("grand centered", centering_opt)){
					newCovar_exp <- paste0("\\beta_{", i, "j}(", newCovar_name, "- \\bar{", newCovar_name, "}_{\\cdot \\cdot})")
				}
			}
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





##---------------------------------------------------------------------------------------##
##										DEPRECATED CODE									 ##
##---------------------------------------------------------------------------------------##

## ->> Attempting to put greek letters in a datatable (also includes column of selectizeInput widgets)
		# output$level_2_options <- renderDataTable({

		# 	level_1_vars <- NULL
		# 	if("Intercept" %in% input$level_1_vars){
		# 		# level_1_vars <- c(level_1_vars, HTML("$$ \\beta_{0j} $$"))
		# 		level_1_vars <- c(level_1_vars, "$$ \\beta_{0j} $$")
		# 	}
			
		# 	nonIntercept_vars <- setdiff(input$level_1_vars, "Intercept")
		# 	if(length(nonIntercept_vars) > 0){
		# 		for(i in 1:length(nonIntercept_vars)){
		# 			## Note: concatenating HMTL objects together renders a character vector, not a vector of HTML objects.
		# 			level_1_vars <- c(level_1_vars, paste0("$$ \\beta_{", i, "j} $$"))
		# 		}
		# 	}

		# 	level_2_options_table <- data.frame(Coefficient = rep(NA, length(level_1_vars))
		# 										,Covariates = shinyInput(selectizeInput
		# 											,id = "level_2_covariate_"
		# 											,len = length(input$level_1_vars)
		# 											,choices = c(setdiff(names(rVals$DT), isolate(input$outcome_var)))
		# 											,multiple = T
		# 											)
  # 												,stringsAsFactors = FALSE
  # 												)
		# 	for(i in 1:nrow(level_2_options_table)){
		# 		level_2_options_table$Coefficient[i] <- HTML(level_1_vars[i])
		# 	}

		# 	dat <- datatable(level_2_options_table
		# 					,rownames = FALSE
		# 					,escape = FALSE
		# 					,options = list(dom = "t"
		# 									,preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }') 
	 #  										,drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
	 #  										)) %>% formatStyle(names(level_2_options_table), target = "row", backgroundColor = "#222222")
			
		# 	return(dat)
		# 	}
		# )