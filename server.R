library(shiny)
library(shinyBS)
library(foreign)
library(data.table)
library(DT)

## TODO:
## - Determine best way to print models of individual coefficients.
## - Make UI for level k dependent on that of level k-1
## - Format mixed model now that coffiecient models can be [somewhat stored] in GatherCovarsHLM.

shinyServer(function(input, output, session) {

	##------------------ INITIALIZE USEFUL STORAGE/FUNS -----------------##

	rVals <- reactiveValues(DT = NULL)

	shinyInput <- function(FUN, len, id, ...) { 
	  inputs = character(len) 
	  for (i in seq_len(len)) { 
		inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
	  } 
	  inputs 
	} 

	shinyValue <- function(id, len) { 
	  unlist(lapply(seq_len(len), function(i) { 
		value = input[[paste0(id, i)]] 
		if (is.null(value)) NA else value 
	  })) 
	} 

	## Make list of selectInputs for covariate selection at each level.
	MakeCovarSelection <- function(level, isTerminal = F){
		if(level < 2){stop("Do not apply this function to level 1!")}
		# print(paste("Making covariate selection UI for level = ", level))
		
		## Outline covariate choices for higher-level models.
		if(isTerminal){
			choices <- c("Intercept", paste0(rep(names(rVals$DT), each = 2), c("__uncentered", "__grand_centered")))
		} else{
			choices <- c("Intercept", paste0(rep(names(rVals$DT), each = 3), c("__uncentered", "__group_centered", "__grand_centered")))
		}

		output[[paste0("level_",level,"_covariates")]] <- renderUI({

			## If level > 2, we store each coefficient's model separately, so need to aggregate all level-k coefficients separately.
			if(level > 2){
				covariate_names <- grep(paste0("level_", level-1, "_var_"), names(input), value = T)
				covariate_names <- as.character(sapply(covariate_names, function(x){input[[x]]}))
			## If level == 1, all coefficients are stored in one input widget.
			} else{
				covariate_names <- input[[paste0("level_",level-1, "_vars")]]
			}

			if("Intercept" %in% covariate_names){
				l1 <- lapply(c(1), function(x){

						interceptLabel <- letters[9:(9 + rVals$k - 1)]
						interceptLabel[1:(level-1)] <- "0"
						interceptLabel <- paste0(interceptLabel, collapse = "")

						selectizeInput(paste0("level_", level, "_var_Intercept")
							,label = HTML(paste0("&beta;-", interceptLabel, " covariates:"))
							,choices = choices
							,selected = "Intercept"
							,multiple = T)
						})
			} else{
				l1 <- NULL
			}
			nonIntercept_vars <- setdiff(covariate_names, "Intercept")
			if(length(nonIntercept_vars) > 0){
				l2 <- lapply(1:length(nonIntercept_vars), function(x){

						coefLabel <- letters[9:(9 + rVals$k - 1)]
						coefLabel[1:(level-1)] <- x
						coefLabel <- paste0(coefLabel, collapse = "")
						
						selectizeInput(paste0("level_",level,"_var_", nonIntercept_vars[x]) ## Each level-k (k>1) covariate stored separately
							,label = HTML(paste0("&beta;-", coefLabel, " covariates:"))		## as "level_k_var_[name]"
							,choices = choices
							,selected = "Intercept"
							,multiple = T)
						})
			} else{
				l2 <- NULL
			}

			l <- append(l1, l2)
			l
		})
	}

	## Make list of checkboxInputs for random effect selection
	MakeRandomEffectSelection <- function(level){
		if(level < 2){stop("Do not apply this function to level 1!")}
		print(paste("Making random effect selection UI for level = ", level))

		output[[paste0("level_", level, "_randomEffects")]] <- renderUI({

			## If level > 2, we store each coefficient's model separately, so need to aggregate all level-k coefficients separately.
			if(level > 2){
				covariate_names <- grep(paste0("level_", level-1, "_var_"), names(input), value = T)
				covariate_names <- as.character(sapply(covariate_names, function(x){input[[x]]}))
			## If level == 1, all coefficients are stored in one input widget.
			} else{
				covariate_names <- input[[paste0("level_",level-1, "_vars")]]
			}

			if("Intercept" %in% covariate_names){
				l1 <- lapply(c(1), function(x){
						div(style="height: 65px;",
							checkboxInput(paste0("level_", level, "_randomEffect_Intercept")
								,label = HTML("Has random effect?")
								,value = T)
							)
						})
			} else{
				l1 <- NULL
			}

			nonIntercept_vars <- setdiff(covariate_names, "Intercept")
			if(length(nonIntercept_vars) > 0){
				l2 <- lapply(1:length(nonIntercept_vars), function(x){
						div(style="height: 65px;",
							checkboxInput(paste0("level_", level, "_randomEffect_", nonIntercept_vars[x])
								,label = HTML(paste0("Has random effect?"))
								,value = T)
							)
						})
			} else{
				l2 <- NULL
			}

			l <- append(l1, l2)
			l
		})
	}

	##----------------------------- FILE/DATA ----------------------------##
	
	## LOAD IN DATA FILE.
	observeEvent(input$upload_data, {

		rVals$k <- input$hlm_k
		print(paste0("k = ", rVals$k))

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
							choices = c("Intercept"
											,paste0(rep(setdiff(names(rVals$DT), input$outcome_var), each = 3)
													, c("__uncentered", "__group_centered", "__grand_centered"))
										), ## Disallow user from picking outcome variable as a predictor.
							selected = "Intercept",
							multiple = T)
		})
	})

	obs <- observeEvent(input$level_1_vars, {

		## LEVEL k COVARIATE SELECTION, RANDOM EFFECT CHOICE
		if(input$hlm_k >= 2){
			for(k in 2:input$hlm_k){
				if(k > 2){
					output[[paste0("level_", k, "_header")]] <- renderUI(HTML(paste("<h3> Level", k, "Model Selection </h3>")))
				}

				MakeCovarSelection(level = k, k==input$hlm_k)
				MakeRandomEffectSelection(level = k)
			}
		}

	})

	##----------------- GATHER MODEL SPECIFICATIONS BY LEVEL  ---------------##
	# GatherCovarsHLM <- eventReactive(input$run_button, {
	GatherModelsHLM <- reactive({

		if( !is.null(input$level_1_vars) ){

			## GATHER COVARIATES AND CENTERING STATUS FOR FIRST LEVEL MODEL.
			l <- list(); for(i in 1:input$hlm_k){l[[i]] <- list()}

			l[[1]][["covariates"]] <- input$level_1_vars
			l[[1]][["centering"]] <- vector(mode = "character", length = length(input$level_1_vars))

			for(i in 1:length(input$level_1_vars)){
				single_covariate <- input$level_1_vars[[i]]

				if(grepl("Intercept", single_covariate)){
					l[[1]][["centering"]][i] <- NA
				}
				if(grepl("__uncentered", single_covariate)){
					var__centering <- strsplit(single_covariate, "__")[[1]]
					l[[1]][["covariates"]][i] <- var__centering[1]
					l[[1]][["centering"]][i] <- var__centering[2]
				}
				if(grepl("__grand_centered", single_covariate)){
					var__centering <- strsplit(single_covariate, "__")[[1]]
					l[[1]][["covariates"]][i] <- var__centering[1]
					l[[1]][["centering"]][i] <- var__centering[2]
				}
				if(grepl("__group_centered", single_covariate)){
					var__centering <- strsplit(single_covariate, "__")[[1]]
					l[[1]][["covariates"]][i] <- var__centering[1]
					l[[1]][["centering"]][i] <- var__centering[2]
				}
			}

			## GATHER NAMES OF MODELING COVARIATES, CENTERING, AND RANDOM EFFECT STATUS FOR ALL
			## COEFFICIENTS BEING MODELED IN LEVEL K-1.
			for(i in 2:input$hlm_k){
				all_level_i_vars <- grep(paste0("^level_", i, "_var_"), names(input), value = T)

				if( length(all_level_i_vars) > 0 ){
					
					all_level_i_randomEffects <- grep(paste0("level_", i, "_randomEffect_"), names(input), value = T)
					
					for(ii in 1:length(all_level_i_vars)){
						this_coefficient <- all_level_i_vars[ii] 		   ## These are the covariates for modeling a level-1 covariate.
						this_randomEffect <- all_level_i_randomEffects[ii] ## Need to robustify to make this is ordered same was as all_level_i_vars...!

						l[[i]][[this_coefficient]] <- list() ## Make all level_j elements a list to store "covariates" vector for modeling the coefficient, "centering" vector, and "randomEffects" vector.

						this_coefficients_covariates <- input[[this_coefficient]]
						this_coefficients_randomEffect <- input[[this_randomEffect]]

						# var__centering <- strsplit(this_coefficient, "__")[[1]] ## (level_j-1) covariates are labeled with centering status, remove it.
						# this_coefficient <- var__centering[1]

						l[[i]][[this_coefficient]][["randomEffects"]] <- this_coefficients_randomEffect ## User may specify only a random effect and no coefficients.

						if(length(this_coefficients_covariates) > 0){

							l[[i]][[this_coefficient]][["covariates"]] <- as.character(sapply(this_coefficients_covariates, function(x){strsplit(x, "__")[[1]][1]}))
							l[[i]][[this_coefficient]][["centering"]] <- vector(mode = "character", length = length(this_coefficients_covariates))
							
							for(iii in 1:length(this_coefficients_covariates)){
								single_coefficient_covariate <- this_coefficients_covariates[iii]

								if(grepl("Intercept", single_coefficient_covariate)){
									l[[i]][[this_coefficient]][["centering"]][iii] <- NA
								}
								if(grepl("__uncentered", single_coefficient_covariate)){
									l[[i]][[this_coefficient]][["centering"]][iii] <- "uncentered"
								}
								if(grepl("__grand_centered", single_coefficient_covariate)){
									l[[i]][[this_coefficient]][["centering"]][iii] <- "grand_centered"
								}
								if(grepl("__group_centered", single_coefficient_covariate)){
									l[[i]][[this_coefficient]][["centering"]][iii] <- "group_centered"
								}
							}
						}
					}
				}
			}
			return(l)
		}
	})

	## RENDER MIXED/COMBINED MODEL
	obs <- observe({

		l <- GatherModelsHLM()
		print("l:")
		print(l)

		outcome <- NULL
		if(!is.null(input$outcome_var)){
			outcome <- paste0(input$outcome_var, "_{", paste0(letters[9:(9 + rVals$k - 1)], collapse = ""), "} ")
		}

		## TODO: (1) Display individual-level models using a different greek letter per level.
		##		 (2) Create Mixed Model string, format into variable named RHS (right hand side of equation)
		##		 (3) Create LME4 formula
		# RHS <- CreateRHS(l, level = input$hlm_k)
		# LME4_FORMULA <- CreateLME4Formula(l, level = input$hlm_k)

		intercept <- NULL
		if("Intercept" %in% input$level_1_vars){
			intercept <- "\\beta_{0j}"
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