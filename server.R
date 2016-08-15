setwd("~/NU_scripts/HLM_shiny/")
library(shiny)
library(shinyBS)
library(foreign)
library(data.table)
library(DT)

## TODO:
##  - Update GatherModelsHLM and WriteLevelLatex to reflect new list comprehensions.
##  - When user selects the "View Models In Aggregate" tabPanel run GatherModelsHLM and WriteLatexModel to gather models up on tab.
##    (see http://shiny.rstudio.com/articles/action-buttons.html for reference on how to use tabsetPanel change as action button.)


shinyServer(function(input, output, session) {

	##------------------ INITIALIZE USEFUL STORAGE/FUNS -----------------##

	GREEKLETTERS <- c("beta", "gamma", "delta", "xi", "pi", "phi")
	RESIDUALLETTERS <- c("e", "r", "u", "f") ## TODO: find out letters typically used to denote random effects coefficient models for level > 3.
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

	## Make list of selectInputs and checkboxInputs for covariate & random effect selection at each level.
	## Have user click conditional panel to move on to level-2 model, repeat. Etc.
	MakeLevelUI <- function(level, isTerminal = F){
		
		if(level < 2){stop("Do not apply this function to a level 1 model!")}
		print(paste0("Making covariate selection UI for level = ", level))
		hlm_k <- input$hlm_k
		modelLetter <- GREEKLETTERS[(level-1)]										## Label coefficient model with level-1 greek letter.
		
		## Outline covariate choices for higher-level models.
		if(isTerminal){
			choices <- c("Intercept", paste0(rep(names(isolate(rVals$DT)), each = 2), c("__uncentered", "__grand_centered")))
		} else{
			choices <- c("Intercept", paste0(rep(names(isolate(rVals$DT)), each = 3), c("__uncentered", "__group_centered", "__grand_centered")))
		}

		## Create random effects UI
		output[[paste0("level_", level, "_randomEffects")]] <- renderUI({
		## If level > 2, we store each coefficient's model separately, so need to aggregate all level-k coefficients separately.
			if(level == 2){
				covariateNames <- input[[paste0("level_",level-1, "_model")]] 				## level_1_model contains names of level-1 regressors.
				nonIntercept_vars <- setdiff(covariateNames, "Intercept")
				coefSubcript <- paste0(letters[(9+(level-1)):(9+hlm_k-1)], collapse = "")	## Subscript will contain at least (k-level) letters.
				shift <- "Intercept" %in% covariateNames

				if(shift){
					l1 <- lapply(c(1), function(x){

							coefSubscript <- paste0(0, coefSubcript, collapse = "")
							coef <- paste0(modelLetter, "_{", coefSubscript, "}")
							coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", coefSubscript, "</sub> has random effect?", collapse = ""))

							div(style="height: 65px;",
								checkboxInput(paste0("level_", level, "_randomEffect_", coef)
									,label = coefHTML
									,value = T)
								)
						})

				} else{
					l1 <- NULL
				}

				if(length(nonIntercept_vars) > 0){
					l2 <- lapply(1:length(nonIntercept_vars), function(x){

						coefSubscript <- paste0(x, coefSubcript, collapse = "")
						coef <- paste0(modelLetter, "_{", coefSubscript, "}")
						coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", coefSubscript, "</sub> has random effect?", collapse = ""))
						
						div(style="height: 65px;",
								checkboxInput(paste0("level_", level, "_randomEffect_", coef)
									,label = coefHTML
									,value = T)
								)
					})

				} else{
					l2 <- NULL
				}
				l <- append(l1, l2)

			} 
			if(level > 2) {

				## Gather names of all input widgets corresponding to level-j coefficients.
				coefficientsToModel <- paste0("level_",level-1, "_model_")
				coefficientsToModel <- grep(coefficientsToModel, names(input), value = T)
				l <- list()

				## Iterate over all coefficients in level-j building corresponding UI for each coefficient.
				for(i in 1:length(coefficientsToModel)){

					covariateNames <- input[[coefficientsToModel[i]]]
					coefficientLabel <- strsplit(coefficientsToModel[i], "_model_")[[1]][2]		## from widget inputId gather the greek symbol + subscript part, e.g. beta_{0jk}
					subscript <- strsplit(coefficientLabel, "_\\{")[[1]][2]						## gather the "0jk}" part of "beta_{0jk}"
					nonIntercept_vars <- setdiff(covariateNames, "Intercept")
				
					shift <- "Intercept" %in% covariateNames

					if(shift){
						l1 <- lapply(c(1), function(x){
							substr(subscript, (level-1), (level-1)) <- "0"
							coef <- paste0(modelLetter, "_{", subscript)
							coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", gsub("}", "", subscript), "</sub> has random effect?", collapse = ""))

							div(style="height: 67px;",
								checkboxInput(paste0("level_", level, "_randomEffect_", coef)
									,label = coefHTML
									,value = T)
								)
						})

					} else{
						l1 <- NULL
					}

					if(length(nonIntercept_vars) > 0){
						l2 <- lapply(1:length(nonIntercept_vars), function(x){

							substr(subscript, (level-1), (level-1)) <- as.character(x)
							coef <- paste0(modelLetter, "_{", subscript)
							coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", gsub("}", "", subscript), "</sub> has random effect?", collapse = ""))
							
							div(style="height: 67px;",
								checkboxInput(paste0("level_", level, "_randomEffect_", coef)
									,label = coefHTML
									,value = T)
								)
						})

					} else{
						l2 <- NULL
					}
					## Update l for each coefficient in level j-1
					tmp <- append(l1, l2)
					l <- append(l, tmp)
				}
			}
			return(l)
		})

		## Create covariate selection UI for each coefficient to model.
		output[[paste0("level_",level,"_covariates")]] <- renderUI({

			if (level == 2){
				covariateNames <- input[[paste0("level_",level-1, "_model")]] 				## level_1_model contains names of level-1 regressors.
				nonIntercept_vars <- setdiff(covariateNames, "Intercept")
				coefSubcript <- paste0(letters[(9+(level-1)):(9+hlm_k-1)], collapse = "")	## Subscript will contain at least (k-level) letters.
				
				shift <- "Intercept" %in% covariateNames

				if(shift){
					l1 <- lapply(c(1), function(x){

						coefSubscript <- paste0(0, coefSubcript, collapse = "")
						coef <- paste0(modelLetter, "_{", coefSubscript, "}")
						coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", coefSubscript, "</sub> covariates:", collapse = ""))

						selectizeInput(paste0("level_", level, "_model_", coef)
							,label = coefHTML
							,choices = choices
							,selected = "Intercept"
							,multiple = T)
						})

				} else{
					l1 <- NULL
				}

				if(length(nonIntercept_vars) > 0){
					l2 <- lapply(1:length(nonIntercept_vars), function(x){

						coefSubscript <- paste0(x, coefSubcript, collapse = "")
						coef <- paste0(modelLetter, "_{", coefSubscript, "}")
						coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", coefSubscript, "</sub> covariates:", collapse = ""))
						
						selectizeInput(paste0("level_",level,"_model_", coef) 	## Potentially not a unique inputId when level >= 3.
							,label = coefHTML
							,choices = choices
							,selected = "Intercept"
							,multiple = T)
					})

				} else{
					l2 <- NULL
				}

				l <- append(l1, l2)

			} else{
				## Gather names of all input widgets corresponding to level-j coefficients.
				coefficientsToModel <- paste0("level_",level-1, "_model_")
				coefficientsToModel <- grep(coefficientsToModel, names(input), value = T)
				l <- list()

				## Iterate over all coefficients in level-j building corresponding UI for each coefficient.
				for(i in 1:length(coefficientsToModel)){

					covariateNames <- input[[coefficientsToModel[i]]]
					coefficientLabel <- strsplit(coefficientsToModel[i], "_model_")[[1]][2]		## from widget inputId gather the greek symbol + subscript part, e.g. beta_{0jk}
					subscript <- strsplit(coefficientLabel, "_\\{")[[1]][2]						## gather the "0jk}" part of "beta_{0jk}"
					nonIntercept_vars <- setdiff(covariateNames, "Intercept")
				
					shift <- "Intercept" %in% covariateNames

					if(shift){
						l1 <- lapply(c(1), function(x){
							substr(subscript, (level-1), (level-1)) <- "0"
							coef <- paste0(modelLetter, "_{", subscript)
							coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", gsub("}", "", subscript), "</sub> covariates:", collapse = ""))

							selectizeInput(paste0("level_", level, "_model_", coef)
								,label = coefHTML
								,choices = choices
								,selected = "Intercept"
								,multiple = T)
						})

					} else{
						l1 <- NULL
					}

					if(length(nonIntercept_vars) > 0){
						l2 <- lapply(1:length(nonIntercept_vars), function(x){

							substr(subscript, (level-1), (level-1)) <- as.character(x)
							coef <- paste0(modelLetter, "_{", subscript)
							coefHTML <- HTML(paste0("&", modelLetter, ";<sub>", gsub("}", "", subscript), "</sub> covariates:", collapse = ""))
							
							selectizeInput(paste0("level_",level,"_model_", coef) 	## Potentially not a unique inputId when level >= 3.
								,label = coefHTML
								,choices = choices
								,selected = "Intercept"
								,multiple = T)
						})

					} else{
						l2 <- NULL
					}
					## Update l for each coefficient in level j-1
					tmp <- append(l1, l2)
					l <- append(l, tmp)
				}
			}
			return(l)	## renderUI to return list (l) of HTML widgets.
		})
	}

	##----------------------------- FILE/DATA, UP-FRONT MODS TO MAKE ----------------------------##
	
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
			rVals$X <- data.table(V1 = rep(NA, nrow(dt)))
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
		if(input$hlm_k >= 2){
			lapply(2:input$hlm_k, function(i){
			  selectizeInput(inputId = paste0("level_id_", i),
							 label = paste("Level", i, "ID variable"),
							 choices = names(rVals$DT),
							 multiple = F)
			})
		}
	})

	## Update the sidebarPanel in Model Selection tabset so that we can only see relevant number of levels.
	obs <- observeEvent(input$hlm_k, {
		updateRadioButtons(session,
			,inputId = "select_level"
			,choices = c("Outcome", paste0("Level-", 1:input$hlm_k))
		)
	})

	## MAKE OUTCOME VARIABLE SELECTION UI.
	output$outcome <- renderUI({
		selectizeInput(inputId = "outcome_var",
						label = "",
						choices = names(rVals$DT),
						selected = names(rVals$DT)[1],
						multiple = F)
	})

	## MAKE LEVEL-1 MODEL SELECTION UI.
	output$select_level_1_vars <- renderUI({
		selectizeInput(inputId = "level_1_model",
						label = "Covariates",
						choices = c("Intercept"
									,paste0(rep(setdiff(names(rVals$DT), input$outcome_var), each = 3) ## Disallow user from picking outcome variable as a predictor.
									,c("__uncentered", "__group_centered", "__grand_centered"))
									),
						selected = "Intercept",
						multiple = T)
	})

	##------------------------------ SPECIFY MODEL --------------------------##
	obs <- observeEvent(input$update_level_1, { 
		## LEVEL k COVARIATE SELECTION, RANDOM EFFECT CHOICE
		if(input$hlm_k >= 2){
			MakeLevelUI(level = 2, 2==input$hlm_k) 	## Build UI to select level-k model.
			l <- GatherModelsHLM()
			# print(l$l)
			WriteLevelLatex(l$l, input$hlm_k)
		}
	})
	obs <- observeEvent(input$update_level_2, { 
		## LEVEL k COVARIATE SELECTION, RANDOM EFFECT CHOICE
		if(input$hlm_k >= 2){
			MakeLevelUI(level = 3, 3==input$hlm_k) 	## Build UI to select level-k model.
			l <- GatherModelsHLM()
			print(l$l)
			WriteLevelLatex(l$l, input$hlm_k)
		}
	})
	obs <- observeEvent(input$update_level_3, { 
		## LEVEL k COVARIATE SELECTION, RANDOM EFFECT CHOICE
		if(input$hlm_k >= 2){
			MakeLevelUI(level = 4, 4==input$hlm_k) 	## Build UI to select level-k model.
			l <- GatherModelsHLM()
			# print(l$l)
			WriteLevelLatex(l$l, input$hlm_k)
		}
	})
	# obs <- observe({print(names(input))})
	obs <- observeEvent(input$browser, {browser()})

	##----------------- GATHER MODEL SPECIFICATIONS BY LEVEL  ---------------##
	
	GatherModelsHLM <- function(){
		print("running GatherModelsHLM...")
		hlm_k <- input$hlm_k
		modelMatrixCovariates <- NULL ## This vector to hold names of features to pull from grand set of all available features.

		## GATHER COVARIATES AND CENTERING STATUS FOR FIRST (1st) LEVEL MODEL.
		l <- list(); for(i in c(1:hlm_k)){l[[i]] <- list()}

		l[[1]][["covariates"]] <- input$level_1_model
		l[[1]][["centering"]] <- vector(mode = "character", length = length(input$level_1_model))
		l[[1]][["label"]] <- vector(mode = "character", length = length(input$level_1_model))
		l[[1]][["outcome"]] <- paste0(input$outcome_var, "_{", paste0(letters[9:(9+hlm_k-1)], collapse = ""), "}")

		coefLetters <- paste0(letters[(9+1):(9+hlm_k-1)], collapse = "")
		shift <- "Intercept" %in% l[[1]][["covariates"]]	## shift will index coefficient subscripts depending on whether
																				## we're modeling an intercept or not.

		for(i in 1:length(l[[1]][["covariates"]])){
			coefficientCovariate <- input$level_1_model[[i]]
			l[[1]][["label"]][i] <- NA

			if(grepl("Intercept", coefficientCovariate)){
				l[[1]][["centering"]][i] <- NA
				l[[1]][["covariates"]][i] <- "Intercept"
				l[[1]][["label"]][i] <- paste0(GREEKLETTERS[1], "_{0", coefLetters, "}")
				modelMatrixCovariates <- c(modelMatrixCovariates, "Intercept")
			} else{
				var__centering <- strsplit(coefficientCovariate, "__")[[1]]
				l[[1]][["covariates"]][i] <- var__centering[1]
				l[[1]][["centering"]][i] <- var__centering[2]
			}
			if(is.na(l[[1]][["label"]][i])){
				l[[1]][["label"]][i] <- paste0(GREEKLETTERS[1], "_{", ifelse(shift, (i-1), i), coefLetters, "}")
				modelMatrixCovariates <- c(modelMatrixCovariates, coefficientCovariate)
			}
		}

		for(level in 2:hlm_k){
		
			if(level == 2){
				numModels <- length(l[[1]]$covariates)
			} else{
				# pattern <- paste0("level_", (level-1), "_model_beta_\\{[0-9]{", (level-1), "}[jklmn]\\}")
				pattern <- paste0("level_", level, "_model")
				level_j_models <- grep(pattern, names(input), value = T)
				numModels <- length(level_j_models)
			}

			if(numModels > 0){
				for(i in 1:numModels){
					
					if(level == 2){
						outcome <- l[[1]]$label[i]
						modelName <- paste0("level_2_model_", outcome)
					}

					## outcome is the coefficient being modeled.
					if(level > 2){
						modelName <- level_j_models[i]
						outcome <- strsplit(modelName, paste0("level_", level, "_model_"))[[1]][2] ## outcome is "greek letter + _{ subcript }"
					}

					l[[level]][[modelName]] <- list()
					l[[level]][[modelName]]$outcome <- outcome
					coefficientLetter <- GREEKLETTERS[level]
					coefficientSubscript <- strsplit(outcome, "_\\{")[[1]][2]
          
         			randomEffectName <- paste0("level_", level, "_randomEffect_", gsub("\\}", "\\\\}", gsub("\\{", "\\\\{", outcome)))
          			UIHasBeenRendered <- length(grep(randomEffectName, names(input))) > 0

          			if(UIHasBeenRendered){
            			l[[level]][[modelName]]$randomEffect <- input[[grep(randomEffectName, names(input), value = T)]]

  						## Gather up the user's choice 
  						coefficientCovariates <- input[[modelName]]
  						shift <- "Intercept" %in% coefficientCovariates
  
  						if(shift){
  							coefficientCovariates <- c("Intercept", setdiff(coefficientCovariates, "Intercept"))
  						}
  						numCovar <- length(coefficientCovariates)
  
		  				l[[level]][[modelName]]$covariates <- vector(mode = "character", length = numCovar)
		  				l[[level]][[modelName]]$label <- vector(mode = "character", length = numCovar)
		  				l[[level]][[modelName]]$centering <- vector(mode = "character", length = numCovar)
  
							for(j in 1:numCovar){
								coefficientCovariate <- coefficientCovariates[j]

  							var__centering <- strsplit(coefficientCovariate, "__")[[1]]
  							l[[level]][[modelName]]$covariates[j] <- var__centering[1]

								## Label coefficient covariates with greek letters with appropriate subscripts.
								if(coefficientCovariate == "Intercept"){
									l[[level]][[modelName]]$centering[j] <- NA
									substr(coefficientSubscript, level, level) <- "0"
								} else{
									l[[level]][[modelName]]$centering[j] <- var__centering[2]
									substr(coefficientSubscript, level, level) <- as.character(ifelse(shift, (j-1), j))
								}	
								l[[level]][[modelName]][["label"]][j] <- paste0(coefficientLetter, "_{", coefficientSubscript)
								modelMatrixCovariates <- c(modelMatrixCovariates, coefficientCovariate)					
							}
					} else{
						## Remove current and higher up levels from our aggregation list because UI hasn't been rendered yet (user hasn't defined models yet)
						while(length(l) >= level){
							l[[length(l)]] <- NULL
						}
						break
					}
				}
			} else{
				while(length(l) >= level){
					l[[length(l)]] <- NULL
				}
				break
			}
		}
		print("Finished assembling GatherModelsHLM list.")
		return(list(l = l, modelMatrixCovariates = unique(modelMatrixCovariates)))
	}

	## Render the Latex strings representing each level's coefficient models.
	WriteLevelLatex <- function(l, k){
		numLevelsToRender <- length(l)

		# if(any(unlist(lapply(l, length)) == 0)){browser()}

		texModelsList <- list()
		
		for(level in 1:numLevelsToRender){
			texModelsList[[level]] <- list()

			numEquations <- ifelse(level == 1, 1, length(l[[level]]))

			for(i in 1:numEquations){
				if(level == 1){
					model <- l[[level]]
					ranef <- TRUE
				} else{
					model <- l[[level]][[i]]
					ranef <- model$randomEffect
				}

				outcome <- ifelse(level == 1, model$outcome, paste0("\\", model$outcome))
				tex <- paste0(outcome, " = ")

				numCovar <- length(model$covariates)
				for(j in 1:numCovar){
					covariate <- model$covariates[j]
					centering <- model$centering[j]
					coef <- model$label[j]

					sep <- ifelse(j == numCovar, "", " + ")
					
					if(!is.na(centering)){
						if(centering == "grand_centered"){
							covariate <- paste0("(", covariate, " - \\bar{", covariate, "}", "_{", paste0(rep("\\cdot", (k-(level-1))), collapse = ""), "})")
						} else if(centering == "group_centered"){
							covariate <- paste0("(", covariate, " - \\bar{", covariate, "}", "_{\\cdot ", paste0(letters[(9 + level):(9 + k - 1)], collapse = ""), "})")	
						} else{
							covariate <- paste0("(", covariate, ")")
						}
						tex <- paste0(tex, paste0("\\", coef, covariate, collapse = ""), sep = sep) ## non-intercept covariate case.
					} else{
						tex <- paste0(tex, paste0("\\", coef), sep = sep) ## Intercept case, just append greek coefficient.
					}
				}
				if(ranef){
					errSubscript <- strsplit(outcome, "_")[[1]][2]
					tex <- paste0(tex, " + ", paste0(RESIDUALLETTERS[level], "_", errSubscript))
				}
				texModelsList[[level]][[i]] <- paste0("$$", tex, "$$")
			}
		}
		print(texModelsList)
		for(i in 1:numLevelsToRender){
			equations <- texModelsList[[i]]
			output[[paste0("level_", i, "_tex")]] <- renderUI({
				lapply(equations, function(x){
					div(style="height: 65px;",
						withMathJax(x)
					)
				})
			})
		}
	}

})