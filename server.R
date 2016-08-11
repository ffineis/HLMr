setwd("~/NU_scripts/HLM_shiny/")
library(shiny)
library(shinyBS)
library(foreign)
library(data.table)
library(DT)

## TODO:
##  - change MakeCovarSelection to generate UI widgets with inputId's that include the greek, subscripted coefficient
##    of the coefficient in the level prior that's being modeled. This way, we can label the coefficients correctly in
##    an arbitrary level.
##  - change MakeCovarSelection to run a for loop, generating all coefficient UI for levels j+1 -> k, if a level-j model
##    was altered. The main idea going forward is that user will click "update level j" and then an event observer will trigger,
##    making MakeCovarSelection run (this will reset all of levels j+1 -> k UI and create appropriate UI based on user selection),
##    making GatherModelsHLM run, and making WriteLatexModel run for all levels j -> k.
##  - Make observeEvent observers for each of update_level_* actionButtons.
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

	## Make list of selectInputs for covariate selection at each level.
	## NEW STRATEGY: WAIT FOR USER TO SPECIFY ONE LEVEL AT A TIME:
	## Select level-1 model. Render model list, level-1 model.
	## Have user click conditional panel to move on to level-2 model, repeat. Etc.
	MakeCovarSelection <- function(level, isTerminal = F){
		if(level < 2){stop("Do not apply this function to level 1!")}
		print(paste0("Making covariate selection UI for level = ", level))
		
		## Outline covariate choices for higher-level models.
		if(isTerminal){
			choices <- c("Intercept", paste0(rep(names(isolate(rVals$DT)), each = 2), c("__uncentered", "__grand_centered")))
		} else{
			choices <- c("Intercept", paste0(rep(names(isolate(rVals$DT)), each = 3), c("__uncentered", "__group_centered", "__grand_centered")))
		}

		output[[paste0("level_",level,"_covariates")]] <- renderUI({

			## If level > 2, we store each coefficient's model separately, so need to aggregate all level-k coefficients separately.
			if(level > 2){
				covariate_names <- grep(paste0("level_", level-1, "_model_"), names(input), value = T)
				covariate_names <- as.character(sapply(covariate_names, function(x){isolate(input[[x]])}))
			} else if (level == 2){
				covariate_names <- isolate(input[[paste0("level_",level-1, "_model")]]) ## level_1_model contains names of level-1 regressors.
			}

			if("Intercept" %in% covariate_names){
				l1 <- lapply(c(1), function(x){

						interceptLabel <- letters[9:(9 + isolate(input$hlm_k) - 1)]
						interceptLabel[1:(level-1)] <- "0"
						interceptLabel <- paste0(interceptLabel, collapse = "")

						selectizeInput(paste0("level_", level, "_model_Intercept")	## Potentially not unique.
							,label = HTML(paste0("&", GREEKLETTERS[(level-1)], ";-", interceptLabel, " covariates:"))
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

						coefLabel <- letters[9:(9 + isolate(input$hlm_k) - 1)]
						coefLabel[1:(level-1)] <- x
						coefLabel <- paste0(coefLabel, collapse = "")
						
						selectizeInput(paste0("level_",level,"_model_", nonIntercept_vars[x]) 	## Potentially not a unique UI id.
							,label = HTML(paste0("&", GREEKLETTERS[(level-1)], ";-", coefLabel, " covariates:"))
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

	## Make list of checkboxInputs for random effect selection.
	MakeRandomEffectSelection <- function(level){
		if(level < 2){stop("Do not apply this function to level 1!")}
		# print(paste("Making random effect selection UI for level = ", level))

		output[[paste0("level_", level, "_randomEffects")]] <- renderUI({

			## If level > 2, we store each coefficient's model separately, so need to aggregate all level-k coefficients separately.
			if(level > 2){
				covariate_names <- grep(paste0("level_", level-1, "_model_"), names(isolate(input)), value = T)
				covariate_names <- as.character(sapply(covariate_names, function(x){isolate(input[[x]])}))
			## If level == 1, all coefficients are stored in one input widget.
			} else{
				covariate_names <- isolate(input[[paste0("level_",level-1, "_model")]])
			}

			if(length(covariate_names) > 0){
				l <- lapply(1:length(covariate_names), function(x){
						div(style="height: 65px;",
							checkboxInput(paste0("level_", level, "_randomEffect_", covariate_names[x])
								,label = HTML(paste0("Has random effect?"))
								,value = T)
							)
						})
			} else{
				l <- NULL
			}

			l
		})
	}

	## Render the Latex strings representing each level's coefficient models.
	WriteLevelLatex <- function(l, level, k){
		texModelsList <- list()
		if(level == 1){
			outcomeSubscript <- paste0("_{", paste0(letters[9:(9 + k - 1)], collapse = ""), "}")
			outcome <- paste0(input$outcome_var, outcomeSubscript)
			tex <- paste0(outcome, " = ")
			nModels <- 1
		} else{
			nModels <- length(l[[level]])
		}
		for(i in 1:nModels){
			if(level == 1){
				model <- l[[level]]
				ranef <- TRUE
			} else{
				model <- l[[level]][[i]]
			}
			if(level > 1){
				greekOutcome <- GREEKLETTERS[(level-1)]		## greek letter of coefficient we're modeling (level-1)
				outcomeSubscript <- strsplit(names(l[[level]])[i], greekOutcome)[[1]][2] ## models are labed by coefficients being modeled, grab coefficient
				outcome <- paste0("\\", greekOutcome, outcomeSubscript)
				tex <- paste0(outcome, " = ")
				ranef <- model$randomEffects
			}

			numCovar <- length(model$covariates)
			for(j in 1:numCovar){

				greekCoef <- paste0("\\", model$label[j])
				covariate <- model$covariates[j]
				centering <- model$centering[j]
				
				sep <- ifelse(j == numCovar, "", " + ")

				if(!is.na(centering)){
					if(centering == "grand_centered"){
						covariate <- paste0("(", covariate, " - \\bar{", covariate, "}", "_{", paste0(rep("\\cdot", (k-(level-1))), collapse = ""), "})")
					} else if(centering == "group_centered"){
						covariate <- paste0("(", covariate, " - \\bar{", covariate, "}", "_{\\cdot", paste0(letters[(9 + level):(9 + k - 1)], collapse = ""), "})")	
					} else {
						covariate <- paste0("(", covariate, ")")
					}
					
					tex <- paste0(tex, paste0(greekCoef, covariate, collapse = ""), sep = sep) ## non-intercept covariate case.
				} else{
					tex <- paste0(tex, greekCoef, sep = sep) ## Intercept case, just append greek coefficient.
				}
			}
			## If model has random effect specified, append it.
			if(ranef){
				tex <- paste0(tex, " + ", paste0(RESIDUALLETTERS[level], outcomeSubscript, collapse = ""))
			}

			texModelsList[[i]] <- paste0("$$", tex, "$$")
		}
		output[[paste0("level_", level, "_tex")]] <- renderUI({
			lapply(texModelsList, function(x){
				div(style="height: 65px;",
					withMathJax(x)
					)
				})
		})
		return(texModelsList)
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

	## MAKE LEVEL-1 UI.
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
	obs <- observeEvent(input$level_1_model, { ## REPLACE THIS WITH update_level_* event observer!!!!
		## LEVEL k COVARIATE SELECTION, RANDOM EFFECT CHOICE
		if(input$hlm_k >= 2){
			for(k in 2:input$hlm_k){
				MakeCovarSelection(level = k, k==input$hlm_k) 	## Build UI to select level-k model.
				MakeRandomEffectSelection(level = k)			## Build UI for toggling random effects for level-k model coefficients.
			}
		}
	})

	##----------------- GATHER MODEL SPECIFICATIONS BY LEVEL  ---------------##
	
	GatherModelsHLM <- function(){
		hlm_k <- input$hlm_k
		modelMatrixCovariates <- NULL ## This vector to hold names of features to pull from grand set of all available features.

		## GATHER COVARIATES AND CENTERING STATUS FOR FIRST (1st) LEVEL MODEL.
		l <- list(); for(i in c(1:hlm_k)){l[[i]] <- list()}

		l[[1]][["covariates"]] <- input$level_1_model
		l[[1]][["centering"]] <- vector(mode = "character", length = length(input$level_1_model))
		l[[1]][["label"]] <- vector(mode = "character", length = length(input$level_1_model))

		modelLetter <- GREEKLETTERS[1]
		coefLetters <- paste0(letters[(9+1):(9+hlm_k-1)], collapse = "")
		shift <- ifelse("Intercept" %in% l[[1]][["covariates"]], TRUE, FALSE)	## shift will index coefficient subscripts depending on whether
																				## we're modeling an intercept or not.

		for(i in 1:length(l[[1]][["covariates"]])){
			coefficientCovariate <- input$level_1_model[[i]]
			l[[1]][["label"]][i] <- NA


			if(grepl("Intercept", coefficientCovariate)){
				l[[1]][["centering"]][i] <- NA
				l[[1]][["label"]][i] <- paste0(modelLetter, "_{0", coefLetters, "}")
				modelMatrixCovariates <- c(modelMatrixCovariates, "Intercept")
			} else{
				var__centering <- strsplit(coefficientCovariate, "__")[[1]]
			}
			if(grepl("__uncentered", coefficientCovariate)){
				l[[1]][["covariates"]][i] <- var__centering[1]
				l[[1]][["centering"]][i] <- var__centering[2]
			}
			if(grepl("__grand_centered", coefficientCovariate)){
				l[[1]][["covariates"]][i] <- var__centering[1]
				l[[1]][["centering"]][i] <- var__centering[2]
			}
			if(grepl("__group_centered", coefficientCovariate)){
				l[[1]][["covariates"]][i] <- var__centering[1]
				l[[1]][["centering"]][i] <- var__centering[2]
			}
			if(is.na(l[[1]][["label"]][i])){
				l[[1]][["label"]][i] <- paste0(modelLetter, "_{", ifelse(shift, (i-1), i), coefLetters, "}")
				modelMatrixCovariates <- c(modelMatrixCovariates, coefficientCovariate)
			}
		}

		## GATHER COVARIATES AND CENTERING STATUS FOR SECOND (2nd) LEVEL MODELS.
		for(i in c(1:length(l[[1]]$covariates))){
			greekLabel <- l[[1]]$label[i]									## e.g. beta_{0jk}, or beta_{1jk}
			subscript <- strsplit(greekLabel, split = "\\{")[[1]][2]		## Coefficient subscript

			modelName <- paste0("level_2_model_", greekLabel) 	## Label this model by regression coefficient.
			l[[2]][[modelName]] <- list()

			## Gather random effects for level 2 coefficients.
			randomEffects <- grep(paste0("level_2_randomEffect_", l[[1]]$covariates[i]), names(input), value = T) ## level_2_randomEffect_[covariate + centering label]
			coefficientCovariates <- grep(paste0("level_2_model_", l[[1]]$covariates[i]), names(input), value = T)

			if(length(randomEffects) > 0 & length(coefficientCovariates) > 0){
				coefficientCovariates <- input[[coefficientCovariates]]
				l[[2]][[modelName]]$randomEffects <- input[[randomEffects]] ## e.g. level_2_model_beta_{0j}$randomEffects <- input$level_2_randomEffect_Intercept

				## Model, label, and describe the centering of the covariates of each of the level-1 coefficients:
				l[[2]][[modelName]]$covariates <- vector(mode = "character", length = length(coefficientCovariates))
				l[[2]][[modelName]]$label <- l[[2]][[modelName]]$covariates
				l[[2]][[modelName]]$centering <- l[[2]][[modelName]]$covariates
				shift <- ifelse("Intercept" %in% coefficientCovariates, TRUE, FALSE)

				## Go through this coefficient's model's coefficients.
				for(j in c(1:length(coefficientCovariates))){
					coefficientCovariate <- coefficientCovariates[j]

					var__centering <- strsplit(coefficientCovariate, "__")[[1]]
					l[[2]][[modelName]]$covariates[j] <- var__centering[1]

					## Label coefficient covariates with greek letters with appropriate subscripts.
					if(coefficientCovariate == "Intercept"){
						l[[2]][[modelName]]$centering[j] <- NA
						substr(subscript, 2, 2) <- "0"
					} else{
						substr(subscript, 2, 2) <- as.character(ifelse(shift, (j-1), j))
						if(grepl("__uncentered", coefficientCovariate)){
							l[[2]][[modelName]]$centering[j] <- var__centering[2]
						}
						if(grepl("__grand_centered", coefficientCovariate)){
							l[[2]][[modelName]]$centering[j] <- var__centering[2]
						}
						if(grepl("__group_centered", coefficientCovariate)){
							l[[2]][[modelName]]$centering[j] <- var__centering[2]
						}
						modelMatrixCovariates <- c(modelMatrixCovariates, coefficientCovariate)
					}
					l[[2]][[modelName]]$label[j] <- paste0(GREEKLETTERS[2], "_{", subscript) ## beta_{0jk} -> beta_{00k}
				}
			}
		}

		## GATHER COVARIATES AND CENTERING STATUS FOR (k > 2)-LEVEL MODELS BASED ON (k-1)-LEVEL MODELS.
		if(hlm_k >= 3){
			for(i in c(3:hlm_k)){														## i == level we're gathering data on.
				for(j in c(1:length(l[[(i-1)]]))){										## Iterate over different coefficients to model in level (i-1)
					for(k in c(1:length(l[[(i-1)]][[j]]$covariates))){						## Iterate over different coefficients in 

						if(j > 1){browser()}
						greekLabel <- l[[(i-1)]][[j]]$label[k]							## Greek label of coefficient in level (i-1) beta_{0jk}, or beta_{1jk}
						subscript <- strsplit(greekLabel, split = "\\{")[[1]][2]		## Coefficient subscript

						modelName <- paste0("level_", i, "_model_", greekLabel)			## Name for model for particular coefficient in level (i-1) model.
						l[[i]][[modelName]] <- list()

						coefficientCovariates <- grep(paste0("level_", (i-1), "_model_", l[[(i-1)]][[j]]$covariates[k]), names(input), value = T)
						randomEffects <- grep(paste0("level_", i, "_randomEffect_", l[[(i-1)]][[j]]$covariates[k]), names(input), value = T) ## level_2_randomEffect_[covariate + centering label]
						
						if(length(randomEffects) > 0 & length(coefficientCovariates) > 0){ ## Causes bugs when actual length is already 1, but user changes number models to > 1.
							l[[i]][[modelName]]$randomEffects <- input[[randomEffects]]

							## Model, label, and describe the centering of the covariates of each of the level-1 coefficients:
							coefficientCovariates <- input[[coefficientCovariates]]
							l[[i]][[modelName]]$covariates <- vector(mode = "character", length = length(coefficientCovariates))
							l[[i]][[modelName]]$label <- l[[2]][[modelName]]$covariates
							l[[i]][[modelName]]$centering <- l[[2]][[modelName]]$covariates
							shift <- ifelse("Intercept" %in% coefficientCovariates, TRUE, FALSE)

							## Go through this coefficient's model's coefficients.
							for(m in c(1:length(coefficientCovariates))){
								coefficientCovariate <- coefficientCovariates[m]

								var__centering <- strsplit(coefficientCovariate, "__")[[1]]
								l[[i]][[modelName]]$covariates[m] <- var__centering[1]

								## Label coefficient covariates with greek letters with appropriate subscripts.
								if(coefficientCovariate == "Intercept"){
									l[[i]][[modelName]]$centering[m] <- NA
									substr(subscript, 2, 2) <- "0"
								} else{
									substr(subscript, 2, 2) <- as.character(ifelse(shift, (m-1), j))
									if(grepl("__uncentered", coefficientCovariate)){
										l[[i]][[modelName]]$centering[m] <- var__centering[2]
									}
									if(grepl("__grand_centered", coefficientCovariate)){
										l[[i]][[modelName]]$centering[m] <- var__centering[2]
									}
									if(grepl("__group_centered", coefficientCovariate)){
										l[[i]][[modelName]]$centering[m] <- var__centering[2]
									}
									modelMatrixCovariates <- c(modelMatrixCovariates, coefficientCovariate)
								}
								l[[i]][[modelName]]$label[m] <- paste0(GREEKLETTERS[i], "_{", subscript) ## beta_{0jk} -> beta_{00k}
							}
						}
					}
				}
			}
		}
		# print("Finished assembling GatherModelsHLM list.")
		return(list(l = l, modelMatrixCovariates = unique(modelMatrixCovariates)))
	}

	##--------------------- RENDER MIXED/COMBINED MODEL  ------------------##

	obs <- observe({

	})
})