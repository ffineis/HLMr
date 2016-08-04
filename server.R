library(shiny)
library(shinyBS)
library(foreign)
library(data.table)
library(DT)

## TODO:
## - Fix GatherModelsHLM
## - Write function to call within GatherModelsHLM that will take labels and random effect status and
##		format Latex equations.
## - Make UI for level k dependent on that of level k-1

## PROBLEMS:
## (1) Dynamic creation of UI coupled with the observer calling GatherModelsHLM() leads to timing issues --
##		e.g. the 1st order covariates change, but before the UI can be rendered for the newly required >1-level model specifications,
##		GatherModelsHLM is already asking for input$level_2_model_beta_{10} which technically hasn't been rendered yet.

## POSSIBLE SOLUTION:
## (1) Instead of calling MakeCovarSelection and MakeRandomEffectSelection in a for loop within an event observer,
##		try moving the for loop within the functions themselves.



shinyServer(function(input, output, session) {

	##------------------ INITIALIZE USEFUL STORAGE/FUNS -----------------##

	GREEKLETTERS <- c("beta", "gamma", "delta", "xi", "pi", "phi")
	RESIDUALLETTERS <- c("e", "r", "u", "f") ## TODO: find out letters typically used to denote random effects coefficient models for level > 3.
	rVals <- reactiveValues(DT = NULL)
	# updateHLMList <- reactiveValues(on = TRUE)

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
			choices <- c("Intercept", paste0(rep(names(isolate(rVals$DT)), each = 2), c("__uncentered", "__grand_centered")))
		} else{
			choices <- c("Intercept", paste0(rep(names(isolate(rVals$DT)), each = 3), c("__uncentered", "__group_centered", "__grand_centered")))
		}

		output[[paste0("level_",level,"_covariates")]] <- renderUI({

			## If level > 2, we store each coefficient's model separately, so need to aggregate all level-k coefficients separately.
			if(level > 2){
				covariate_names <- grep(paste0("level_", level-1, "_model_"), names(input), value = T)
				covariate_names <- as.character(sapply(covariate_names, function(x){isolate(input[[x]])}))
			## If level == 1, all coefficients are stored in one input widget.
			} else{
				covariate_names <- isolate(input[[paste0("level_",level-1, "_model")]])
			}

			if("Intercept" %in% covariate_names){
				l1 <- lapply(c(1), function(x){

						interceptLabel <- letters[9:(9 + isolate(input$hlm_k) - 1)]
						interceptLabel[1:(level-1)] <- "0"
						interceptLabel <- paste0(interceptLabel, collapse = "")

						selectizeInput(paste0("level_", level, "_model_Intercept")	
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

						coefLabel <- letters[9:(9 + isolate(input$hlm_k) - 1)]
						coefLabel[1:(level-1)] <- x
						coefLabel <- paste0(coefLabel, collapse = "")
						
						selectizeInput(paste0("level_",level,"_model_", nonIntercept_vars[x])
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
		lapply(2:input$hlm_k, function(i){
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
			selectizeInput(inputId = "level_1_model",
							label = "Covariates",
							choices = c("Intercept"
										,paste0(rep(setdiff(names(rVals$DT), input$outcome_var), each = 3) ## Disallow user from picking outcome variable as a predictor.
										,c("__uncentered", "__group_centered", "__grand_centered"))
										),
							selected = "Intercept",
							multiple = T)
		})
	})

	obs <- observeEvent(input$level_1_model, {
		# updateHLMList$on <- FALSE

		## LEVEL k COVARIATE SELECTION, RANDOM EFFECT CHOICE
		if(input$hlm_k >= 2){
			for(k in 2:input$hlm_k){
				if(k > 2){
					output[[paste0("level_", k, "_header")]] <- renderUI(HTML(paste("<h3> Level", k, "Model Selection </h3>")))
				}

				MakeCovarSelection(level = k, k==input$hlm_k) 	## Build UI to select level-k model.
				MakeRandomEffectSelection(level = k)			## Build UI for toggling random effects for level-k model coefficients.
			}
		}

		# updateHLMList$on <- TRUE
	})

	##----------------- GATHER MODEL SPECIFICATIONS BY LEVEL  ---------------##
	
	GatherModelsHLM <- function(){
		hlm_k <- input$hlm_k

		## GATHER COVARIATES AND CENTERING STATUS FOR FIRST LEVEL MODEL.
		l <- list(); for(i in c(1:hlm_k)){l[[i]] <- list()}

		l[[1]][["covariates"]] <- input$level_1_model
		l[[1]][["centering"]] <- vector(mode = "character", length = length(input$level_1_model))
		l[[1]][["label"]] <- vector(mode = "character", length = length(input$level_1_model))

		modelLetter <- GREEKLETTERS[1]
		coefLetters <- paste0(letters[(9+1):(9+hlm_k-1)], collapse = "")
		shift <- ifelse("Intercept" %in% l[[1]][["covariates"]], TRUE, FALSE)

		for(i in 1:length(l[[1]][["covariates"]])){
			coefficientCovariate <- input$level_1_model[[i]]
			l[[1]][["label"]][i] <- NA

			if(grepl("Intercept", coefficientCovariate)){
				l[[1]][["centering"]][i] <- NA
				l[[1]][["label"]][i] <- paste0(modelLetter, "_{0", coefLetters, "}")
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
			}
		}

		## GATHER COVARIATES AND CENTERING STATUS FOR SECOND LEVEL MODELS.
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
					}
					l[[2]][[modelName]]$label[j] <- paste0(GREEKLETTERS[i], "_{", subscript) ## beta_{0jk} -> beta_{00k}
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
								}
								l[[i]][[modelName]]$label[m] <- paste0(GREEKLETTERS[i], "_{", subscript) ## beta_{0jk} -> beta_{00k}
							}
						}
					}
				}
			}
		}
		print("Finished assembling GatherModelsHLM list.")
		return(l)
	}

	##----------------- PRINT COEFFICIENT MODELS BY LEVEL, BUILD MODEL MATRIX (DEPRECATED, MOVE TO WITHIN GatherModelsHLM)  ---------------##

	## Print out individual models for levels 1:k.
	## EVENTUALLY... use this to build the lmer formula object to build HLM.
	# CoefficientModels(lst, hlm_k){

	# 	tmpDT <- copy(isolate(rvals$DT))

	# 	for(level in 1:hlm_k){

	# 		allSubscript <- paste0(letters[(9+level-1):(9 + hlm_k - 1)], collapse = "")
	# 		groupLettersSubscript <- paste0(letters[(9+level):(9+hlm_k-1)], collapse = "")
	# 		model_tex <- NULL

	# 		if(level == 1{

	# 			## Render tex for overall outcome variable, e.g. total.fruits from Arabidopsis dataset.
	# 			if(!is.null(input$outcome_var)){
	# 				outcome <- paste0(input$outcome_var, "_{", allSubscript, "} ")
	# 			}
	# 			## If first level, then there's just one model, the one for y_{allSubscript}
	# 			num_models <- 1
	# 		} else{
	# 			coefficientModelList <- lst[[level]]
	# 			num_models <- length(coefficientModelList)
	# 			modelNames <- names(coefficientModelList) ## Character vector of entries level_*_model_[coefficient/covariate name]
	# 			outcomeLetter <- GREEKLETTERS[(level-1)] ## Greek letter for coefficient we're modeling.
	# 		}

	# 		modelLetter <- GREEKLETTERS[level]	## Greek letter for coefficients used to model upper-level coefficient.

	# 		for(j in 1:num_models){

	# 			if(level > 1){
	# 				thisModel <- coefficientModelList[[j]]
	# 				outcome <- paste0(outcomeLetter, " = ")		## TODO: PROPER SUBSCRIPTING!
	# 			} else {
	# 				thisModel <- lst[[1]]
	# 				outcome <- paste0(input$outcome, "_{", allSubscript, "} = ") ## Sufficient for overall outcome labeling.
	# 			}
	# 			model_tex <- outcome

	# 			covariates <- thisModel$covariates
	# 			centering <- thisModel$centering
	# 			nonIntercept_vars <- setdiff(covariates, "Intercept")
				
	# 			## If applicable, render Level-1 intercept tex coefficient.
	# 			if("Intercept" %in% covariates){
	# 				intercept <- paste0(modelLetter, "_{0", groupLettersSubscript, "} ")
	# 				model_tex <- paste0(outcome, intercept)
	# 				shift <- TRUE
	# 			} else{
	# 				shift <- FALSE
	# 			}

	# 			##---- Append centered Level-1 covariates to rvals$X, the eventual model matrix. ----##
	# 			##---- Create coefficient and covariate combination for each non-intercept term of Level-1 model. ----##

	# 			## How to progress along vector of coefficients in model: need to increase index by 1 if intercept is included.
	# 			rng <- ifelse(shift, c(2:(length(nonIntercept_vars)+1)), seq_len(length(nonIntercept_vars)))

	# 			for(k in rng){
	# 				centeringStatus <- centering[k]
	# 				coef <- paste0(GREEKLETTERS[level], "_{", ifelse(shift, (k-1), k), groupLettersSubscript, "}")
	# 				covar <- paste0("(", covariates[k], "_{", allSubscript, "}")

	# 				if(!is.na(centeringStatus)){ ## Do not apply centering to intercept, which will have centering status of NA.
	# 					if(centeringStatus == "uncentered"){
	# 						rVals$X[, eval(paste0(covariates[k])) := tmpDT$DT[[covariates[k]]] ]
	# 						covar <- paste0(covar, ")")
	# 					}
	# 					if(centeringStatus == "group_centered"){
	# 						tmpDT[, eval(paste0(covariates[k], "_grpc")) := mean(get(covariates[k])), by = c(input[[paste0("level_id_", (level+1))]]) ]  	## Calculate level-(k+1) group means vector.
	# 						rVals$X[, eval(paste0(covariates[k], "_grpc")) := (tmpDT[[covariates[k]]] - tmpDT[[paste0(covariates[k], "_grpc")]]) ]			## Subtract group means vector.
	# 						covar <- paste0(covar, " - \\bar{", covariates[k], "_{\\cdot", groupLettersSubscript, "}})")											## e.g. (reg_{ijk} - \bar{reg_{.jk}})
	# 					}
	# 					if(centeringStatus == "grand_centered"){
	# 						rVals$X[, eval(paste0(covariates[k], "_grndc")) := (tmpDT[[covariates[k]]] - mean(tmpDT[[covariates[k]]])) ]					## Calculate grand mean of this covariate, subtract from observed values.
	# 						covar <- paste0(covar, " - ", covariates[k], "_{", paste0(rep("\\cdot", (hlm_k-(level-1))), collapse = ""), "})")										## e.g. (reg_{ijk} - reg_{...})
	# 					}

	# 					model_tex <- paste0(level_1_tex, coef, collapse = " + ")
	# 				}
	# 			}

	# 			## Render residual error tex.
	# 			residualError <- "\\epsilon_{",paste0(letters[9:(9+rVals$k-1)], collapse = ""), "} "
	# 			level_1_tex <- paste0(level_1_tex, residualError, collapse = " + ")
	# 		}
			
	# 	}
	# }

	##--------------------- RENDER MIXED/COMBINED MODEL  ------------------##

	obs <- observe({

		conditions <- (!is.null(input$level_1_model))
		for(i in 2:input$hlm_k){
			conditions <- c(conditions, c((length(grep(paste0("level_", i, "_randomEffect"), names(input))) > 0), (length(grep(paste0("level_", i, "_model_"), names(input))) > 0)))
		}
		if(all(conditions)){
			l <- GatherModelsHLM()
			print("l:")
			print(l)
			cat("\n\n")
			saveRDS(names(input), "/Users/fineiskid/Desktop/inputList.RDS")
			saveRDS(l, "/Users/fineiskid/Desktop/modelList.RDS")
		}
		## TODO: (1) Display individual-level models using a different greek letter per level.
		##		 (2) Create Mixed Model string, format into variable named RHS (right hand side of equation)
		##		 (3) Create LME4 formula
		# CoefficientModels(l, level = input$hlm_k)
		# RHS <- CreateRHS(l, level = input$hlm_k)
		# LME4_FORMULA <- CreateLME4Formula(l, level = input$hlm_k)
	})
})