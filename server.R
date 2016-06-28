library(shiny)
library(shinyBS)
library(foreign)


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
	})
  
  output$levelIDs <- renderUI({
    lapply(1:input$hlm_k, function(i){
      selectizeInput(inputId = paste0("level_id_", i),
                     label = paste("Level", i, "ID"),
                     choices = names(rVals$DT),
                     multiple = F)
    })
  })

})