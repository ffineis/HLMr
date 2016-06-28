shinyUI(navbarPage("HLMr", theme = "bootstrap.css",
	
  tabPanel("File/Data",
    
    tabsetPanel("Load data or model",
  		
      tabPanel("New Model",
  			tags$br(),
        tags$br(),
        fluidRow(
          column(4, offset = 1,
            fileInput('upload_data', 'Upload data for model',
      			          accept = c(
      			            'text/csv',
      			            'text/comma-separated-values',
      			            'text/tab-separated-values',
      			            'text/plain',
      			            '.csv',
      			            '.tsv',
                        '.sav',
                        '.dat')
            )
          ),
          column(3, offset = 2,
            numericInput('hlm_k', label = "HLM-k (order of model)", value = 2, min = 1, max = 6, step = 1)
          )
        ),
        fluidRow(
          column(4, offset = 1,
            uiOutput("levelIDs")
          )
        )
  		),
  		tabPanel("Load Existing Model",
  			tags$p("Widget for loading .RDS file with old model goes here")
  		),
  		tabPanel("Data",
  		    tags$p("Summary statistics for loaded data go here.")
  		)
    )
	),

	tabPanel("Specify Model",
		fluidRow(
			column(12, tags$p("Dynamic data.table with model, covariates, intercept, and fixed effects input widgets here"))
		),
		fluidRow(
			column(12, tags$p("Display mixed model here, below the big data.table above."))
		)
	),

	tabPanel("Other Settings",
	  tabsetPanel("Load data or model",
  		tabPanel("Iteration Settings",
  			tags$p("Iteration stuff goes here")
  		),
  		tabPanel("Estimation Settings",
  			tags$p("Estimation stuff goes here")
  		),
  		tabPanel("Hypothesis Settings",
  			tags$p("Hypothesis stuff goes here")
  		),
  		tabPanel("Output Settings",
  			tags$p("Im not going to let user mess with my output, but if I did I'd put it here.")
  		)
	  )
	),

	tabPanel("Run",
		fluidRow(
      tags$br(),
			column(12,
				fluidRow(
					column(4, offset = 3,
						bsButton("run_button", label = "RUN", style = "success", type = "action", width = '100px')
					),
					column(2,
						checkboxInput("save_box", label = "Save", value = T, width = '400px')
					)
				)
			)
		)
	),
  
	tabPanel("Output",
		tags$p("Output to display here. Not sure what output will look like yet.")
	)
))