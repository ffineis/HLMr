shinyUI(navbarPage("HLMr", theme = "bootstrap.css",
	
  tabPanel("File/Data",
    withMathJax(),

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
        tags$br(),
        tags$br(),
        fluidRow(
          column(4, offset = 1,
            uiOutput("levelIDs")
          ),
          column(4, offset = 2,
            uiOutput("na_opt")
          )
        )
  		),
  		tabPanel("Load Existing Model",
  			tags$p("Widget for loading .RDS file with old model goes here")
  		),
  		tabPanel("Data",
  		    tags$br(),
          dataTableOutput("numeric_summary_dt")
  		)
    )
	),

	tabPanel("Specify Model",

    tags$h3("Mixed Model"),
    fluidRow(
      column(3, offset = 1, uiOutput("MixedModel"))
    ),

    tags$h3("Outcome Variable"),
    fluidRow(
      column(3, offset = 1,
        uiOutput("outcome")
      )
    ),
    fluidRow(
      column(3, offset = 1,
        checkboxInput("edit_outcome",
                      "Enter outcome variable editor",
                      value = F)
      )
    ),
    fluidRow(
      column(3, offset = 1,
        conditionalPanel("input.edit_outcome === true",
          selectInput("outcome_var_options",
                      "Select Outcome Variable Distribution",
                      choices = c("Normal (Continuous)", "Bernoulli (0 or 1)", "Poisson", "Binomial", "Multinomial", "Ordinal"),
                      multiple = F)
        )
      )
    ),
    
    tags$h3("Level 1 Model Selection"),
    fluidRow(
      column(3, offset = 1, uiOutput("select_level_1_vars"))
    ),

    tags$h3("Level 2 Model Selection"), ## Make this uiOutput...
    fluidRow(
      # column(10, offset = 1, dataTableOutput("level_2_options"))
      column(3, offset = 1, uiOutput("level_2_covariates"))
      ,column(4, uiOutput("level_2_randomEffects"))
    ),

    htmlOutput("level_3_header"),
    fluidRow(
      column(3, offset = 1, uiOutput("level_3_covariates"))
      ,column(4, uiOutput("level_3_randomEffects"))
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