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
            numericInput('hlm_k', label = "HLM-k (order of model)", value = 2, min = 1, max = 4, step = 1)
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

    tabsetPanel("Choose and View Models", ##This title does not appear anywhere.
      tabPanel("Model Selection",
        sidebarLayout(
          sidebarPanel(
            radioButtons("select_level", ## Change this in session to hlm_k
              "Outcome/Level Selection",
              c("Outcome", paste0("Level-", 1:4)),
              selected = "Outcome"
            ),
            width = 3 ## Change width of sidebar selection to smaller than default of 4 units.
          ),
          mainPanel(
            column(9, offset = 1,
              fluidRow(
                conditionalPanel('input.select_level === "Outcome"',
                  fluidRow(
                    column(5, selectInput("outcome_var_distribution",
                      "Select Outcome Variable Distribution",
                      choices = c("Normal (Continuous)", "Bernoulli (0 or 1)", "Poisson", "Binomial", "Multinomial", "Ordinal"),
                      multiple = F)
                    ),
                    column(4, selectInput("outcome_var_preprocess",
                      "Preprocess Outcome Variable",
                      choices = c("None", "Center", "Scale by std-dev", "Center+Scale"),
                      selected = "None",
                      multiple = F)
                    )
                  ),
                  fluidRow(
                    column(5, uiOutput("outcome"))
                  )
                ),
                conditionalPanel('input.select_level === "Level-1"',
                  tags$h3("Level 1 Model Selection"),
                  fluidRow(
                    column(5, uiOutput("select_level_1_vars"))
                  ),
                  fluidRow(
                    column(9, uiOutput("level_1_tex"))
                  )
                ),
                conditionalPanel('input.select_level === "Level-2"',
                  tags$h3("Level 2 Model Selection"),
                  fluidRow(
                    column(5, uiOutput("level_2_covariates"))
                    ,column(3, uiOutput("level_2_randomEffects"))
                  ),
                  fluidRow(
                    column(9, uiOutput("level_2_tex"))
                  )
                ),
                conditionalPanel('input.select_level === "Level-3"',
                  tags$h3("Level 3 Model Selection"),
                  fluidRow(
                    column(5, uiOutput("level_3_covariates"))
                    ,column(3, uiOutput("level_3_randomEffects"))
                  ),
                  fluidRow(
                    column(9, uiOutput("level_3_tex"))
                  )
                ),
                conditionalPanel('input.select_level === "Level-4"',
                  tags$h3("Level 4 Model Selection"),
                  fluidRow(
                    column(5, uiOutput("level_4_covariates"))
                    ,column(3, uiOutput("level_4_randomEffects"))
                  ),
                  fluidRow(
                    column(9, uiOutput("level_4_tex"))
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel("View Models in Aggregate",
        tags$p("Allow user to view Levels 1 -> k in aggregate here.")
      )
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