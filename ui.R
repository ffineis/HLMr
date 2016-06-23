shinyUI(navbarPage("HLMr", theme = "bootstrap.css",
	tabPanel("File",
    tabsetPanel("Load data or model",
  		tabPanel("New Model",
  			tags$p("Load data widget goes here        Model type radio button goes here")
  		),
  		tabPanel("Existing Model",
  			tags$p("Widget for loading .RDS file with old model goes here")
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
			column(12,
				fluidRow(
					column(6,
						bsButton("run_button", label = "RUN", style = "success", type = "action"),
						tags$style(type="text/css", "#run_button  { vertical-align: middle;}")
					),
					column(6,
						checkboxInput("save_box", label = "Save", value = T, width = '400px'),
						tags$style(type="text/css", "#save_box { text-align:center; display: block;}")
					)
				)
			)
		)
	),
	tabPanel("Output",
		tags$p("Output to display here. Not sure what output will look like yet.")
	)
))