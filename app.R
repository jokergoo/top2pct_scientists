
library(shiny)
library(glue)
library(DT)
library(tibble)


df_single_year = readRDS("df_single_year.rds")
key_single_year = readRDS("key_single_year.rds")

df_career = readRDS("df_career.rds")
key_career = readRDS("key_career.rds")

cols = c("rank (ns)", "authfull", "inst_name", "cntry", "sm-subfield-1", "sm-subfield-2")

df_single_year$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_single_year))}\"+\"-\"+Math.random());false;'>{df_single_year$authfull}</a>", collapse = FALSE)
df_career$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_career))}\"+\"-\"+Math.random());false;'>{df_career$authfull}</a>", collapse = FALSE)


env = new.env()

env$df = df_single_year
env$key = key_single_year

ui = fluidPage(
	h1("World top 2% scientists"),
	radioButtons("category", label = "Category", choices = c("Single year 2021" = "single_year", "Whole career" = "career"), inline = TRUE),
	p("Search text allows regular expression."),
	dataTableOutput("tb"),
	hr(),
	HTML("<p>Data source: <a href='https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/4'>https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/4</a></p>")
)

server = function(input, output, session) {

	observeEvent(input$category, {

		if(input$category == "single_year") {
			env$df = df_single_year
			env$key = key_single_year
		} else {
			env$df = df_career
			env$key = key_career
		}

		output$tb = renderDataTable(env$df[, cols], 
			server = TRUE, 
			escape = FALSE,
			rownames = FALSE,
			colnames = c("Rank", "Author", "Institute", "Country", "Field", "Sub-field"),
			selection = "none",
			filter = 'top',
			options = list(ordering = FALSE, pageLength = 25, 
				search = list(regex = TRUE, caseInsensitive = TRUE),
				dom = 'ltipr',
				autoWidth = TRUE
			)
		)
	})

	observeEvent(input$author_index, {
		author_index = input$author_index
		author_index = gsub("-\\d\\.\\d+$", "", author_index)
		author_index = as.numeric(author_index)

		author = as.list(env$df[author_index, ])
		author$authfull = gsub("<.*?>", "", author$authfull)

		message(paste0("Author: ", author$authfull))
			
		tb = lapply(names(author), function(x) {
	        	HTML(glue("<tr><th>{env$key[x]}:</th><td>{author[[x]]}</td></tr>"))
	        })
		tb$class = "table table-bordered"
		showModal(modalDialog(
	        title = glue("Information for {author$authfull}"),
	        do.call(tags$table, tb),
	        easyClose = TRUE,
	        size = "l"
	    ))
	})
}

shinyApp(ui = ui, server = server)



