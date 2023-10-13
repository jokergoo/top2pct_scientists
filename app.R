
library(shiny)
library(glue)
library(DT)
library(tibble)


if(0) {
	library(readxl)
	file_single = "~/Downloads/Table_1_Authors_singleyr_2022_pubs_since_1788_wopp_extracted_202310.xlsx"
	file_career = "~/Downloads/Table_1_Authors_career_2022_pubs_since_1788_wopp_extracted_202310.xlsx"

	tb = read_xlsx(file_single, sheet = 2)
	key = read_xlsx(file_single, sheet = 1)
	key = structure(key$DESCRIPTION, names = key$FIELD)

	saveRDS(tb, file = "df_single_year_2022.rds", compress = "xz")
	saveRDS(key, file = "key_single_year_2022.rds", compress = "xz")

	tb = read_xlsx(file_career, sheet = 2)
	key = read_xlsx(file_career, sheet = 1)
	key = structure(key$DESCRIPTION, names = key$FIELD)

	saveRDS(tb, file = "df_career_2022.rds", compress = "xz")
	saveRDS(key, file = "key_career_2022.rds", compress = "xz")
}

year = 2022

df_single_year = readRDS(paste0("df_single_year_", year, ".rds"))
key_single_year = readRDS(paste0("key_single_year_", year, ".rds"))

df_career = readRDS(paste0("df_career_", year, ".rds"))
key_career = readRDS(paste0("key_career_", year, ".rds"))

cols = c("rank (ns)", "authfull", "inst_name", "cntry", "sm-subfield-1", "sm-subfield-2")

df_single_year$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_single_year))}\"+\"-\"+Math.random());false;'>{df_single_year$authfull}</a>", collapse = FALSE)
df_career$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_career))}\"+\"-\"+Math.random());false;'>{df_career$authfull}</a>", collapse = FALSE)


env = new.env()

env$df = df_single_year
env$key = key_single_year

choices = c(foo = "single_year", "Whole career" = "career")
names(choices)[1] = paste0("Single year ", year)

ui = fluidPage(
	h1("World top 2% scientists"),
	radioButtons("category", label = "Category", choices = choices, inline = TRUE),
	p("Search text allows regular expression. The 'Rank' column is based on the 'rank (ns)' column in the original Excel table."),
	dataTableOutput("tb"),
	hr(),
	HTML("<p>Data source: <a href='https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/6'>https://elsevier.digitalcommonsdata.com/datasets/btchxktzyw/6</a></p>"),
	HTML("<p>Source code for this shiny app: <a href='https://github.com/jokergoo/top2pct_scientists'>https://github.com/jokergoo/top2pct_scientists</a></p>"),
	tags$br(),
	tags$br()

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



