
library(shiny)
library(glue)
library(DT)
library(tibble)


if(0) {
	library(readxl)
	file_single = "~/Downloads/Table_1_Authors_singleyr_2023_pubs_since_1788_wopp_extracted_202408.xlsx"
	file_career = "~/Downloads/Table_1_Authors_career_2023_pubs_since_1788_wopp_extracted_202408.xlsx"

	tb = read_xlsx(file_single, sheet = 2)
	key = read_xlsx(file_single, sheet = 1)
	key = structure(key$DESCRIPTION, names = key$FIELD)

	saveRDS(tb, file = "df_single_year_2023.rds", compress = "xz")
	saveRDS(key, file = "key_single_year_2023.rds", compress = "xz")

	tb = read_xlsx(file_career, sheet = 2)
	key = read_xlsx(file_career, sheet = 1)
	key = structure(key$DESCRIPTION, names = key$FIELD)

	saveRDS(tb, file = "df_career_2023.rds", compress = "xz")
	saveRDS(key, file = "key_career_2023.rds", compress = "xz")
}

year = 2023

# df_single_year = readRDS(paste0("df_single_year_", year, ".rds"))
# key_single_year = readRDS(paste0("key_single_year_", year, ".rds"))

# df_career = readRDS(paste0("df_career_", year, ".rds"))
# key_career = readRDS(paste0("key_career_", year, ".rds"))


# df_single_year$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_single_year))}\"+\"-\"+Math.random());false;'>{df_single_year$authfull}</a>", collapse = FALSE)
# df_career$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_career))}\"+\"-\"+Math.random());false;'>{df_career$authfull}</a>", collapse = FALSE)


env = new.env()

# env$df_single_year = df_single_year
# env$df_career = df_career
# env$key_single_year = key_single_year
# env$key_career = key_career

env$year = ""

choices = c("Single year" = "single_year", "Whole career" = "career")

choices_years = c("2023" = "2023", "2022" = "2022", "2021" = "2021")


ui = fluidPage(
	h1("World top 2% scientists"),
	radioButtons("year", label = "Year", choices = choices_years, inline = TRUE, selected = "2023"),
	radioButtons("category", label = "Category", choices = choices, inline = TRUE, selected = "single_year"),
	p("Search text allows regular expression. The 'Rank' column is based on the 'rank (ns)' column in the original Excel table."),
	dataTableOutput("tb"),
	hr(),
	HTML("<p>Data source: <a href='https://doi.org/10.17632/btchxktzyw.7'>https://doi.org/10.17632/btchxktzyw.7</a>.</p>"),
	HTML("<p>Source code for this shiny app: <a href='https://github.com/jokergoo/top2pct_scientists'>https://github.com/jokergoo/top2pct_scientists</a>.</p>"),
	tags$br(),
	tags$br()
)

server = function(input, output, session) {

	event_trigger = reactive({
		list(input$category, input$year)
	})

	observeEvent(event_trigger(), {

		if(input$year != env$year) {

			message("loading data for year ", input$year)
			showNotification(paste0("loading data for year ", input$year, "..."), duration = 8)

			df_single_year = readRDS(paste0("df_single_year_", input$year, ".rds"))
			key_single_year = readRDS(paste0("key_single_year_", input$year, ".rds"))

			df_career = readRDS(paste0("df_career_", input$year, ".rds"))
			key_career = readRDS(paste0("key_career_", input$year, ".rds"))

			df_single_year$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_single_year))}\"+\"-\"+Math.random());false;'>{df_single_year$authfull}</a>", collapse = FALSE)
			df_career$authfull = glue("<a href='#' onclick='Shiny.setInputValue(\"author_index\", \"{seq_len(nrow(df_career))}\"+\"-\"+Math.random());false;'>{df_career$authfull}</a>", collapse = FALSE)

			env$df_single_year = df_single_year
			env$df_career = df_career
			env$key_single_year = key_single_year
			env$key_career = key_career

			env$year = input$year

		}

		if(input$category == "single_year") {
			df = env$df_single_year
			key = env$key_single_year
		} else {
			df = env$df_career
			key = env$key_career
		}

		cols = c("rank (ns)", "authfull", "inst_name", "cntry", "sm-subfield-1", "sm-subfield-2")

		output$tb = renderDataTable(df[, cols], 
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

		if(input$category == "single_year") {
			df = env$df_single_year
			key = env$key_single_year
		} else {
			df = env$df_career
			key = env$key_career
		}

		author = as.list(df[author_index, ])
		author$authfull = gsub("<.*?>", "", author$authfull)

		message(paste0("Author: ", author$authfull))
			
		tb = lapply(names(author), function(x) {
	        	HTML(glue("<tr><th>{key[x]}:</th><td>{author[[x]]}</td></tr>"))
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



