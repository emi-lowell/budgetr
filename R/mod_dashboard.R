#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinydashboard
#' @import reactable
#' @import dplyr
#' @import lubridate
#' @import scales
#' @import tidyr
#' @import ggplot2
#' @import ggiraph
#'
#' @importFrom shiny NS tagList

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectInput(
      inputId = ns("year_select"),
      label = "Select a year: ",
      choices = 2023:2030,
      selected = NULL,
      multiple = FALSE
    ),
    valueBoxOutput(outputId = ns("year_spend_value")),
    valueBoxOutput(outputId = ns("year_income_value")),
    valueBoxOutput(outputId = ns("year_diff_value")),
    column(6, girafeOutput(outputId = ns("year_chart"))),
    column(6, reactableOutput(outputId = ns("year_breakdown")))
  )
}

#' dashboard Server Functions
#'
#' @noRd
mod_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    load(file.path(getwd(), "data/initial_data.RData"))

    observeEvent(input$year_select, {
      yearzzz <- reactive({
        input$year_select
      })

      yearly_expense_df <- expense_data %>%
        filter(year(expense_date) == yearzzz()) %>%
        group_by(year(expense_date)) %>%
        summarise(year_expense = sum(expense_amount, na.rm = TRUE), .groups = "drop")

      yearly_income_df <- income_data %>%
        filter(year(income_date) == yearzzz()) %>%
        group_by(year(income_date)) %>%
        summarise(year_income = sum(income_amount, na.rm = TRUE), .groups = "drop")

      income_df_prep <- income_data %>%
        rename(
          date = income_date,
          category = income_category,
          amount = income_amount
        ) %>%
        mutate(type = "Income")

      expense_df_prep <- expense_data %>%
        rename(
          date = expense_date,
          category = expense_category,
          amount = expense_amount
        ) %>%
        mutate(type = "Expense")

      bind_exp_inc <- bind_rows(income_df_prep, expense_df_prep)

      year_data <- bind_exp_inc %>%
        mutate(date_year = year(date)) %>%
        filter(date_year == yearzzz()) %>%
        group_by(date_year, category, type) %>%
        summarise(year_amount = sum(amount, na.rm = TRUE), .groups = "drop")



      chartzzz <- year_data %>%
        ggplot() +
        geom_col_interactive(
          aes(
            x = as.character(date_year),
            y = year_amount,
            fill = type
          ),
          width = .1
        ) +
        theme_minimal() +
        scale_fill_manual(values = c(
          "#990000",
          "#009900"
        )) +
        scale_y_continuous(
          labels = scales::dollar_format(),
          limits = c(0, 150000)
        ) +
        labs(
          x = "Year",
          y = "Value"
        ) +
        guides(fill = guide_legend_interactive(title = "Type")) +
        theme(panel.background = element_rect(color = "#00000000"),
              )

      output$year_spend_value <- renderValueBox({
        valueBox(
          subtitle = "Yearly Spending",
          value = dollar(unique(yearly_expense_df$year_expense)),
          icon = icon("file-invoice-dollar"),
          color = "maroon"
        )
      })

      output$year_income_value <- renderValueBox({
        valueBox(
          subtitle = "Yearly Income",
          value = dollar(unique(yearly_income_df$year_income)),
          icon = icon("hand-holding-dollar"),
          color = "green"
        )
      })

      output$year_diff_value <- renderValueBox({
        valueBox(
          subtitle = "Net Yearly",
          value = dollar(unique(yearly_income_df$year_income) - unique(yearly_expense_df$year_expense)),
          icon = icon("piggy-bank"),
          color = "fuchsia"
        )
      })


      output$year_chart <- renderGirafe({
        girafe(ggobj = chartzzz,
               bg = "transparent")
      })

      output$year_breakdown <- renderReactable({
        options(reactable.theme = reactableTheme(
          style = list(
            fontFamily = "Segoe UI"
          )
        ))

        reactable(
          bind_exp_inc %>%
            mutate(date_year = year(format.POSIXct(date)),
                   date = format.POSIXct(date)),
          groupBy = c("date_year", "type", "category"),
          columns = list(
            date = colDef(name = "Date"),
            date_year = colDef(name = "Year"),
            type = colDef(name = "Type"),
            category = colDef(name = "Category"),
            amount = colDef(
              name = "Amount",
              aggregate = "sum",
              format = colFormat(
                currency = "USD",
                separators = TRUE
              )
            )
          )
        )
      })
    })
  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard_1")

## To be copied in the server
# mod_dashboard_server("dashboard_1")
