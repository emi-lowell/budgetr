#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      dashboardHeader(
        title = "budgetr"
      ),
      dashboardSidebar(
        menuItem(text = "dashboard",
                 tabName = "dashboard_tab")
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "dashboard_tab",
                  mod_dashboard_ui("dashboard_1"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "budgetr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
