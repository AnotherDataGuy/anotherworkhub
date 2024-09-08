#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      dark = TRUE,
      header = bs4Dash::dashboardHeader(
        uiOutput("user_profile")
      ),
      sidebar = bs4Dash::dashboardSidebar(
        width = "320px",
        collapsed = FALSE,
        uiOutput("app_logo"),
        uiOutput("sidebar_menu")
      ),

      controlbar = bs4Dash::dashboardControlbar(
        width = "380px",
        id = "right_control_bar",
        style = "font-size: small;",
        # skin = "dark",
        collapsed = FALSE,
        overlay = FALSE,
        pinned = TRUE,
        right = TRUE,
        controlbarMenu(
          id = "controlbar_menu",
          bs4Dash::controlbarItem(
            title = "Options",
            collapsed = FALSE,
            mod_section_right_sidebar_tools_ui("section_right_sidebar_tools_1")
          )
        )
      ),

      footer = dashboardFooter(),
      body = bs4Dash::dashboardBody(
        uiOutput("auth_output"),
        # Upper part of the body section

        fluidRow(
          column(width = 9

          ),
          column(width = 3,
                 style = "text-align: right;padding: 3px 32px 0 0px;",
                 selectInput("language", "Langue/ Language:", choices = c("Français" = "FR", "English" = "EN"), selected = "Français")
          )
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "tab_pitch_improver",
            mod_section_pitch_improver_ui("section_pitch_improver_1")
          ),
          shinydashboard::tabItem(
            tabName = "tab_interview_simulator",
            div(class = "communication-context-icon",
                tags$i(class = "fa fa-comments-o fa-comments-o-background")
            ),
            mod_section_interview_simulator_ui("section_interview_simulator_1")
          )
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
    tags$script(src = "main_indicators.js"),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "anotherworkhub"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
