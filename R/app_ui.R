#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      dark = TRUE,
      header = bs4Dash::dashboardHeader(),
      sidebar = bs4Dash::dashboardSidebar(
        # flat = TRUE,

        collapsed = TRUE,
        # tags$div(
        #   style = "text-align: center;",
        #   tags$img(src = "www/DALL·E 2023-12-26 06.20.09.png", id = "le_style_du_logo")
        # ),
        uiOutput("sidebar_menu")
      ),

      controlbar = bs4Dash::dashboardControlbar(
        id = "right_control_bar",
        style = "width: 350px; font-size: small;",
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
        uiOutput("auth_output "),
        # Upper part of the body section

        fluidRow(
          column(width = 9
                 # div(class = "communication-context-icon",
                 #     tags$i(class = "fa fa-comments-o fa-comments-o-background")
                 # )
          ),
          column(width = 3,
                 style = "text-align: right;padding: 3px 32px 0 0px;",
                 selectInput("language", "Langue/ Language:", choices = c("Français" = "FR", "English" = "EN"), selected = "Français")
          )
        ),
        shinydashboard::tabItems(
          mod_section_pitch_improver_ui("section_pitch_improver_1")
          # mod_section_interview_simulator_ui("section_interview_simulator_1")
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
