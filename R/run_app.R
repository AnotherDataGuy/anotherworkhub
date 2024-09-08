#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  with_golem_options(
    app = shinyApp(

      ui = shinymanager::secure_app(
        tags_top = tags$div(
          tags$img(

            src="https://plus.unsplash.com/premium_photo-1677252438450-b779a923b0f6?q=80&w=2960&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D",
            # src = "awh_logo.png",
            # src = system.file("awh_logo.png", package = "anotherworkhub"),
            height = "150px", width = "150px"
          ),
          h4("Welcome to AnotherWorkhub!"),
          p("For more details about this app, please visit its associated ",
            a(href = "https://github.com/AnotherDataGuy/anotherworkhub", "GitHub repository.", target = "_blank"),
            style = "text-align: center; font-size: 14px; color: gray;"
          )
        ),
        app_ui
        ),
      # ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

