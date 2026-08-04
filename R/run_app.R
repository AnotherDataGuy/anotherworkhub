#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom future plan multisession
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  # Set ANOTHERWORKHUB_DISABLE_LOGIN=true in .Renviron to skip shinymanager locally.
  auth_disabled <- isTRUE(as.logical(Sys.getenv("ANOTHERWORKHUB_DISABLE_LOGIN", "false")))
  is_prod <- isTRUE(getOption("golem.app.prod", FALSE))

  if (is_prod && auth_disabled) {
    stop(
      "ANOTHERWORKHUB_DISABLE_LOGIN=true while golem.app.prod is TRUE. ",
      "Refusing to start: unset ANOTHERWORKHUB_DISABLE_LOGIN for shared/production deployments.",
      call. = FALSE
    )
  }

  # Ensure future() calls (pitch analyses, interview coaching) actually run
  # off the main Shiny process. Override with ANOTHERWORKHUB_FUTURE_WORKERS.
  workers_raw <- Sys.getenv("ANOTHERWORKHUB_FUTURE_WORKERS", unset = "2")
  workers <- suppressWarnings(as.integer(workers_raw))
  if (is.na(workers) || workers < 1L) {
    workers <- 2L
  }
  future::plan(future::multisession, workers = workers)

  app_ui_wrapped <- if (auth_disabled) {
    app_ui
  } else {
    shinymanager::secure_app(
      tags_top = tags$div(
        tags$img(
          src = "https://images.unsplash.com/photo-1577563908411-5077b6dc7624",
          width = "150px"
        ),
        h4("Welcome to AnotherWorkhub!"),
        p(
          "Sign in with the credentials provided by the app administrator.",
          style = "text-align: center; font-size: 14px; color: gray;"
        )
      ),
      app_ui
    )
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui_wrapped,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

