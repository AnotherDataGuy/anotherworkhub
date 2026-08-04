#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import shinymanager
#' @importFrom httr POST content_type_json add_headers content
#' @import shinyjs
#' @import stringr
#' @import tokenizers
#' @import promises
#' @import future
#' @noRd
app_server <- function(input, output, session) {

  # Get translations once at server initialization
  translations <- get_translations()
  # get api
  api_pwd <- Sys.getenv("API_KEY")

  # Authentication (skipped when ANOTHERWORKHUB_DISABLE_LOGIN=true)
  auth_disabled <- isTRUE(as.logical(Sys.getenv("ANOTHERWORKHUB_DISABLE_LOGIN", "false")))
  # secure_server() returns a reactiveValues, not a reactive conductor
  res_auth <- NULL

  if (auth_disabled) {
    output$user_profile <- renderUI({ NULL })
    output$auth_output <- renderUI({ NULL })
  } else {
    res_auth <- shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(
        db = "anotherworkhubusers.sqlite",
        passphrase = Sys.getenv("shinymanagerauth")
      ),
      timeout = 600
    )
    output$user_profile <- renderUI({ NULL })
    output$auth_output <- renderUI({ NULL })
  }

  # Per-session + daily usage guard (shared OpenAI key protection)
  usage_guard <- create_usage_guard(
    user_id = security_client_id(session)
  )
  if (!auth_disabled) {
    observe({
      uid <- res_auth$user
      if (!is.null(uid) && nzchar(uid)) {
        usage_guard$set_user_id(uid)
      }
    })
  }

  language_input <- reactive({ input$language %||% "FR" })

  output$header_language_ui <- renderUI({
    lang <- language_input()
    tags$div(
      class = "awh-lang-switch",
      icon("globe"),
      shiny::radioButtons(
        "language",
        label = tags$span(class = "awh-lang-label", t_lang(translations$app$language_label, lang)),
        choiceNames = c("FR", "EN"),
        choiceValues = c("FR", "ENG"),
        selected = lang,
        inline = TRUE
      )
    )
  })

  output$bottom_navigation_ui <- renderUI({
    lang <- language_input() %||% "FR"
    tags$div(
      id = "bottom-sidebar",
      div(
        class = "bottom-sidebar-menu",
        actionButton(
          "btn_home",
          label = tagList(icon("house"), tags$span(t_lang(translations$app$nav_home, lang))),
          class = "nav-link active"
        ),
        actionButton(
          "btn_interview",
          label = tagList(icon("id-card"), tags$span(t_lang(translations$app$nav_interview, lang))),
          class = "nav-link"
        ),
        actionButton(
          "btn_pitch",
          label = tagList(icon("sliders"), tags$span(t_lang(translations$app$nav_pitch, lang))),
          class = "nav-link"
        )
      ),
      tags$button(id = "toggle-bottom-sidebar", icon("chevron-down"))
    )
  })

  # Home / landing content
  output$home_content <- renderUI({
    lang <- language_input() %||% "FR"
    h <- translations$home

    div(
      class = "awh-home awh-home--canvas",
      div(
        class = "home-ambient",
        tags$img(
          src = "www/anotherworkhub_deco.png",
          class = "home-deco-art",
          alt = ""
        ),
        div(class = "home-orb home-orb--a"),
        div(class = "home-orb home-orb--b")
      ),
      div(
        class = "home-stage",
        div(
          class = "home-intro",
          tags$p(class = "home-eyebrow", t_lang(h$hero_eyebrow, lang)),
          tags$h1(class = "home-display", t_lang(h$hero_title, lang)),
          tags$p(class = "home-lead", t_lang(h$hero_subtitle, lang))
        ),
        div(
          class = "home-lanes",
          tags$button(
            class = "home-lane home-lane--interview awh-goto",
            `data-target` = "interview",
            type = "button",
            tags$span(class = "home-lane-index", "01"),
            tags$span(class = "home-lane-title", t_lang(h$feature_interview_title, lang)),
            tags$span(class = "home-lane-desc", t_lang(h$feature_interview_desc, lang)),
            tags$span(class = "home-lane-action", t_lang(h$feature_interview_cta, lang), icon("arrow-right"))
          ),
          tags$button(
            class = "home-lane home-lane--pitch awh-goto",
            `data-target` = "pitch",
            type = "button",
            tags$span(class = "home-lane-index", "02"),
            tags$span(class = "home-lane-title", t_lang(h$feature_pitch_title, lang)),
            tags$span(class = "home-lane-desc", t_lang(h$feature_pitch_desc, lang)),
            tags$span(class = "home-lane-action", t_lang(h$feature_pitch_cta, lang), icon("arrow-right"))
          )
        ),
        div(
          class = "home-prose",
          tags$h2(class = "home-prose-title", t_lang(h$how_title, lang)),
          tags$p(class = "home-prose-text", t_lang(h$how_body, lang))
        ),
        div(
          class = "home-prose home-prose--stance",
          tags$h2(class = "home-prose-title", t_lang(h$stance_title, lang)),
          tags$p(class = "home-prose-text", t_lang(h$stance_body, lang))
        )
      )
    )
  })
  outputOptions(output, "home_content", suspendWhenHidden = FALSE)

  # Module UI outputs
  output$interview_content <- renderUI({
    mod_section_interview_simulator_ui("section_interview_simulator_1")
  })
  outputOptions(output, "interview_content", suspendWhenHidden = FALSE)

  output$pitch_content <- renderUI({
    mod_section_pitch_improver_ui("section_pitch_improver_1")
  })
  outputOptions(output, "pitch_content", suspendWhenHidden = FALSE)

  # Initialize modules
  mod_section_interview_simulator_server(
    "section_interview_simulator_1",
    api_pwd = api_pwd,
    language_input = language_input,
    translations = translations,
    usage_guard = usage_guard
  )

  mod_section_pitch_improver_server(
    "section_pitch_improver_1",
    api_pwd,
    language_input,
    translations,
    usage_guard = usage_guard
  )



  # Section switching (show/hide + transition) is handled client-side in
  # golem_add_external_resources() to keep it smooth and round-trip free.
}
