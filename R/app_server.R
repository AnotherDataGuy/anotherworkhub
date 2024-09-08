#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import bs4Dash
#' @import shinymanager
#' @importFrom httr POST content_type_json add_headers content
#' @import dplyr
#' @import shinyjs
#' @import stringr
#' @import tidytext
#' @import tokenizers
#' @import wordcloud2
#' @import ggplot2
#' @import promises
#' @import future
#' @import waiter
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  ################### Auth
  api_pwd <- readLines(con = system.file("app/www/gab.txt", package = "anotherworkhub"))
  # Ensure to securely handle and possibly remove newline or other unwanted characters
  api_pwd <- gsub("\n", "", api_pwd)

  anotherworkhubusers <- read.csv(system.file("app/www/anotherworkhubusers.csv", package = "anotherworkhub"), sep=";")


  shinymanager::set_labels(
    language = "fr",
    "Please authenticate" = "Authentification",
    "Username:" = "Utilisateur :",
    "Password:" = "Mot de passe :"
  )

  # # access

  # create a dataframe which you can import
  # data.frame(
  #     user = c("user1", "user2"), # mandatory user = c("shiny", "shinymanager"),
  #     password = c("pwd1","pwd2"), # mandatory
  #     admin = c(TRUE, FALSE),
  #     stringsAsFactors = FALSE
  #   )

  # or using a csv file

  # system.file("extdata/anotherworkhubusers.csv", package = "anotherworkhub")


  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      anotherworkhubusers
    ),
    timeout = 600
  )


  output$dynamic_title <- renderUI({

    if (input$language == "EN") {
      shiny::h1("Welcome! ")
    } else {
      shiny::h1("Bienvenue ! :)")
    }

  })

  output$app_logo <- renderUI({
    div(
      style = "text-align: center; padding: 10px;",
      # tags$img(src = "inst/extdata/AWH DALL·E 2024-09-08 16.02.04.png", height = "150px", width = "150px")

      tags$img(

        src = "https://plus.unsplash.com/premium_photo-1677252438450-b779a923b0f6?q=80&w=2960&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D",
        # src = system.file("Image3.jpg", package = "anotherworkhub"),
        # src =system.file("extdata/awh_logo.png", package = "anotherworkhub"),
        height = "150px", width = "150px"
      )

    )
  })

  output$sidebar_menu <- renderUI({
    req(input$language)
    shinydashboard::sidebarMenu(
      id= "main_tabs",
      fixed = TRUE,
      # Conditionally render menu items based on the selected language
      if (input$language == "EN") {
        shinydashboard::menuItem(
          "Improve my pitch (written/ spoken)",
          tabName = "tab_pitch_improver",
          icon = icon("sliders")
        )
      } else {
        shinydashboard::menuItem(
          "Améliorer mon pitch (écrit/ parlé)",
          tabName = "tab_pitch_improver",
          icon = icon("sliders")
        )
      },
      if (input$language == "EN") {
        shinydashboard::menuItem(
          "Simulate an interview",
          tabName = "tab_interview_simulator",
          icon = icon("id-card")
        )
      } else {
        shinydashboard::menuItem(
          "Simuler un entretien",
          tabName = "tab_interview_simulator",
          icon = icon("id-card")
        )
      }
    )
  })


  # Set default tab to 'Pitch Improver' when the app loads
  observe({
    updateTabsetPanel(session, "main_tabs", selected = "tab_pitch_improver")
  })

  # Reactive value to track the current language selection
  language_input <- reactive({ input$language })

  # Reactive value to track the active tab
  active_tab <- reactive({
    input$main_tabs
  })

  # Initialize right-side sidebar module and pass active tab information
  right_sidebar_reactives <- mod_section_right_sidebar_tools_server(
    "section_right_sidebar_tools_1",
    language_input = language_input,
    active_tab = active_tab
  )

  # Initialize Pitch Improver module
  mod_section_pitch_improver_server(
    "section_pitch_improver_1",
    api_pwd = api_pwd,
    language_input = language_input,
    communication_context_input = right_sidebar_reactives$communication_context,
    recipient_of_the_pitch_input = right_sidebar_reactives$recipient_of_the_pitch,
    recipients_background_input = right_sidebar_reactives$recipients_background,
    hierarchical_status_input = right_sidebar_reactives$hierarchical_status,
    recipients_activity_input = right_sidebar_reactives$recipients_activity,
    recipients_expertise_input = right_sidebar_reactives$recipients_expertise,
    expectations_level_input = right_sidebar_reactives$expectations_level
  )

  # Initialize Interview Simulator module
  mod_section_interview_simulator_server(
    "section_interview_simulator_1",
    api_pwd = api_pwd,
    language_input = language_input,
    interview_format_input = right_sidebar_reactives$interview_format,
    assessment_criteria_input = right_sidebar_reactives$assessment_criteria,
    time_constraints_input = right_sidebar_reactives$time_constraints,
    cultural_fit_focus_input = right_sidebar_reactives$cultural_fit_focus,
    scenario_questions_input = right_sidebar_reactives$scenario_questions,
    follow_up_process_input = right_sidebar_reactives$follow_up_process,
    company_culture_input = right_sidebar_reactives$company_culture,
    company_key_values_input = right_sidebar_reactives$company_key_values,
    company_challenges_input = right_sidebar_reactives$company_challenges,
    growth_phase_input = right_sidebar_reactives$growth_phase,
    competitors_input = right_sidebar_reactives$competitors,
    company_reputation_input = right_sidebar_reactives$company_reputation,
    work_environment_input = right_sidebar_reactives$work_environment,
    key_responsibilities_input = right_sidebar_reactives$key_responsibilities,
    required_skills_input = right_sidebar_reactives$required_skills,
    team_structure_input = right_sidebar_reactives$team_structure,
    key_challenges_input = right_sidebar_reactives$key_challenges,
    performance_expectations_input = right_sidebar_reactives$performance_expectations,
    career_advancement_input = right_sidebar_reactives$career_advancement,
    preferred_questions_input = right_sidebar_reactives$preferred_questions,
    personality_input = right_sidebar_reactives$personality,
    industry_experience_input = right_sidebar_reactives$industry_experience,
    cultural_background_input = right_sidebar_reactives$cultural_background,
    decision_power_input = right_sidebar_reactives$decision_power,
    professional_background_input = right_sidebar_reactives$professional_background,
    candidate_expectations_input = right_sidebar_reactives$candidate_expectations
  )


  # })





}
