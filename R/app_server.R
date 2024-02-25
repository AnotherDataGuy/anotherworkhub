#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import bs4Dash
#' @import shinymanager
#' @importFrom httr POST content_type_json add_headers content
#' @import ggplot2
#' @import promises
#' @import future
#' @import waiter
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  ################### Auth
  # api_pwd <- system.file("app/www/gab.txt", package = "anotherworkhub")

  # api_pwd <- reactive({ system.file("path/to/password.txt", package = "yourPackageName") })



  api_pwd <- readLines(con = system.file("app/www/gab.txt", package = "anotherworkhub"))

  # Ensure to securely handle and possibly remove newline or other unwanted characters
  api_pwd <- gsub("\n", "", api_pwd)



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

  anotherworkhubusers <- read.csv("anotherworkhubusers.csv", sep=";")

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      anotherworkhubusers
    )
  )


  output$dynamic_title <- renderUI({

    if (input$language == "EN") {
      shiny::h1("Welcome! ")
    } else {
      shiny::h1("Bienvenue ! :)")
    }

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
      }
      # if (input$language == "EN") {
      #   shinydashboard::menuItem(
      #     "Simulate an interview",
      #     tabName = "tab_interview_simulator",
      #     icon = icon("id-card")
      #   )
      # } else {
      #   shinydashboard::menuItem(
      #     "Simuler un entretien",
      #     tabName = "tab_interview_simulator",
      #     icon = icon("id-card")
      #   )
      # }
    )
  })


  observe({
    updateTabsetPanel(session, "main_tabs", selected = "tab_pitch_improver")
  })


  # variables between modules
  language_input <- reactive({ input$language })

  # Initialize modules

  # observeEvent(input$language, {
    pitch_improver_reactives <- mod_section_right_sidebar_tools_server("section_right_sidebar_tools_1", language_input)


    mod_section_pitch_improver_server("section_pitch_improver_1",
                                      api_pwd=api_pwd,
                                      language_input = language_input,
                                      communication_context_input = pitch_improver_reactives$communication_context,
                                      recipient_of_the_pitch_input = pitch_improver_reactives$recipient_of_the_pitch,
                                      recipients_background_input = pitch_improver_reactives$recipients_background,
                                      hierarchical_status_input = pitch_improver_reactives$hierarchical_status,
                                      recipients_activity_input = pitch_improver_reactives$recipients_activity,
                                      recipients_expertise_input = pitch_improver_reactives$recipients_expertise,
                                      expectations_level_input = pitch_improver_reactives$expectations_level
    )
    # mod_section_interview_simulator_server("section_interview_simulator_1",
    #                                        api_pwd=api_pwd,
    #                                        language_input=language_input
    #                                        )


  # })





}
