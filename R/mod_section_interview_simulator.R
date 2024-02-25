#' section_interview_simulator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_section_interview_simulator_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(
    tabName = "tab_interview_simulator",
    uiOutput(ns("interview_interface"))

  )
}

#' section_interview_simulator Server Functions
#'
#' @noRd
mod_section_interview_simulator_server <- function(id, api_pwd, language_input){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    api_key <- api_pwd

    admin_prompt_interview_simulator <- reactive({
      req(language_input)
      if (language_input() == "EN") {
        "You simulate an interview based on the parameters given by the user. Your role is to be the person conducting the interview."
      } else {
        "Tu simules un entretien selon les paramètres que te donne l'utilisateur. Ton rôle est d'être la personne qui mène l'entretien"
      }
    })

    output$interview_text_area <- renderUI({
      # Determine the label based on the selected language
      textAreaInput(
        inputId= ns("text_input_interview"),
        label="Digame....",
        rows = 7,
        width = "100%"
      )
    })

    output$gpt_button_interview <- renderUI({
      actionButton(
        inputId = ns("gpt_update_interview"),
        label = "Envoyer")
    })

    # Initialize the conversation with a placeholder
    output$gpt_response_interview <- renderUI({
      HTML("<div class='empty-response'>...</div>")
    })

    # Initialize a reactive value to store the conversation
    conversation_interview <- reactiveVal(list())

    observeEvent(input$gpt_update_interview, {
      req(input$text_input_interview, api_key, admin_prompt_interview_simulator)

      user_input <- input$text_input_interview
      admin_prompt_interview_simulator <- admin_prompt_interview_simulator()

      # Add user input to the conversation
      current_conversation_interview <- conversation_interview()
      current_conversation_second_degre <- c(current_conversation_second_degre, list(paste0("<div class='chat-line user'>User : ", user_input, "</div>")))

      # Call your function to interact with GPT
      response_interview <- tryCatch({
        fct_interact_with_gpt_api_only_text(
          api_key=api_key,
          user_input=user_input,
          admin_prompt= admin_prompt_interview_simulator,
          model = "gpt-4"
        )
      }, error = function(e) {
        paste("Error: ", e$message)
      })

      # Add GPT response to the conversation
      current_conversation_interview <- c(current_conversation_interview, list(paste0("<div class='chat-line gpt'>GPT : ", response_interview, "</div>")))

      # Update the conversation
      conversation_interview(current_conversation_interview)

      # Update the UI
      output$gpt_response_interview <- renderUI({
        html_content <- lapply(current_conversation_interview, HTML)
        do.call(tagList, html_content)
      })
    })


    output$interview_interface <- renderUI({

      title <- if (language_input() == "EN") "How can I help?" else if (language_input() == "FR") "Comment je peux aider ?"

      tagList(
        fluidRow(h1(title)),
        fluidRow(
          htmlOutput(ns("gpt_response_interview")),
          class = "response-container"

        ),
        fluidRow(
          uiOutput(ns("interview_text_area"))),
        fluidRow(
          uiOutput(ns("gpt_button_interview"))
        )
      )
    })





  })
}

## To be copied in the UI
# mod_section_interview_simulator_ui("section_interview_simulator_1")

## To be copied in the server
# mod_section_interview_simulator_server("section_interview_simulator_1")
