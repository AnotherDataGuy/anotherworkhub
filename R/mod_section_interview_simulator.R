#' section_interview_simulator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs delay enable disable hide show useShinyjs

mod_section_interview_simulator_ui <- function(id) {
  ns <- NS(id)
  shinydashboard::tabItem(
    tags$head(
      tags$style(HTML("

      /* Chat container */
  .chat-container {
    display: flex;
    flex-direction: column;
    height: 400px;
    overflow-y: auto;
    padding: 15px;
    border: 1px solid #ccc;
    border-radius: 12px;
    background-color: #f9f9f9;
    box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    scrollbar-width: thin;
    scrollbar-color: #888 #f5f5f5;
  }

  /* Chat bubbles */
  .chat-bubble {
    max-width: 100%;
    padding: 15px;
    margin: 1.5vw;
    border-radius: 20px;
    font-size: 18px;
    line-height: 1.6;
    position: relative;
    box-shadow: 0px 3px 6px rgba(0, 0, 0, 0.1);
  }

  /* User chat bubble */
  .user-bubble {
    background-color: #34c759;
    color: white;
    align-self: flex-end;
    border-bottom-right-radius: 0;
    font-weight: bold;
  }

  /* Assistant chat bubble */
  .gpt-bubble {
    background-color: #f0f0f0;
    color: black;
    align-self: flex-start;
    border-bottom-left-radius: 0;
    font-weight: normal;
  }

  /* Timestamp styling */
  .timestamp {
    font-size: 12px;
    color: gray;
    position: absolute;
    bottom: -18px;
    right: 10px;
  }

  /* Input area */
  .input-area {
    margin-top: 15px;
    display: flex;
    justify-content: center;
  }

  /* Textarea styling */
  .input-area textarea {
    width: 80%;
    height: 50px;
    padding: 10px;
    border-radius: 12px;
    border: 1px solid #ccc;
    font-size: 14px;
    box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);
  }

  /* Send button styling */
  .input-area button {
    width: 15%;
    height: 50px;
    margin-left: 12px;
    background-color: #007BFF;
    color: white;
    font-size: 16px;
    border-radius: 12px;
    border: none;
    cursor: pointer;
    transition: background-color 0.2s ease, box-shadow 0.2s ease;
  }

  /* Button hover effect */
  .input-area button:hover {
    background-color: #0056b3;
    box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);
  }

  /* Scrollbar styling */
  .chat-container::-webkit-scrollbar {
    width: 8px;
  }

  .chat-container::-webkit-scrollbar-thumb {
    background-color: #888;
    border-radius: 8px;
  }

  .chat-container::-webkit-scrollbar-track {
    background: #f5f5f5;
  }

                      "))
    ),
    tabName = ns("tab_interview_simulator"),
    style = "margin: 2vw;",
    shinyjs::useShinyjs(),  # Include shinyjs for interactivity

    # titlePanel("Job Interview Simulation"),

    # Required and Optional Fields
    div(class = "section-header"
        # h3("Customize Your Interview")
        ),
    uiOutput(ns("interview_simulator_ui"))
  )
}

#' section_interview_simulator Server Functions
#'
#' @noRd
mod_section_interview_simulator_server <- function(id, api_pwd, language_input,
                                                   interview_format_input, assessment_criteria_input, time_constraints_input,
                                                   cultural_fit_focus_input, scenario_questions_input, follow_up_process_input,
                                                   company_culture_input, company_key_values_input, company_challenges_input,
                                                   growth_phase_input, competitors_input, company_reputation_input,
                                                   work_environment_input, key_responsibilities_input, required_skills_input,
                                                   team_structure_input, key_challenges_input, performance_expectations_input,
                                                   career_advancement_input, preferred_questions_input, personality_input,
                                                   industry_experience_input, cultural_background_input, decision_power_input,
                                                   professional_background_input, candidate_expectations_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session$userData$history <- list()  # Chat history
    session$userData$step <- 1  # Step counter
    session$userData$api_calls <- 0  # Track API calls

    # Function to get translated labels
    get_label <- function(en_label, fr_label) {
      if (language_input() == "EN") en_label else fr_label
    }

    # Function to handle translations for sector choices
    get_sector_choices <- function(language) {
      if (language == "EN") {
        return(c(
          "Agriculture, Forestry, and Fishing",
          "Mining and Quarrying",
          "Manufacturing",
          "Electricity, Gas, Steam, and Air Conditioning Supply",
          "Water Supply, Sewerage, Waste Management, and Remediation Activities",
          "Construction",
          "Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles",
          "Transportation and Storage",
          "Accommodation and Food Service Activities",
          "Information and Communication",
          "Financial and Insurance Activities",
          "Real Estate Activities",
          "Professional, Scientific, and Technical Activities",
          "Administrative and Support Service Activities",
          "Public Administration and Defence; Compulsory Social Security",
          "Education",
          "Human Health and Social Work Activities",
          "Arts, Entertainment, and Recreation",
          "Other Service Activities"
        ))
      } else {
        return(c(
          "Agriculture, Sylviculture et Pêche",
          "Extraction Minière et Carrières",
          "Industrie Manufacturière",
          "Production et Distribution d'Électricité, de Gaz, de Vapeur et de Climatisation",
          "Captage, Traitement et Distribution d'Eau; Assainissement, Gestion des Déchets",
          "Construction",
          "Commerce de Gros et de Détail; Réparation de Véhicules Automobiles et de Motocycles",
          "Transport et Entreposage",
          "Hébergement et Services de Restauration",
          "Information et Communication",
          "Activités Financières et d'Assurance",
          "Activités Immobilières",
          "Activités Professionnelles, Scientifiques et Techniques",
          "Activités de Services Administratifs et de Soutien",
          "Administration Publique et Défense; Sécurité Sociale Obligatoire",
          "Éducation",
          "Activités de Santé Humaine et d'Action Sociale",
          "Arts, Spectacles et Activités Récréatives",
          "Autres Activités de Services"
        ))
      }
    }

    # Dynamically render the UI based on language_input
    output$interview_simulator_ui <- renderUI({
      fluidPage(
        titlePanel(get_label("Job Interview Simulation", "Simulation d'entretien d'embauche")),

        # Required and Optional Fields
        div(class = "section-header", h3(get_label("Customize Your Interview", "Personnalisez votre entretien"))),
        fluidRow(
          column(
            width = 6,
            textInput(ns("name"), get_label("Candidate's Name (required)", "Nom du candidat (obligatoire)")),
            textInput(ns("job_title"), get_label("Job Title (required)", "Intitulé du poste (obligatoire)")),
            selectInput(ns("company_sector"),
                        get_label("Company Sector (required)", "Secteur d'activité (obligatoire)"),
                        choices = get_sector_choices(language_input())
            )
          ),
          column(
            width = 6,
            textInput(ns("context"), get_label("Context (optional, e.g., what the interviewer might know about you)",
                                               "Contexte (facultatif, ex: ce que l'intervieweur pourrait savoir sur vous)")),
            selectInput(ns("company_size"), get_label("Company Size (optional)", "Taille de l'entreprise (facultatif)"),
                        choices = c("", get_label("Very small", "Très petite"), get_label("Small", "Petite"),
                                    get_label("Medium", "Moyenne"), get_label("Large", "Grande")))
          )
        ),

        # Start and Refresh Buttons
        fluidRow(
          column(12,
                 actionButton(ns("start_interview"), get_label("Start the Interview", "Commencer l'entretien"),
                              class = "btn btn-primary"),
                 actionButton(ns("refresh"), get_label("Refresh", "Actualiser"),
                              style = "display: none;", class = "btn btn-secondary"))
        ),

        # Chat UI (Interview Simulation)
        div(style = "margin-top: 30px;", h3(get_label("Interview Simulation", "Simulation d'entretien")),
            div(class = "chat-container", uiOutput(ns("chat_ui"))),
            div(class = "input-area",
                textAreaInput(ns("user_input"), NULL, placeholder = get_label("Type your message here...",
                                                                              "Tapez votre message ici..."), width = "80%"),
                actionButton(ns("send_message"), get_label("Send", "Envoyer"), style = "display: none;"))
        )
      )
    })

    # Function to handle cooldown
    apply_cooldown <- function() {
      shinyjs::disable(ns("send_message"))
      shinyjs::delay(10000, shinyjs::enable(ns("send_message")))
    }

    # Function to get the current timestamp
    get_timestamp <- function() {
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }

    # Function to update the chat UI with user and assistant messages
    updateChatUI <- function() {
      output$chat_ui <- renderUI({
        tagList(
          lapply(session$userData$history, function(msg) {
            if (msg$role == "user") {
              div(class = "chat-bubble user-bubble", msg$content, span(class = "timestamp", msg$timestamp))
            } else {
              div(class = "chat-bubble gpt-bubble", msg$content, span(class = "timestamp", msg$timestamp))
            }
          })
        )
      })
    }





    # Start the interview
    observeEvent(input$start_interview, {
      print("Start interview button clicked")

      # Debugging statement before attempting to hide/show buttons
      print("Attempting to hide/show buttons")

      tryCatch({
        shinyjs::hide("start_interview")
        shinyjs::show("refresh")
        shinyjs::show("send_message")
        print("Buttons updated successfully")
      }, error = function(e) {
        print(paste("shinyjs error:", e))
      })

      # Prepare the system message for the GPT API request
      system_message <- paste(
        "You are conducting a job interview.",
        "\nInterview Format:", interview_format_input(),
        "\nAssessment Criteria:", assessment_criteria_input(),
        "\nCompany Culture:", company_culture_input(),
        if (company_key_values_input() != "") paste("\nCompany Key Values:", company_key_values_input()),
        "\nYour task is to ask one question at a time and wait for the candidate's response."
      )

      session$userData$context <- list(list(role = "system", content = system_message))

      print(paste("System message to GPT:", system_message))

      # Make GPT API call
      gpt_response <- tryCatch({
        POST(
          url = "https://api.openai.com/v1/chat/completions",
          add_headers(Authorization = paste("Bearer", api_pwd)),
          body = list(
            model = "gpt-4o-mini",  # Replace with correct model name
            messages = session$userData$context
          ),
          encode = "json"
        )
      }, error = function(e) {
        print(paste("GPT API call failed:", e))
        NULL
      })

      # Handle API response
      if (!is.null(gpt_response) && gpt_response$status_code == 200) {
        assistant_message <- list(
          role = "assistant",
          content = httr::content(gpt_response)$choices[[1]]$message$content
        )
        session$userData$history <- append(session$userData$history, list(assistant_message))
        session$userData$api_calls <- session$userData$api_calls + 1

        # Update chat UI
        updateChatUI()
      } else {
        print("GPT API call failed or returned an error.")
      }
    })

    # Send user message
    observeEvent(input$send_message, {
      req(input$user_input)

      # Limit API calls to 20
      if (session$userData$api_calls >= 20) {
        showNotification("API call limit reached. Please restart the session.", type = "error")
        return(NULL)
      }

      # Add user message to history
      user_message <- list(role = "user", content = input$user_input, timestamp = get_timestamp())
      session$userData$history <- append(session$userData$history, list(user_message))

      # Clear input field
      updateTextInput(session, "user_input", value = "")

      # Increment step counter
      step_info <- paste("This is step", session$userData$step, "of the interview.")
      session$userData$step <- session$userData$step + 1

      # Summarize after every 6 steps
      if (session$userData$step %% 6 == 0) {
        summary_message <- list(role = "system", content = paste("Summary so far after", session$userData$step, "steps."))
        session$userData$history <- append(session$userData$history, list(summary_message))
      }

      # Send conversation history to GPT API
      gpt_messages <- append(session$userData$context, session$userData$history)
      gpt_messages <- append(gpt_messages, list(list(role = "system", content = step_info)))

      # API call to GPT
      gpt_response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_pwd)),
        body = list(
          model = "gpt-4o-mini",  # Correct model name
          messages = gpt_messages
        ),
        encode = "json"
      )

      # Handle errors
      if (gpt_response$status_code != 200) {
        showNotification("Error communicating with GPT API.", type = "error")
        return(NULL)
      }

      # Process GPT response
      assistant_message <- list(
        role = "assistant",
        content = httr::content(gpt_response)$choices[[1]]$message$content,
        timestamp = get_timestamp()
      )
      session$userData$history <- append(session$userData$history, list(assistant_message))
      session$userData$api_calls <- session$userData$api_calls + 1

      # Apply cooldown for 10 seconds
      apply_cooldown()

      # Update chat UI
      updateChatUI()
    })

    # Refresh the session
    observeEvent(input$refresh, {
      session$userData$history <- list()
      session$userData$api_calls <- 0
      session$userData$step <- 1
      output$chat_ui <- renderUI({ NULL })
      shinyjs::hide("send_message")
      shinyjs::show("start_interview")
      shinyjs::hide("refresh")
    })

  })
}






## To be copied in the UI
# mod_section_interview_simulator_ui("section_interview_simulator_1")

## To be copied in the server
# mod_section_interview_simulator_server("section_interview_simulator_1")
