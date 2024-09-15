#' section_right_sidebar_tools UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_section_right_sidebar_tools_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dynamic_sidebar_content"))  # Output that will display conditional UI
  )
}


#' section_right_sidebar_tools Server Functions
#'
#' @noRd
mod_section_right_sidebar_tools_server <- function(id, language_input, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Observe active tab and render the corresponding sidebar UI
    observe({
      # Ensure that tab is not NULL or empty
      req(active_tab())
      tab <- active_tab()  # Get the currently selected tab

      output$dynamic_sidebar_content <- renderUI({
        if (tab == "tab_pitch_improver") {
          # Render UI for Pitch Improver
          fluidRow(
            uiOutput(ns("communication_context_ui")),
            uiOutput(ns("recipient_of_the_pitch_ui")),
            uiOutput(ns("recipients_background_ui")),
            uiOutput(ns("hierarchical_status_ui")),
            uiOutput(ns("recipients_activity_ui")),
            uiOutput(ns("recipients_expertise_ui")),
            uiOutput(ns("expectations_level_ui"))
          )
        } else if (tab == "tab_interview_simulator") {
          # Render UI for Interview Simulator
          tabsetPanel(
            id = "section_right_sidebar_tools",
            header = if (language_input() == "EN") "Interview Options" else "Options de l'entretien",
            tabPanel(if (language_input() == "EN") "Interview & job details" else "Détails de l'entretien et de l'emploi",
                     div(class = "tab-content",
                         fluidRow(
                           h4(if (language_input() == "EN") "📝 Interview details" else "📝 Détails de l'entretien"),
                           column(12, selectInput(ns("interview_format"),
                                                  if (language_input() == "EN") "Interview Format" else "Format de l'entretien",
                                                  choices = if (language_input() == "EN") {
                                                    c("", "In-person", "Video", "Panel", "Case Study", "Whiteboard Challenge")
                                                  } else {
                                                    c("", "En personne", "Vidéo", "Panel", "Étude de cas", "Défi tableau blanc")
                                                  })),
                           column(12, selectInput(ns("assessment_criteria"),
                                                 if (language_input() == "EN") "Assessment Criteria" else "Critères d'évaluation",
                                                 choices = c("", "Behavioral fit", "Technical proficiency", "Problem-solving", "Leadership"))),
                           column(12, selectInput(ns("time_constraints"),
                                                 if (language_input() == "EN") "Time Constraints" else "Contraintes de temps",
                                                 choices = c("", "Quick", "Detailed", "Phased"))),
                           column(12, selectInput(ns("cultural_fit_focus"),
                                                 if (language_input() == "EN") "Cultural Fit Focus" else "Concentration sur l'adéquation culturelle",
                                                 choices = c("", "High", "Moderate", "Low"))),
                           column(12, textInput(ns("scenario_questions"),
                                               if (language_input() == "EN") "Scenario-based Questions" else "Questions basées sur des scénarios")),
                           column(12, selectInput(ns("follow_up_process"),
                                                 if (language_input() == "EN") "Follow-up Process" else "Processus de suivi",
                                                 choices = c("", "Quick turnaround", "Slow", "Multiple rounds")))
                         ),
                         fluidRow(
                           h4(if (language_input() == "EN") "🏄 About the job" else "🏄 À propos de l'emploi"),
                           column(12, textInput(ns("key_responsibilities"),
                                               if (language_input() == "EN") "Key Responsibilities" else "Responsabilités clés")),
                           column(12, textInput(ns("required_skills"),
                                               if (language_input() == "EN") "Required Skills" else "Compétences requises")),
                           column(12, textInput(ns("team_structure"),
                                               if (language_input() == "EN") "Team Structure" else "Structure de l'équipe")),
                           column(12, textInput(ns("key_challenges"),
                                               if (language_input() == "EN") "Key Challenges" else "Défis clés")),
                           column(12, textInput(ns("performance_expectations"),
                                               if (language_input() == "EN") "Performance Expectations" else "Attentes en matière de performance")),
                           column(12, textInput(ns("career_advancement"),
                                               if (language_input() == "EN") "Career Advancement" else "Avancement de carrière"))
                         )
                     )
            ),
            tabPanel(if (language_input() == "EN") "About the Company & the interviewer" else "À propos de l'entreprise et de l'intervieweur",
                     div(class = "tab-content",
                         fluidRow(
                           h4(if (language_input() == "EN") "🏢 The Company" else "🏢 L'entreprise"),
                           column(12, selectInput(ns("company_culture"),
                                                 if (language_input() == "EN") "Company Culture" else "Culture de l'entreprise",
                                                 choices = c("", "Formal", "Casual", "Startup", "Corporate"))),
                           column(12, textInput(ns("company_key_values"),
                                               if (language_input() == "EN") "Company Key Values" else "Valeurs clés de l'entreprise")),
                           column(12, textInput(ns("company_challenges"),
                                               if (language_input() == "EN") "Business Challenges" else "Défis commerciaux")),
                           column(12, selectInput(ns("growth_phase"),
                                                 if (language_input() == "EN") "Growth Phase" else "Phase de croissance",
                                                 choices = c("", "Startup", "Scaling", "Mature"))),
                           column(12, textInput(ns("competitors"),
                                               if (language_input() == "EN") "Competitors" else "Concurrents")),
                           column(12, textInput(ns("company_reputation"),
                                               if (language_input() == "EN") "Company Reputation" else "Réputation de l'entreprise")),
                           column(12, selectInput(ns("work_environment"),
                                                 if (language_input() == "EN") "Work Environment" else "Environnement de travail",
                                                 choices = c("", "Remote", "Hybrid", "In-office")))
                         ),
                         fluidRow(
                           h4(if (language_input() == "EN") "🎯 The interviewer" else "🎯 L'intervieweur"),
                           column(12, selectInput(ns("preferred_questions"),
                                                 if (language_input() == "EN") "Preferred Questions" else "Questions préférées",
                                                 choices = c("", "Competency-based", "Behavioral", "Technical", "Situational"))),
                           column(12, selectInput(ns("personality"),
                                                 if (language_input() == "EN") "Personality" else "Personnalité",
                                                 choices = c("", "Extroverted", "Introverted", "Detail-oriented", "Visionary"))),
                           column(12, textInput(ns("industry_experience"),
                                               if (language_input() == "EN") "Industry Experience" else "Expérience dans l'industrie")),
                           column(12, textInput(ns("cultural_background"),
                                               if (language_input() == "EN") "Cultural Background" else "Contexte culturel")),
                           column(12, selectInput(ns("decision_power"),
                                                 if (language_input() == "EN") "Decision-making Power" else "Pouvoir décisionnel",
                                                 choices = c("", "Full authority", "Advisory role", "Part of a panel"))),
                           column(12, textInput(ns("professional_background"),
                                               if (language_input() == "EN") "Professional Background" else "Contexte professionnel")),
                           column(12, textInput(ns("candidate_expectations"),
                                               if (language_input() == "EN") "Candidate Expectations" else "Attentes du candidat"))
                         )
                     )
            )
          )
        }
      })

      ### Render the Pitch Improver inputs
      if (tab == "tab_pitch_improver") {
        output$recipient_of_the_pitch_ui <- renderUI({
          label <- if (language_input() == "EN") "Who's your pitch addressed to?" else "À qui est adressé votre pitch ?"
          textInput(ns("recipient_of_the_pitch"), label)
        })

        output$communication_context_ui <- renderUI({
          req(language_input())
          label <- if (language_input() == "EN") "What's the context of the exchange?" else "Quel est le contexte de l'échange ?"

          # Set choices for English
          choices_en <- list(
            "Application" = c(
              "Spontaneous application" = "spontaneous_application",
              "Reply to an offer" = "offer_reply"
            ),
            "Interviews" = c(
              "Phone Screening" = "phone_screening",
              "One-on-One Interview" = "one_on_one_interview"
            ),
            "Meetings" = c(
              "Formal Meeting" = "first_formal_meeting",
              "Informal Meeting" = "first_informal_meeting"
            ),
            "Networking" = c(
              "Networking Event" = "networking_event",
              "Follow-up After Networking Event" = "followup_after_networking"
            ),
            "Job Offers" = c(
              "Job Offer Acceptance" = "job_offer_acceptance",
              "Job Offer Clarification" = "job_offer_clarification",
              "Rejecting a Job Offer" = "rejecting_job_offer"
            ),
            "Post-Interview" = c(
              "Asking for Feedback" = "asking_for_feedback",
              "Follow-up After Interview" = "followup_after_interview"
            )
          )

          # Set choices for French
          choices_fr <- list(
            "Candidature" = c(
              "Candidature spontanée" = "spontaneous_application",
              "Réponse à une offre" = "offer_reply"
            ),
            "Entretiens" = c(
              "Entretien téléphonique" = "phone_screening",
              "Entretien individuel" = "one_on_one_interview"
            ),
            "Réunions" = c(
              "Réunion formelle" = "first_formal_meeting",
              "Réunion informelle" = "first_informal_meeting"
            ),
            "Réseautage" = c(
              "Événement de réseautage" = "networking_event",
              "Suivi après un événement de réseautage" = "followup_after_networking"
            ),
            "Offres d'emploi" = c(
              "Acceptation d'offre d'emploi" = "job_offer_acceptance",
              "Clarification d'offre d'emploi" = "job_offer_clarification",
              "Refus d'offre d'emploi" = "rejecting_job_offer"
            ),
            "Post-Entretien" = c(
              "Demande de retour d'information" = "asking_for_feedback",
              "Suivi après l'entretien" = "followup_after_interview"
            )
          )

          # Choose the right set of choices based on the language
          choices <- if (language_input() == "EN") {
            choices_en
          } else {
            choices_fr
          }

          selectInput(inputId = ns("communication_context"), label = label, choices = choices, selected = "spontaneous_application")
        })

        output$recipients_background_ui <- renderUI({
          label <- if (language_input() == "EN") "Background of your recipient/ personality" else "Contexte/ parcours/ personnalité de votre interlocuteur/ destinataire"
          textInput(ns("recipients_background"), label)
        })

        output$recipients_activity_ui <- renderUI({
          label <- if (language_input() == "EN") "Sector of activity of your recipient" else "Secteur d'activité de votre interlocuteur/ destinataire"
          textInput(ns("recipients_activity"), label)
        })

        output$recipients_expertise_ui <- renderUI({
          label <- if (language_input() == "EN") "Expertise of your recipient" else "Expertise de votre interlocuteur/ destinataire"
          textInput(ns("recipients_expertise"), label)
        })

        output$hierarchical_status_ui <- renderUI({
          label <- if (language_input() == "EN") "Hierarchical status of the Recipient" else "Statut hiérarchique du destinataire"
          choices <- if (language_input() == "EN") {
            c("Entry Level / Junior" = "entry_level", "Manager / Supervisor" = "manager", "Senior Manager / Department Head" = "senior_manager", "Director / Vice President" = "director", "CEO / Executive" = "ceo")
          } else {
            c("Niveau d'entrée / Débutant" = "entry_level", "Manager / Superviseur" = "manager", "Manager Senior / Chef de département" = "senior_manager", "Directeur / Vice-Président" = "director", "PDG / Exécutif" = "ceo")
          }
          selectInput(ns("hierarchical_status"), label, choices = choices, selected = "manager")
        })

        output$expectations_level_ui <- renderUI({
          label <- if (language_input() == "EN") "Level of expectations" else "Niveau d'attentes"
          choices <- if (language_input() == "EN") {
            c("Minimal" = "Minimal", "Moderate" = "Moderate", "High" = "High", "Very high" = "Very high", "Exceptionally High Expectations" = "Exceptionally High")
          } else {
            c("Minimum" = "Minimum", "Modéré" = "Modéré", "Elevé" = "Elevé", "Très élevé" = "Très élevé", "Exceptionnellement élevé" = "Exceptionnellement élevé")
          }
          radioButtons(inputId = ns("expectations_level"), label = label, choices = choices, selected = "Modéré")
        })
      }
    })

    # Return reactive values
    return(list(
      # Pitch Improver reactive inputs
      communication_context = reactive(input$communication_context),
      recipient_of_the_pitch = reactive(input$recipient_of_the_pitch),
      recipients_background = reactive(input$recipients_background),
      hierarchical_status = reactive(input$hierarchical_status),
      recipients_activity = reactive(input$recipients_activity),
      recipients_expertise = reactive(input$recipients_expertise),
      expectations_level = reactive(input$expectations_level),
      # Interview Simulator reactive inputs
      interview_format = reactive(input$interview_format),
      assessment_criteria = reactive(input$assessment_criteria),
      time_constraints = reactive(input$time_constraints),
      cultural_fit_focus = reactive(input$cultural_fit_focus),
      scenario_questions = reactive(input$scenario_questions),
      follow_up_process = reactive(input$follow_up_process),
      company_culture = reactive(input$company_culture),
      company_key_values = reactive(input$company_key_values),
      company_challenges = reactive(input$company_challenges),
      growth_phase = reactive(input$growth_phase),
      competitors = reactive(input$competitors),
      company_reputation = reactive(input$company_reputation),
      work_environment = reactive(input$work_environment),
      key_responsibilities = reactive(input$key_responsibilities),
      required_skills = reactive(input$required_skills),
      team_structure = reactive(input$team_structure),
      key_challenges = reactive(input$key_challenges),
      performance_expectations = reactive(input$performance_expectations),
      career_advancement = reactive(input$career_advancement),
      preferred_questions = reactive(input$preferred_questions),
      personality = reactive(input$personality),
      industry_experience = reactive(input$industry_experience),
      cultural_background = reactive(input$cultural_background),
      decision_power = reactive(input$decision_power),
      professional_background = reactive(input$professional_background),
      candidate_expectations = reactive(input$candidate_expectations)
    ))
  })
}


## To be copied in the UI
# mod_section_right_sidebar_tools_ui("section_right_sidebar_tools_1")

## To be copied in the server
# mod_section_right_sidebar_tools_server("section_right_sidebar_tools_1")
