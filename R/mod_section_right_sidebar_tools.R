#' section_right_sidebar_tools UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_section_right_sidebar_tools_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      style="font-size: large; align-self: center;",
      uiOutput(ns("communication_context_ui"))),
    fluidRow(
      # column(width = 6,
      uiOutput(ns("recipient_of_the_pitch_ui")),
      uiOutput(ns("recipients_background_ui")),
      uiOutput(ns("hierarchical_status_ui")),
      # ),
      # column(width = 6,
      uiOutput(ns("recipients_activity_ui")),
      uiOutput(ns("recipients_expertise_ui")),
      uiOutput(ns("expectations_level_ui"))
      # )
    )
  )
}

#' section_right_sidebar_tools Server Functions
#'
#' @noRd
mod_section_right_sidebar_tools_server <- function(id, language_input){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ### Recipient characteristics - Caractéristiques de l'interlocuteur
    observe({

      lang <- language_input()
      output$recipient_of_the_pitch_ui <- renderUI({

        label <- if (lang == "EN") "Who's your pitch addressed to?" else "À qui est adressé votre pitch ?"
        textInput(
          ns("recipient_of_the_pitch"),
          label
          # validate = function(value) {
          #   if (nchar(value) > 25 & label =="EN") {
          #     return("25 characters max.")
          #   }else if(nchar(value) > 25 & label =="FR"){
          #     return("Max 25 caractères")
          #   }
          # }
          )
      })
      output$communication_context_ui <- renderUI({
        req(lang)
        if (lang == "EN") {
          label <- "What's the context of the exchange?"
          choices <- list(
            "Application" = c("Spontaneous application" = "spontaneous_application",
                              "Reply to an offer" = "offer_reply"),
            "Interviews" = c("Phone Screening" = "phone_screening",
                             "One-on-One Interview" = "one_on_one_interview"),
            "Meetings" = c("Formal Meeting" = "first_formal_meeting",
                           "Informal Meeting" = "first_informal_meeting"),
            "Networking" = c("Networking Event" = "networking_event",
                             "Follow-up After Networking Event" = "followup_after_networking",
                             "LinkedIn Message" = "linkedin_message",
                             "Referral Introduction" = "referral_introduction"),
            "Job Offers" = c("Job Offer Acceptance" = "job_offer_acceptance",
                             "Job Offer Clarification" = "job_offer_clarification",
                             "Rejecting a Job Offer" = "rejecting_job_offer"),
            "Post-Interview" = c("Asking for Feedback" = "asking_for_feedback",
                                 "Follow-up After Interview" = "followup_after_interview")
          )
        } else if(lang == "FR") {
          label <- "Quel est le contexte de l'échange ?"
          choices <- list(
            "Candidature" = c("Candidature spontanée" = "spontaneous_application",
                              "Réponse à une offre" = "offer_reply"),
            "Entretiens" = c("Entretien téléphonique" = "phone_screening",
                             "Entretien individuel" = "one_on_one_interview"),
            "Réunions" = c("Réunion formelle" = "first_formal_meeting",
                           "Réunion informelle" = "first_informal_meeting"),
            "Réseautage" = c("Événement de réseautage" = "networking_event",
                             "Suivi après un événement de réseautage" = "followup_after_networking",
                             "Message LinkedIn" = "linkedin_message",
                             "Introduction par référence" = "referral_introduction"),
            "Offres d'emploi" = c("Acceptation d'offre d'emploi" = "job_offer_acceptance",
                                  "Clarification d'offre d'emploi" = "job_offer_clarification",
                                  "Refus d'offre d'emploi" = "rejecting_job_offer"),
            "Après l'entretien" = c("Demande de retour d'information" = "asking_for_feedback",
                                    "Suivi après l'entretien" = "followup_after_interview")
          )
        }

        selectInput(
          inputId = ns("communication_context"),
          label = label,
          choices = choices,
          selected = "first_email"
        )
      })



      output$recipients_background_ui <- renderUI({
        label <- if (lang == "EN") "Background of your recipient/ personality" else "Contexte/ parcours/ personnalité de votre interlocuteur/ destinataire"
        textInput(ns("recipients_background"), label)
      })
      output$recipients_activity_ui <- renderUI({
        label <- if (lang == "EN") "Sector of activity of your recipient" else "Secteur d'activité de votre interlocuteur/ destinataire"
        textInput(ns("recipients_activity"), label)
      })
      output$recipients_expertise_ui <- renderUI({
        label <- if (lang == "EN") "Expertise of your recipient" else "Expertise de votre interlocuteur/ destinataire"
        textInput(ns("recipients_expertise"), label)
      })

      # Reactive UI for hierarchical status and demand level
      output$hierarchical_status_ui <- renderUI({
        label <- if (language_input() == "EN") {
          "Hierarchical statuts of the Recipient"
        } else {
          "Statut hiérarchique du destinataire"
        }

        choices_en <- c(
          "Entry Level / Junior" = "entry_level",
          "Manager / Supervisor" = "manager",
          "Senior Manager / Department Head" = "senior_manager",
          "Director / Vice President" = "director",
          "CEO / Executive" = "ceo"
        )

        choices_fr <- c(
          "Niveau d'entrée / Débutant" = "entry_level",
          "Manager / Superviseur" = "manager",
          "Manager Senior / Chef de département" = "senior_manager",
          "Directeur / Vice-Président" = "director",
          "PDG / Exécutif" = "ceo"
        )

        choices <- if (language_input() == "EN") choices_en else choices_fr

        selectInput(
          ns("hierarchical_status"),
          label,
          choices = choices,
          selected = c("Manager / Superviseur" = "manager")
          )
      })


      # Reactive UI for hierarchical status and demand level
      output$expectations_level_ui <- renderUI({
        label <- if (language_input() == "EN") {
          "Level of expectations"
        } else {
          "Niveau d'attentes"
        }

        expect_choices_en <- c(
          "Minimal" = "Minimal",
          "Moderate" = "Moderate",
          "High" = "High",
          "Very high" = "Very high",
          "Exceptionally High Expectations" = "Exceptionally High"
        )

        expect_choices_fr <- c(
          "Minimum" = "Minimum",
          "Modéré" = "Modéré",
          "Elevé" = "Elevé",
          "Très élevé" = "Très élevé",
          "Exceptionnellement élevé" = "Exceptionnellement élevé"
        )


        choices <- if (language_input() == "EN") expect_choices_en else if (language_input() == "FR") expect_choices_fr

        radioButtons(inputId=ns("expectations_level"),
                     label=label,
                     choices = choices,
                     selected = "Modéré"
                     )
      })




    })





    # Define reactive expressions for the inputs created in this module
    communication_context_reactive <- reactive({
      # req(input$communication_context )
      all_choices <- c("spontaneous_application", "offer_reply", "phone_screening", "one_on_one_interview", "first_formal_meeting", "first_informal_meeting", "networking_event", "followup_after_networking", "linkedin_message", "referral_introduction", "job_offer_acceptance", "job_offer_clarification", "rejecting_job_offer", "asking_for_feedback", "followup_after_interview")
      validate(
        need(input$communication_context %in% all_choices, "Le choix sélectionné est invalide.")
      )

      input$communication_context
      })

    recipient_of_the_pitch_reactive <- reactive({
      req(language_input())

      recipient_validate <- if (language_input() == "EN") "Recipîent: 25 characters max." else if (language_input() == "FR") "Interlocuteur/ destinataire : Max 25 caractères"

      validate(
        need(
          nchar(input$recipient_of_the_pitch) <= 25,
          recipient_validate
          )
        )

      input$recipient_of_the_pitch
      })

    recipients_background_reactive <- reactive({

      recipient_bg_validate <- if (language_input() == "EN") "Recipîent's background: 25 characters max." else if (language_input() == "FR") "Contexte de l'interlocuteur/ destinataire : Max 25 caractères"

      validate(
        need(
          nchar(input$recipients_background) <= 25,
          recipient_bg_validate
        )
      )
      input$recipients_background
      })

    hierarchical_status_reactive <- reactive({
      # req(input$hierarchical_status)
      input$hierarchical_status
      })

    recipients_activity_reactive <- reactive({
      recipient_sector <- if (language_input() == "EN") "Recipîent's sector: 25 characters max." else if (language_input() == "FR") "Secteur de l'interlocuteur/ destinataire : Max 25 caractères"

      validate(
        need(
          nchar(input$recipients_activity) <= 25,
          recipient_sector
        )
      )
      input$recipients_activity
      })

    recipients_expertise_reactive <- reactive({
      # req(input$recipients_expertise)
      input$recipients_expertise
      })
    expectations_level_reactive <- reactive({ input$expectations_level
      })

    # Return these reactive expressions
    return(list(
      communication_context = communication_context_reactive,
      recipient_of_the_pitch = recipient_of_the_pitch_reactive,
      recipients_background = recipients_background_reactive,
      hierarchical_status = hierarchical_status_reactive,
      recipients_activity = recipients_activity_reactive,
      recipients_expertise = recipients_expertise_reactive,
      expectations_level = expectations_level_reactive
    ))



  })
}

## To be copied in the UI
# mod_section_right_sidebar_tools_ui("section_right_sidebar_tools_1")

## To be copied in the server
# mod_section_right_sidebar_tools_server("section_right_sidebar_tools_1")
