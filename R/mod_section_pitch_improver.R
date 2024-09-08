#' section_pitch_improver UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stringr str_remove_all str_squish
#' @importFrom tokenizers tokenize_sentences tokenize_words
#' @importFrom dplyr count filter
#' @importFrom tidytext unnest_tokens
#' @import wordcloud2

mod_section_pitch_improver_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabItem(
    tabName = "tab_pitch_improver",
    fluidRow(
      uiOutput(ns("app_description"))
    ),
    # fluidRow(
    #   width = 12,
    #   box(title = "Word Cloud", width = 12, solidHeader = TRUE, collapsible = TRUE,
    #       wordcloud2Output(ns("word_cloud_output"))
    #   )
    # ),
    fluidRow(
      # style="color: dimgray;", #background: lightgrey;
      width = 12,
      uiOutput(ns("pitch_text_area"))
    ),

    hr(class = "hresthetics"),


    fluidRow(
      box(
        title = "",
        closable = TRUE,
        width = 12,
        # height = "500px",
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          style= "width: 108%;",
          # column(
          #   width = 1
          # ),
          column(
            width = 6,
            uiOutput(ns("main_indicators_output"))
          ),
          column(
            width = 3,
            uiOutput(ns("reading_speed_slider"))
          )

        )
      )),

    fluidRow(
      width=12,
      uiOutput(ns("pitch_improver_user_text_area"))
    ),
    fluidRow(

      # autoWaiter(),
      column(width=2),
      column(width=8,
             style= "border-bottom-style: dashed;",
             uiOutput(ns("recap_prompt"))
      ),
      column(width=2)
    ),
    fluidRow(
      column(width=4,),
      column(width=4,uiOutput(ns("gpt_button_pitch_improver"))),
      column(width=4,)
    ),
    fluidRow(
      id= ns("gpt_pitch_analysis"),

      # autoWaiter(),
      uiOutput(ns("orthography_and_grammar")),
      uiOutput(ns("structure_and_coherence")),
      uiOutput(ns("potential_questions")),
      uiOutput(ns("sentiment"))
    )
  )
}

#' section_pitch_improver Server Functions
#'
#' @noRd
mod_section_pitch_improver_server <- function(id, api_pwd, language_input, communication_context_input, recipient_of_the_pitch_input, recipients_background_input, hierarchical_status_input, recipients_activity_input, recipients_expertise_input, expectations_level_input){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    api_key <- api_pwd

    admin_prompt_pitch_improver <- reactive({
      req(language_input())
      if (language_input() == "EN") {
        "Use the user's text to generate a very short report (600 characters top) on the quality of their pitch based on the information from their interlocutor. Be very attentive, critical if necessary. The report must be brief but contain actionable insights. Argument your answer with examples from user input."
      } else {
        "Utiliser le texte de l’utilisateur pour générer un très court rapport sur la qualité de son pitch. Soyez très attentif, critique si nécessaire. Le rapport doit être bref mais contenir des informations exploitables. La réponse doit être argumentée avec des exemples tirés des entrées de l'utilisateur pour qu'il puisse s'améliorer."
      }
    })

    #### TITLE

    output$app_description <- renderUI({
      # Get the current language
      lang <- language_input()

      # Switch UI based on language
      if (lang == "EN") {
        HTML('<h2 style="font-size: 16px;">Preparing your application for your future position? This application helps you improve your professional pitches in various communication contexts. You can tailor the analysis according to specific criteria such as your recipient, the level of exigency, and more. The application operates using generative artificial intelligence. By nature, responses will vary each time a report is generated. The application does not in any way replace the advice of an expert.</h2>')
      } else if (lang == "FR") {
        HTML('<h2 style="font-size: 16px;">Vous préparez votre candidature pour votre futur poste ? Cette application vous permet d\'améliorer vos pitchs professionnels dans différents contextes d\'échange. Vous pouvez calibrer l\'analyse selon des critères spécifiques tels que votre destinataire, le niveau d\'exigence, etc. L\'application fonctionne à l\'aide d\'intelligences artificielles de type génératives. Par nature, les réponses seront différentes à chaque fois qu\'un rapport est généré. L\'application ne remplace en aucun cas les avis d\'un expert.</h2>')
      } else {
        HTML('<h2 style="font-size: 18px;">Language Not Supported</h2>')
      }
    })


    output$pitch_improver_user_text_area <- renderUI({
      # Determine the label based on the selected language
      label_text <- if (language_input() == "EN") {
        "Pitch to analyze:"
      } else {
        "Pitch à analyser :"
      }

      textAreaInput(
        inputId = ns("text_input_pitch_improver"),
        label = label_text,
        rows = 9,
        width = "100%"
      )
    })

    output$gpt_button_pitch_improver <- renderUI({
      # Determine the label based on the selected language
      button_label <- if (language_input() == "EN") {
        "Start Analysis 🚀"
      } else {
        "Démarrer l'analyse 🚀"
      }

      actionButton(
        inputId = ns("gpt_update_pitch_improver_button"),
        label = button_label
      )
    })



    ######################## main indicators


    output$reading_speed_slider <- renderUI({
      req(language_input())
      label <- if (language_input() == "EN") {
        "Adjust Reading Speed (Words Per Minute):"
      } else {
        "Ajuster la vitesse de lecture (mots par minute) pour calibrer l'estimation de la durée du pitch à l'oral :"
      }

      sliderInput(ns("reading_speed"), label, value = 150, min = 90, max = 180)
    })

    english_choices_map <- c(
      "spontaneous_application" = "Spontaneous application",
      "offer_reply" = "Reply to an offer",
      "phone_screening" = "Phone Screening",
      "first_formal_meeting" = "First Formal Meeting",
      "first_informal_meeting" = "First Informal Meeting",
      "networking_event" = "Networking Event",
      "followup_after_networking" = "Follow-up After Networking Event",
      "linkedin_message" = "LinkedIn Message",
      "referral_introduction" = "Referral Introduction",
      "one_on_one_interview" = "One-on-One Interview",
      "job_offer_acceptance" = "Job Offer Acceptance",
      "job_offer_clarification" = "Job Offer Clarification",
      "rejecting_job_offer" = "Rejecting a Job Offer",
      "asking_for_feedback" = "Asking for Feedback",
      "followup_after_interview" = "Follow-up After Interview"
    )

    french_choices_map <- c(
      "spontaneous_application" = "Candidature spontanée",
      "offer_reply" = "Réponse à une offre",
      "phone_screening" = "Entretien téléphonique",
      "first_formal_meeting" = "Première réunion formelle",
      "first_informal_meeting" = "Première réunion informelle",
      "networking_event" = "Événement de réseautage",
      "followup_after_networking" = "Suivi après un événement de réseautage",
      "linkedin_message" = "Message LinkedIn",
      "referral_introduction" = "Introduction par référence",
      "one_on_one_interview" = "Entretien individuel",
      "job_offer_acceptance" = "Acceptation d'offre d'emploi",
      "job_offer_clarification" = "Clarification d'offre d'emploi",
      "rejecting_job_offer" = "Refus d'offre d'emploi",
      "asking_for_feedback" = "Demande de retour d'information",
      "followup_after_interview" = "Suivi après l'entretien"
    )



    calculate_summary <- function(text, reading_speed, language) {
      if (is.null(text) || text == "") {
        return(data.frame(
          "Total Words" = 0,
          "Total Characters" = 0,
          "Total Sentences" = 0,
          "Total Paragraphs" = 0,
          "Pitch Length (seconds)" = 0
        ))
      }

      # Function to clean text for word counting
      clean_for_words <- function(text) {
        text <- tolower(text)
        text <- stringr::str_remove_all(text, "[[:punct:]]")
        text <- stringr::str_remove_all(text, "[[:digit:]]")
        text <- stringr::str_squish(text)
        return(text)
      }

      # Original text
      original_text <- text

      # Cleaned text for word counting
      cleaned_text <- clean_for_words(text)

      # char count
      characters <- length(tokenizers::tokenize_words(cleaned_text)[[1]])

      # word count (including punctuation)
      words <- nchar(original_text)

      # characters <- stringi::stri_length(original_text)
      # characters <- stringi::stri_length(input$text_input_pitch_improver)
      # characters <- stringi::stri_length(input$text_input_pitch_improver)


      # Sentence count
      sentences <- length(tokenizers::tokenize_sentences(original_text)[[1]])

      # Paragraph count
      paragraphs <- length(stringi::stri_split_regex(original_text, "\\R{2,}")[[1]])

      pitch_length_seconds <- words / reading_speed * 60

      if (language == "EN") {
        summary_data <- data.frame(
          "Total Words" = words,
          "Total Characters" = characters,
          "Total Sentences" = sentences,
          "Total Paragraphs" = paragraphs,
          "Pitch Length (seconds)" = pitch_length_seconds
        )
      } else {
        summary_data <- data.frame(
          "Nombre total de mots" = words,
          "Nombre total de caractères" = characters,
          "Nombre total de phrases" = sentences,
          "Nombre total de paragraphes" = paragraphs,
          "Durée du pitch (secondes)" = pitch_length_seconds
        )
      }

      return(summary_data)
    }




    # Create a reactive expression to calculate summary
    reactive_summary_data <- reactive({
      req(input$reading_speed, language_input())
      calculate_summary(input$text_input_pitch_improver, input$reading_speed, language=language_input())
    })






    output$main_indicators_output <- renderUI({
      req(language_input())
      donnees <- reactive_summary_data()
      lang <- language_input()

      # Définir les étiquettes selon la langue
      labels <- if(lang == "EN") {
        c("Total Characters", "Total Words", "Total Sentences", "Total Paragraphs", "Pitch Length (s)")
      } else {
        c("Nombre de caractères", "Nombre de mots", "Nombre de phrases", "Nombre de paragraphes", "Durée du pitch (s)")
      }

      # Construire le HTML
      html_content <- paste0(
        '<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css">',
        '<div class="stats-container">',
        paste0(
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[1]], '</span>',
          '<span class="stat-label">', labels[1], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[2]], '</span>',
          '<span class="stat-label">', labels[2], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[3]], '</span>',
          '<span class="stat-label">', labels[3], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[4]], '</span>',
          '<span class="stat-label">', labels[4], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[5]], '</span>',
          '<span class="stat-label">', labels[5], '</span>',
          '</div>',
          '</div>'
        ),
        collapse = ""
      )

      HTML(html_content)
    })




    # Render the word cloud output
    # output$word_cloud_output <- renderWordcloud2({
    #   req(input$text_input_pitch_improver)  # Ensure the input field has a value
    #
    #   # Helper function to generate the word cloud
    #   generate_word_cloud <- function(text) {
    #     # Use tibble() instead of data_frame()
    #     word_data <- tibble::tibble(text = text) |>
    #       tidytext::unnest_tokens(word, text) |>
    #       dplyr::filter(nchar(word) > 3) |>
    #       dplyr::count(word, sort = TRUE) |>
    #       dplyr::filter(n > 2)  # Only words that appear more than 2 times
    #
    #     # If no words meet the criteria, return NULL
    #     if (nrow(word_data) == 0) return(NULL)
    #
    #     # Create word cloud
    #     wordcloud2::wordcloud2(word_data, size = 0.7)
    #   }
    #
    #   generate_word_cloud(input$text_input_pitch_improver)
    # })









    construct_sentence <- function(input_value, english_text, french_text, lang, is_text_input = FALSE) {
      # Check if input value is NA or NULL
      if (is.null(input_value) || is.na(input_value) || input_value == "") {
        return("")
      }

      if (is_text_input) {
        # Directly use the input value for textInput
        label <- input_value
      } else {
        # For selectInput: Use the choices map for label lookup
        choices_map <- if (lang == "EN") english_choices_map else french_choices_map
        label <- if (input_value %in% names(choices_map)) choices_map[[input_value]] else "Unknown choice"
      }

      sentence <- if (lang == "EN") {
        paste0(english_text, " ", label, ".")
      } else {
        paste0(french_text, " ", label, ".")
      }

      return(sentence)
    }

    construct_sentence_niveau <- function(input_value, english_text, french_text, lang, is_text_input = FALSE, labels = NULL) {
      if (!is.null(input_value) && input_value != "" && !is.na(input_value)) {
        if (is_text_input) {
          # Directly use the input value for textInput
          label <- input_value
        } else {
          # Use the provided labels for selectInput label lookup
          label <- if (input_value %in% names(labels)) labels[[input_value]] else "Unknown choice"
        }

        sentence <- if (lang == "EN") {
          paste0(english_text, " ", label, ".")
        } else {
          paste0(french_text, " ", label, ".")
        }
      } else {
        sentence <- ""
      }

      return(sentence)
    }


    construct_general_sentence <- function(input_value, english_text, french_text, lang, labels = NULL) {
      # Check if input value is NA, NULL, or empty
      if (is.null(input_value) || is.na(input_value) || input_value == "") {
        return("")
      }

      # Attempt to find a label for the input value
      label <- ""
      if (!is.null(labels) && input_value %in% names(labels)) {
        # If input_value matches a key in labels, use the corresponding label
        label <- labels[[input_value]]
      } else if (!is.null(labels) && input_value %in% labels) {
        # If input_value is a label itself in labels, use it directly
        label <- input_value
      } else {
        # If input_value is not in labels, use it directly (assuming it's a string)
        label <- input_value
      }

      # Construct the sentence based on the language
      sentence <- if (lang == "EN") {
        paste0(english_text, " ", label, ".")
      } else {
        paste0(french_text, " ", label, ".")
      }

      return(sentence)
    }


    construct_context_sentence <- function(input_value, english_text, french_text, lang) {
      # Check if input value is NA, NULL, or empty
      if (is.null(input_value) || is.na(input_value) || input_value == "") {
        return("")
      }

      # Directly use the input value (selected value) for communication context
      label <- input_value

      # Construct the sentence based on the language
      sentence <- if (lang == "EN") {
        paste0(english_text, " ", label, ".")
      } else {
        paste0(french_text, " ", label, ".")
      }

      return(sentence)
    }







    ############## labels

    hierarchical_status_labels_en <- c(
      "entry_level" = "Entry Level / Junior",
      "manager" = "Manager / Supervisor",
      "senior_manager" = "Senior Manager / Department Head",
      "director" = "Director / Vice President",
      "ceo" = "CEO / Executive"
    )

    hierarchical_status_labels_fr <- c(
      "entry_level" = "Niveau d'entrée / Débutant",
      "manager" = "Manager / Superviseur",
      "senior_manager" = "Manager Senior / Chef de département",
      "director" = "Directeur / Vice-Président",
      "ceo" = "PDG / Exécutif"
    )

    expect_choices_en <- c(
      "Minimal" = "Minimal",
      "Moderate" = "Moderate",
      "High" = "High",
      "Very high" = "Very high",
      "Exceptionally High Expectations" = "Exceptionally High Expectations"
    )

    expect_choices_fr <- c(
      "Minimum" = "Minimum",
      "Modéré" = "Modéré",
      "Elevé" = "Elevé",
      "Très élevé" = "Très élevé",
      "Exceptionnellement élevé" = "Exceptionnellement élevé"
    )






    # Define a reactive expression for constructing the prompt summary
    prompt_summary_for_user <- reactive({
      # Construct sentences based on inputs
      lang <- language_input()
      # Use the appropriate label set based on the selected language
      hierarchical_status_labels <- if (lang == "EN") hierarchical_status_labels_en else hierarchical_status_labels_fr
      expect_choices_labels <- if (lang == "EN") expect_choices_en else expect_choices_fr

      # Construct sentences
      sentences <- c(
        construct_sentence(communication_context_input(), "<b>The context of the interaction</b>: ", "<br><b>Le contexte de l'interaction</b> : ", lang),
        construct_sentence(recipient_of_the_pitch_input(), "<br><b>Pitch recipient</b>: ", "<br><b>Le pitch s'adresse à</b> : ", lang, TRUE),
        construct_sentence(recipients_background_input(), "<br><b>Personality or background of the recipient of the pitch</b>: ", "<br><b>Personnalité ou parcours du destinataire</b> :", lang, TRUE),
        construct_sentence(recipients_activity_input(), "<br><b>Activity sector of the recipient</b>: ", "<br><b>Secteur d'activité du destinataire</b> : ", lang, TRUE),
        construct_sentence(recipients_expertise_input(), "<br><b>Expertise of the recipient</b>: ", "<br><b>Expertise du destinataire</b> : ", lang, TRUE),
        construct_sentence_niveau(hierarchical_status_input(), "<br><b>Hierarchical status of the recipient</b>: ", "<br><b>Statut hiérarchique du destinataire</b> : ", lang, FALSE, hierarchical_status_labels),
        construct_general_sentence(expectations_level_input(), "<br><b>Level of expectations</b>: ", "<br><b>Niveau d'exigence</b> : ", lang, expect_choices_labels)
      )


      # Other code remains unchanged
      # Filter out empty sentences
      sentences_with_content <- sentences[sentences != ""]

      # Ensure input$text_input_pitch_improver is not NA
      user_input <- ifelse(is.na(input$text_input_pitch_improver), "", input$text_input_pitch_improver)

      # Construct the final message
      final_message_for_api <- paste(c(sentences_with_content, "<br><b>Pitch</b>: " , user_input), collapse = " ")

      if (nchar(final_message_for_api) > 0) {
        final_message_for_api
      } else {
        "<p>No content yet.</p>"
      }

    })


    output$recap_prompt <- renderUI({
      # Check if prompt_summary_for_user() is not NULL or empty
      if (!is.null(prompt_summary_for_user()) && nchar(prompt_summary_for_user()) > 0) {
        # Render the prompt summary as HTML
        tagList(
          HTML(prompt_summary_for_user()),
          tags$script('showTextSlowly();')
        )
      } else {
        # Render a placeholder or informative message when summary is NULL or empty
        HTML("<p>No prompt summary available yet.</p>")
      }
    })


    responses <- reactiveValues(
      ortography_and_grammar = NULL,
      structure_and_coherence = NULL,
      potential_questions = NULL,
      sentiment = NULL
    )



    observeEvent(input$gpt_update_pitch_improver_button, {

      # Define reactive values for each response
      user_input <- prompt_summary_for_user()
      lang <- language_input()

      # Define admin prompts for orthography and grammar
      admin_prompt_orthography_and_grammar <- reactive({
        if(lang == "EN") {
          "Analyze only the grammar and orthography of the user's pitch and give insights only on what needs to be changed. No other aspects. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters."
        } else {
          "Analyse uniquement la grammaire et l'orthographe du pitch de l'utilisateur et fais un retour uniquement sur les aspects à modifier. Aucun autre aspect. Ton neutre. Utilise des bullets points, des sauts de ligne ou du HTML pour améliorer la lisibilité. Limiter la réponse à 400 caractères."
        }
      })

      # Define admin prompts for logical structure and coherence
      admin_prompt_structure_and_coherence <- reactive({
        if(lang == "EN") {
          "Analyze only the structure of the user's pitch. The pitch must be logically organized, without contradictions or ambiguities. No other aspects. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters."
        } else {
          "Analyse la structure, la clarté, la cohérence et le sens du pitch fourni par l'utilisateur. Aucun autre aspect. Ton neutre. Utilisez le HTML ou des sauts de ligne pour la lisibilité. Limite de 400 caractères."
        }
      })

      # Define admin prompts for potential questions
      admin_prompt_potential_questions <- reactive({
        if(lang == "EN") {
          "Generate questions the recipient might have after receiving the user's pitch. Nothing else. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 500 characters."
        } else {
          "Génère des questions que le destinataire pourrait avoir après avoir reçu le pitch de l'utilisateur. Aucune autre chose. Ton neutre. Formate le texte pour inclure des sauts de ligne ou de la police en gras pour améliorer la lisibilité. Limiter la réponse à 500 caractères."
        }
      })

      # Define admin prompts for emotional valence
      admin_prompt_sentiment_response <- reactive({
        if(lang == "EN") {
          "Analyze the emotional valence of the user's text. Nothing else. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters."
        } else {
          "Analyse la valence émotionnelle du texte de l'utilisateur. Aucune autre chose. Ton neutre. Formate le texte pour inclure des sauts de ligne ou de la police en gras pour améliorer la lisibilité. Limiter la réponse à 400 caractères."
        }
      })

      showModal(
        modalDialog("Analyse de l'orthographe et de la grammaire en cours...",
                    easyClose = FALSE)
      )

      future({
        fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_orthography_and_grammar(), "gpt-4o")
      }) %...>% {
        responses$ortography_and_grammar <- .
        print("Orthography and Grammar response received")
      }

      removeModal()


      showModal(
        modalDialog("Analyse de la structure en cours...",
                    easyClose = FALSE)
      )

      future({
        fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_structure_and_coherence(), "gpt-4o")
      }) %...>% {
        responses$structure_and_coherence <- .
        print("Structure and Coherence response received")
      }

      removeModal()

      showModal(
        modalDialog("Générant des questions éventuelles...",
                    easyClose = FALSE)
      )

      future({
        fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_potential_questions(), "gpt-3.5-turbo")
      }) %...>% {
        responses$potential_questions <- .
        print("Potential Questions response received")
      }


      removeModal()

      showModal(
        modalDialog("Analyse de la valence émotionnelle...",
                    easyClose = FALSE)
      )

      future({
        fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_sentiment_response(), "gpt-3.5-turbo")
      }) %...>% {
        responses$sentiment <- .
        print("Sentiment Analysis response received")
      }


      removeModal()

    })

    ####################

    output$orthography_and_grammar <- renderUI({
      req(responses$ortography_and_grammar)
      column(width=12,
             h3("Orthographe et grammaire"),
             HTML(responses$ortography_and_grammar)
      )
    })

    output$structure_and_coherence <- renderUI({
      req(responses$structure_and_coherence)
      column(width=12,
             h3("Structure et cohérence"),
             HTML(responses$structure_and_coherence)
      )
    })

    output$potential_questions <- renderUI({
      req(responses$potential_questions)
      column(width=12,
             h3("Questions éventuelles"),
             HTML(responses$potential_questions)
      )
    })

    output$sentiment <- renderUI({
      req(responses$sentiment)
      column(width=12,
             h3("Valence émotionnelle"),
             HTML(responses$sentiment)
      )
    })

    #############






  })
}

## To be copied in the UI
# mod_section_pitch_improver_ui("section_pitch_improver_1")

## To be copied in the server
# mod_section_pitch_improver_server("section_pitch_improver_1")
