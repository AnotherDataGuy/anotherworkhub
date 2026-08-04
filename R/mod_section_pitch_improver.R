#' section_pitch_improver UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList debounce
#' @importFrom promises then
#' @importFrom future future
#' @importFrom stringr str_remove_all str_squish
#' @importFrom tokenizers tokenize_sentences tokenize_words
#' @importFrom htmltools htmlEscape
mod_section_pitch_improver_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css", crossorigin = "anonymous"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML", crossorigin = "anonymous")
    ),
    shinyjs::useShinyjs(),

    div(
      class = "pitch-improver-container",
      uiOutput(ns("app_description")),

      fluidRow(
        class = "pitch-layout awh-split-layout",
        column(
          width = 12,
          class = "pitch-setup-col awh-col-narrow",
          div(
            class = "pitch-setup-panel awh-panel",
            uiOutput(ns("required_fields_header")),
            div(
              class = "compact-field",
              uiOutput(ns("context_label_ui")),
              uiOutput(ns("communication_context_ui"))
            ),
            div(
              class = "compact-field",
              uiOutput(ns("recipient_label_ui")),
              uiOutput(ns("recipient_of_the_pitch_ui"))
            ),
            div(
              class = "compact-field",
              uiOutput(ns("hierarchical_status_ui"))
            ),
            div(
              class = "compact-field expectations-section",
              uiOutput(ns("expectations_section_header")),
              uiOutput(ns("expectations_grid"))
            ),
            div(class = "optional-toggle", uiOutput(ns("optional_fields_header"))),
            shinyjs::hidden(
              div(
                id = ns("optional_fields"),
                class = "optional-fields-panel",
                div(class = "compact-field", uiOutput(ns("background_label_ui")), uiOutput(ns("recipients_background_ui"))),
                div(class = "compact-field", uiOutput(ns("activity_label_ui")), uiOutput(ns("recipients_activity_ui"))),
                div(class = "compact-field", uiOutput(ns("expertise_label_ui")), uiOutput(ns("recipients_expertise_ui")))
              )
            )
          )
        ),

        column(
          width = 12,
          class = "pitch-workspace-col awh-col-wide",
          div(
            class = "pitch-workspace-panel awh-panel",
            div(class = "compact-field", uiOutput(ns("gpt_language_ui"))),
            div(class = "compact-field", uiOutput(ns("pitch_improver_user_text_area"))),
            uiOutput(ns("pitch_char_progress")),
            uiOutput(ns("main_indicators_output")),
            div(class = "pitch-recap-card", uiOutput(ns("recap_prompt"))),
            div(
              class = "analyze-button",
              uiOutput(ns("gpt_button_pitch_improver")),
              uiOutput(ns("informative_message"))
            )
          ),
          div(
            id = ns("gpt_pitch_analysis"),
            class = "analysis-results-grid",
            uiOutput(ns("orthography_and_grammar")),
            uiOutput(ns("structure_and_coherence")),
            uiOutput(ns("potential_questions")),
            uiOutput(ns("sentiment"))
          )
        )
      )
    )
  )
}

#' section_pitch_improver Server Functions
#'
#' @noRd
mod_section_pitch_improver_server <- function(id, api_pwd, language_input, translations, usage_guard = NULL) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    api_key <- api_pwd
    p18n <- translations$pitch
    security_limits_cfg <- if (is.null(usage_guard)) security_limits() else usage_guard$get_limits()

    rv <- reactiveValues(
      hierarchical_status = "entry_level",
      expectations_level = NULL,
      analysis_in_flight = FALSE
    )

    english_choices_map <- translations$english_choices_map
    french_choices_map <- translations$french_choices_map
    hierarchical_status_labels_en <- translations$hierarchical_status_labels_en
    hierarchical_status_labels_fr <- translations$hierarchical_status_labels_fr
    expect_choices_en <- translations$expect_choices_en
    expect_choices_fr <- translations$expect_choices_fr

    # Required Fields Header
    output$required_fields_header <- renderUI({
      div(
        class = "required-fields-header",
        tags$i(class = "fa fa-asterisk"),
        span(t_lang(p18n$required_fields, language_input()))
      )
    })

    # Context Label
    output$context_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-handshake-o"),
        span(t_lang(p18n$context_label, language_input()))
      )
    })

    # Recipient Label
    output$recipient_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-user"),
        span(t_lang(p18n$recipient_label, language_input()))
      )
    })

    # Hierarchical Label
    output$hierarchical_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-sitemap"),
        span(t_lang(p18n$hierarchical_label, language_input()))
      )
    })

    # Optional Fields Header
    output$optional_fields_header <- renderUI({
      actionLink(
        ns("toggle_optional"),
        span(
          tags$i(class = "fa fa-plus-circle", style = "color: #3498db; margin-right: 8px;"),
          tags$span(
            style = "color: #3498db; font-weight: 500;",
            t_lang(p18n$optional_toggle, language_input())
          )
        ),
        style = "text-decoration: none;"
      )
    })

    # Background Label
    output$background_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-id-card"),
        span(t_lang(p18n$background_label, language_input()))
      )
    })

    # Activity Label
    output$activity_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-building"),
        span(t_lang(p18n$activity_label, language_input()))
      )
    })

    # Expertise Label
    output$expertise_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-graduation-cap"),
        span(t_lang(p18n$expertise_label, language_input()))
      )
    })

    output$expectations_section_header <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fas fa-star"),
        span(t_lang(p18n$expectations_label, language_input()))
      )
    })

    output$expectations_grid <- renderUI({
      lang <- language_input()
      default_level <- if (lang == "ENG") "High" else "Elev\u00e9"
      selected <- input$expectations_level %||% rv$expectations_level %||% default_level
      levels <- p18n$expectations_levels[[lang]]

      div(
        class = "expectations-grid",
        lapply(levels, function(level) {
          div(
            class = if (identical(selected, level$value)) "expectation-card selected" else "expectation-card",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              ns("expectations_level"),
              level$value
            ),
            span(class = "expectation-label", level$label),
            span(class = "expectation-description", level$desc)
          )
        })
      )
    })



    # Debounce expensive pitch-text derived reactives (summary / recap).
    pitch_text_debounced <- shiny::debounce(
      reactive(input$text_input_pitch_improver %||% ""),
      millis = 300
    )

    # Create a reactive value to track input validity
    input_is_valid <- reactive({
      !is.null(input$communication_context) &&
        !is.null(input$recipient_of_the_pitch) &&
        !is.null(input$hierarchical_status) &&
        nchar(pitch_text_debounced()) > 100
    })

    output$informative_message <- renderUI({
      if (!input_is_valid()) {
        lang <- language_input()
        message <- t_lang(p18n$validation_message, lang)
        div(class = "informative-message", message)
      }
    })



    # Reactive value to store the selected GPT response language
    gpt_language_input <- reactive({
      req(input$gpt_language_messages)
      input$gpt_language_messages
    })

    # Dynamic GPT Language Selection UI
    output$gpt_language_ui <- renderUI({
      lang <- language_input()

      selected_lang <- if (lang == "ENG") "ENG" else "FR"

      selectInput(
        ns("gpt_language_messages"),
        label = t_lang(p18n$gpt_language_label, lang),
        choices = p18n$gpt_language_choices[[lang]],
        width = "100%",
        selected = selected_lang
      )
    })

    output$app_description <- renderUI({
      lang <- language_input()
      div(
        class = "section-hero pitch-hero",
        div(class = "section-hero-icon", tags$i(class = "fas fa-bullhorn")),
        div(
          class = "section-hero-text",
          h2(t_lang(p18n$hero_title, lang), class = "section-hero-title"),
          p(t_lang(p18n$hero_subtitle, lang), class = "section-hero-subtitle")
        )
      )
    })

    output$pitch_char_progress <- renderUI({
      lang <- language_input()
      text_len <- nchar(pitch_text_debounced())
      target <- 100
      max_chars <- security_limits_cfg$max_pitch_chars %||% 4000L
      pct <- min(100, round((text_len / target) * 100))
      over_max <- text_len > max_chars

      div(
        class = "char-progress",
        div(
          class = "char-progress-header",
          span(t_lang(p18n$char_progress_label, lang)),
          span(
            class = if (over_max) {
              "char-progress-count over"
            } else if (text_len >= target) {
              "char-progress-count ready"
            } else {
              "char-progress-count"
            },
            sprintf("%d / %d (max %d)", text_len, target, max_chars)
          )
        ),
        div(
          class = "char-progress-track",
          div(class = "char-progress-fill", style = sprintf("width: %d%%;", pct))
        )
      )
    })

    # User text area
    output$pitch_improver_user_text_area <- renderUI({
      lang <- language_input() %||% "FR"
      label_text <- t_lang(p18n$pitch_label, lang)
      default_text <- t_lang(p18n$pitch_default, lang)
      sample_texts <- c(
        t_lang(p18n$pitch_default, "FR"),
        t_lang(p18n$pitch_default, "ENG")
      )
      current <- isolate(input$text_input_pitch_improver)
      value <- if (
        is.null(current) ||
          !nzchar(trimws(current)) ||
          current %in% sample_texts
      ) {
        default_text
      } else {
        current
      }

      textAreaInput(
        inputId = ns("text_input_pitch_improver"),
        label = label_text,
        value = value,
        rows = 9,
        width = "100%"
      )
    })

    # GPT button
    output$gpt_button_pitch_improver <- renderUI({
      button_label <- paste0(t_lang(p18n$analyze_button, language_input()), " \U0001f680")

      disabled <- !input_is_valid()

      actionButton(
        inputId = ns("gpt_update_pitch_improver_button"),
        label = button_label,
        disabled = disabled
      )
    })

    output$recipient_of_the_pitch_ui <- renderUI({
      div(
        class = "pitch-recipient-input-wrapper",
        textInput(
          inputId = ns("recipient_of_the_pitch"),
          label = NULL,
          placeholder = t_lang(p18n$recipient_placeholder, language_input())
        ) %>% tagAppendAttributes(class = "pitch-recipient-input")
      )
    })

    # Communication context
    output$communication_context_ui <- renderUI({
      div(
        class = "context-select-wrapper",
        selectInput(
          inputId = ns("communication_context"),
          label = NULL,
          width = "100%",
          choices = if(language_input() == "ENG") {
            list(
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
          } else {
            list(
              "Candidature" = c(
                "Candidature spontan\u00e9e" = "spontaneous_application",
                "R\u00e9ponse \u00e0 une offre" = "offer_reply"
              ),
              "Entretiens" = c(
                "Entretien t\u00e9l\u00e9phonique" = "phone_screening",
                "Entretien individuel" = "one_on_one_interview"
              ),
              "R\u00e9unions" = c(
                "R\u00e9union formelle" = "first_formal_meeting",
                "R\u00e9union informelle" = "first_informal_meeting"
              ),
              "R\u00e9seautage" = c(
                "\u00c9v\u00e9nement de r\u00e9seautage" = "networking_event",
                "Suivi apr\u00e8s un \u00e9v\u00e9nement de r\u00e9seautage" = "followup_after_networking"
              ),
              "Offres d'emploi" = c(
                "Acceptation d'offre d'emploi" = "job_offer_acceptance",
                "Clarification d'offre d'emploi" = "job_offer_clarification",
                "Refus d'offre d'emploi" = "rejecting_job_offer"
              ),
              "Post-Entretien" = c(
                "Demande de retour d'information" = "asking_for_feedback",
                "Suivi apr\u00e8s l'entretien" = "followup_after_interview"
              )
            )
          },
          selected = "spontaneous_application"
        ) %>% tagAppendAttributes(class = "context-select")
      )
    })



    # Recipient's background
    output$recipients_background_ui <- renderUI({
      div(
        class = "form-group",
        tags$label(
          class = "input-label",
          t_lang(p18n$background_label, language_input())
        ),
        textInput(ns("recipients_background"), label = NULL)
      )
    })

    # Recipient's activity sector
    output$recipients_activity_ui <- renderUI({
      div(
        class = "form-group",
        tags$label(
          class = "input-label",
          t_lang(p18n$activity_label, language_input())
        ),
        textInput(ns("recipients_activity"), label = NULL)
      )
    })

    # Recipient's expertise
    output$recipients_expertise_ui <- renderUI({
      div(
        class = "form-group",
        tags$label(
          class = "input-label",
          t_lang(p18n$expertise_label, language_input())
        ),
        textInput(ns("recipients_expertise"), label = NULL)
      )
    })





    observe({
      req(input$hierarchical_status)
      rv$hierarchical_status <- input$hierarchical_status
    })

    observeEvent(input$expectations_level, {
      rv$expectations_level <- input$expectations_level
    }, ignoreNULL = FALSE)

    output$hierarchical_status_ui <- renderUI({
      selected_status <- input$hierarchical_status %||% "entry_level"

      choices <- p18n$hierarchical_roles[[language_input()]]

      tagList(
        tags$div(
          class = "form-group",
          uiOutput(ns("hierarchical_label_ui")),
          tags$div(
            class = "avatar-grid",
            lapply(names(choices), function(value) {
              tags$div(
                class = if(value == selected_status) "avatar-item selected" else "avatar-item",
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("hierarchical_status"), value),
                tags$div(
                  class = "avatar-circle",
                  tags$i(class = paste0("fas fa-", choices[[value]]$icon))
                ),
                tags$div(
                  class = "avatar-label",
                  choices[[value]]$label
                )
              )
            })
          )
        )
      )
    })

    reactive_summary_data <- reactive({
      req(language_input())
      calculate_summary(pitch_text_debounced(), language = language_input())
    })

    observeEvent(input$toggle_optional, {
      shinyjs::toggle(id = "optional_fields", anim = TRUE)
    })

    output$main_indicators_output <- renderUI({
      req(language_input())
      donnees <- reactive_summary_data()
      lang <- language_input()

      # D\u00e9finir les \u00e9tiquettes selon la langue
      labels <- p18n$stats_labels[[lang]]

      # Construire le HTML
      icons <- c("fa-font", "fa-align-left", "fa-paragraph", "fa-layer-group")
      html_content <- paste0(
        '<div class="stats-container">',
        paste0(
          vapply(seq_along(labels), function(i) {
            paste0(
              '<div class="stat-item">',
              '<div class="stat-icon"><i class="fas ', icons[i], '"></i></div>',
              '<div class="stat-background"></div>',
              '<span class="stat-value">', donnees[[i]], '</span>',
              '<span class="stat-label">', labels[i], '</span>',
              '</div>'
            )
          }, character(1)),
          collapse = ""
        ),
        '</div>'
      )

      HTML(html_content)
    })







    # Define a reactive expression for constructing the prompt summary
    prompt_summary_for_user <- reactive({
      lang <- language_input()
      hierarchical_status_labels <- if (lang == "ENG") translations$hierarchical_status_labels_en else translations$hierarchical_status_labels_fr
      expect_choices_labels <- if (lang == "ENG") translations$expect_choices_en else translations$expect_choices_fr

      sentences <- c(
        construct_sentence(
          input$communication_context,
          "<b>The context of the interaction</b>: ",
          "<br><b>Le contexte de l'interaction</b> : ",
          lang,
          FALSE,
          translations
        ),
        construct_sentence(
          input$recipient_of_the_pitch,
          "<br><b>Pitch recipient</b>: ",
          "<br><b>Le pitch s'adresse \u00e0</b> : ",
          lang,
          TRUE,
          translations
        ),
        construct_sentence(
          input$recipients_background,
          "<br><b>Personality or background of the recipient of the pitch</b>: ",
          "<br><b>Personnalit\u00e9 ou parcours du destinataire</b> :",
          lang,
          TRUE,
          translations
        ),
        construct_sentence(
          input$recipients_activity,
          "<br><b>Activity sector of the recipient</b>: ",
          "<br><b>Secteur d'activit\u00e9 du destinataire</b> : ",
          lang,
          TRUE,
          translations
        ),
        construct_sentence(
          input$recipients_expertise,
          "<br><b>Expertise of the recipient</b>: ",
          "<br><b>Expertise du destinataire</b> : ",
          lang,
          TRUE,
          translations
        ),
        construct_sentence_niveau(
          input$hierarchical_status,
          "<br><b>Hierarchical status of the recipient</b>: ",
          "<br><b>Statut hi\u00e9rarchique du destinataire</b> : ",
          lang,
          FALSE,
          hierarchical_status_labels,
          unknown_label = t_lang(p18n$unknown_choice, lang)
        ),
        construct_general_sentence(
          input$expectations_level %||% if (lang == "ENG") "High" else "Elev\u00e9",
          "<br><b>Level of expectations</b>: ",
          "<br><b>Niveau d'exigence</b> : ",
          lang,
          expect_choices_labels
        )
      )
      sentences_with_content <- sentences[sentences != ""]
      final_message_for_api <- paste(
        c(sentences_with_content, "<br><b>Pitch</b>: ", pitch_text_debounced()),
        collapse = " "
      )
      final_message_for_api
    })



    # Recap prompt (escape free-text pitch to avoid XSS from user content)
    output$recap_prompt <- renderUI({
      lang <- language_input()
      summary <- prompt_summary_for_user()
      if (!is.null(summary) && nchar(summary) > 0) {
        safe_summary <- htmltools::htmlEscape(summary)
        # Allow the intentional <br>/<b> markers from construct_sentence helpers
        safe_summary <- gsub("&lt;br&gt;", "<br>", safe_summary, fixed = TRUE)
        safe_summary <- gsub("&lt;b&gt;", "<b>", safe_summary, fixed = TRUE)
        safe_summary <- gsub("&lt;/b&gt;", "</b>", safe_summary, fixed = TRUE)
        tagList(
          HTML(safe_summary),
          tags$script("showTextSlowly();")
        )
      } else {
        HTML(sprintf("<p>%s</p>", htmltools::htmlEscape(t_lang(p18n$recap_empty, lang))))
      }
    })


    responses <- reactiveValues(
      orthography_and_grammar = NULL,
      structure_and_coherence = NULL,
      potential_questions = NULL,
      sentiment = NULL
    )



    observeEvent(input$gpt_update_pitch_improver_button, {
      req(input_is_valid())
      user_input <- prompt_summary_for_user()
      pitch_text <- input$text_input_pitch_improver %||% ""
      lang <- gpt_language_input()
      ui_lang <- language_input()

      if (!nzchar(api_key %||% "")) {
        showNotification(
          t_lang(p18n$notify_api_key_missing, ui_lang),
          type = "error",
          duration = NULL
        )
        return()
      }

      if (!is.null(usage_guard)) {
        budget <- usage_guard$check_pitch_run(pitch_text)
        if (!isTRUE(budget$ok)) {
          showNotification(
            usage_guard_message(budget, translations, ui_lang, feature = "pitch"),
            type = "warning",
            duration = 8
          )
          return()
        }
        usage_guard$begin_pitch_run()
      }
      rv$analysis_in_flight <- TRUE

      # Define admin prompts
      admin_prompt_orthography_and_grammar <- reactive({
        if(lang == "ENG") {
          "Analyze only the grammar and orthography of the user's pitch and give insights only on what needs to be changed or improved. No other aspects. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "Analyse uniquement la grammaire et l'orthographe du pitch de l'utilisateur et fais un retour uniquement sur les aspects \u00e0 modifier ou \u00e0 am\u00e9liorer. Aucun autre aspect. Ton neutre. Utilise des bullets points, des sauts de ligne ou du HTML pour am\u00e9liorer la lisibilit\u00e9. Limiter la r\u00e9ponse \u00e0 400 caract\u00e8res. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      admin_prompt_structure_and_coherence <- reactive({
        if(lang == "ENG") {
          "Analyze only the structure of the user's pitch. The pitch must be logically organized, without contradictions or ambiguities. No other aspects. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "Analyse la structure, la clart\u00e9, la coh\u00e9rence et le sens du pitch fourni par l'utilisateur. Aucun autre aspect. Ton neutre. Utilisez le HTML ou des sauts de ligne pour la lisibilit\u00e9. Limite de 400 caract\u00e8res. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      admin_prompt_potential_questions <- reactive({
        if(lang == "ENG") {
          "Generate questions the recipient might have after receiving the user's pitch. Nothing else. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 500 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "G\u00e9n\u00e8re des questions que le destinataire pourrait avoir apr\u00e8s avoir re\u00e7u le pitch de l'utilisateur. Aucune autre chose. Ton neutre. Formate le texte pour inclure des sauts de ligne ou de la police en gras pour am\u00e9liorer la lisibilit\u00e9. Limiter la r\u00e9ponse \u00e0 500 caract\u00e8res. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      admin_prompt_sentiment_response <- reactive({
        if(lang == "ENG") {
          "Analyze the emotional valence of the user's text. Nothing else. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "Analyse la valence \u00e9motionnelle du texte de l'utilisateur. Aucune autre chose. Ton neutre. Formate le texte pour inclure des sauts de ligne ou de la police en gras pour am\u00e9liorer la lisibilit\u00e9. Limiter la r\u00e9ponse \u00e0 400 caract\u00e8res. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      finish_pitch_run <- function(success = TRUE) {
        rv$analysis_in_flight <- FALSE
        if (!is.null(usage_guard)) {
          if (isTRUE(success)) {
            usage_guard$record_api_calls(security_limits_cfg$pitch_batch_cost %||% 4L)
          }
          usage_guard$end_pitch_run(success = success)
        }
        removeModal()
      }

      # Orthography and Grammar Analysis
      showModal(modalDialog(
        t_lang(p18n$modal_orthography, ui_lang),
        easyClose = FALSE
      ))

      future::future({
        fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_orthography_and_grammar(), "gpt-4o-mini")
      }) %>%
        promises::then(
          function(result) {
            responses$orthography_and_grammar <- result
            removeModal()

            # Structure Analysis
            showModal(modalDialog(
              t_lang(p18n$modal_structure, ui_lang),
              easyClose = FALSE
            ))

            future::future({
              fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_structure_and_coherence(), "gpt-4o-mini")
            }) %>%
              promises::then(
                function(result) {
                  responses$structure_and_coherence <- result
                  removeModal()

                  # Potential Questions Analysis
                  showModal(modalDialog(
                    t_lang(p18n$modal_questions, ui_lang),
                    easyClose = FALSE
                  ))

                  future::future({
                    fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_potential_questions(), "gpt-4o-mini")
                  }) %>%
                    promises::then(
                      function(result) {
                        responses$potential_questions <- result
                        removeModal()

                        # Sentiment Analysis
                        showModal(modalDialog(
                          t_lang(p18n$modal_sentiment, ui_lang),
                          easyClose = FALSE
                        ))

                        future::future({
                          fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_sentiment_response(), "gpt-4o-mini")
                        }) %>%
                          promises::then(
                            function(result) {
                              responses$sentiment <- result
                              finish_pitch_run(success = TRUE)
                            },
                            onRejected = function(e) {
                              finish_pitch_run(success = FALSE)
                              showNotification(conditionMessage(e), type = "error", duration = NULL)
                            }
                          )
                      },
                      onRejected = function(e) {
                        finish_pitch_run(success = FALSE)
                        showNotification(conditionMessage(e), type = "error", duration = NULL)
                      }
                    )
                },
                onRejected = function(e) {
                  finish_pitch_run(success = FALSE)
                  showNotification(conditionMessage(e), type = "error", duration = NULL)
                }
              )
          },
          onRejected = function(e) {
            finish_pitch_run(success = FALSE)
            showNotification(conditionMessage(e), type = "error", duration = NULL)
          }
        )
    })

    ####################

    render_analysis_card <- function(title, icon_name, content) {
      div(
        class = "analysis-card",
        div(
          class = "analysis-card-header",
          tags$i(class = paste0("fas fa-", icon_name)),
          title
        ),
        div(class = "analysis-card-body", renderMarkdown(content))
      )
    }

    output$orthography_and_grammar <- renderUI({
      req(responses$orthography_and_grammar)
      title <- t_lang(p18n$analysis_orthography, language_input())
      render_analysis_card(title, "spell-check", responses$orthography_and_grammar)
    })

    output$structure_and_coherence <- renderUI({
      req(responses$structure_and_coherence)
      title <- t_lang(p18n$analysis_structure, language_input())
      render_analysis_card(title, "project-diagram", responses$structure_and_coherence)
    })

    output$potential_questions <- renderUI({
      req(responses$potential_questions)
      title <- t_lang(p18n$analysis_questions, language_input())
      render_analysis_card(title, "question-circle", responses$potential_questions)
    })

    output$sentiment <- renderUI({
      req(responses$sentiment)
      title <- t_lang(p18n$analysis_sentiment, language_input())
      render_analysis_card(title, "heart", responses$sentiment)
    })

    for (out_id in c(
      "required_fields_header", "context_label_ui", "recipient_label_ui",
      "hierarchical_label_ui", "optional_fields_header", "background_label_ui",
      "activity_label_ui", "expertise_label_ui", "expectations_section_header",
      "expectations_grid", "informative_message", "gpt_language_ui",
      "app_description", "pitch_char_progress", "pitch_improver_user_text_area",
      "gpt_button_pitch_improver", "recipient_of_the_pitch_ui",
      "communication_context_ui", "recipients_background_ui",
      "recipients_activity_ui", "recipients_expertise_ui",
      "hierarchical_status_ui", "main_indicators_output", "recap_prompt",
      "orthography_and_grammar", "structure_and_coherence", "potential_questions",
      "sentiment"
    )) {
      outputOptions(output, out_id, suspendWhenHidden = FALSE)
    }

  })
}




## To be copied in the UI
# mod_section_pitch_improver_ui("section_pitch_improver_1")

## To be copied in the server
# mod_section_pitch_improver_server("section_pitch_improver_1")
