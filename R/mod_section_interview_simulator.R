#' Interview Simulator Module
#'
#' @description A Shiny module for conducting simulated interviews
#' @param id The module ID
#' @param api_pwd API password
#' @param language_input Reactive expression for language selection
#' @param translations Translation bundle from `get_translations()`
#'
#' @import shiny
#' @import shinyjs
#' @import httr
#' @import jsonlite
#' @importFrom promises then
#' @importFrom utils modifyList
#' @importFrom htmltools htmlEscape
#'
#' @noRd

# UI Function ---------------------------------------------------------------

mod_section_interview_simulator_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "interview-simulator-section",
    shinyjs::useShinyjs(),

    uiOutput(ns("main_header")),

    fluidRow(
      id = ns("interview_layout"),
      class = "interview-layout awh-split-layout",
      column(
        width = 12,
        id = ns("setup_col"),
        class = "interview-setup-col awh-col-narrow",
        div(
          class = "interview-setup-panel awh-panel",
          div(
            class = "setup-panel-toolbar",
            uiOutput(ns("toggle_setup_ui"), inline = TRUE)
          ),
          uiOutput(ns("setup_collapsed_summary")),
          div(
            class = "setup-panel-body",
            uiOutput(ns("setup_progress")),
            uiOutput(ns("basic_information")),
            div(class = "compact-field", uiOutput(ns("gpt_language_ui"))),
            uiOutput(ns("interview_details")),
            div(
              class = "interview-action-bar",
              actionButton(
                ns("start_interview"),
                label = tagList(icon("play"), tags$span(class = "start-label", "\u2026")),
                class = "btn-interview btn-interview-primary"
              ),
              uiOutput(ns("restart_interview"))
            )
          )
        )
      ),
      column(
        width = 12,
        class = "interview-chat-col awh-col-wide",
        div(
          id = ns("interview_chat_wrap"),
          class = "interview-chat-wrap awh-panel",
          div(
            class = "chat-panel-header",
            div(
              class = "chat-panel-header-main",
              icon("comments"),
              uiOutput(ns("chat_header_title"), inline = TRUE)
            ),
            uiOutput(ns("chat_feedback_progress"))
          ),
          div(
            class = "chat-container",
            id = ns("chat_container"),
            uiOutput(ns("chat_ui"))
          ),
          div(
            id = ns("input_area"),
            class = "input-area input-area-pending",
            uiOutput(ns("chat_input_hint")),
            textAreaInput(
              ns("user_input"),
              label = NULL,
              placeholder = "",
              width = "100%"
            ),
            actionButton(
              ns("send_message"),
              label = tagList(icon("paper-plane"), tags$span(class = "send-label", "\u2026")),
              class = "btn-interview btn-interview-primary send-button"
            )
          )
        )
      )
    )
  )
}

#' section_interview_simulator Server Functions
#'
#' @noRd
mod_section_interview_simulator_server <- function(id, api_pwd, language_input, translations, usage_guard = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- translations$interview
    security_limits_cfg <- if (is.null(usage_guard)) security_limits() else usage_guard$get_limits()
    interview_cooldown_secs <- security_limits_cfg$interview_cooldown_secs %||% 10L

    # Initialize reactive values
    rv <- reactiveValues(
      thread_id = NULL,
      api_calls = 0,
      step = 1,
      ui_ready = FALSE,
      chat_active = FALSE,
      last_send_at = NULL,
      last_analyzed_count = 0L,
      last_analysis_text = NULL,
      analysis_in_flight = FALSE,
      setup_collapsed = FALSE,
      config = list(
        api_key = api_pwd,
        model = Sys.getenv("INTERVIEW_MODEL", "gpt-4o-mini")
      )
    )

    chat_messages <- reactiveVal(NULL)

    set_chat_messages <- function(messages) {
      chat_messages(messages)
      scroll_chat_to_bottom()
    }

    set_live_layout <- function(active, collapse_setup = TRUE) {
      if (isTRUE(active)) {
        shinyjs::addClass("interview_layout", "interview-layout--live")
        if (isTRUE(collapse_setup)) {
          rv$setup_collapsed <- TRUE
          shinyjs::addClass("setup_col", "interview-setup-col--collapsed")
        }
      } else {
        shinyjs::removeClass("interview_layout", "interview-layout--live")
        rv$setup_collapsed <- FALSE
        shinyjs::removeClass("setup_col", "interview-setup-col--collapsed")
      }
    }

    replace_loading_analysis_message <- function(text, status = "done", round = NULL) {
      messages <- chat_messages() %||% list()
      loading_idx <- which(vapply(messages, function(m) {
        identical(m$role, "analysis") && identical(m$status, "loading")
      }, logical(1)))
      if (length(loading_idx) == 0) {
        return()
      }
      idx <- loading_idx[[length(loading_idx)]]
      msg <- messages[[idx]]
      messages[[idx]] <- list(
        role = "analysis",
        text = text %||% "",
        status = status,
        round = round %||% msg$round %||% 1L
      )
      set_chat_messages(messages)
    }

    scroll_chat_to_bottom <- function() {
      shinyjs::delay(80, {
        shinyjs::runjs(sprintf(
          "var c=document.getElementById('%s');if(c){c.scrollTop=c.scrollHeight;}",
          ns("chat_container")
        ))
      })
    }

    set_chat_input_active <- function(active) {
      lang <- language_input() %||% "FR"
      rv$chat_active <- isTRUE(active)
      if (isTRUE(active)) {
        set_live_layout(TRUE, collapse_setup = TRUE)
        shinyjs::removeClass("input_area", "input-area-pending")
        shinyjs::enable("user_input")
        shinyjs::enable("send_message")
        updateTextAreaInput(
          session,
          "user_input",
          placeholder = t_lang(i18n$input_placeholder, lang)
        )
      } else {
        shinyjs::addClass("input_area", "input-area-pending")
        shinyjs::disable("user_input")
        shinyjs::disable("send_message")
        updateTextAreaInput(
          session,
          "user_input",
          value = "",
          placeholder = t_lang(i18n$chat_empty, lang)
        )
      }
    }

    reset_chat_welcome <- function() {
      chat_messages(NULL)
      rv$last_analyzed_count <- 0L
      rv$last_analysis_text <- NULL
      rv$analysis_in_flight <- FALSE
    }

    gpt_language_input <- reactive({
      req(input$gpt_language_messages)
      input$gpt_language_messages
    })

    interview_config <- reactive({
      modifyList(
        rv$config,
        list(language = gpt_language_input())
      )
    })

    get_translation <- function(type, key, language) {
      t_path(translations, "interview", type, key, lang = language)
    }

    interview_choice <- function(key, language) {
      i18n$choices[[key]][[language]]
    }

    observe({
      lang <- language_input() %||% "FR"
      updateActionButton(
        session,
        "start_interview",
        label = tagList(icon("play"), t_lang(i18n$start, lang))
      )
      updateActionButton(
        session,
        "send_message",
        label = tagList(icon("paper-plane"), t_lang(i18n$send, lang))
      )
    })

    output$chat_input_hint <- renderUI({
      if (isTRUE(rv$chat_active)) {
        return(NULL)
      }
      lang <- language_input() %||% "FR"
      tags$p(
        class = "input-area-hint",
        icon("info-circle"),
        t_lang(i18n$chat_empty, lang)
      )
    })

    observe({
      req(input$user_input, input$send_message)
      if (!isTRUE(rv$ui_ready)) {
        rv$ui_ready <- TRUE
        if (!isTRUE(rv$chat_active)) {
          set_chat_input_active(FALSE)
        }
      }
    })

    output$chat_header_title <- renderUI({
      t_lang(i18n$live_chat, language_input())
    })

    output$chat_feedback_progress <- renderUI({
      if (!isTRUE(rv$chat_active)) {
        return(NULL)
      }

      lang <- language_input() %||% "FR"
      remaining <- messages_until_next_analysis(chat_messages() %||% list())

      if (remaining == 0L) {
        if (isTRUE(rv$analysis_in_flight)) {
          return(tags$span(
            class = "chat-feedback-badge chat-feedback-badge-loading",
            icon("spinner", class = "fa-spin"),
            t_lang(i18n$analysis_loading, lang)
          ))
        }
        return(tags$span(
          class = "chat-feedback-badge chat-feedback-badge-ready",
          icon("check-circle"),
          t_lang(i18n$analysis_ready, lang)
        ))
      }

      tags$span(
        class = "chat-feedback-badge",
        icon("chart-line"),
        sprintf(t_lang(i18n$analysis_in_n, lang), remaining)
      )
    })

    output$toggle_setup_ui <- renderUI({
      if (!isTRUE(rv$chat_active)) {
        return(NULL)
      }

      lang <- language_input() %||% "FR"
      collapsed <- isTRUE(rv$setup_collapsed)
      actionButton(
        ns("toggle_setup"),
        label = if (collapsed) {
          t_lang(i18n$toggle_setup_show, lang)
        } else {
          t_lang(i18n$toggle_setup_hide, lang)
        },
        icon = icon(if (collapsed) "chevron-right" else "chevron-left"),
        class = "btn-setup-toggle"
      )
    })

    output$setup_collapsed_summary <- renderUI({
      if (!isTRUE(rv$chat_active) || !isTRUE(rv$setup_collapsed)) {
        return(NULL)
      }

      req(input$name, input$job_title)
      div(
        class = "setup-collapsed-summary",
        tags$p(class = "setup-collapsed-name", input$name),
        tags$p(class = "setup-collapsed-role", input$job_title)
      )
    })

    observeEvent(input$toggle_setup, {
      rv$setup_collapsed <- !isTRUE(rv$setup_collapsed)
      if (isTRUE(rv$setup_collapsed)) {
        shinyjs::addClass("setup_col", "interview-setup-col--collapsed")
      } else {
        shinyjs::removeClass("setup_col", "interview-setup-col--collapsed")
      }
    })

    output$restart_interview <- renderUI({
      actionButton(
        ns("restart_interview"),
        t_lang(i18n$restart, language_input()),
        icon = icon("sync"),
        class = "btn-interview btn-interview-secondary"
      )
    })

    observeEvent(input$restart_interview, {
      # Reset reactive values
      rv$thread_id <- NULL
      rv$api_calls <- 0
      rv$step <- 1

      rv$chat_active <- FALSE
      rv$last_send_at <- NULL
      if (!is.null(usage_guard)) {
        usage_guard$reset_interview_counters()
      }

      shinyjs::enable("start_interview")
      set_chat_input_active(FALSE)
      set_live_layout(FALSE)
      reset_chat_welcome()
    })




    # Helper functions specific to thread-based operations
    helpers <- list(
      getTimestamp = \() format(Sys.time(), "%Y-%m-%d %H:%M:%S"),

      applyCooldown = \() {
        rv$last_send_at <- Sys.time()
        shinyjs::disable("send_message")
        shinyjs::delay(as.integer(interview_cooldown_secs) * 1000L, {
          if (isTRUE(rv$chat_active)) {
            shinyjs::enable("send_message")
          }
        })
      },

      cooldown_remaining = function() {
        if (is.null(rv$last_send_at)) {
          return(0)
        }
        remaining <- interview_cooldown_secs - as.numeric(difftime(Sys.time(), rv$last_send_at, units = "secs"))
        max(0, ceiling(remaining))
      },

      validateInputs = function() {
        req(input$name, input$job_title, input$company_sector)
        if (nchar(input$name) < 2 || nchar(input$job_title) < 2) {
          showNotification(
            t_lang(i18n$notify_fill_required, language_input()),
            type = "warning"
          )
          return(FALSE)
        }
        return(TRUE)
      },

      formatInitialContext = function() {
        # Helper function to check if a value exists, is not empty, and is not the default empty selection
        has_value <- function(x) {
          !is.null(x) && !is.na(x) && x != "" && nchar(trimws(x)) > 0
        }

        # Format single value with label
        format_value <- function(value, label_key) {
          label <- get_translation("labels", label_key, language_input())
          return(sprintf("%s: %s", label, value))
        }

        # Collect all filled values
        filled_values <- character(0)

        # Basic Information - each field separately to ensure capture
        if (has_value(input$name)) {
          filled_values <- c(filled_values, format_value(input$name, "candidate_name"))
        }
        if (has_value(input$job_title)) {
          filled_values <- c(filled_values, format_value(input$job_title, "position"))
        }
        if (has_value(input$company_sector)) {
          filled_values <- c(filled_values, format_value(input$company_sector, "company_sector"))
        }

        # Interview Format section
        if (has_value(input$interview_format)) {
          filled_values <- c(filled_values, format_value(input$interview_format, "format_label"))
        }
        if (has_value(input$assessment_criteria)) {
          filled_values <- c(filled_values, format_value(input$assessment_criteria, "criteria_label"))
        }
        if (has_value(input$time_constraints)) {
          filled_values <- c(filled_values, format_value(input$time_constraints, "time_label"))
        }
        if (has_value(input$follow_up_process)) {
          filled_values <- c(filled_values, format_value(input$follow_up_process, "follow_up_label"))
        }

        # Company Information section
        if (has_value(input$company_culture)) {
          filled_values <- c(filled_values, format_value(input$company_culture, "culture_label"))
        }
        if (has_value(input$company_values)) {
          filled_values <- c(filled_values, format_value(input$company_values, "values_label"))
        }
        if (has_value(input$company_challenges)) {
          filled_values <- c(filled_values, format_value(input$company_challenges, "challenges_label"))
        }
        if (has_value(input$growth_phase)) {
          filled_values <- c(filled_values, format_value(input$growth_phase, "growth_label"))
        }
        if (has_value(input$company_reputation)) {
          filled_values <- c(filled_values, format_value(input$company_reputation, "reputation_label"))
        }
        if (has_value(input$work_environment)) {
          filled_values <- c(filled_values, format_value(input$work_environment, "environment_label"))
        }

        # Job Details section
        if (has_value(input$responsibilities)) {
          filled_values <- c(filled_values, format_value(input$responsibilities, "responsibilities_label"))
        }
        if (has_value(input$skills)) {
          filled_values <- c(filled_values, format_value(input$skills, "skills_label"))
        }
        if (has_value(input$team)) {
          filled_values <- c(filled_values, format_value(input$team, "team_label"))
        }
        if (has_value(input$job_challenges)) {
          filled_values <- c(filled_values, format_value(input$job_challenges, "job_challenges_label"))
        }
        if (has_value(input$performance)) {
          filled_values <- c(filled_values, format_value(input$performance, "performance_label"))
        }
        if (has_value(input$career)) {
          filled_values <- c(filled_values, format_value(input$career, "career_label"))
        }

        # Combine all values
        if (length(filled_values) > 0) {
          return(paste(filled_values, collapse = "\n"))
        }
        return("")
      }
    )

    maybe_trigger_live_analysis <- function(messages) {
      if (!should_trigger_live_analysis(messages, rv$last_analyzed_count)) {
        return()
      }
      if (isTRUE(rv$analysis_in_flight)) {
        return()
      }
      if (!is.null(usage_guard)) {
        budget <- usage_guard$check_api_budget(1L)
        if (!isTRUE(budget$ok)) {
          return()
        }
      }

      interview_count <- count_interview_chat_messages(messages)
      window <- get_live_analysis_window(messages)
      coaching_round <- (interview_count - 1L) %/% 4L
      insert_idx <- length(messages)

      rv$analysis_in_flight <- TRUE
      set_chat_messages(c(
        messages[seq_len(insert_idx - 1L)],
        list(list(
          role = "analysis",
          text = "",
          status = "loading",
          round = coaching_round
        )),
        messages[insert_idx]
      ))

      job_context <- helpers$formatInitialContext()
      lang <- gpt_language_input()
      api_key <- rv$config$api_key
      prior_analysis <- rv$last_analysis_text
      error_text <- t_lang(i18n$analysis_error, lang)
      analyzed_through <- interview_count - 1L

      future::future({
        run_live_interview_analysis(
          window = window,
          job_context = job_context,
          language = lang,
          api_key = api_key,
          prior_analysis = prior_analysis
        )
      }) %>%
        promises::then(
          function(result) {
            shiny::isolate({
              if (!is.null(usage_guard)) {
                usage_guard$record_api_calls(1L)
              }
              rv$api_calls <- rv$api_calls + 1
              rv$last_analyzed_count <- analyzed_through
              rv$last_analysis_text <- result
              rv$analysis_in_flight <- FALSE
              replace_loading_analysis_message(result, status = "done", round = coaching_round)
            })
          },
          onRejected = function(e) {
            shiny::isolate({
              rv$analysis_in_flight <- FALSE
              replace_loading_analysis_message(error_text, status = "error", round = coaching_round)
            })
          }
        )
    }

    run_interviewer_reply <- function(conversation_id, user_message, base_messages) {
      lang <- language_input() %||% "FR"
      config <- interview_config()

      # Show the user message + a "typing" indicator right away, then fetch the
      # reply on the next tick so the loader is visible during the API call.
      shinyjs::disable("send_message")
      set_chat_messages(c(
        base_messages,
        list(list(role = "assistant", text = "", status = "loading"))
      ))

      later::later(function() {
        shiny::withReactiveDomain(session, shiny::isolate({
          tryCatch({
            response <- process_message(conversation_id, user_message, config)
            final_messages <- c(
              base_messages,
              list(list(role = "assistant", text = response, status = "done"))
            )
            set_chat_messages(final_messages)
            if (!is.null(usage_guard)) {
              usage_guard$record_api_calls(1L)
            }
            rv$api_calls <- rv$api_calls + 1
            maybe_trigger_live_analysis(final_messages)
          }, error = function(e) {
            # Roll back the optimistic user message on failure.
            set_chat_messages(base_messages[seq_len(max(0L, length(base_messages) - 1L))])
            showNotification(
              sprintf(t_lang(i18n$notify_error, lang), conditionMessage(e)),
              type = "error",
              duration = NULL
            )
          })

          if (isTRUE(rv$chat_active)) {
            shinyjs::enable("send_message")
          }
        }))
      }, delay = 0.05)

      invisible(NULL)
    }


    output$main_header <- renderUI({
      lang <- language_input()
      div(
        class = "section-hero interview-hero",
        div(
          class = "section-hero-icon",
          icon("user-tie")
        ),
        div(
          class = "section-hero-text",
          h2(t_lang(i18n$hero_title, lang), class = "section-hero-title"),
          p(t_lang(i18n$hero_subtitle, lang), class = "section-hero-subtitle")
        )
      )
    })

    output$setup_progress <- renderUI({
      lang <- language_input() %||% "FR"
      steps <- i18n$progress[[lang]] %||% i18n$progress[["FR"]]

      div(
        class = "setup-progress",
        lapply(seq_along(steps), function(i) {
          div(
            class = "setup-progress-step",
            span(class = "setup-progress-index", i),
            span(class = "setup-progress-label", steps[[i]])
          )
        })
      )
    })


    output$company_sector_ui_set <- renderUI({
      selectInput(
        ns("company_sector"),
        NULL,
        choices = interview_choice("company_sector", language_input()),
        width = "100%",
        selectize = FALSE
      )
    })



    output$gpt_language_ui <- renderUI({
      lang <- language_input()

      selected_lang <- if (lang == "ENG") "ENG" else "FR"

      selectInput(
        ns("gpt_language_messages"),
        label = t_lang(i18n$gpt_language_label, lang),
        choices = i18n$gpt_language_choices[[lang]],
        width = "100%",
        selected = selected_lang
      )
    })

    output$basic_information <- renderUI({
      div(
        class = "setup-block setup-block-required",
        div(
          class = "setup-block-title",
          icon("id-card"),
          get_translation("labels", "basic_settings_title", language_input())
        ),
        div(
          class = "compact-field",
          tags$label(icon("user"), get_translation("labels", "candidate_name", language_input())),
          textInput(
            ns("name"),
            NULL,
            placeholder = get_translation("placeholders", "name", language_input())
          )
        ),
        div(
          class = "compact-field",
          tags$label(icon("briefcase"), get_translation("labels", "position", language_input())),
          textInput(
            ns("job_title"),
            NULL,
            placeholder = get_translation("placeholders", "job_title", language_input())
          )
        ),
        div(
          class = "compact-field",
          tags$label(icon("building"), get_translation("labels", "company_sector", language_input())),
          uiOutput(ns("company_sector_ui_set"))
        )
      )
    })


    output$interview_details <- renderUI({
      lang <- language_input()

      div(
        class = "interview-accordion",
        tags$details(
          class = "interview-accordion-panel",
          tags$summary(
            class = "interview-accordion-header",
            icon("comments"),
            get_translation("labels", "interview_format_title", lang)
          ),
          div(
            class = "interview-accordion-body",
            div(
              class = "compact-field",
              tags$label(icon("file-alt"), get_translation("labels", "format_label", lang)),
              selectInput(
                ns("interview_format"),
                NULL,
                choices = interview_choice("interview_format", lang),
                width = "100%",
                selectize = FALSE
              )
            ),
            div(
              class = "compact-field",
              tags$label(icon("tasks"), get_translation("labels", "criteria_label", lang)),
              selectInput(
                ns("assessment_criteria"),
                NULL,
                choices = interview_choice("assessment_criteria", lang),
                width = "100%",
                selectize = FALSE
              )
            ),
            div(
              class = "compact-field",
              tags$label(icon("clock"), get_translation("labels", "time_label", lang)),
              selectInput(
                ns("time_constraints"),
                NULL,
                choices = interview_choice("time_constraints", lang),
                width = "100%",
                selectize = FALSE
              )
            ),
            div(
              class = "compact-field",
              tags$label(icon("sync"), get_translation("labels", "follow_up_label", lang)),
              selectInput(
                ns("follow_up_process"),
                NULL,
                choices = interview_choice("follow_up_process", lang),
                width = "100%",
                selectize = FALSE
              )
            )
          )
        ),
        tags$details(
          class = "interview-accordion-panel",
          tags$summary(
            class = "interview-accordion-header",
            icon("building"),
            get_translation("labels", "company_info_title", lang)
          ),
          div(
            class = "interview-accordion-body",
            div(
              class = "compact-field",
              tags$label(icon("landmark"), get_translation("labels", "culture_label", lang)),
              selectInput(
                ns("company_culture"),
                NULL,
                choices = interview_choice("company_culture", lang),
                width = "100%",
                selectize = FALSE
              )
            ),
            div(
              class = "compact-field",
              tags$label(icon("heart"), get_translation("labels", "values_label", lang)),
              textInput(ns("company_values"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("chart-line"), get_translation("labels", "growth_label", lang)),
              selectInput(
                ns("growth_phase"),
                NULL,
                choices = interview_choice("growth_phase", lang),
                width = "100%",
                selectize = FALSE
              )
            ),
            div(
              class = "compact-field",
              tags$label(icon("exclamation-circle"), get_translation("labels", "challenges_label", lang)),
              textInput(ns("company_challenges"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("star"), get_translation("labels", "reputation_label", lang)),
              textInput(ns("company_reputation"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("home"), get_translation("labels", "environment_label", lang)),
              selectInput(
                ns("work_environment"),
                NULL,
                choices = interview_choice("work_environment", lang),
                width = "100%",
                selectize = FALSE
              )
            )
          )
        ),
        tags$details(
          class = "interview-accordion-panel",
          tags$summary(
            class = "interview-accordion-header",
            icon("briefcase"),
            get_translation("labels", "job_details_title", lang)
          ),
          div(
            class = "interview-accordion-body",
            div(
              class = "compact-field",
              tags$label(icon("tasks"), get_translation("labels", "responsibilities_label", lang)),
              textInput(ns("responsibilities"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("check-circle"), get_translation("labels", "skills_label", lang)),
              textInput(ns("skills"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("users"), get_translation("labels", "team_label", lang)),
              textInput(ns("team"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("exclamation-triangle"), get_translation("labels", "job_challenges_label", lang)),
              textInput(ns("job_challenges"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("chart-bar"), get_translation("labels", "performance_label", lang)),
              textInput(ns("performance"), NULL)
            ),
            div(
              class = "compact-field",
              tags$label(icon("road"), get_translation("labels", "career_label", lang)),
              textInput(ns("career"), NULL)
            )
          )
        )
      )
    })



    # Control Buttons
    # output$control_buttons <- renderUI({
    #   actionButton(ns("start_interview"), "D\u00e9marrer l'entretien", class = "btn-primary")
    # })

    render_analysis_message <- function(msg, lang) {
      is_loading <- identical(msg$status, "loading")
      is_error <- identical(msg$status, "error")
      round_label <- sprintf(t_lang(i18n$analysis_round, lang), msg$round %||% 1L)

      div(
        class = "chat-message chat-message-analysis",
        div(class = "analysis-divider", tags$span(round_label)),
        div(
          class = "chat-message-meta",
          icon("lightbulb"),
          t_lang(i18n$chat_analysis, lang)
        ),
        div(
          class = paste(
            "chat-bubble analysis-bubble",
            if (is_loading) "analysis-bubble-loading",
            if (is_error) "analysis-bubble-error"
          ),
          if (is_loading) {
            tagList(
              div(class = "analysis-loading-dots", span("."), span("."), span(".")),
              tags$p(t_lang(i18n$analysis_loading, lang))
            )
          } else if (is_error) {
            tags$p(class = "analysis-error-text", htmltools::htmlEscape(msg$text))
          } else {
            renderMarkdown(msg$text)
          }
        )
      )
    }

    render_chat_message <- function(msg, lang) {
      if (identical(msg$role, "analysis")) {
        return(render_analysis_message(msg, lang))
      }

      is_user <- identical(msg$role, "user")
      is_loading <- identical(msg$status, "loading")

      if (!is_user && !is_loading && (is.null(msg$text) || !nzchar(msg$text))) {
        return(NULL)
      }
      if (is_user && !nzchar(msg$text %||% "")) {
        return(NULL)
      }

      div(
        class = paste0("chat-message ", if (is_user) "chat-message-user" else "chat-message-gpt"),
        div(
          class = "chat-message-meta",
          if (is_user) {
            tagList(icon("user"), t_lang(i18n$chat_you, lang))
          } else {
            tagList(icon("user-tie"), t_lang(i18n$chat_interviewer, lang))
          }
        ),
        if (is_loading) {
          div(
            class = "chat-bubble gpt-bubble gpt-bubble-typing",
            role = "status",
            `aria-label` = t_lang(i18n$assistant_typing, lang),
            div(
              class = "typing-indicator",
              tags$span(), tags$span(), tags$span()
            )
          )
        } else {
          div(
            class = paste0("chat-bubble ", if (is_user) "user-bubble" else "gpt-bubble"),
            if (is_user) {
              htmltools::htmlEscape(msg$text)
            } else {
              renderMarkdown(msg$text)
            }
          )
        }
      )
    }

    output$chat_ui <- renderUI({
      messages <- chat_messages()
      lang <- language_input() %||% "FR"

      if (is.null(messages) || length(messages) == 0) {
        return(div(
          class = "chat-empty-state",
          icon("comment-dots"),
          tags$p(t_lang(i18n$chat_empty, lang))
        ))
      }

      tagList(lapply(messages, render_chat_message, lang = lang))
    })

    # Start Interview Event Handler
    observeEvent(input$start_interview, {
      req(input$name, input$job_title, input$company_sector)

      lang <- language_input() %||% "FR"
      config <- interview_config()

      if (!nzchar(config$api_key %||% "")) {
        showNotification(
          t_lang(i18n$notify_api_key_missing, lang),
          type = "error",
          duration = NULL
        )
        return()
      }

      if (!is.null(usage_guard)) {
        budget <- usage_guard$check_api_budget(1L)
        if (!isTRUE(budget$ok)) {
          showNotification(
            usage_guard_message(budget, translations, lang, feature = "interview"),
            type = "warning",
            duration = 8
          )
          return()
        }
      }

      initial_context <- helpers$formatInitialContext()
      if (!nzchar(initial_context)) {
        showNotification(
          t_lang(i18n$notify_context_error, lang),
          type = "error"
        )
        return()
      }

      # Reveal the live layout and a "typing" bubble immediately, then fetch
      # the opening question on the next tick so the loader is actually shown
      # before the (blocking) API call starts.
      shinyjs::disable("start_interview")
      set_live_layout(TRUE)
      set_chat_messages(list(list(role = "assistant", text = "", status = "loading")))

      later::later(function() {
        shiny::withReactiveDomain(session, shiny::isolate({
          tryCatch({
            conversation_id <- init_conversation(config)
            response <- process_message(conversation_id, initial_context, config)

            rv$thread_id <- conversation_id
            if (!is.null(usage_guard)) {
              usage_guard$record_api_calls(1L)
            }
            rv$api_calls <- rv$api_calls + 1
            set_chat_messages(list(list(role = "assistant", text = response, status = "done")))
            set_chat_input_active(TRUE)
          }, error = function(e) {
            rv$thread_id <- NULL
            shinyjs::enable("start_interview")
            set_chat_input_active(FALSE)
            set_live_layout(FALSE)
            reset_chat_welcome()
            showNotification(
              sprintf(t_lang(i18n$notify_error, lang), conditionMessage(e)),
              type = "error",
              duration = NULL
            )
          })
        }))
      }, delay = 0.05)
    })

    observeEvent(input$send_message, {
      req(rv$chat_active, rv$thread_id, input$user_input)

      message_text <- trimws(input$user_input)
      if (nchar(message_text) < 2) {
        showNotification(t_lang(i18n$notify_short_message, language_input()), type = "warning")
        return()
      }

      cooldown <- helpers$cooldown_remaining()
      if (cooldown > 0) {
        showNotification(
          sprintf(t_lang(i18n$notify_cooldown, language_input()), cooldown),
          type = "warning"
        )
        return()
      }

      if (!is.null(usage_guard)) {
        budget <- usage_guard$check_interview_message(message_text)
        if (!isTRUE(budget$ok)) {
          showNotification(
            usage_guard_message(budget, translations, language_input(), feature = "interview"),
            type = "warning",
            duration = 8
          )
          return()
        }
      }

      helpers$applyCooldown()
      updateTextAreaInput(session, "user_input", value = "")

      if (!is.null(usage_guard)) {
        usage_guard$record_interview_message()
      }

      current_messages <- chat_messages() %||% list()
      pending_messages <- c(
        current_messages,
        list(list(role = "user", text = message_text))
      )

      run_interviewer_reply(rv$thread_id, message_text, pending_messages)
    })











    # Keep setup fields alive while another section is visible (client-side tabs).
    for (out_id in c(
      "main_header", "setup_progress", "basic_information", "gpt_language_ui",
      "interview_details", "company_sector_ui_set", "toggle_setup_ui",
      "setup_collapsed_summary", "restart_interview", "chat_header_title",
      "chat_feedback_progress", "chat_ui", "chat_input_hint"
    )) {
      outputOptions(output, out_id, suspendWhenHidden = FALSE)
    }

  })
}





## To be copied in the UI
# mod_section_interview_simulator_ui("section_interview_simulator_1")

## To be copied in the server
# mod_section_interview_simulator_server("section_interview_simulator_1")
