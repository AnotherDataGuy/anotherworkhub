#' Interview Simulator Helper Functions
#'
#' @description OpenAI Responses API + Conversations API helpers for the
#'   interview simulator (replaces deprecated Assistants API threads/runs).
#'
#' @importFrom httr GET POST add_headers http_error http_status content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom curl new_handle handle_setheaders handle_setopt curl_fetch_multi multi_run new_pool
#' @noRd

.openai_response_body <- function(response) {
  if (length(response$content) == 0) {
    return("")
  }
  rawToChar(response$content)
}

.openai_json_post <- function(url, api_key, body = list()) {
  response <- httr::POST(
    url = url,
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw",
    httr::timeout(120)
  )

  if (httr::http_error(response)) {
    details <- .openai_response_body(response)
    stop(sprintf(
      "HTTP error: %s%s",
      httr::http_status(response)$message,
      if (nzchar(details)) paste0(" \u2014 ", details) else ""
    ))
  }

  content <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
  if (!is.null(content$error)) {
    stop(content$error$message)
  }

  content
}

.openai_json_get <- function(url, api_key, query = NULL) {
  response <- httr::GET(
    url = url,
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    query = query,
    httr::timeout(120)
  )

  if (httr::http_error(response)) {
    details <- .openai_response_body(response)
    stop(sprintf(
      "HTTP error: %s%s",
      httr::http_status(response)$message,
      if (nzchar(details)) paste0(" \u2014 ", details) else ""
    ))
  }

  content <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
  if (!is.null(content$error)) {
    stop(content$error$message)
  }

  content
}

#' Extract assistant text from a Responses API payload
#' @noRd
extract_response_text <- function(response_content) {
  if (!is.null(response_content$output_text) && nzchar(response_content$output_text)) {
    return(response_content$output_text)
  }

  if (is.null(response_content$output)) {
    return(NULL)
  }

  texts <- character(0)
  for (item in response_content$output) {
    if (is.null(item$type) || item$type != "message" || is.null(item$content)) {
      next
    }
    for (block in item$content) {
      if (!is.null(block$type) && block$type == "output_text" && !is.null(block$text)) {
        texts <- c(texts, block$text)
      }
    }
  }

  if (length(texts) == 0) {
    return(NULL)
  }

  paste(texts, collapse = "\n")
}

#' Extract plain text from a conversation message item
#' @noRd
extract_message_item_text <- function(message_item) {
  if (is.null(message_item$content)) {
    return("")
  }

  texts <- vapply(
    message_item$content,
    function(block) {
      if (!is.null(block$type) && block$type %in% c("input_text", "output_text")) {
        return(if (is.null(block$text)) "" else block$text)
      }
      ""
    },
    character(1)
  )

  paste(texts[nzchar(texts)], collapse = "\n")
}

#' Initialize Conversation
#'
#' @param config List with `api_key`.
#' @return Conversation id (`conv_...`).
#' @noRd
init_conversation <- function(config) {
  tryCatch({
    content <- .openai_json_post("https://api.openai.com/v1/conversations", config$api_key, list())
    content$id
  }, error = function(e) {
    stop(sprintf("Conversation creation failed: %s", e$message))
  })
}

#' Backward-compatible alias for init_conversation
#' @noRd
init_thread <- init_conversation

#' Create a model response inside a conversation (non-streaming)
#' @noRd
create_conversation_response <- function(conversation_id, message, config) {
  body <- .build_conversation_response_body(conversation_id, message, config, stream = FALSE)
  .openai_json_post("https://api.openai.com/v1/responses", config$api_key, body)
}

#' Build request body for a conversation response (streamed or not)
#' @noRd
.build_conversation_response_body <- function(conversation_id, message, config, stream = FALSE) {
  language <- if (is.null(config$language)) "FR" else config$language
  instructions <- if (is.null(config$instructions)) {
    get_interview_simulator_instructions(language)
  } else {
    config$instructions
  }
  limits <- security_limits()
  max_output <- as.integer(config$max_output_tokens %||% limits$max_output_tokens)
  if (is.na(max_output) || max_output <= 0L) {
    max_output <- 800L
  }

  # Interview context / chat turns are untrusted; wrap unless caller already did.
  prepared_message <- if (isTRUE(config$skip_user_wrap)) {
    as.character(message %||% "")
  } else {
    prepare_user_content_for_llm(
      message,
      max_chars = limits$max_input_chars,
      label = config$user_content_label %||% "INTERVIEW_USER_INPUT"
    )
  }

  body <- list(
    model = if (is.null(config$model)) "gpt-4o-mini" else config$model,
    conversation = conversation_id,
    instructions = with_security_instructions(instructions),
    max_output_tokens = max_output,
    input = list(
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = prepared_message
          )
        )
      )
    ),
    store = TRUE
  )

  if (isTRUE(stream)) {
    body$stream <- TRUE
  }

  body
}

#' Consume OpenAI Responses API SSE chunks
#' @noRd
consume_openai_sse_buffer <- function(buffer, on_event = NULL) {
  if (is.null(buffer) || !nzchar(buffer)) {
    return("")
  }

  buffer <- gsub("\r\n", "\n", buffer, fixed = TRUE)

  incomplete <- ""
  if (!grepl("\n$", buffer)) {
    last_break <- max(unlist(gregexpr("\n", buffer, fixed = TRUE)))
    if (last_break > 0) {
      incomplete <- substring(buffer, last_break + 1)
      buffer <- substring(buffer, 1, last_break - 1)
    } else {
      return(buffer)
    }
  }

  if (!nzchar(buffer)) {
    return(incomplete)
  }

  blocks <- strsplit(buffer, "\n\n", fixed = TRUE)[[1]]
  for (block in blocks) {
    if (!nzchar(block)) {
      next
    }
    lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
    data_lines <- lines[grepl("^data:", lines)]
    for (line in data_lines) {
      payload <- sub("^data:\\s*", "", line)
      if (!nzchar(payload) || identical(payload, "[DONE]")) {
        next
      }
      event <- tryCatch(
        jsonlite::fromJSON(payload, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.null(event) && is.function(on_event)) {
        on_event(event)
      }
    }
  }

  incomplete
}

#' Extract text delta from a Responses API stream event
#' @noRd
openai_stream_event_delta <- function(event) {
  if (is.null(event$type)) {
    return(NULL)
  }
  if (identical(event$type, "response.output_text.delta") && !is.null(event$delta)) {
    return(event$delta)
  }
  if (identical(event$type, "error")) {
    msg <- if (!is.null(event$error$message)) event$error$message else "Streaming API error"
    stop(msg)
  }
  if (identical(event$type, "response.failed")) {
    stop("The model failed to complete the streamed response.")
  }
  NULL
}

#' Stream a model response inside a conversation (non-blocking SSE)
#'
#' Uses curl's asynchronous multi interface driven by a `later` polling loop so
#' the Shiny session stays responsive and tokens reach the browser as they
#' arrive (ChatGPT-like streaming). Returns immediately; results are delivered
#' through the `on_done` / `on_error` callbacks.
#'
#' @param session Shiny session used to push live tokens and run callbacks.
#' @param stream_id DOM id suffix (namespaced by caller).
#' @param on_progress Optional callback receiving the accumulated text.
#' @param on_done Callback receiving the final assistant text.
#' @param on_error Callback receiving an error message string.
#' @noRd
stream_conversation_response_async <- function(conversation_id,
                                               message,
                                               config,
                                               session = NULL,
                                               stream_id = NULL,
                                               on_progress = NULL,
                                               on_done = NULL,
                                               on_error = NULL) {
  body <- .build_conversation_response_body(conversation_id, message, config, stream = TRUE)
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")

  sse_carry <- ""
  full_text <- ""
  finished <- FALSE

  send_token <- function(done = FALSE) {
    if (!is.null(session) && !is.null(stream_id)) {
      session$sendCustomMessage("interview_stream_token", list(
        id = session$ns(stream_id),
        text = full_text,
        done = done
      ))
    }
  }

  handle_event <- function(event) {
    delta <- openai_stream_event_delta(event)
    if (!is.null(delta) && nzchar(delta)) {
      full_text <<- paste0(full_text, delta)
      send_token(FALSE)
      if (is.function(on_progress)) {
        on_progress(full_text)
      }
    }
  }

  finish_error <- function(msg) {
    if (finished) {
      return(invisible(NULL))
    }
    finished <<- TRUE
    if (is.function(on_error)) {
      shiny::withReactiveDomain(session, on_error(msg))
    }
    invisible(NULL)
  }

  data_cb <- function(chunk, final = FALSE) {
    if (length(chunk) == 0) {
      return(invisible(NULL))
    }
    sse_carry <<- consume_openai_sse_buffer(
      paste0(sse_carry, rawToChar(chunk)),
      on_event = handle_event
    )
    invisible(NULL)
  }

  done_cb <- function(res) {
    if (finished) {
      return(invisible(NULL))
    }
    if (nzchar(sse_carry)) {
      consume_openai_sse_buffer(paste0(sse_carry, "\n\n"), on_event = handle_event)
    }
    if (isTRUE(res$status_code >= 400L)) {
      details <- if (length(res$content) > 0) rawToChar(res$content) else ""
      finish_error(sprintf(
        "HTTP error: %s%s",
        res$status_code,
        if (nzchar(details)) paste0(" \u2014 ", details) else ""
      ))
      return(invisible(NULL))
    }
    if (!nzchar(full_text)) {
      finish_error("No assistant text found in streamed Responses API payload")
      return(invisible(NULL))
    }
    finished <<- TRUE
    send_token(TRUE)
    if (is.function(on_done)) {
      shiny::withReactiveDomain(session, on_done(full_text))
    }
    invisible(NULL)
  }

  fail_cb <- function(msg) {
    finish_error(msg)
  }

  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    Authorization = paste("Bearer", config$api_key),
    `Content-Type` = "application/json",
    Accept = "text/event-stream"
  )
  curl::handle_setopt(handle, postfields = body_json, timeout = 120L)

  pool <- curl::new_pool()
  curl::curl_fetch_multi(
    "https://api.openai.com/v1/responses",
    done = done_cb,
    fail = fail_cb,
    data = data_cb,
    pool = pool,
    handle = handle
  )

  poll <- function() {
    if (finished) {
      return(invisible(NULL))
    }
    state <- tryCatch(
      curl::multi_run(timeout = 0.15, pool = pool),
      error = function(e) {
        finish_error(conditionMessage(e))
        list(pending = 0L)
      }
    )
    if (!finished && isTRUE(state$pending > 0L)) {
      later::later(poll, delay = 0)
    }
    invisible(NULL)
  }
  poll()

  invisible(NULL)
}

#' Process Complete Message Flow
#'
#' @param conversation_id Conversation id (`conv_...`).
#' @param message User or context message.
#' @param config API configuration list.
#' @return Assistant response text.
#' @noRd
process_message <- function(conversation_id, message, config) {
  tryCatch({
    response_content <- create_conversation_response(conversation_id, message, config)
    text <- extract_response_text(response_content)
    if (is.null(text) || !nzchar(text)) {
      stop("No assistant text found in Responses API payload")
    }
    text
  }, error = function(e) {
    stop(sprintf("Error in process_message: %s", e$message))
  })
}

#' List conversation messages for chat UI rendering
#' @noRd
list_conversation_messages <- function(conversation_id, config) {
  tryCatch({
    content <- .openai_json_get(
      sprintf("https://api.openai.com/v1/conversations/%s/items", conversation_id),
      config$api_key,
      query = list(limit = 100, order = "asc")
    )

    items <- content$data
    if (is.null(items)) {
      return(list())
    }

    messages <- Filter(function(item) {
      !is.null(item$type) && item$type == "message" &&
        !is.null(item$role) && item$role %in% c("user", "assistant")
    }, items)

    lapply(messages, function(item) {
      list(
        role = item$role,
        text = extract_message_item_text(item)
      )
    })
  }, error = function(e) {
    stop(sprintf("Failed to list conversation items: %s", e$message))
  })
}

#' Fetch latest assistant response (legacy helper)
#' @noRd
fetch_response <- function(conversation_id, config) {
  messages <- list_conversation_messages(conversation_id, config)
  assistant_messages <- Filter(function(m) m$role == "assistant", messages)
  if (length(assistant_messages) == 0) {
    return(NULL)
  }
  assistant_messages[[length(assistant_messages)]]$text
}

#' Get Current Timestamp
#' @noRd
get_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' @noRd
is_interview_chat_message <- function(message) {
  identical(message$role, "user") || identical(message$role, "assistant")
}

#' Count user/assistant messages only (excludes coaching bubbles)
#' @noRd
count_interview_chat_messages <- function(messages) {
  if (is.null(messages) || length(messages) == 0) {
    return(0L)
  }
  sum(vapply(messages, is_interview_chat_message, logical(1)))
}

#' @noRd
filter_interview_chat_messages <- function(messages) {
  if (is.null(messages) || length(messages) == 0) {
    return(list())
  }
  Filter(is_interview_chat_message, messages)
}

#' Whether coaching should run after the recruiter reply closing a 4-message block
#' @noRd
should_trigger_live_analysis <- function(messages, last_analyzed_count = 0L) {
  n <- count_interview_chat_messages(messages)
  if (n < 5L) {
    return(FALSE)
  }
  if (n %% 4L != 1L) {
    return(FALSE)
  }
  (n - 1L) > last_analyzed_count
}

#' Recent IA/user block that coaching should evaluate
#' @noRd
get_live_analysis_window <- function(messages) {
  interview_msgs <- filter_interview_chat_messages(messages)
  n <- length(interview_msgs)
  if (n < 4L) {
    return(interview_msgs)
  }
  interview_msgs[(n - 4L):(n - 1L)]
}

#' Messages until the next coaching checkpoint
#' @noRd
messages_until_next_analysis <- function(messages) {
  n <- count_interview_chat_messages(messages)
  if (n >= 5L && (n - 1L) %% 4L == 0L) {
    return(0L)
  }
  target <- if (n < 4L) {
    5L
  } else {
    (((n - 1L) %/% 4L) + 1L) * 4L + 1L
  }
  max(0L, target - n)
}

#' Format recent exchange for the coaching model
#' @noRd
format_live_analysis_user_input <- function(window, job_context, prior_analysis = NULL, language = "FR") {
  role_labels <- if (language == "ENG") {
    c(assistant = "Interviewer", user = "Candidate")
  } else {
    c(assistant = "Recruteur", user = "Candidat")
  }

  transcript <- paste(
    vapply(window, function(m) {
      label <- role_labels[[m$role]] %||% m$role
      sprintf("%s: %s", label, m$text)
    }, character(1)),
    collapse = "\n\n"
  )

  parts <- character(0)
  if (!is.null(job_context) && nzchar(job_context)) {
    parts <- c(parts, paste0("JOB CONTEXT:\n", job_context))
  }
  parts <- c(
    parts,
    paste0("RECENT EXCHANGE (last ", length(window), " messages):\n", transcript)
  )
  if (!is.null(prior_analysis) && nzchar(prior_analysis)) {
    parts <- c(
      parts,
      paste0("PREVIOUS COACHING FEEDBACK:\n", prior_analysis)
    )
  }

  # Outer wrap is applied by fct_interact_with_gpt_api_only_text.
  paste(parts, collapse = "\n\n")
}

#' Run coaching analysis via Chat Completions (separate from interview thread)
#' @noRd
run_live_interview_analysis <- function(window,
                                        job_context,
                                        language = "FR",
                                        api_key,
                                        prior_analysis = NULL,
                                        model = Sys.getenv("INTERVIEW_ANALYSIS_MODEL", "gpt-4o-mini")) {
  user_input <- format_live_analysis_user_input(
    window = window,
    job_context = job_context,
    prior_analysis = prior_analysis,
    language = language
  )
  instructions <- get_interview_live_analysis_instructions(language)

  result <- fct_interact_with_gpt_api_only_text(
    api_key = api_key,
    user_input = user_input,
    admin_prompt = instructions,
    model = model
  )

  if (grepl("^Error in API request:", result)) {
    stop(result)
  }
  if (identical(result, "No response found in the API return.")) {
    stop(result)
  }

  result
}
