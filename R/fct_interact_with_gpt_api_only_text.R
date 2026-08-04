#' fct_interact_with_gpt_api_only_text
#'
#' Chat Completions helper with output token cap and prompt-injection framing.
#'
#' @param api_key OpenAI API key.
#' @param user_input Untrusted user text (wrapped automatically unless already prepared).
#' @param admin_prompt System instructions.
#' @param model Model id.
#' @param max_tokens Output token cap (defaults to security limit).
#' @param wrap_user If TRUE, truncate and wrap user content as untrusted data.
#' @return Model text, or an error string.
#' @import httr
#' @noRd
fct_interact_with_gpt_api_only_text <- function(api_key,
                                                user_input,
                                                admin_prompt,
                                                model = "gpt-4o-mini",
                                                max_tokens = NULL,
                                                wrap_user = TRUE) {
  limits <- security_limits()
  max_tokens <- as.integer(max_tokens %||% limits$max_output_tokens)
  if (is.na(max_tokens) || max_tokens <= 0L) {
    max_tokens <- 800L
  }

  prepared_user <- if (isTRUE(wrap_user)) {
    prepare_user_content_for_llm(user_input, max_chars = limits$max_input_chars)
  } else {
    as.character(user_input %||% "")
  }

  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      max_tokens = max_tokens,
      model = model,
      messages = list(
        list(role = "system", content = with_security_instructions(admin_prompt)),
        list(role = "user", content = prepared_user)
      )
    )
  )

  # Error handling
  if (httr::http_error(response)) {
    return(paste("Error in API request:", status_code(response)))
  }

  # Extract and store the ChatGPT response
  json_content <- httr::content(response, as = "parsed")
  if (
    length(json_content$choices) >= 1L &&
      !is.null(json_content$choices[[1]]$message$content)
  ) {
    return(json_content$choices[[1]]$message$content)
  }
  "No response found in the API return."
}
