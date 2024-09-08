#' fct_interact_with_gpt_api_only_text
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import httr
#' @noRd
fct_interact_with_gpt_api_only_text <- function(api_key, user_input, admin_prompt, model = "gpt-4o-mini") {
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      max_tokens = 2000,
      model = model,
      messages = list(
        list(role = "system", content = admin_prompt),
        list(role = "user", content = user_input)
      )
    )
  )

  # Error handling
  if (httr::http_error(response)) {
    return(paste("Error in API request:", status_code(response)))
  }

  # Extract and store the ChatGPT response
  json_content <- httr::content(response, as = "parsed")
  if (!is.null(json_content$choices[[1]]$message$content)) {
    text_response <- json_content$choices[[1]]$message$content
    return(text_response)
  } else {
    return("No response found in the API return.")
  }

}
