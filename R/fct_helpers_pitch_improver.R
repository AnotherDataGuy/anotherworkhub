#' Render Markdown Text
#'
#' @description This function renders markdown text by converting it to HTML.
#' It supports various input formats such as JSON, XML, LaTeX, HTML, and plain text.
#'
#' @param text A character string containing the text to be rendered.
#'
#' @return An HTML object containing the rendered text.
#'
#' @details The function checks the input text and applies the appropriate rendering based on the format:
#'   - **JSON**: If the text is a valid JSON string (validated using `jsonlite::validate()`),
#'     it is parsed using `jsonlite::fromJSON()`, formatted using `jsonlite::toJSON()`,
#'     and converted to HTML using `commonmark::markdown_html()`.
#'   - **XML**: If the text contains an XML declaration (e.g., `<?xml version="1.0"?>`), it is parsed using
#'     `xml2::read_xml()`. The resulting object is converted to a string representation and rendered as HTML.
#'   - **HTML**: If the text is wrapped in HTML tags, it is directly returned as HTML using `HTML()`.
#'   - **LaTeX**: If the text contains LaTeX expressions enclosed in dollar signs (`$...$`),
#'     the dollar signs are removed, and the expression is wrapped in a `<span>` tag with the class "math"
#'     to be compatible with MathJax.
#'   - **Markdown**: If the text contains non-alphanumeric characters (excluding whitespace), it is treated as
#'     Markdown and converted to HTML using `commonmark::markdown_html()`.
#'   - **Plain Text**: If none of the above conditions are met, the text is treated as plain text and wrapped in a
#'     paragraph (`<p>`) tag.
#'
#' @note This function assumes the use of the `commonmark` package for rendering Markdown and `xml2` for XML parsing.
#' Ensure these packages are installed.
#'
#' @importFrom jsonlite fromJSON toJSON validate
#' @importFrom xml2 read_xml
#' @importFrom commonmark markdown_html
#' @importFrom shiny HTML
#' @importFrom htmltools htmlEscape
#'
#' @examples
#' renderMarkdown('# Hello, World!')
#' renderMarkdown('{"name": "John", "age": 30}')
#' renderMarkdown('<?xml version="1.0"?><root><tag>Content</tag></root>')
#' renderMarkdown('$E=mc^2$')
#' renderMarkdown('This is **bold** and this is _italic_.')
#' renderMarkdown('Just a plain text string.')
#'
#' @noRd
renderMarkdown <- function(text) {
  if (is.null(text) || nchar(text) == 0) {
    return(HTML(""))
  }

  if (jsonlite::validate(text)) {
    data <- jsonlite::fromJSON(text)
    # Ensure formatJSON is defined or remove this line
    # formattedText <- formatJSON(data)
    formattedText <- jsonlite::toJSON(data, pretty = TRUE, auto_unbox = TRUE)
    return(HTML(sanitize_untrusted_html(commonmark::markdown_html(formattedText))))
  }

  if (grepl("^<\\?xml", text)) {
    xmlData <- xml2::read_xml(text)
    # Ensure formatXML is defined or provide alternative handling
    # formattedText <- formatXML(xmlData)
    formattedText <- as.character(xmlData)
    return(HTML(sanitize_untrusted_html(commonmark::markdown_html(formattedText))))
  }

  if (grepl("^<.*>$", text)) {
    # Never trust raw HTML from model/user content (XSS / injection surface).
    return(HTML(sanitize_untrusted_html(commonmark::markdown_html(text))))
  }

  if (grepl("\\$.*\\$", text)) {
    latex_html <- gsub("\\$", "", text) # Remove dollar signs for MathJax
    return(HTML(sprintf(
      '<span class="math">%s</span>',
      htmltools::htmlEscape(latex_html)
    )))
  }

  if (grepl("[^\\s\\w]", text)) {
    return(HTML(sanitize_untrusted_html(commonmark::markdown_html(text))))
  }

  # Default case: wrap plain text in a paragraph tag
  return(HTML(sprintf("<p>%s</p>", htmltools::htmlEscape(text))))
}




#' Calculate Summary Statistics
#'
#' @description This function calculates various summary statistics for a given text, including word count, character count, sentence count, and paragraph count.
#'
#' @param text A character string containing the text to be analyzed.
#' @param language A character string specifying the language of the text, either "ENG" for English or "FR" for French.
#'
#' @return A data frame containing the calculated summary statistics.
#'
#' @details The function performs the following steps:
#'   1. Cleans the text by removing punctuation, digits, and extra whitespace.
#'   2. Counts the number of characters, words, sentences, and paragraphs in the text.
#'   3. Returns a data frame with the summary statistics, labeled according to the specified language.
#'
#' @importFrom stringr str_remove_all str_squish
#' @importFrom tokenizers tokenize_words tokenize_sentences
#' @importFrom stringi stri_split_regex
#'
#' @examples
#' text <- "This is a sample text. It contains multiple sentences and paragraphs."
#' language <- "ENG"
#' summary <- calculate_summary(text, language)
#' print(summary)
#'
#' @noRd
calculate_summary <- function(text, language) {
  if (is.null(text) || text == "") {
    return(data.frame(
      "Total Characters" = 0,
      "Total Words" = 0,
      "Total Sentences" = 0,
      "Total Paragraphs" = 0
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

  original_text <- text
  cleaned_text <- clean_for_words(text)

  characters <- nchar(original_text)
  words <- length(tokenizers::tokenize_words(cleaned_text)[[1]])
  sentences <- length(tokenizers::tokenize_sentences(original_text)[[1]])
  paragraphs <- length(stringi::stri_split_regex(original_text, "\\R{2,}")[[1]])

  if (language == "ENG") {
    summary_data <- data.frame(
      "Total Characters" = characters,
      "Total Words" = words,
      "Total Sentences" = sentences,
      "Total Paragraphs" = paragraphs
    )
  } else {
    summary_data <- data.frame(
      "Nombre total de caract\u00e8res" = characters,
      "Nombre total de mots" = words,
      "Nombre total de phrases" = sentences,
      "Nombre total de paragraphes" = paragraphs
    )
  }

  return(summary_data)
}


#' Construct Sentence
#'
#' @description This function constructs a sentence based on the provided input value, English text, French text, and language.
#'
#' @param input_value The input value to be used in the sentence construction.
#' @param english_text The English text to be used in the sentence construction.
#' @param french_text The French text to be used in the sentence construction.
#' @param lang A character string specifying the language, either "ENG" for English or "FR" for French.
#' @param is_text_input A logical value indicating whether the input value is from a text input or not.
#'
#' @return A character string representing the constructed sentence.
#'
#' @details The function constructs a sentence based on the following logic:
#'   - If the input value is NA, NULL, or an empty string, an empty string is returned.
#'   - If `is_text_input` is TRUE, the input value is directly used as the label in the sentence.
#'   - If `is_text_input` is FALSE, the function looks up the label based on the input value and the provided choices map.
#'   - The sentence is constructed by concatenating the appropriate text (English or French) with the label and a period.
#'
#' @examples
#' input_value <- "option1"
#' english_text <- "The selected option is"
#' french_text <- "L'option s\u00e9lectionn\u00e9e est"
#' lang <- "ENG"
#' is_text_input <- FALSE
#' sentence <- construct_sentence(input_value, english_text, french_text, lang, is_text_input)
#' print(sentence)
#'
#' @noRd
construct_sentence <- function(input_value, english_text, french_text, lang, is_text_input = FALSE, translations = NULL) {
  if (is.null(input_value) || is.na(input_value) || input_value == "") {
    return("")
  }

  if (is_text_input) {
    label <- input_value
  } else {
    # Now using the passed translations
    choices_map <- if (lang == "ENG") translations$english_choices_map else translations$french_choices_map
    label <- if (input_value %in% names(choices_map)) {
      choices_map[[input_value]]
    } else {
      t_lang(translations$pitch$unknown_choice, lang)
    }
  }

  sentence <- if (lang == "ENG") {
    paste0(english_text, " ", label, ".")
  } else {
    paste0(french_text, " ", label, ".")
  }

  return(sentence)
}


#' Construct Sentence with Niveau
#'
#' @description This function constructs a sentence based on the provided input value, English text, French text, language, and labels.
#'
#' @param input_value The input value to be used in the sentence construction.
#' @param english_text The English text to be used in the sentence construction.
#' @param french_text The French text to be used in the sentence construction.
#' @param lang A character string specifying the language, either "ENG" for English or "FR" for French.
#' @param is_text_input A logical value indicating whether the input value is from a text input or not.
#' @param labels A named list or vector containing labels for the input value.
#'
#' @return A character string representing the constructed sentence.
#'
#' @details The function constructs a sentence based on the following logic:
#'   - If the input value is NA, NULL, or an empty string, an empty string is returned.
#'   - If `is_text_input` is TRUE, the input value is directly used as the label in the sentence.
#'   - If `is_text_input` is FALSE, the function looks up the label based on the input value and the provided labels.
#'   - The sentence is constructed by concatenating the appropriate text (English or French) with the label and a period.
#'
#' @examples
#' input_value <- "level1"
#' english_text <- "The selected level is"
#' french_text <- "Le niveau s\u00e9lectionn\u00e9 est"
#' lang <- "ENG"
#' is_text_input <- FALSE
#' labels <- list(level1 = "Beginner", level2 = "Intermediate", level3 = "Advanced")
#' sentence <- construct_sentence_niveau(input_value, english_text, french_text, lang, is_text_input, labels)
#' print(sentence)
#'
#' @noRd
construct_sentence_niveau <- function(input_value, english_text, french_text, lang, is_text_input = FALSE, labels = NULL, unknown_label = "Unknown choice") {
  if (!is.null(input_value) && input_value != "" && !is.na(input_value)) {
    if (is_text_input) {
      # Directly use the input value for textInput
      label <- input_value
    } else {
      # Use the provided labels for selectInput label lookup
      label <- if (input_value %in% names(labels)) {
        labels[[input_value]]
      } else {
        unknown_label
      }
    }

    sentence <- if (lang == "ENG") {
      paste0(english_text, " ", label, ".")
    } else {
      paste0(french_text, " ", label, ".")
    }
  } else {
    sentence <- ""
  }

  return(sentence)
}


#' Construct General Sentence
#'
#' @description This function constructs a general sentence based on the provided input value, English text, French text, language, and labels.
#'
#' @param input_value The input value to be used in the sentence construction.
#' @param english_text The English text to be used in the sentence construction.
#' @param french_text The French text to be used in the sentence construction.
#' @param lang A character string specifying the language, either "ENG" for English or "FR" for French.
#' @param labels A named list or vector containing labels for the input value.
#'
#' @return A character string representing the constructed sentence.
#'
#' @details The function constructs a sentence based on the following logic:
#'   - If the input value is NA, NULL, or an empty string, an empty string is returned.
#'   - If the input value matches a key in the provided labels, the corresponding label is used in the sentence.
#'   - If the input value is a label itself in the provided labels, it is used directly in the sentence.
#'   - If the input value is not found in the labels, it is used directly in the sentence (assuming it's a string).
#'   - The sentence is constructed by concatenating the appropriate text (English or French) with the label and a period.
#'
#' @examples
#' input_value <- "option1"
#' english_text <- "The selected option is"
#' french_text <- "L'option s\u00e9lectionn\u00e9e est"
#' lang <- "ENG"
#' labels <- list(option1 = "First Option", option2 = "Second Option")
#' sentence <- construct_general_sentence(input_value, english_text, french_text, lang, labels)
#' print(sentence)
#'
#' @noRd
construct_general_sentence <- function(input_value, english_text, french_text, lang, labels = NULL, unknown_label = "Unknown choice") {
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
  sentence <- if (lang == "ENG") {
    paste0(english_text, " ", label, ".")
  } else {
    paste0(french_text, " ", label, ".")
  }

  return(sentence)
}

#' Construct Context Sentence
#'
#' @description This function constructs a sentence based on the provided input value, English text, French text, and language.
#'
#' @param input_value The input value to be used in the sentence construction.
#' @param english_text The English text to be used in the sentence construction.
#' @param french_text The French text to be used in the sentence construction.
#' @param lang A character string specifying the language, either "ENG" for English or "FR" for French.
#'
#' @return A character string representing the constructed sentence.
#'
#' @details The function constructs a sentence based on the following logic:
#'   - If the input value is NA, NULL, or an empty string, an empty string is returned.
#'   - The input value is directly used as the label in the sentence.
#'   - The sentence is constructed by concatenating the appropriate text (English or French) with the label and a period.
#'
#' @examples
#' input_value <- "context1"
#' english_text <- "The selected context is"
#' french_text <- "Le contexte s\u00e9lectionn\u00e9 est"
#' lang <- "ENG"
#' sentence <- construct_context_sentence(input_value, english_text, french_text, lang)
#' print(sentence)
#'
#' @noRd
construct_context_sentence <- function(input_value, english_text, french_text, lang) {
  # Check if input value is NA, NULL, or empty
  if (is.null(input_value) || is.na(input_value) || input_value == "") {
    return("")
  }

  # Directly use the input value (selected value) for communication context
  label <- input_value

  # Construct the sentence based on the language
  sentence <- if (lang == "ENG") {
    paste0(english_text, " ", label, ".")
  } else {
    paste0(french_text, " ", label, ".")
  }

  return(sentence)
}






#' Generate Input Field
#'
#' @description This function generates an input field based on the provided parameters.
#'
#' @param ns A namespace function.
#' @param input_id A character string specifying the input ID.
#' @param label_text A character string specifying the label text for the input field.
#' @param input_type A character string specifying the type of input field. Possible values are "text" (default), "select", or "radio".
#' @param choices A vector or list of choices for the "select" or "radio" input types (optional).
#' @param selected The default selected value for the "select" or "radio" input types (optional).
#'
#' @return A Shiny input element based on the specified type.
#'
#' @details The function generates an input field based on the following logic:
#'   - If `input_type` is "text", a `textInput` is created using the `ns` function, `input_id`, and `label_text`.
#'   - If `input_type` is "select", a `selectInput` is created using the `ns` function, `input_id`, `label_text`, `choices`, and `selected`.
#'   - If `input_type` is "radio", `radioButtons` are created using the `ns` function, `input_id`, `label_text`, `choices`, and `selected`.
#'
#' @examples
#' # Generate a text input field
#' input_text <- generate_input_field(ns, "text_input", "Enter text:")
#'
#' # Generate a select input field with choices
#' input_select <- generate_input_field(ns, "select_input", "Select an option:", input_type = "select",
#'                                      choices = c("Option 1", "Option 2", "Option 3"), selected = "Option 1")
#'
#' # Generate radio buttons with choices
#' input_radio <- generate_input_field(ns, "radio_input", "Select a value:", input_type = "radio",
#'                                     choices = c("Value 1", "Value 2", "Value 3"), selected = "Value 2")
#'
#' @import shiny
#'
#' @noRd
generate_input_field <- function(ns, input_id, label_text, input_type = "text", choices = NULL, selected = NULL) {
  if (input_type == "text") {
    textInput(ns(input_id), label_text)
  } else if (input_type == "select") {
    selectInput(ns(input_id), label_text, choices = choices, selected = selected)
  } else if (input_type == "radio") {
    radioButtons(ns(input_id), label_text, choices = choices, selected = selected)
  }
}
