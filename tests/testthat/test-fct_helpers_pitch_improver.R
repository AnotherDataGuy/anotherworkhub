test_that("renderMarkdown handles different input formats correctly", {
  # Test empty inputs
  expect_equal(as.character(renderMarkdown("")), "")
  expect_equal(as.character(renderMarkdown(NULL)), "")

  # Test JSON input
  json_input <- '{"name": "John", "age": 30}'
  expect_type(renderMarkdown(json_input), "character")

  # Test HTML input
  html_input <- '<p>This is HTML</p>'
  expect_type(renderMarkdown(html_input), "character")

  # Test LaTeX input
  latex_input <- '$E=mc^2$'
  latex_result <- as.character(renderMarkdown(latex_input))
  expect_true(grepl('class="math"', latex_result))

  # Test plain text
  plain_input <- "Just plain text"
  expect_type(renderMarkdown(plain_input), "character")
})

test_that("calculate_summary handles text analysis correctly", {
  text <- "This is a test. It has two sentences."

  result_eng <- calculate_summary(text, language = "ENG")
  expect_s3_class(result_eng, "data.frame")
  expect_named(result_eng, c(
    "Total.Characters", "Total.Words", "Total.Sentences", "Total.Paragraphs"
  ))
  expect_equal(result_eng$Total.Characters, nchar(text))
  expect_equal(result_eng$Total.Words, 8L)
  expect_equal(result_eng$Total.Sentences, 2L)
  expect_equal(result_eng$Total.Paragraphs, 1L)

  result_fr <- calculate_summary(text, language = "FR")
  expect_s3_class(result_fr, "data.frame")
  expect_named(result_fr, c(
    "Nombre.total.de.caractères", "Nombre.total.de.mots",
    "Nombre.total.de.phrases", "Nombre.total.de.paragraphes"
  ))
  expect_equal(unname(unlist(result_fr)), unname(unlist(result_eng)))

  empty_result <- calculate_summary("", "ENG")
  expect_equal(nrow(empty_result), 1)
  expect_equal(ncol(empty_result), 4)
  expect_equal(empty_result$Total.Characters, 0)
  expect_equal(empty_result$Total.Words, 0)
})

test_that("calculate_summary counts paragraphs on blank lines", {
  text <- "First paragraph.\n\nSecond paragraph."
  result <- calculate_summary(text, language = "ENG")
  expect_equal(result$Total.Paragraphs, 2L)
})

test_that("construct_sentence generates correct output", {
  translations <- list(
    english_choices_map = list(opt1 = "First Option"),
    french_choices_map = list(opt1 = "Première Option"),
    pitch = list(unknown_choice = c(ENG = "Unknown choice", FR = "Choix inconnu"))
  )

  # Test basic English output
  expect_equal(
    construct_sentence("opt1", "The option is", "L'option est", "ENG", FALSE, translations),
    "The option is First Option."
  )

  expect_equal(
    construct_sentence("missing", "The option is", "L'option est", "FR", FALSE, translations),
    "L'option est Choix inconnu."
  )

  # Test empty input
  expect_equal(construct_sentence("", "Text is", "Le texte est", "ENG"), "")
  expect_equal(construct_sentence(NULL, "Text is", "Le texte est", "ENG"), "")
})

test_that("construct_sentence_niveau handles different inputs", {
  labels <- list(level1 = "Beginner")

  # Test basic output
  expect_equal(
    construct_sentence_niveau("level1", "Level is", "Le niveau est", "ENG", FALSE, labels),
    "Level is Beginner."
  )

  expect_equal(
    construct_sentence_niveau(
      "missing", "Level is", "Le niveau est", "FR", FALSE, labels,
      unknown_label = "Choix inconnu"
    ),
    "Le niveau est Choix inconnu."
  )

  # Test empty input
  expect_equal(
    construct_sentence_niveau("", "Level is", "Le niveau est", "ENG", FALSE, labels),
    ""
  )
})

test_that("generate_input_field creates input elements", {
  ns <- NS("test")

  # Test text input existence
  text_input <- generate_input_field(ns, "text_id", "Text Label")
  expect_s3_class(text_input, "shiny.tag")

  # Test select input existence
  select_input <- generate_input_field(
    ns,
    "select_id",
    "Select Label",
    "select",
    choices = c("A", "B")
  )
  expect_s3_class(select_input, "shiny.tag")
})
