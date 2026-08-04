collect_bilingual_gaps <- function(x, path = character()) {
  gaps <- character()
  if (is.character(x) && length(x) > 0 && !is.null(names(x))) {
    if ("ENG" %in% names(x) || "FR" %in% names(x)) {
      id <- if (length(path)) paste(path, collapse = ".") else "<root>"
      if (!("ENG" %in% names(x))) gaps <- c(gaps, paste0(id, ": missing ENG"))
      if (!("FR" %in% names(x))) gaps <- c(gaps, paste0(id, ": missing FR"))
    }
    return(gaps)
  }
  if (is.list(x)) {
    for (nm in names(x)) {
      gaps <- c(
        gaps,
        collect_bilingual_gaps(x[[nm]], c(path, nm))
      )
    }
  }
  gaps
}

test_that("get_translations returns a structured bundle", {
  tr <- anotheRworkhub:::get_translations()
  expect_type(tr, "list")
  expect_true(all(c("app", "home", "interview", "pitch") %in% names(tr)))
})

test_that("all bilingual strings define ENG and FR", {
  tr <- anotheRworkhub:::get_translations()
  gaps <- collect_bilingual_gaps(tr)
  expect_equal(gaps, character())
})

test_that("t_lang resolves UI languages with sensible fallbacks", {
  item <- c(ENG = "Hello", FR = "Bonjour")
  expect_equal(anotheRworkhub:::t_lang(item, "ENG"), "Hello")
  expect_equal(anotheRworkhub:::t_lang(item, "FR"), "Bonjour")
  expect_equal(anotheRworkhub:::t_lang(item, NULL), "Bonjour")
  expect_equal(anotheRworkhub:::t_lang(NULL, "FR"), "")
})

test_that("t_path resolves nested interview labels", {
  tr <- anotheRworkhub:::get_translations()
  expect_equal(
    anotheRworkhub:::t_path(tr, "interview", "labels", "position", lang = "FR"),
    "Poste"
  )
  expect_equal(
    anotheRworkhub:::t_path(tr, "interview", "labels", "missing_key", lang = "ENG"),
    ""
  )
})

test_that("home how/stance copy exists in both UI languages", {
  tr <- anotheRworkhub:::get_translations()
  for (key in c("how_title", "how_body", "stance_title", "stance_body")) {
    expect_true(nzchar(anotheRworkhub:::t_lang(tr$home[[key]], "ENG")))
    expect_true(nzchar(anotheRworkhub:::t_lang(tr$home[[key]], "FR")))
  }
  expect_match(tr$home$stance_body[["ENG"]], "not doing the work for you")
  expect_match(tr$home$stance_body[["FR"]], "esprit critique")
})

test_that("interview and pitch choice lists stay aligned by length", {
  tr <- anotheRworkhub:::get_translations()
  for (key in names(tr$interview$choices)) {
    expect_length(tr$interview$choices[[key]]$ENG, length(tr$interview$choices[[key]]$FR))
  }
  expect_length(tr$english_choices_map, length(tr$french_choices_map))
  expect_setequal(names(tr$english_choices_map), names(tr$french_choices_map))
})

test_that("new interview notification strings are bilingual", {
  tr <- anotheRworkhub:::get_translations()
  i18n <- tr$interview
  for (key in c(
    "notify_api_key_missing",
    "notify_context_error",
    "notify_error",
    "notify_cooldown",
    "notify_session_quota",
    "notify_daily_quota",
    "notify_interview_message_quota",
    "notify_message_too_long"
  )) {
    expect_true("ENG" %in% names(i18n[[key]]))
    expect_true("FR" %in% names(i18n[[key]]))
    expect_true(nzchar(anotheRworkhub:::t_lang(i18n[[key]], "ENG")))
    expect_true(nzchar(anotheRworkhub:::t_lang(i18n[[key]], "FR")))
  }
})

test_that("pitch security notification strings are bilingual", {
  tr <- anotheRworkhub:::get_translations()
  p18n <- tr$pitch
  for (key in c(
    "notify_session_quota",
    "notify_daily_quota",
    "notify_pitch_quota",
    "notify_pitch_cooldown",
    "notify_pitch_in_flight",
    "notify_pitch_too_long",
    "notify_api_key_missing"
  )) {
    expect_true("ENG" %in% names(p18n[[key]]))
    expect_true("FR" %in% names(p18n[[key]]))
    expect_true(nzchar(anotheRworkhub:::t_lang(p18n[[key]], "ENG")))
    expect_true(nzchar(anotheRworkhub:::t_lang(p18n[[key]], "FR")))
  }
})

test_that("pitch recap_empty and unknown_choice resolve in both languages", {
  tr <- anotheRworkhub:::get_translations()
  p18n <- tr$pitch
  expect_equal(anotheRworkhub:::t_lang(p18n$recap_empty, "ENG"), "No prompt summary available yet.")
  expect_equal(anotheRworkhub:::t_lang(p18n$recap_empty, "FR"), "Aucun récapitulatif disponible pour le moment.")
  expect_equal(anotheRworkhub:::t_lang(p18n$unknown_choice, "FR"), "Choix inconnu")
})
