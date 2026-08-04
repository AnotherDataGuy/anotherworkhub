test_that("interview simulator UI renders", {
  ui <- mod_section_interview_simulator_ui(id = "test")
  expect_s3_class(ui, "shiny.tag")
  expect_true("id" %in% names(formals(mod_section_interview_simulator_ui)))
})

test_that("renderMarkdown strips script and event handlers from model text", {
  dirty <- paste(
    "Nice answer.",
    '<script>alert("xss")</script>',
    '<p onclick="alert(1)">Keep me</p>',
    '<iframe src="https://evil.test"></iframe>'
  )
  html <- as.character(anotheRworkhub:::renderMarkdown(dirty))
  expect_false(grepl("<script", html, ignore.case = TRUE))
  expect_false(grepl("onclick", html, ignore.case = TRUE))
  expect_false(grepl("<iframe", html, ignore.case = TRUE))
  expect_match(html, "Keep me")
})

test_that("interview module server namespace works", {
  translations <- anotheRworkhub:::get_translations()
  testServer(
    mod_section_interview_simulator_server,
    args = list(
      api_pwd = "dummy_key",
      language_input = reactive("FR"),
      translations = translations,
      usage_guard = NULL
    ),
    {
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))
    }
  )
})

test_that("interview send_message is blocked by usage guard before any API call", {
  translations <- anotheRworkhub:::get_translations()
  anotheRworkhub:::security_ledger_reset()
  on.exit(anotheRworkhub:::security_ledger_reset(), add = TRUE)

  guard <- anotheRworkhub:::create_usage_guard(
    user_id = "interview-quota-user",
    limits = list(
      max_api_calls_session = 40L,
      max_api_calls_daily = 80L,
      max_interview_messages = 0L,
      max_pitch_runs = 5L,
      max_input_chars = 8000L,
      max_message_chars = 2000L,
      max_pitch_chars = 4000L,
      max_output_tokens = 800L,
      pitch_cooldown_secs = 30L,
      interview_cooldown_secs = 0L,
      pitch_batch_cost = 4L
    )
  )

  api_called <- FALSE
  with_mocked_bindings(
    process_message = function(...) {
      api_called <<- TRUE
      "should not run"
    },
    .package = "anotheRworkhub",
    {
      testServer(
        mod_section_interview_simulator_server,
        args = list(
          api_pwd = "dummy_key",
          language_input = reactive("ENG"),
          translations = translations,
          usage_guard = guard
        ),
        {
          # Activate chat path without going through start_interview API.
          rv$chat_active <- TRUE
          rv$thread_id <- "conv_test"
          session$setInputs(user_input = "This is a real answer", send_message = 1L)
          session$flushReact()
          expect_false(api_called)
          expect_equal(guard$get_usage()$interview_messages, 0L)
        }
      )
    }
  )
})

test_that("interview start is blocked when API key is missing", {
  translations <- anotheRworkhub:::get_translations()
  api_called <- FALSE

  with_mocked_bindings(
    init_conversation = function(...) {
      api_called <<- TRUE
      "conv_x"
    },
    process_message = function(...) {
      api_called <<- TRUE
      "hello"
    },
    .package = "anotheRworkhub",
    {
      testServer(
        mod_section_interview_simulator_server,
        args = list(
          api_pwd = "",
          language_input = reactive("FR"),
          translations = translations,
          usage_guard = NULL
        ),
        {
          session$setInputs(
            name = "Ada",
            job_title = "Analyste",
            company_sector = "Education",
            gpt_language_messages = "FR",
            start_interview = 1L
          )
          session$flushReact()
          expect_false(api_called)
          expect_null(rv$thread_id)
        }
      )
    }
  )
})
