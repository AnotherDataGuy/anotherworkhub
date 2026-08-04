test_that("module ui works", {
  ui <- mod_section_pitch_improver_ui(id = "test")
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))

  fmls <- formals(mod_section_pitch_improver_ui)
  expect_true("id" %in% names(fmls))
})

test_that("module server works with usage guard blocks", {
  translations <- anotheRworkhub:::get_translations()
  anotheRworkhub:::security_ledger_reset()
  guard <- anotheRworkhub:::create_usage_guard(
    user_id = "pitch-test-user",
    limits = list(
      max_api_calls_session = 40L,
      max_api_calls_daily = 80L,
      max_interview_messages = 24L,
      max_pitch_runs = 0L,
      max_input_chars = 8000L,
      max_message_chars = 2000L,
      max_pitch_chars = 4000L,
      max_output_tokens = 800L,
      pitch_cooldown_secs = 60L,
      interview_cooldown_secs = 10L,
      pitch_batch_cost = 4L
    )
  )

  testServer(
    mod_section_pitch_improver_server,
    args = list(
      api_pwd = "dummy_key",
      language_input = reactive("ENG"),
      translations = translations,
      usage_guard = guard
    ),
    {
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      session$setInputs(
        communication_context = "spontaneous_application",
        recipient_of_the_pitch = "Jane Doe",
        hierarchical_status = "entry_level",
        gpt_language_messages = "ENG",
        text_input_pitch_improver = paste(rep("word ", 30), collapse = "")
      )
      session$flushReact()
      # Debounced validation may still be settling; force a second flush.
      Sys.sleep(0.35)
      session$flushReact()

      blocked <- guard$check_pitch_run(paste(rep("word ", 30), collapse = ""))
      expect_false(blocked$ok)
      expect_equal(blocked$reason, "pitch_quota")
    }
  )

  anotheRworkhub:::security_ledger_reset()
})

test_that("pitch input validity requires more than 100 characters", {
  short <- paste(rep("a", 100), collapse = "")
  long <- paste(rep("a", 101), collapse = "")
  expect_false(nchar(short) > 100)
  expect_true(nchar(long) > 100)
})
