#' Pick a bilingual string for the active UI language
#'
#' @param item Named character vector with `ENG` and/or `FR` keys.
#' @param lang Language code: `"ENG"` or `"FR"`.
#' @return Localized string.
#' @noRd
t_lang <- function(item, lang) {
  if (is.null(item)) {
    return("")
  }
  lang <- lang %||% "FR"
  if (!is.null(item[[lang]])) {
    return(item[[lang]])
  }
  if (!is.null(item[["ENG"]])) {
    return(item[["ENG"]])
  }
  if (length(item) > 0) {
    return(item[[1]])
  }
  ""
}

#' Nested translation lookup: `t_path(translations, "interview", "labels", "position", lang)`
#'
#' @noRd
t_path <- function(translations, ..., lang) {
  node <- translations
  for (key in list(...)) {
    node <- node[[key]]
    if (is.null(node)) {
      return("")
    }
  }
  t_lang(node, lang)
}
