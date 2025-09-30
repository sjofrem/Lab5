#' @keywords internal
k_check <- function(resp) {
  if (!httr2::resp_is_error(resp)) return(resp)
  body <- tryCatch(httr2::resp_body_json(resp, check_type = FALSE), error = function(e) NULL)
  msg <- sprintf("HTTP %s %s", httr2::resp_status(resp), httr2::resp_status_desc(resp))
  if (is.list(body)) {
    detail <- body$message %||% body$error %||% body$detail %||% NULL
    if (!is.null(detail)) msg <- paste(msg, "-", detail)
  }
  stop(msg, call. = FALSE)
}
`%||%` <- function(x, y) if (!is.null(x)) x else y
