#' @keywords internal
k_req <- function(base = getOption("kolada.base_url")) {
  httr2::request(base) |>
    httr2::req_user_agent(getOption("kolada.ua")) |>
    httr2::req_timeout(getOption("kolada.timeout")) |>
    httr2::req_headers(Accept = "application/json")
}

#' @keywords internal
k_do <- function(req) {
  req |>
    httr2::req_retry(max_tries = 3, backoff = ~ 0.5 * 2^(.x - 1)) |>
    httr2::req_perform() |>
    k_check() |>
    httr2::resp_body_json(check_type = FALSE, simplifyVector = TRUE)
}
