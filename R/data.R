#' KPI data (municipality level)
#' @name kolada_data
#' @title KPI data (municipality level) for any combo of kpi / municipality / year
#' @param kpi Character vector of KPI ids (e.g., "N00003")
#' @param municipality Character vector of municipality ids (e.g., "0180")
#' @param year Numeric/character vector of years (e.g., 2019:2026)
#' @param from_date Optional "YYYY-MM-DD"…
#' @param region_type Optional "municipality" or "region"…
#' @param page Page number
#' @param per_page Items per page
#' @return Kolada envelope list(values, count, next_page/next_url, previous_*)
#' @export
kolada_data <- function(kpi = NULL, municipality = NULL, year = NULL,
                 from_date = NULL, region_type = NULL,
                 page = 1, per_page = 5000) {

  have <- c(!is.null(kpi), !is.null(municipality), !is.null(year))
  if (sum(have) < 2)
    stop("Provide at least two of: kpi, municipality, year.", call. = FALSE)
  req <- k_req()
  seg <- "data"

  if (!is.null(kpi) && !is.null(municipality) && !is.null(year)) {
    seg <- paste0(seg, "/kpi/", to_csv(kpi),
                  "/municipality/", to_csv(municipality),
                  "/year/", to_csv(year))
  } else if (!is.null(kpi) && !is.null(year)) {
    seg <- paste0(seg, "/kpi/", to_csv(kpi), "/year/", to_csv(year))
  } else if (!is.null(municipality) && !is.null(year)) {
    seg <- paste0(seg, "/municipality/", to_csv(municipality), "/year/", to_csv(year))
  } else if (!is.null(kpi) && !is.null(municipality)) {
    seg <- paste0(seg, "/kpi/", to_csv(kpi), "/municipality/", to_csv(municipality))
  }

  if (!is.null(from_date)) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", from_date))
      stop("from_date must be 'YYYY-MM-DD'.", call. = FALSE)
    req <- httr2::req_url_query(req, from_date = from_date)
  }
  if (!is.null(region_type)) {
    req <- httr2::req_url_query(req, region_type = region_type)
  }

  req <- req |>
    httr2::req_url_path_append(seg) |>
    httr2::req_url_query(page = page, per_page = per_page)

  k_do(req)
}

#' Get data by KPI + year
#' @description Convenience wrapper around [kolada_data()] for `kpi` + `year`.
#' @rdname kolada_data
#' @export
data_kpi_year <- function(kpi, year, from_date = NULL, region_type = NULL,
                          page = 1, per_page = 5000) {
  kolada_data(kpi = kpi, year = year, from_date = from_date, region_type = region_type,
              page = page, per_page = per_page)
}

#' Get data by municipality + year
#' @description Convenience wrapper around [kolada_data()] for `municipality` + `year`.
#' @rdname kolada_data
#' @export
data_municipality_year <- function(municipality, year, from_date = NULL, region_type = NULL,
                                   page = 1, per_page = 5000) {
  kolada_data(municipality = municipality, year = year,
              from_date = from_date, region_type = region_type,
              page = page, per_page = per_page)
}

#' Get data by KPI + municipality
#' @description Convenience wrapper around [kolada_data()] for `kpi` + `municipality`.
#' @rdname kolada_data
#' @export
data_kpi_municipality <- function(kpi, municipality, from_date = NULL, region_type = NULL,
                                  page = 1, per_page = 5000) {
  kolada_data(kpi = kpi, municipality = municipality,
              from_date = from_date, region_type = region_type,
              page = page, per_page = per_page)
}



#' OU-level KPI data
#' Build path using any combo of kpi / ou / year (at least two required).
#'
#' @param kpi Character vector of KPI IDs (kpi_id)
#' @param ou  Character vector of OU IDs (ou_id)
#' @param year Numeric/character vector of years
#' @param from_date Optional "YYYY-MM-DD" filter (created/updated/deleted since)
#' @param page Page number (default 1)
#' @param per_page Items per page (default 5000)
#' @return Kolada envelope list(values, count, next_page/next_url, previous_*)
#' @export
ou_data <- function(kpi = NULL, ou = NULL, year = NULL,
                    from_date = NULL, page = 1, per_page = 5000) {

  have <- c(!is.null(kpi), !is.null(ou), !is.null(year))
  if (sum(have) < 2) {
    stop("Provide at least two of: kpi, ou, year.", call. = FALSE)
  }
  req <- k_req()
  seg <- "oudata"
  if (!is.null(kpi) && !is.null(ou) && !is.null(year)) {
    seg <- paste0(
      seg, "/kpi/", to_csv(kpi),
      "/ou/",  to_csv(ou),
      "/year/", to_csv(year)
    )
  } else if (!is.null(kpi) && !is.null(year)) {
    seg <- paste0(seg, "/kpi/", to_csv(kpi), "/year/", to_csv(year))
  } else if (!is.null(kpi) && !is.null(ou)) {
    seg <- paste0(seg, "/kpi/", to_csv(kpi), "/ou/", to_csv(ou))
  } else if (!is.null(ou) && !is.null(year)) {
    seg <- paste0(seg, "/ou/", to_csv(ou), "/year/", to_csv(year))
  }
  if (!is.null(from_date)) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", from_date))
      stop("from_date must be 'YYYY-MM-DD'.", call. = FALSE)
    req <- httr2::req_url_query(req, from_date = from_date)
  }

  req <- req |>
    httr2::req_url_path_append(seg) |>
    httr2::req_url_query(page = page, per_page = per_page)

  k_do(req)
}

#' OU data by KPI + year
#' @description Convenience wrapper around [ou_data()] for `kpi` + `year`.
#' @inheritParams ou_data
#' @param kpi Character vector of KPI ids.
#' @param year Numeric/character vector of years.
#' @return Kolada envelope list(values, count, next_page/next_url, previous_*).
#' @export
ou_data_kpi_year <- function(kpi, year, from_date = NULL, page = 1, per_page = 5000) {
  ou_data(kpi = kpi, year = year, from_date = from_date, page = page, per_page = per_page)
}

#' OU data by KPI + OU
#' @description Convenience wrapper around [ou_data()] for `kpi` + `ou`.
#' @inheritParams ou_data
#' @param kpi Character vector of KPI ids.
#' @param ou  Character vector of OU ids.
#' @return Kolada envelope list(values, count, next_page/next_url, previous_*).
#' @export
ou_data_kpi_ou <- function(kpi, ou, from_date = NULL, page = 1, per_page = 5000) {
  ou_data(kpi = kpi, ou = ou, from_date = from_date, page = page, per_page = per_page)
}

#' OU data by OU + year
#' @description Convenience wrapper around [ou_data()] for `ou` + `year`.
#' @inheritParams ou_data
#' @param ou Character vector of OU ids.
#' @param year Numeric/character vector of years.
#' @return Kolada envelope list(values, count, next_page/next_url, previous_*).
#' @export
ou_data_ou_year <- function(ou, year, from_date = NULL, page = 1, per_page = 5000) {
  ou_data(ou = ou, year = year, from_date = from_date, page = page, per_page = per_page)
}


