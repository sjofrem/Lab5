#' KPIs
#' @param id vector of KPI ids; if given, filters by id
#' @param title substring match on title
#' @param page Page number
#' @param per_page results per page
#' @return Kolada envelope: list(values, count, next_page, previous_page)
#' @export
kpi <- function(id = NULL, title = NULL, page = 1, per_page = 5000) {
  req <- k_req()

  if (!is.null(id)) {
    ids <- to_csv(id)
    req <- req |>
      httr2::req_url_path_append(paste0("kpi/", ids))
  } else {
    req <- req |> httr2::req_url_path_append("kpi")
    if (!is.null(title)) {
      req <- httr2::req_url_query(req, title = title)
    }
  }

  req <- req |>
    httr2::req_url_query(page = page, per_page = per_page)

  k_do(req)
}

#' KPI groups
#' @param id vector of KPI group ids; if given, filters by id
#' @param title substring match on title
#' @param page Page number
#' @param per_page results per page
#' @return Kolada envelope: list(values, count, next_page, previous_page)
#' @export
kpi_groups <- function(id = NULL, title = NULL, page = 1, per_page = 5000) {
  req <- k_req()

  if (!is.null(id)) {
    ids <- to_csv(id)
    req <- req |>
      httr2::req_url_path_append(paste0("kpi_groups/", ids))
  } else {
    req <- req |> httr2::req_url_path_append("kpi_groups")
    if (!is.null(title)) {
      req <- httr2::req_url_query(req, title = title)
    }
  }

  req <- req |>
    httr2::req_url_query(page = page, per_page = per_page)

  k_do(req)
}

#' Municipality
#' @param id vector of municipalities ids; if given, filters by id
#' @param title substring match on title
#' @param type substring match on type. It can be "municipality" or "region"
#' @param page Page number
#' @param per_page results per page
#' @return Kolada envelope: list(values, count, next_page, previous_page)
#' @export
municipality <- function(id = NULL, title = NULL, type = NULL, page = 1, per_page = 5000) {
  req <- k_req()

  if (!is.null(id)) {
    ids <- to_csv(id)
    req <- req |>
      httr2::req_url_path_append(paste0("municipality/", ids))
  } else {
    req <- req |> httr2::req_url_path_append("municipality")
    if (!is.null(type)) {
      req <- httr2::req_url_query(req, region_type = type)
    }
    if (!is.null(title)) {
      req <- httr2::req_url_query(req, title = title)
    }
  }

  req <- req |>
    httr2::req_url_query(page = page, per_page = per_page)

  k_do(req)
}

#' Municipality groups
#' @param id municipality id(s); if given, filter by id
#' @param title substring match on title
#' @param page Page number
#' @param per_page results per page
#' @return Kolada envelope: list(values, count, next_page, previous_page)
#' @export
municipality_group <- function(id = NULL, title = NULL, page = 1, per_page = 5000) {
  req <- k_req()

  if (!is.null(id)) {
    ids <- to_csv(id)
    req <- req |>
      httr2::req_url_path_append(paste0("municipality_group/", ids))
  } else {
    req <- req |> httr2::req_url_path_append("municipality_group")
    if (!is.null(title)) {
      req <- httr2::req_url_query(req, title = title)
    }
  }

  req <- req |>
    httr2::req_url_query(page = page, per_page = per_page)

  k_do(req)
}

#' Organizational units (OU)
#' @param id ou id(s); if given, filter by id
#' @param municipality_id vector of municipalities ids; if given, filters by id
#' @param title substring match on title
#' @param page Page number
#' @param per_page results per page
#' @return Kolada envelope: list(values, count, next_page, previous_page)
#' @export
ou <- function(id = NULL, municipality_id = NULL, title = NULL, page = 1, per_page = 5000) {
  req <- k_req()

  if (!is.null(id)) {
    ids <- to_csv(id)
    req <- req |>
      httr2::req_url_path_append(paste0("ou/", ids))
  } else {
    req <- req |> httr2::req_url_path_append("ou")
    if (!is.null(municipality_id)) {
      req <- httr2::req_url_query(req, municipality = to_csv(municipality_id))
    }
    if (!is.null(title)) {
      req <- httr2::req_url_query(req, title = title)
    }
  }

  req <- req |>
    httr2::req_url_query(page = page, per_page = per_page)

  k_do(req)
}
