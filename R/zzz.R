.onLoad <- function(libname, pkgname) {
  op <- list(
    kolada.base_url = "https://api.kolada.se/v3",
    kolada.timeout  = 30
  )
  toset <- !(names(op) %in% names(options()))
  if (any(toset)) options(op[toset])
  invisible()
}
