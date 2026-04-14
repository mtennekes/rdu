json_list2tibble = function(x) {
  purrr::map_dfr(x, ~tibble::as_tibble_row(.x))
}

path_exists <- function(x, path) {
  !identical(
    purrr::pluck(x, !!!path, .default = NULL),
    NULL
  )
}

list_to_vector_strict <- function(x) {
  if (!is.list(x)) {
    stop("Expected a list, got: ", typeof(x), call. = FALSE)
  }

  if (any(vapply(x, is.list, logical(1)))) {
    stop("Cannot cast: list contains nested list(s)", call. = FALSE)
  }

  vec <- unlist(x, recursive = FALSE, use.names = TRUE)

  if (!is.atomic(vec)) {
    stop("Result is not an atomic vector", call. = FALSE)
  }

  vec
}

#' Read rdu object from json file
#'
#' Read rdu object from json file
#'
#' @param file json file
#' @return `rdu` object
#' @export
rdu_read = function(file) {
  j = jsonlite::read_json(file)

  # data frames
  nms = c("ocr", "total_discount", "total_no_discount", "table_raw", "table_processed", "throughput$ocr_filter")
  for (nm in nms) {
    ml = strsplit(nm, "$", fixed = TRUE)[[1]]
    if (path_exists(j, ml)) {
      j = purrr::modify_in(j, ml, json_list2tibble)
    }
  }

  # unnamed vectors
  nms = c("meta$image_dim", "meta$lns$products", "meta$lns$prices")
  for (nm in nms) {
    ml = strsplit(nm, "$", fixed = TRUE)[[1]]
    if (path_exists(j, ml)) {
      j = purrr::modify_in(j, ml, list_to_vector_strict)
    }
  }

  class(j) = c("rdu", "list")
  j

}

#' Write rdu object to json file
#'
#' Write rdu object to json file
#'
#' @param x `rdu` object
#' @param file target json file name
#' @param include_throughput should intermediate output be included?
#' @return `x`
#' @export
#' @importFrom purrr map_dfr pluck modify_in
rdu_write = function(x, file, include_throughput = FALSE) {
  local({
    if (!include_throughput) {
      x$ocr = NULL
      x$meta = NULL
      x$throughput = NULL
      x$table_raw = NULL
    }
    rj = jsonlite::toJSON(x)
    dir = dirname(file)
    if (!dir.exists(dir)) stop("Folder ", dir, " not found")
    write(rj, file = file)
  })
  x
}
