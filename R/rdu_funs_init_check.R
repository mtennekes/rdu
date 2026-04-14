#' Initiate a rdu object
#'
#' Initiate a rdu object
#'
#' @param file_image path the the image file (required)
#' @param file_OCR path the the OCR file (required)
#' @param meta meta information, not used yet
#' @param shop Set the shop template (use `NA` to determine automatically later on)
#' @param total_discount Total price paid, so with discounts already applied. If not specified it is read from the receipt.
#' @return an rdu object
#' @import optparse readr jpeg grid png dplyr tidyr yaml jsonlite
#' @importFrom lobstr tree
#' @importFrom cli cli_warn cli_abort cli_inform
#' @importFrom stringdist stringdist
#' @export
rdu_init = function(file_image, file_OCR, meta = list(), shop = NA, total_discount = NA) {
  if (!is.na(shop)) { #cli_abort("Automatic shop detection not yet implemented")
    if (!(shop %in% names(templates))) cli_abort("shop {.str {shop}} unknown")
  }

  if (missing(file_image)) cli_abort("file_image missing ")
  if (!file.exists(file_image)) cli_abort("file_image incorrect")

  if (missing(file_OCR)) cli_abort("file_OCR missing ")
  if (!file.exists(file_OCR)) cli_abort("file_OCR incorrect")

  if (!is.na(total_discount)) total_discount = data.frame(value = total_discount)

  structure(list(files = list(image = file_image, OCR = file_OCR),
                 ocr = NULL,
                 shop = shop,
                 total_discount = total_discount,
                 meta = meta,
                 throughput = list(),
                 scores = list()),
            class = c("rdu", "list"))
}

#' Check the receipt image
#'
#' Check the receipt image
#'
#' @param r `rdu` object
#' @param include_image should the image itself be included in the returned `rdu` object? `FALSE` by default
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_check_image = function(r, include_image = FALSE) {
  # read image
  is_png = grepl("\\.png$", r$files$image, ignore.case = TRUE)
  is_jpg = grepl("\\.jpe?g$", r$files$image, ignore.case = TRUE)

  if (!is_png && !is_jpg) {
    r$abort = "check_image: unsupported file type"
    return(r)
  }

  tryCatch({
      if (is_png) {
        image = png::readPNG(r$files$image)
      } else if (is_jpg) {
        image = jpeg::readJPEG(r$files$image)
      }
      if (include_image) r$image = image
      r$meta$image_dim = dim(image)
      r
    },
    error = function(e) {
      r$abort = "check_image: corrupt image file"
      r
    }
  )

}

#' Check the OCR file
#'
#' Check the OCR file. Currently checks if the variables text, left, top, width, block_num, par_num and line_num are present. The latter three are probably typical for Tesseract. This function needs to be updated to support Paddle output.
#'
#' @param r `rdu` object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_check_OCR = function(r) {
  if ("abort" %in% names(r)) return(r)

  is_tsv = grepl("\\.tsv$", r$files$OCR, ignore.case = TRUE)

  if (!is_tsv) cli_abort("file_OCR should be a tsv file. Other formats not (yet) supported")

  # read ocr data
  df = read_tsv(r$files$OCR)

  missings = setdiff(c("text", "left", "top", "width", "line_num"), names(df))
  if (length(missings)) {
    r$abort("check_OCR: the following columns are missing: {.str {missings}}")
    return(r)
  } else {
    r$ocr = df %>% filter(!is.na(text))
  }
  r
}
