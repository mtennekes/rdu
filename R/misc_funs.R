find_kde_peak <- function(x, bandwidth = NULL, grid_size = 1024) {
  print("x")
  print(x)
  print("END x")
  # Check if x is numeric
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }

  # Remove NA values
  x <- na.omit(x)

  # If no bandwidth is provided, use the default (Sheather-Jones)
  if (is.null(bandwidth)) {
    bandwidth <- "SJ"  # Could also use "nrd0" (Silverman's rule), "ucv", "bcv", or "SJ-ste"
  }

  # Perform Kernel Density Estimation
  print(x)
  print(bandwidth)
  print(grid_size)
  dens <- density(x, bw = bandwidth, n = grid_size)

  # Find the x-value corresponding to the maximum density
  peak_x <- dens$x[which.max(dens$y)]
  peak_y <- max(dens$y)

  # Return a list containing the peak x-value, the density object, and the peak density
  return(list(peak_x = peak_x, density = dens, peak_y = peak_y))
}

#' Fuzzy substring distance
#'
#' Fuzzy substring distance
#'
#' @param texts vector of texts
#' @param pattern pattern to look for
#' @param ... arguments passed on to `stringdist::stringdist()`
#' @return distance per text
fuzzy_substring_distance <- function(texts, pattern, ...) {
  n <- nchar(pattern)
  result <- numeric(length(texts))
  for (i in seq_along(texts)) {
    txt <- texts[i]
    m <- nchar(txt)
    if (m < n) {
      result[i] <- stringdist::stringdist(txt, pattern, ...)
    } else {
      starts <- 1:(m - n + 1)
      ends <- n:(m)
      substrings <- mapply(substr, txt, starts, ends)
      distances <- stringdist::stringdist(substrings, pattern, ...)
      result[i] <- min(distances)
    }
  }
  return(result)
}



count_letters <- function(x) nchar(gsub("[^a-zA-Z]", "", x))
count_numbers <- function(x) nchar(gsub("[^0-9]", "", x))

# Heuristieken -----------------------------------------------------------------

# Regular expression to match numbers with optional minus sign,
# commas or dots as decimal separators, and thousands separators.
# Handles variations like "-123.45", "123,456.78", "-1.234,56"
regex_numeric <- "^-?\\d{1,3}(?:[.,]\\d{3})*(?:[.,]\\d+)?$"
regex_uppercase <- "^[A-Z]+$"
regex_multiplier <- "^\\d+\\s*[Xx]$"
regex_special <- "^[^a-zA-Z0-9]+$"
regex_any_letter <- "[a-zA-Z]"
regex_any_number <- "[0-9]"

only_uppercase <- function(x) grepl(regex_uppercase, x)
no_lowercase <- function(x) x == toupper(x)

is_number <- function(x) {
  n_numbers <- count_numbers(x)

  !is.na(x) & n_numbers > 0 & (n_numbers > (nchar(x) - 3))
}
is_multiplier <- function(x) grepl(regex_multiplier, x)
is_special <- function(x) grepl(regex_special, x)
contains_letter <- function(x) grepl(regex_any_letter, x)
contains_number <- function(x) grepl(regex_any_number, x)



# convert a string to numeric:
# - anything is accept that contains at least one number and at most one , or . inside the string (otherwise it is not clear where the decimal symbol would be)
# - note: heading/training decimals symbols will be disregarded, because these only would appear if there is a number next to it. In case of two neighboring numbers, the second one is regarded as number behind the decimal symbol
convert_to_numeric <- function(x) {
  x <- gsub(",", ".", x)
  x <- gsub("^[.]+|[.]+$", "", x) # remove head/trail .
  x <- gsub("[^0-9.]", "", x) # Remove non-number characters (except .)
  y = suppressWarnings(as.numeric(x))
  ifelse(is.na(y), NA_character_, x)
}

convert_multi = function(x) {

  if (length(x) > 1) {
    x = sapply(x, convert_to_numeric)
    y = as.numeric(x)
    if (any(is.na(y)) | any(y != round(y))) {
      paste0(x, collapse = "")
    } else {
      paste0(x, collapse = ".")
    }
  } else {
    convert_to_numeric(x)
  }
}

multiplier_values <- function(x) {
  ifelse(
    is_multiplier(x),
    gsub("X", "", x), # Remove "X" and convert to numeric
    NA_character_
  ) |>
    as.integer()
}


subset_df = function(df, xmin, xmax, lnset = NULL) {
  if (is.null(lnset)) {
    df %>%
      filter(x >= xmin,
             x <= xmax)
  } else {
    df %>%
      filter(x >= xmin,
             x <= xmax,
             ln %in% lnset)
  }
}

create_lines = function(df) {
  df_set = split(df, df$ln)
  txts = lapply(df_set, function(d) {
    x = paste(arrange(d, x)$text, collapse = " ")
    gsub("(\\d)([.,])\\s+(\\d)", "\\1\\2\\3", x)
  })
  if (length(txts)) {
    txts_vec = unname(unlist(txts))
  } else {
    txts_vec = character(0)
  }
  data.frame(ln = as.integer(names(txts)),
             text = txts_vec)
}


edit_price_vec = function(prijzen, keep_0 = FALSE) {
  # Stap 1: Vervang () door 0
  prijzen <- gsub("\\(\\)", "0", prijzen)

  # Stap 2: Verwijder ongeldige tekens zoals € en -
  prijzen <- gsub("[^0-9.,]", "", prijzen)  # Verwijder alles behalve cijfers, komma en punt

  # Stap 3: Vervang komma door punt
  prijzen <- gsub(",", ".", prijzen)

  # Stap 4: Zet getallen met 3 of meer cijfers om naar x.xx
  prijzen <- gsub("(\\d{1,})(\\d{2})", "\\1.\\2", prijzen)

  # Stap 5: Zet "89" om naar "0.89"
  prijzen <- gsub("^([0-9]{2})$", "0.\\1", prijzen)

  # Stap 6: Zet lege of ongeldige waarden om naar NA
  if (keep_0) {
    prijzen <- ifelse(prijzen == "0", "0", ifelse(prijzen == "" | !grepl("^\\d+\\.\\d{2}$", prijzen), NA, prijzen))
  } else {
    prijzen <- ifelse(prijzen == "" | !grepl("^\\d+\\.\\d{2}$", prijzen), NA, prijzen)
  }


  # Stap 7: Converteer naar numerieke waarden (optioneel)
  prijzen_num <- as.numeric(prijzen)
  prijzen_num
}
