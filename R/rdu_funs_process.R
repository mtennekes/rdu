#' Check the OCR file
#'
#' Check the OCR file. Currently checks if the variables text, left, top, width, block_num, par_num and line_num are present. The latter three are probably typical for Tesseract. This function needs to be updated to support Paddle output.
#'
#' @param r `rdu` object
#' @param params the parameters object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_filter_cpl = function(r, params) {
  if ("abort" %in% names(r)) return(r)

  #width, df, template) {
  ##### step 2: compute cpl (characters per line) and filter on it

  width = r$meta$image_dim[2]

  ocr = r$ocr

  ocr$cpl = width / (ocr$width / nchar(ocr$text))

  df = ocr %>%
    mutate(too_large = cpl < params$chars_per_line$max,
           too_small = cpl > params$chars_per_line$min)

  cpl_too_small = round(sum(df$too_small) / nrow(df), 3)
  cpl_too_large = round(sum(df$too_large) / nrow(df), 3)

  df = df %>%
    filter(!too_large, !too_small) %>%
    mutate(too_large = NULL, too_small = NULL)


  r$throughput$ocr_filter = df


  if (nrow(df) < 2) {
    r$abort = "filter_cpl: ocr data contains 0 or 1 rows"
    return(r)
  }

  r$scores = c(r$scores, list(cpl_too_small = cpl_too_small,
                              cpl_too_large = cpl_too_large))
  r
}

# Not used anymore: now the cpl is calculated for general upper and lowerbound (du_params) instead of per shop.
# Why? In order to detect the shop, this filter should ideally already be applied. It is possible, because there is hardly any differences between supermarkts

# # Compute
# rdu_filter_cpl2 = function(r, templates) {
#   #width, df, template) {
#   ##### step 2: compute cpl (characters per line) and filter on it
#
#   width = r$meta$image_dim[2]
#
#   r$ocr$cpl = width / (r$ocr$width / nchar(r$ocr$text))
#
#   # determine median cpl
#   cpl_peak = round(find_kde_peak(r$ocr$cpl)$peak_x, 3)
#
#   # shop template
#   tp = templates[[r$shop]]
#
#   # indicator: cpl correction: should be close to 1
#   cpl_peak_rel = round(cpl_peak / tp$chars_per_line$std, 3)
#
#   df = r$ocr %>%
#     mutate(too_large = cpl < tp$chars_per_line$max,
#            too_small = cpl > tp$chars_per_line$min)
#
#   cpl_too_small = round(sum(df$too_small) / nrow(df), 3)
#   cpl_too_large = round(sum(df$too_large) / nrow(df), 3)
#
#   df = df %>%
#     filter(!too_large, !too_small)
#
#   r$throughput$ocr_filter = df
#   r$scores = c(r$scores, list(cpl_peak = cpl_peak,          # cpl peak (ab)
#                   cpl_peak_rel = cpl_peak_rel, # cpl peak (rel w.r.t. template std cpl)
#                   cpl_too_small = cpl_too_small,
#                   cpl_too_large = cpl_too_large))
#
#   r
# }

#' Compute line numbers
#'
#' Compute line numbers
#'
#' @param r `rdu` object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_compute_line_numbers = function(r) {
  if ("abort" %in% names(r)) return(r)

  ocr = r$throughput$ocr_filter
  # use ocr line numbers (for the time being)

  if (all(c("block_num", "par_num") %in% names(ocr))) {
    ocr$lns_unique = ocr$block_num * 1e6 + ocr$par_num * 1e3 + ocr$line_num

    # make sure line numbers are 1, 2, ...
    ocr$ln = as.integer(factor(ocr$lns_unique))
    ocr$lns_unique = NULL
  } else {
    ocr_ln = ocr$line_num
  }


  r$throughput$ocr_filter = ocr
  r
}

#' Create lines
#'
#' Create lines
#'
#' @param r `rdu` object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_create_lines = function(r) {
  if ("abort" %in% names(r)) return(r)

  ocr = r$throughput$ocr_filter
  if (!"ln" %in% names(ocr)) cli_warn("Line numbers couldn't be generated because the column {.str ln} is missing. Run {.fun rdu_compute_line_numbers} first")

  text_per_line = split(ocr$text, ocr$ln)
  r$throughput$text_lines = unname(sapply(text_per_line, paste, collapse = " "))
  r
}

#' Detect shop
#'
#' Detect shop
#'
#' @param r `rdu` object
#' @param templates the shop templates
#' @param params the parameters object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_detect_shop = function(r, templates, params) {
  if ("abort" %in% names(r)) return(r)
  if (!is.na(r$shop)) return(r)

  txts = r$throughput$text_lines

  # NOTE: a 'keyword' in this procedure can also be a subsentence.

  # required keywords: for a shop match, all these keywords need to be found
  # for each shop, calculate the mean distance (fuzzy substr)
  xlst = lapply(templates, function(tp) {
    kw = tp$detect_shop_required

    sapply(kw, function(kwi) {
      min(fuzzy_substring_distance(txts, kwi))
    })
  })
  x = sapply(xlst, mean)

  # unique keywords: these are keywords that are unique to 1 shop
  # however, they are not required (they may not always there, e.g. slogans)
  # if (at least one) of such keywords are found, the 'pool' of candidate shops is defined by those shop(s)
  # for each shop, calculate the min distance because one match is sufficient (fuzzy substr)
  ylst = sapply(templates, function(tp) {
    kw = tp$detect_shop_unique

    if (length(kw) == 0) return(Inf)
    res = sapply(kw, function(kwi) {
      min(fuzzy_substring_distance(txts, kwi))
    })

  })
  y = sapply(ylst, min)

  x_pool = names(x)[x< params$match_tolerance_required]
  y_pool = names(y)[y< params$match_tolerance_unique]

  if (!length(x_pool)) {
    r$abort = "detect shop: keywords of detect_shop_required didn't meet the threshold match_tolerance_required"
    return(r)
  } else if (length(y_pool)) {
    if (!length(intersect(x_pool, y_pool))) {
      r$abort = "detect shop: keywords of detect_shop_unique found, but for the corresponding shop(s) the keywords of detect_shop_required didn't meet the threshold match_tolerance_required"
      return(r)
    }
    shop = names(which.min(x[y_pool]))
  } else {
    shop = names(which.min(x))
  }

  r$throughput$detect_required = xlst
  r$throughput$detect_unique = ylst

  r$scores$shop_match_required = as.list(x)
  r$scores$shop_match_unique = as.list(y)


  r$shop = shop

  r
}

#' Match product anchors
#'
#' Match product anchors
#'
#' @param r `rdu` object
#' @param templates the shop templates
#' @param params the parameters object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_match_product_anchors = function(r, templates, params) {
  if ("abort" %in% names(r)) return(r)

  # anchor_tolerance parameter: the string dist threshold: every match below is considered successfull

  # shop template
  tp = templates[[r$shop]]

  kw = tp$product_anchors

  kw = lapply(kw, function(kwi) {
    # use fuzzy substring machting: e.g. "OMSCHRIJVING" is almost a substring in the line "OMSCHRI JVING %$#T^#$"
    fsd = fuzzy_substring_distance(r$throughput$text_lines, kwi$text)
    i = which.min(fsd)
    if (fsd[i] <= params$anchor_tolerance) {
      kwi$ln = i
      kwi$dist = min(fsd)
    }
    kwi
  })

  kw_df = bind_rows(kw)

  # in case there are no keywords that mark the end or start (like Jumbo-kort), set it to NA (and consider the start line 1 in rdu_get_product_lines)
  if (!("items_start" %in% names(kw_df))) kw_df$items_start = NA_integer_
  if (!("items_end" %in% names(kw_df))) kw_df$items_end = NA_integer_

  kw_df = kw_df %>%
    mutate(ln_prod_start = ln + items_start,
           ln_prod_end = ln + items_end)

  r$throughput$product_anchors = kw_df
  r
}



#' Get product lines
#'
#' Get product lines
#'
#' @param r `rdu` object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_get_product_lines = function(r) {
  if ("abort" %in% names(r)) return(r)

  lns = r$throughput$text_lines

  if (all(is.na(r$throughput$product_anchors$ln_prod_start))) {
    #cli_warn("No keywords found that indicate the start, so start set to 1")
    start = 1
  } else {
    start = min(r$throughput$product_anchors$ln_prod_start, na.rm = TRUE)
  }

  if (all(is.na(r$throughput$product_anchors$ln_prod_end))) {
    r$abort = "get_product_lines: product block cannot be found because no keywords found that indicate the end"
    return(r)

    #cli_warn("No keywords found that indicate the end, so start set to {length(lns)}")
    #end = length(lns)
  } else {
    end = min(r$throughput$product_anchors$ln_prod_end, na.rm = TRUE)
  }


  r$meta$ln_prod_start = start
  r$meta$ln_prod_end = end

  if (start > end) {
    r$abort = "get_product_lines: product block cannot be found because start and end line numbers are not correct"
    return(r)
  }
  r$throughput$text_lines_prod = lns[start:end]
  r
}


#' Extract table
#'
#' Extract table
#'
#' @param r `rdu` object
#' @param templates the shop templates
#' @param params the parameters object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_extract_table = function(r, templates, params) {
  if ("abort" %in% names(r)) return(r)

  # shop template
  tp = templates[[r$shop]]


  x_prod_from = tp$products$from
  x_prod_to = tp$products$to

  x_price_from = tp$prices$from
  x_price_to = tp$prices$to

  start = r$meta$ln_prod_start
  end = r$meta$ln_prod_end

  dm = r$meta$image_dim

  # subset product block (line numbers)
  # derive variables
  df = r$throughput$ocr_filter %>%
    filter(ln >= start, ln <= end) %>%
    mutate(x = left / dm[2],
           text_num = convert_to_numeric(text),
           nchar = nchar(text))



  # find 'discount' lines

  if (tp$discount$method == "subtract") {
    lns_discount = local({
      df2 = df %>%
        mutate(dist_discount = stringdist::stringdist(tolower(text), tp$discount$text),
               pos_discount = abs(x - tp$discount$from)) %>%
        filter(dist_discount <= params$discount_tolerance & pos_discount <= params$discount_pos_diff)
      unique(df2$ln)
    })
    lns_discounted_prods = integer(0)
  } else if (tp$discount$method == "nextline") {
    lns_discount = local({
      df2 = df %>%
        mutate(dist_discount = stringdist::stringdist(text, tp$discount$text),
               pos_discount = abs(x - tp$discount$from)) %>%
        filter(dist_discount <= params$discount_tolerance & pos_discount <= params$discount_pos_diff)
      unique(df2$ln)
    })
    lns_discounted_prods = integer(0)
  } else if (tp$discount$method == "link") {
    lns_discounted_prods = local({
      df2 = df %>%
        mutate(dist_discount = stringdist::stringdist(text, tp$discount$text),
          pos_discount = abs(x - tp$discount$from)) %>%
        filter(dist_discount < params$discount_tolerance & pos_discount <= params$discount_pos_diff)
      unique(df2$ln)
    })
    lns_discount = integer(0)
  } else {
    lns_discount = integer(0)
    lns_discounted_prods = integer(0)
  }

  # find all texts (products, but also other text lines)
  df_txt = df %>%
    subset_df(x_prod_from - params$products_pos_diff, x_prod_to) %>%
    group_by(ln) %>%
    reframe(x = min(x)) %>%
    ungroup() %>%
    filter(abs(x - x_prod_from) <= params$products_pos_diff)


  lns_text = df_txt$ln



  # create text lines (all minus 'discount')
  lns_prod = setdiff(lns_text, lns_discount)
  df_p = subset_df(df, x_prod_from - params$products_pos_diff, x_prod_to, lns_prod)


  tdf = create_lines(df_p)




  # filter out 'skip' keywords
  if (length(tp$skip)) {
    skip_ids = unlist(lapply(tp$skip, function(ts) {
      which(fuzzy_substring_distance(tdf$text, ts$text) <= params$skip_tolerance)
    }))
    skip_lns = tdf$ln[skip_ids]
  } else {
    skip_lns = integer(0)
  }

  tdf_product = tdf %>%
    filter(!(ln %in% skip_lns)) %>%
    rename("product" = "text")



  tdf_price = df %>%
    subset_df(x_price_from - params$price_pos_diff, x_price_to) %>%
    create_lines() %>%
    filter(!(ln %in% skip_lns)) %>%
    rename("price" = "text")

  tdf_discount = df %>%
    subset_df(x_price_from - params$discount_pos_diff, x_price_to, lnset = lns_discount) %>%
    create_lines() %>%
    mutate(ln = ln - 1) %>%
    rename("discount" = "text")

  if (length(lns_discounted_prods)) {
    # for 'link' (AH) to be improved. Now set to NA and later the discount is proportionally distributed among these products
    tdf_discount = rbind(tdf_discount, tibble(ln = lns_discounted_prods,
                                              discount = NA_character_))
  }


  if (tp$discount$method == "nextline") {
    tdf3 = tdf_product
    tdf3$price = ""

    # find the price number one line about the next product line
    # e.g.
    # 1 APPELS
    # 2 aanbieding      2,99
    # 3 BANANEN         1,99
    # Line 1 in tdf_product should correspond to line 2 for the prices

    lns = c(tdf3$ln, end+1L)
    lns_match = sapply(head(lns, -1), function(l) {
      lns[which(lns > l)[1]] - 1L
    })

    tdf_price2 = tdf_price %>%
      filter(ln %in% lns_match)

    tdf3$price[match(tdf_price2$ln, lns_match)] = tdf_price2$price
    tdf3$discount = 0
  } else {
    tdf3 = tdf_product %>%
      left_join(tdf_price, by = "ln") %>%
      left_join(tdf_discount %>% mutate(.matched = TRUE), by = "ln") %>%
      mutate(discount = ifelse(is.na(.matched), 0, discount)) %>%
      select(-.matched)
  }


  # needed to find discounts blocks and totals
  df_all = r$throughput$ocr_filter %>%
    mutate(x = left / dm[2],
           text_num = convert_to_numeric(text),
           nchar = nchar(text))


  # total price (no discount)
  if ("total_no_discount" %in% names(tp)) {
    ln_tnd = local({
      df_tnd = df_all %>%
        mutate(dist = stringdist::stringdist(text, tp$total_no_discount$text),
               cpl = abs(cpl - tp$total_no_discount$chars_per_line),
               pos = abs(x - tp$total_no_discount$from),
               z = dist * params$total_weights$dist + pos * params$total_weights$pos + cpl * params$total_weights$cpl) %>%
        filter(dist <= params$total_tolerance$dist & pos <= params$total_tolerance$dist & cpl < params$total_tolerance$cpl) %>%
        arrange(z) %>%
        slice_head(n = 1)
      df_tnd$ln
    })

    df_tnd = subset_df(df_all, xmin = tp$price_no_discount$from, xmax = tp$price_no_discount$to, lnset = ln_tnd) %>%
      create_lines() %>%
      mutate(value = convert_to_numeric(text))
  } else {
    ln_tnd = numeric(0)
    df_tnd = NULL
  }

  # if (tp$discount$method == "none") {
  #   ln_td = numeric(0)
  #   df_td = NULL
  # } else {
    # total price (discount)


  if (is.na(r$total_discount)) {

    ln_td = local({
      df_td = df_all %>%
        mutate(dist = stringdist::stringdist(text, tp$total_discount$text),
               cpl = abs(cpl - tp$total_discount$chars_per_line),
               pos = abs(x - tp$total_discount$from),
               z = dist * params$total_weights$dist + pos * params$total_weights$pos + cpl * params$total_weights$cpl
        ) %>%
        filter(dist <= params$total_tolerance$dist & pos <= params$total_tolerance$dist & cpl < params$total_tolerance$cpl) %>%
        arrange(z) %>%
        slice_head(n = 1)
      df_td$ln
    })

    df_td = subset_df(df_all, xmin = tp$price_discount$from, xmax = tp$price_discount$to, lnset = ln_td) %>%
      create_lines() %>%
      mutate(value = convert_to_numeric(text))
  # }

    r$total_no_discount = df_tnd
    r$total_discount = df_td
  } else {
    ln_td = NA
  }

  r$meta$lns = list(products = tdf_product$ln,
               prices = tdf_price$ln,
               total_no_discount = ln_tnd,
               total_discount = ln_td)

  r$table_raw = tdf3
  r
}



#' Process table
#'
#' Process table
#'
#' @param r `rdu` object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_process_table = function(r) {
  if ("abort" %in% names(r)) return(r)
  if (!"table_raw" %in% names(r)) cli::cli_abort("no {.str table_raw} found. Please run {.fun rdu_extract_table} first")
  tab = r$table_raw

  tab$price = edit_price_vec(tab$price)
  tab$discount = edit_price_vec(tab$discount, keep_0 = TRUE)

  tab$price_net = tab$price - tab$discount

  r$table_processed = tab
  r
}

#' Check table
#'
#' Check table
#'
#' @param r `rdu` object
#' @return an rdu object. In case of an error, the attribute `"abort"` is added, which contains the error message
#' @export
rdu_check_table = function(r) {
  if ("abort" %in% names(r)) return(r)

  if (!"table_processed" %in% names(r)) cli::cli_abort("no {.str table_processed} found. Please run {.fun rdu_process_table} first")
  tab = r$table_processed

  n = nrow(tab)

  prods_no_price = sum(is.na(tab$price))
  prods_price_perc = round((n - prods_no_price) / n * 100, 3)

  tot = sum(tab$price, na.rm = TRUE)
  tot_net = sum(tab$price_net, na.rm = TRUE)

  total_no_discount_diff = as.numeric(r$total_no_discount$value) - tot
  total_diff = as.numeric(r$total_discount$value) - tot_net


  #ind_no_discount = sum(is.na(tab$discount))
  #ind_no_discount_frac = ind_no_discount_num / n

  r$scores = c(r$scores, list(prods_no_price = prods_no_price,
                              prods_price_perc = prods_price_perc,
                              total_no_discount_diff = total_no_discount_diff,
                              total_diff = total_diff))
  r
}
