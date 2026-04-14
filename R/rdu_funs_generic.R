#' Print an rdu object
#'
#' Print an rdu object
#'
#' @param x `rdu` object
#' @param ... not used
#' @export
print.rdu = function(x, ...) {
  lobstr::tree(x)
}

#' Plot an rdu object
#'
#' Plot an rdu object
#'
#' @param x receipt object
#' @param stage stage of the processing. By default 0, meaning the latest.
#' @param file output file
#' @param image should the image be plotted?
#' @param ocr_alpha alpha transparency for ocr text when plotted on top of the image
#' @param width_ext extention of the width
#' @param ... not used
#' @export
plot.rdu = function(x, stage = 0, file = NULL, image = FALSE, ocr_alpha = 1, width_ext = 1.5, ...) {
  dm = x$meta$image_dim

  meta = x$meta

  if (is.null(dm) || !("image_dim" %in% names(meta))) cli::cli_abort("Run {.fun rdu_check_image} and {.fun rdu_check_OCR} first")

  if (stage == 0) {
    if ("lns" %in% names(meta)) {
      stage = 5#prods, discounts, totals
    } else if ("ln_prod_start" %in% names(meta)) {
      stage = 4#"prod_block"
    } else if ("product_anchors" %in% names(x$throughput)) {
      stage = 3#"keywords"
    } else if ("ln" %in% names(x$throughput$ocr_filter)) {
      stage = 2#"lines"
    } else {
      stage = 1#"raw"
    }
  }

  if (is.null(x$throughput$ocr_filter)) {
    df = x$ocr
  } else {
    df = x$throughput$ocr_filter
  }


  if (stage == 1) width_ext = 1
  if (stage > 1) image = FALSE

  if (!is.null(file)) {
    png(file, width = dm[2] * width_ext, height = dm[1])
    grid.newpage()
  } else {
    grid.newpage()
    asp_img = (dm[2] / dm[1]) * width_ext
    asp_dev = dev.size()[1] / dev.size()[2]
    #browser()
    if (asp_img < asp_dev) {
      # height will be opupied
      vp = viewport(width = unit(asp_img, "snpc"), height = unit(1, "snpc"))
    } else {
      vp = viewport(width = unit(1, "snpc"), height = unit(1/asp_img, "snpc"))
    }
    pushViewport(vp)
  }


  if (stage > 1) {
    # line level (only if rdu_compute_line_numbers is executed)
    mx = max(df$ln)
    yscale = c(-.5, mx + 1.5)
  } else {
    # pixel level
    yscale = c(0,dm[1])
  }


  vp2 = viewport(xscale = c(0, dm[2]), yscale = yscale)
  pushViewport(vp2)

  if (stage > 1) {
    # plot all text equally large
    nlines_fit = convertHeight(unit(1, "npc"), "lines")
    df$cex = nlines_fit / mx
  } else {
    # plot text based on OCR read size
    df$str_width = as.numeric(convertWidth(stringWidth(df$text), unitTo = "native"))
    df$cex = df$width / df$str_width
  }

  if (stage >= 3) {
    df$hasKW = df$ln %in% x$throughput$product_anchors$ln
    if (stage >= 4) df$isPROD = df$ln >= meta$ln_prod_start & df$ln <= meta$ln_prod_end
    if (stage == 3) {
      df$col = ifelse(df$hasKW, "#770077", "#999999")
    } else if (stage == 4) {
      df$col = ifelse(df$hasKW, "#770077", ifelse(df$isPROD, "#2266ff", "#cccccc"))
    } else {
      df$col = ifelse(df$ln %in% meta$lns$products & df$ln %in% meta$lns$prices, "#5544cc",
               ifelse(df$ln %in% meta$lns$products, "#2266ff",
               ifelse(df$ln %in% meta$lns$prices, "#770077",
               ifelse(df$ln %in% meta$lns$total_no_discount, "#008800",
               ifelse(df$ln %in% meta$lns$total_discount, "#006600", "#cccccc")))))
    }
  } else {
    df$col = "#000000"
  }

  if (stage > 1) {
    g_txt = textGrob(df$text, x = unit(df$left, "native"), y = unit(mx - df$ln, "native"), just = c("left", "top"), gp=gpar(cex = df$cex, col = df$col))
  } else {
    g_txt = textGrob(df$text, x = unit(df$left, "native"), y = unit(dm[1] - df$top, "native"), just = c("left", "top"), gp=gpar(cex = df$cex, col = df$col, alpha = ocr_alpha))
  }


  if (image) {
    img = rdu_check_image(x, include_image = TRUE)$image
    g_img = rasterGrob(img)
    grid.draw(g_img)
  }
  grid.draw(g_txt)

  if (!is.null(file)) dev.off()

}
