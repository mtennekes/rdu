file_image = system.file("examples/inputs/receipt_plus.png", package = "rdu")
file_OCR = system.file("examples/inputs/receipt_plus.tsv", package = "rdu")
params = yaml::read_yaml(system.file("examples/config/du_params.yaml", package = "rdu"))
templates = yaml::read_yaml(system.file("examples/config/templates.yaml", package = "rdu"))


r = rdu_init(file_image = file_image, file_OCR = file_OCR, shop = NA, total_discount = 44.12) |>
  rdu_check_image() |>
  rdu_check_OCR() |>
  rdu_filter_cpl(params) |>
  rdu_compute_line_numbers() |>
  rdu_create_lines() |>
  rdu_detect_shop(templates, params) |>
  rdu_match_product_anchors(templates, params) |>
  rdu_get_product_lines() |>
  rdu_extract_table(templates, params) |>
  rdu_process_table() |>
  rdu_check_table() |>
  rdu_write(file = "output.json", include_throughput = FALSE)
