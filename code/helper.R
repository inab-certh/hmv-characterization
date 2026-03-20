join_lookup <- function(df, lkp, id_col, value_col, new_name = NULL) {
  new_name <- new_name %||% value_col
  df |>
    dplyr::left_join(lkp, by = setNames("id", id_col)) |>
    dplyr::rename(!!new_name := !!value_col) |>
    dplyr::select(-dplyr::all_of(id_col))
}

save_gtsummary <- function(tbl, filename, path = ".") {
  stopifnot(inherits(tbl, "gtsummary"))
  base_path <- file.path(path, filename)
  gt_tbl <- gtsummary::as_gt(tbl)
  
  gt::gtsave(gt_tbl, glue::glue("{base_path}.html"))
  gt::gtsave(gt_tbl, glue::glue("{base_path}.pdf"))
  
  clean <- function(x) gsub("\\*\\*|__", "", x)
  
  if (inherits(tbl, "tbl_strata")) {
    strata_labels <- clean(as.character(tbl$df_strata[[1]]))
    
    sub_dfs <- purrr::map(tbl$tbls, \(subtbl) {
      gtsummary::as_tibble(subtbl) |>
        (\(d) purrr::set_names(d, make.unique(clean(names(d)))))() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), clean))
    })
    
    char_col   <- sub_dfs[[1]][, 1, drop = FALSE]
    strata_dfs <- purrr::map(sub_dfs, \(d) d[, -1, drop = FALSE])
    xlsx_df    <- dplyr::bind_cols(char_col, purrr::reduce(strata_dfs, dplyr::bind_cols))
    
    n_strata_cols <- purrr::map_int(strata_dfs, ncol)
    col_starts    <- cumsum(c(2L, n_strata_cols[-length(n_strata_cols)]))  # offset by 1 for char col
    col_ends      <- col_starts + n_strata_cols - 1L
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Table")
    
    purrr::pwalk(
      list(strata_labels, col_starts, col_ends),
      \(label, start, end) {
        openxlsx::writeData(wb, "Table", label, startCol = start, startRow = 1)
        if (end > start) openxlsx::mergeCells(wb, "Table", cols = start:end, rows = 1)
        openxlsx::addStyle(wb, "Table", rows = 1, cols = start,
          style = openxlsx::createStyle(textDecoration = "bold", halign = "center"))
      }
    )
    
    openxlsx::writeData(wb, "Table", xlsx_df, startRow = 2)
    
  } else {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Table")
    xlsx_df <- tbl |>
      gtsummary::as_tibble() |>
      (\(df) purrr::set_names(df, make.unique(clean(names(df)))))() |>
      dplyr::mutate(dplyr::across(dplyr::everything(), clean))
    openxlsx::writeData(wb, "Table", xlsx_df)
  }
  
  openxlsx::saveWorkbook(wb, glue::glue("{base_path}.xlsx"), overwrite = TRUE)
  message(glue::glue("Saved: .html, .pdf, .xlsx in '{path}'"))
}

generate_visit_table <- function(data, visit_number, vars = NULL) {
  visit_data <- data |>
    dplyr::arrange(pat_id, visit_date) |>
    dplyr::group_by(pat_id) |>
    dplyr::mutate(visit_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(visit_rank == visit_number)

  # Subset binary and continuous vars to those requested
  binary_pool     <- if (!is.null(vars)) intersect(binary_vars_dx, vars)     else binary_vars_dx
  continuous_pool <- if (!is.null(vars)) intersect(continuous_vars_dx, vars) else continuous_vars_dx

  valid_binary <- binary_pool |>
    purrr::keep(~ {
      vals <- visit_data[[.x]]
      !all(is.na(vals)) && length(unique(stats::na.omit(vals))) > 1
    })

  all_vars <- c(valid_binary, continuous_pool)

  var_labels <- all_vars |>
    purrr::map_chr(~ {
      lbl <- label_map[.x]
      if (is.na(lbl)) .x else lbl
    }) |>
    purrr::set_names(all_vars)

  visit_data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(valid_binary), ~ factor(.x, levels = c(0, 1))),
      gender = factor(gender, levels = c("ΓΥΝΑΙΚΑ", "ΑΝΔΡΑΣ", "ΑΛΛΟ"))
    ) |>
    dplyr::select(dplyr::all_of(all_vars), gender) |>
    gtsummary::tbl_summary(
      by        = gender,
      missing   = "no",
      type      = purrr::map(valid_binary, ~ "dichotomous") |> purrr::set_names(valid_binary),
      value     = purrr::map(valid_binary, ~ "1")           |> purrr::set_names(valid_binary),
      statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})"),
      digits    = list(gtsummary::all_continuous() ~ 1),
      label     = as.list(var_labels)
    ) |>
    gtsummary::add_overall(last = TRUE) |>
    gtsummary::bold_labels()
}


save_breath_and_sleep_tests <- function(visit_tables, base_path = "results") {
  
  clean <- function(x) gsub("\\*\\*|__", "", x)
  
  save_single <- function(tbl, filepath) {
    tbl |>
      gtsummary::as_gt() |>
      gt::gtsave(glue::glue("{filepath}.html"))
    
    tbl |>
      gtsummary::as_tibble() |>
      (\(df) purrr::set_names(df, make.unique(clean(names(df)))))() |>
      dplyr::mutate(dplyr::across(dplyr::everything(), clean)) |>
      openxlsx::write.xlsx(glue::glue("{filepath}.xlsx"), overwrite = TRUE)
  }
  
  purrr::iwalk(visit_tables, \(category_tables, visit_name) {
    visit_dir <- file.path(base_path, visit_name)
    dir.create(visit_dir, recursive = TRUE, showWarnings = FALSE)
    
    purrr::iwalk(category_tables, \(tbl, category_name) {
      filepath <- file.path(visit_dir, category_name)
      tryCatch(
        save_single(tbl, filepath),
        error = \(e) message(glue::glue("Failed: {visit_name}/{category_name}: {e$message}"))
      )
    })
    
    message(glue::glue("Saved {visit_name} -> {visit_dir}"))
  })
}
