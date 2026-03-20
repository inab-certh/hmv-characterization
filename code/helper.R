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
