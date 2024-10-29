create_subgroup_settings <- function(
    variable_name,
    subgroup_label,
    subgroup_definition = NULL
) {
  list(
    variable_name = variable_name,
    subgroup_label = subgroup_label,
    subgroup_definition = subgroup_definition
  )
}

dynamic_categorize <- function(data, column, cutoffs, category_names) {
  if (length(cutoffs) != length(category_names) - 1) {
    stop("Number of cut-offs must be one less than the number of labels")
  }
  case_conditions <- list()
  case_conditions[[1]] <- glue::glue('{ column } <= cutoffs[1] ~ category_names[1]')

  for (i in seq_along(cutoffs)[-1]) {
    case_conditions[[i]] <- glue::glue(
      '{ column } > cutoffs[i - 1] & { column } <= cutoffs[i] ~ category_names[i]'
    )
  }

  case_conditions[[length(cutoffs) + 1]] <- glue::glue(
    '{ column } > cutoffs[length(cutoffs)] ~ category_names[length(category_names)]'
  )

  case_conditions[[length(cutoffs) + 2]] <- "TRUE ~ NA_character_"

  caseArgs <- dplyr::tibble(
    conditions = sapply(case_conditions, rlang::parse_expr)
  )
  data |>
    dplyr::mutate(result = dplyr::case_when(!!!caseArgs$conditions)) |>
    dplyr::mutate(result = factor(result,levels = category_names))
}





create_dynamic_categorize_settings <- function(column, cutoffs, category_names) {
  return(
    list(
      column = column,
      cutoffs = cutoffs,
      category_names = category_names
    )
  )
}

count_subgroups <- function(data, subgroup_settings) {
  targeted_subgroups <- rep("", length(subgroup_settings))
  for (i in seq_along(subgroup_settings)) {
    if (!is.null(subgroup_settings[[i]]$subgroup_definition)) {
      subgroup_definition_function <- function(data, subgroup_definition) {
        do.call(
          dynamic_categorize,
          args = list(
            data = data,
            column = subgroup_definition$column,
            cutoffs = subgroup_definition$cutoffs,
            category_names = subgroup_definition$category_names
          )
        )
      }
      data <- data |>
        subgroup_definition_function(
          subgroup_definition = subgroup_settings[[i]]$subgroup_definition
        ) |>
        dplyr::rename("{subgroup_settings[[i]]$subgroup_label}" := "result")
    }
    targeted_subgroups[i] <- subgroup_settings[[i]]$subgroup_label
  }

  data |>
    dplyr::group_by(dplyr::across(targeted_subgroups)) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::arrange(dplyr::across(targeted_subgroups))
}

count_within_subgroups <- function(data, subgroup_settings, count_variable) {
  targeted_subgroups <- rep("", length(subgroup_settings))
  for (i in seq_along(subgroup_settings)) {
    if (!is.null(subgroup_settings[[i]]$subgroup_definition)) {
      subgroup_definition_function <- function(data, subgroup_definition) {
        do.call(
          dynamic_categorize,
          args = list(
            data = data,
            column = subgroup_definition$column,
            cutoffs = subgroup_definition$cutoffs,
            category_names = subgroup_definition$category_names
          )
        )
      }
      data <- data |>
        subgroup_definition_function(
          subgroup_definition = subgroup_settings[[i]]$subgroup_definition
        ) |>
        dplyr::rename("{subgroup_settings[[i]]$subgroup_label}" := "result")
    }
    targeted_subgroups[[i]] <- subgroup_settings[[i]]$subgroup_label
  }

  data |>
    dplyr::group_by(dplyr::across(targeted_subgroups)) |>
    dplyr::summarise(n = sum(get(count_variable)))
}

run_within_subgroups <- function(data, subgroup_settings, target_variable, fun, ...) {
  targeted_subgroups <- rep("", length(subgroup_settings))
  for (i in seq_along(subgroup_settings)) {
    if (!is.null(subgroup_settings[[i]]$subgroup_definition)) {
      subgroup_definition_function <- function(data, subgroup_definition) {
        do.call(
          dynamic_categorize,
          args = list(
            data = data,
            column = subgroup_definition$column,
            cutoffs = subgroup_definition$cutoffs,
            category_names = subgroup_definition$category_names
          )
        )
      }
      data <- data |>
        subgroup_definition_function(
          subgroup_definition = subgroup_settings[[i]]$subgroup_definition
        ) |>
        dplyr::rename("{subgroup_settings[[i]]$subgroup_label}" := "result")
    }
    targeted_subgroups[[i]] <- subgroup_settings[[i]]$subgroup_label
  }

  data |>
    dplyr::group_by(dplyr::across(targeted_subgroups)) |>
    dplyr::summarise_at(target_variable, fun, ...) |>
    dplyr::ungroup()
}


count_subgroups_with_percentage <- function(data, subgroup_settings, target_variable) {
  targeted_subgroups <- rep("", length(subgroup_settings))
  for (i in seq_along(subgroup_settings)) {
    if (!is.null(subgroup_settings[[i]]$subgroup_definition)) {
      subgroup_definition_function <- function(data, subgroup_definition) {
        do.call(
          dynamic_categorize,
          args = list(
            data = data,
            column = subgroup_definition$column,
            cutoffs = subgroup_definition$cutoffs,
            category_names = subgroup_definition$category_names
          )
        )
      }
      data <- data |>
        subgroup_definition_function(
          subgroup_definition = subgroup_settings[[i]]$subgroup_definition
        ) |>
        dplyr::rename("{subgroup_settings[[i]]$subgroup_label}" := "result")
    }
    targeted_subgroups[i] <- subgroup_settings[[i]]$subgroup_label
  }

  subgroups_without_target_variable <- targeted_subgroups[-which(targeted_subgroups == target_variable)]

  data |> dplyr::group_by(dplyr::across(subgroups_without_target_variable)) |>
    tidyr::nest() |>
    dplyr::mutate(
      total = purrr::map_dbl(data, nrow),
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::group_by(
      dplyr::across(
        c(subgroups_without_target_variable, target_variable, total)
      )
    ) |>
    tidyr::nest() |>
    dplyr::mutate(
      n = purrr::map_dbl(data, nrow),
      percentage = n / total * 100
    ) |>
    dplyr::select(-data) |>
    dplyr::relocate(total, .after = n) |>
    dplyr::arrange(dplyr::across(c(subgroups_without_target_variable, target_variable))) |>
    dplyr::ungroup()
}



