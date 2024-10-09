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

count_subgroups <- function(data, subgroup_settings) {
  targeted_subgroups <- rep("", length(subgroup_settings))
  for (i in seq_along(subgroup_settings)) {
    if (!is.null(subgroup_settings[[i]]$subgroup_definition)) {
      data <- data |>
        subgroup_settings[[i]]$subgroup_definition() |>
        dplyr::rename("{subgroup_settings[[i]]$subgroup_label}" := "result")
    }
    targeted_subgroups[[i]] <- subgroup_settings[[i]]$subgroup_label
  }

  data |>
    dplyr::group_by(dplyr::across(targeted_subgroups)) |>
    dplyr::summarise(n = dplyr::n())
}

count_within_subgroups <- function(data, subgroup_settings, count_variable) {
  targeted_subgroups <- rep("", length(subgroup_settings))
  for (i in seq_along(subgroup_settings)) {
    if (!is.null(subgroup_settings[[i]]$subgroup_definition)) {
      data <- data |>
        subgroup_settings[[i]]$subgroup_definition() |>
        dplyr::rename("{subgroup_settings[[i]]$subgroup_label}" := "result")
    }
    targeted_subgroups[[i]] <- subgroup_settings[[i]]$subgroup_label
  }

  data |>
    dplyr::group_by(dplyr::across(targeted_subgroups)) |>
    dplyr::summarise(n = sum(get(count_variable)))
}
