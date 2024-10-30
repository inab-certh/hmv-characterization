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
create_dynamic_categorize_settings <- function(column, cutoffs, category_names) {
  return(
    list(
      column = column,
      cutoffs = cutoffs,
      category_names = category_names
    )
  )
}
map_binary <- function(data, column) {

  case_conditions <- list()
  case_conditions[[1]] <- glue::glue('{ column } == 0 ~ "ΟΧΙ"')
  case_conditions[[2]] <- glue::glue('{ column } == 1 ~ "ΝΑΙ"')
  case_conditions[[3]] <- glue::glue('is.na({ column }) ~ "ΑΓΝΩΣΤΟ"')
  caseArgs <- dplyr::tibble(
    conditions = sapply(case_conditions, rlang::parse_expr)
  )
  data |>
    dplyr::mutate(
      "{column}" := factor(
        dplyr::case_when(!!!caseArgs$conditions),
        levels = c("ΝΑΙ", "ΟΧΙ", "ΑΓΝΩΣΤΟ")
      )
    )
}
characteristics_sayy_first_visit_adult <- readr::read_csv("data/characteristics_sayy_first_visit_adult.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 18 ~ "0-18",
      age < 30 ~ "19-30",
      age < 40 ~ "31-40",
      age < 50 ~ "41-50",
      age < 60 ~ "51-60",
      age < 70 ~ "61-70",
      TRUE ~ "71+"
    )
  ) |>
  map_binary("xap") |>
  map_binary("other_obstructive") |>
  map_binary("obesity_subvent") |>
  map_binary("sayy") |>
  map_binary("dmd") |>
  map_binary("myasthenia") |>
  map_binary("nkn") |>
  map_binary("parkinson") |>
  map_binary("heart_failure") |>
  map_binary("other_neurological") |>
  map_binary("diaphragm_malfunction") |>
  map_binary("posttb") |>
  map_binary("kyphoscoliosis") |>
  map_binary("other_limit_lung") |>
  map_binary("sd") |>
  map_binary("aee") |>
  map_binary("ay") |>
  map_binary("pulmonary_hypertension")

device_testing_info_overall_first_visist_adult <-
  readr::read_csv("data/device_testing_info_overall_first_visist_adult.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 18 ~ "0-18",
      age < 30 ~ "19-30",
      age < 40 ~ "31-40",
      age < 50 ~ "41-50",
      age < 60 ~ "51-60",
      age < 70 ~ "61-70",
      TRUE ~ "71+"
    )
  )
breath_and_sleep_test_overall_first_visist_adult <-
  readr::read_csv("data/breath_and_sleep_test_overall_first_visit_adult.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 18 ~ "0-18",
      age < 30 ~ "19-30",
      age < 40 ~ "31-40",
      age < 50 ~ "41-50",
      age < 60 ~ "51-60",
      age < 70 ~ "61-70",
      TRUE ~ "71+"
    )
  )



gender_subgroup_settings = create_subgroup_settings(
  variable_name = "gender",
  subgroup_label = "gender"
)

bmi_condition_subgroup_settings = create_subgroup_settings(
  variable_name = "bmi",
  subgroup_label = "bmi_condition",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "bmi",
    cutoffs = c(25, 30),
    category_names = c("bmi <= 25", "25 < bmi <= 30", "bmi > 30")
  )
)

ahirdi_br_condition_subgroup_settings = create_subgroup_settings(
  variable_name = "ahirdi_br",
  subgroup_label = "ahirdi_br_condition",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "ahirdi_br",
    cutoffs = c(15, 30),
    category_names = c("AHI/RDI <= 15", "15 < AHI/RDI <= 30", "AHI/RDI > 30")
  )
)
psg_ahirdi_condition_subgroup_settings = create_subgroup_settings(
  variable_name = "psg_ahirdi",
  subgroup_label = "psg_ahirdi_condition",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "psg_ahirdi",
    cutoffs = c(15, 30),
    category_names = c("AHI/RDI <= 15", "15 < AHI/RDI <= 30", "AHI/RDI > 30")
  )
)

age_groups_subgroup_settings = create_subgroup_settings(
  variable_name = "age_groups",
  subgroup_label = "age_groups"
)

smoker_status_subgroup_settings <- create_subgroup_settings(
  variable_name = "smoker_status",
  subgroup_label = "smoker_status"
)

alcohol_status_subgroup_settings <- create_subgroup_settings(
  variable_name = "alcohol_status",
  subgroup_label = "alcohol_status"
)
underlying_disease_subgroup_settings <- create_subgroup_settings(
  variable_name = "underlying_disease",
  subgroup_label = "underlying_disease"
)

cardiopathy_subgroup_settings <- create_subgroup_settings(
  variable_name = "cardiopathy",
  subgroup_label = "cardiopathy"
)
sd_subgroup_settings <- create_subgroup_settings(
  variable_name = "sd",
  subgroup_label = "sd"
)
aee_subgroup_settings <- create_subgroup_settings(
  variable_name = "aee",
  subgroup_label = "aee"
)
ay_subgroup_settings <- create_subgroup_settings(
  variable_name = "ay",
  subgroup_label = "ay"
)
pulmonary_hypertension_subgroup_settings <- create_subgroup_settings(
  variable_name = "pulmonary_hypertension",
  subgroup_label = "pulmonary_hypertension"
)

mask_type_subgroup_settings <- create_subgroup_settings(
  variable_name = "mask_type",
  subgroup_label = "mask_type"
)
ma_type_subgroup_settings <- create_subgroup_settings(
  variable_name = "ma_type",
  subgroup_label = "ma_type"
)
humidifier_subgroup_settings <- create_subgroup_settings(
  variable_name = "humidifier",
  subgroup_label = "humidifier"
)
dev_sel_type_subgroup_settings <- create_subgroup_settings(
  variable_name = "dev_sel_type",
  subgroup_label = "dev_sel_type"
)
study_subgroup_settings <- create_subgroup_settings(
  variable_name = "study",
  subgroup_label = "study"
)


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

count_within_subgroups <- function(data, subgroup_settings, count_variable, ...) {
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
    dplyr::summarise(n = sum(get(count_variable), ...))
}

run_within_subgroups <- function(
    data,
    subgroup_settings,
    target_variable,
    fun,
    ...
) {
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
    tidyr::nest() |>
    dplyr::mutate(
      percentage = purrr::map(.x = data, .f = fun, ...)
    ) |>
    dplyr::select(-data) |>
    dplyr::ungroup()
}



count_subgroups_with_percentage <- function(
    data,
    subgroup_settings,
    target_variable
) {
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

  subgroups_without_target_variable <-
    targeted_subgroups[-which(targeted_subgroups == target_variable)]

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
    dplyr::arrange(dplyr::across(
      c(subgroups_without_target_variable, target_variable))
    ) |>
    dplyr::ungroup()
}

form_table <- function(data) {
  formattable_options <- list()
  result <- formattable::formattable(
    data |>
      dplyr::mutate(percentage = percentage / 100) |>
      dplyr::mutate(percentage = formattable::percent(percentage)),
    list(
      percentage = formattable::color_bar(
        fun = "identity",
        color = "lightblue"
      )
    )
  )
}

# calculate_subgroup_size <- function(
#     data,
#     subgroup_settings,
#     target_variable = NULL
# ) {
#   for (i in seq_along(subgroup_settings)) {
#     if (!is.null(subgroup_settings[[i]]$subgroup_definition)) {
#       subgroup_definition_function <- function(data, subgroup_definition) {
#         do.call(
#           dynamic_categorize,
#           args = list(
#             data = data,
#             column = subgroup_definition$column,
#             cutoffs = subgroup_definition$cutoffs,
#             category_names = subgroup_definition$category_names
#           )
#         )
#       }
#       data <- data |>
#         subgroup_definition_function(
#           subgroup_definition = subgroup_settings[[i]]$subgroup_definition
#         ) |>
#         dplyr::rename("{subgroup_settings[[i]]$subgroup_label}" := "result")
#     }
#     targeted_subgroups[i] <- subgroup_settings[[i]]$subgroup_label
#   }
#
#   if (!is.null(target_variable)) {
#
#     targeted_subgroups <-
#       targeted_subgroups[-which(targeted_subgroups == target_variable)]
#
#   }
#
#   data |> dplyr::group_by(dplyr::across(targeted_subgroups)) |>
#     tidyr::nest() |>
#     dplyr::mutate(
#       total = purrr::map_dbl(data, nrow)
#     )  |>
#     dplyr::select(-data)
# }


calculate_subgroup_percentage <- function(data, column) {
  sum(data[[column]], na.rm = T) / nrow(data) * 100
}

calculate_median_in_subgroup <- function(data, column) {
  density(data[[column]], na.rm = TRUE)
}

calculate_density <- function(data, column) {
  if (nrow(data) > 2) {
    result <- density(data[[column]], na.rm = T, n = 100)
    return(data.frame(x = result$x, y = result$y))
  } else {
    return(data.frame(x = mean(data[[column]], y = 1)))
  }
}
