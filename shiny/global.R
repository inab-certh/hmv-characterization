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

map_binary_col <- function(column) {

  .replace_binary <- function(x) {
    if (is.na(x)) return("ΑΓΝΩΣΤΟ")
    else if (x == 0) return("ΟΧΙ")
    else if (x == 1) return("ΝΑΙ")
  }

  sapply(column, .replace_binary)
}

# combine_cols <- function(colum1, column2) {
#   .select_column <- function(x) {
#     if (is.na(all(x))) {
#       return(NA)
#     } else if (is.na(any(x))) {
#       return(x[which(!is.na(x))])
#     } else {
#       return(max(x))
#     }
#   }
#
#   mapply(function(x, y) .select_column(c(x, y)), column1, column2)
# }

combine_cols <- function(data, col1, col2) {
  data |>
    dplyr::mutate(
      combined = purrr::map2({{col1}}, {{col2}}, ~{
        if (all(is.na(c(.x, .y)))) {
          NA
        } else if (any(is.na(c(.x, .y)))) {
          dplyr::coalesce(.x, .y)
        } else {
          max(.x, .y, na.rm = TRUE)
        }
      })
    )
}

characteristics_sayy_first_visit_adult <-
  readr::read_csv("data/characteristics_sayy_first_visit_adult.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 17 ~ "0-17",
      age < 45 ~ "18-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        "xap", "other_obstructive", "obesity_subvent", "sayy", "dmd",
        "myasthenia", "nkn", "parkinson", "heart_failure", "other_neurological",
        "diaphragm_malfunction", "posttb", "kyphoscoliosis", "other_limit_lung",
        "sd", "aee", "ay", "pulmonary_hypertension"
      ),
      ~ map_binary_col(.x)
    )
  ) |>
  combine_cols(ahirdi_br, psg_ahirdi) |>
  dplyr::rename("ahirdi_overall" = "combined")

characteristics_mv_first_visit <- readr::read_csv("data/characteristics_mv_first_visit.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 17 ~ "0-17",
      age < 45 ~ "18-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        "xap", "other_obstructive", "obesity_subvent", "sayy", "dmd",
        "myasthenia", "nkn", "parkinson", "heart_failure", "other_neurological",
        "diaphragm_malfunction", "posttb", "kyphoscoliosis", "other_limit_lung",
        "sd", "aee", "ay", "pulmonary_hypertension"
      ),
      ~ map_binary_col(.x)
    )
  )

device_testing_info_overall_sayy_first_visit_adult <-
  readr::read_csv("data/device_testing_info_overall_sayy_first_visit_adult.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 17 ~ "0-17",
      age < 45 ~ "18-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    )
  )
device_testing_info_overall_mv_first_visit <-
  readr::read_csv("data/device_testing_info_overall_mv_first_visit.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 17 ~ "0-17",
      age < 45 ~ "18-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    )
  )
breath_and_sleep_test_overall_first_visit_adult <-
  readr::read_csv("data/breath_and_sleep_test_overall_first_visit_adult.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 17 ~ "0-17",
      age < 45 ~ "18-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    )
  ) |>
  map_binary("nocturnal_hypoventilation") |>
  map_binary("hypoxemia")

patient_ventilation_mv_first_visit <-
  readr::read_csv("data/patient_ventilation_mv_first_visit.csv") |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "ΑΓΝΩΣΤΟ",
      age <= 17 ~ "0-17",
      age < 45 ~ "18-44",
      age < 65 ~ "45-64",
      TRUE ~ "65+"
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        "application_instructions", "physiotherapy_instructions",
        "emergency_instructions", "family_education", "certified_education"
      ),
      ~ map_binary_col(.x)
    )
  ) |>
  dplyr::filter(ventilation_hours <= 24)

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
    category_names = c("bmi < 25", "25 <= bmi < 30", "bmi >= 30")
  )
)

bmi_25_subgroup_settings <-  create_subgroup_settings(
  variable_name = "bmi",
  subgroup_label = "bmi_25",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "bmi",
    cutoffs = 25,
    category_names = c("bmi < 25", "bmi >= 25")
  )
)

bmi_30_subgroup_settings <-  create_subgroup_settings(
  variable_name = "bmi",
  subgroup_label = "bmi_30",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "bmi",
    cutoffs = 30,
    category_names = c("bmi < 30", "bmi >= 30")
  )
)

ahirdi_br_condition_subgroup_settings = create_subgroup_settings(
  variable_name = "ahirdi_br",
  subgroup_label = "ahirdi_br_condition",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "ahirdi_br",
    cutoffs = c(15, 30),
    category_names = c("AHI/RDI < 15", "15 <= AHI/RDI < 30", "AHI/RDI >= 30")
  )
)

psg_ahirdi_condition_subgroup_settings = create_subgroup_settings(
  variable_name = "psg_ahirdi",
  subgroup_label = "psg_ahirdi_condition",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "psg_ahirdi",
    cutoffs = c(15, 30),
    category_names = c("AHI/RDI < 15", "15 <= AHI/RDI < 30", "AHI/RDI >= 30")
  )
)

ahirdi_overall_condition_subgroup_settings = create_subgroup_settings(
  variable_name = "ahirdi_overall",
  subgroup_label = "ahirdi_overall_condition",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "ahirdi_overall",
    cutoffs = c(15, 30),
    category_names = c("AHI/RDI < 15", "15 <= AHI/RDI < 30", "AHI/RDI >= 30")
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
nocturnal_hypoventilation_subgroup_settings <- create_subgroup_settings(
  variable_name = "nocturnal_hypoventilation",
  subgroup_label = "nocturnal_hypoventilation"
)
hypoxemia_subgroup_settings <- create_subgroup_settings(
  variable_name = "hypoxemia",
  subgroup_label = "hypoxemia"
)
symptom_subgroup_settings <- create_subgroup_settings(
  variable_name = "symptom",
  subgroup_label = "symptom"
)
period_of_usage_subgroup_settings <- create_subgroup_settings(
  variable_name = "period_of_usage",
  subgroup_label = "period_of_usage"
)

xoth_hours_24_condition_subgroup_settings = create_subgroup_settings(
  variable_name = "xoth_hours_24",
  subgroup_label = "xoth_hours_24_condition",
  subgroup_definition = create_dynamic_categorize_settings(
    column = "xoth_hours_24",
    cutoffs = c(11, 19),
    category_names = c(
      "ΧΟΘ <= 10", "10 < ΧΟΘ <= 18", "18 < ΧΟΘ <= 24"
    )
  )
)
other_limit_lung_subgroup_settings <- create_subgroup_settings(
  variable_name = "other_limit_lung",
  subgroup_label = "other_limit_lung"
)
kyphoscoliosis_subgroup_settings <- create_subgroup_settings(
  variable_name = "kyphoscoliosis",
  subgroup_label = "kyphoscoliosis"
)
posttb_subgroup_settings <- create_subgroup_settings(
  variable_name = "posttb",
  subgroup_label = "posttb"
)
diaphragm_malfunction_subgroup_settings <- create_subgroup_settings(
  variable_name = "diaphragm_malfunction",
  subgroup_label = "diaphragm_malfunction"
)
other_neurological_subgroup_settings <- create_subgroup_settings(
  variable_name = "other_neurological",
  subgroup_label = "other_neurological"
)
nkn_subgroup_settings <- create_subgroup_settings(
  variable_name = "nkn",
  subgroup_label = "nkn"
)
myasthenia_subgroup_settings <- create_subgroup_settings(
  variable_name = "myasthenia",
  subgroup_label = "myasthenia"
)
dmd_subgroup_settings <- create_subgroup_settings(
  variable_name = "dmd",
  subgroup_label = "dmd"
)
other_obstructive_subgroup_settings <- create_subgroup_settings(
  variable_name = "other_obstructive",
  subgroup_label = "other_obstructive"
)
obesity_subvent_subgroup_settings <- create_subgroup_settings(
  variable_name = "obesity_subvent",
  subgroup_label = "obesity_subvent"
)
xap_subgroup_settings <- create_subgroup_settings(
  variable_name = "xap",
  subgroup_label = "xap"
)
sayy_subgroup_settings <- create_subgroup_settings(
  variable_name = "sayy",
  subgroup_label = "sayy"
)
certified_education_subgroup_settings <- create_subgroup_settings(
  variable_name = "certified_education",
  subgroup_label = "certified_education"
)
family_education_subgroup_settings <- create_subgroup_settings(
  variable_name = "family_education",
  subgroup_label = "family_education"
)
emergency_instructions_subgroup_settings <- create_subgroup_settings(
  variable_name = "emergency_instructions",
  subgroup_label = "emergency_instructions"
)
physiotherapy_instructions_subgroup_settings <- create_subgroup_settings(
  variable_name = "physiotherapy_instructions",
  subgroup_label = "physiotherapy_instructions"
)
application_instructions_subgroup_settings <- create_subgroup_settings(
  variable_name = "application_instructions",
  subgroup_label = "application_instructions"
)
invasive_ventilation_subgroup_settings <- create_subgroup_settings(
  variable_name = "invasive_ventilation",
  subgroup_label = "invasive_ventilation"
)
ventilation_type_subgroup_settings <- create_subgroup_settings(
  variable_name = "ventilation_type",
  subgroup_label = "ventilation_type"
)
ventilation_status_subgroup_settings <- create_subgroup_settings(
  variable_name = "ventilation_status",
  subgroup_label = "ventilation_status"
)
ventilation_reason_subgroup_settings <- create_subgroup_settings(
  variable_name = "ventilation_reason",
  subgroup_label = "ventilation_reason"
)
tracheostomy_type_subgroup_settings <- create_subgroup_settings(
  variable_name = "tracheostomy_type",
  subgroup_label = "tracheostomy_type"
)

dynamic_categorize <- function(data, column, cutoffs, category_names) {
  if (length(cutoffs) != length(category_names) - 1) {
    stop("Number of cut-offs must be one less than the number of labels")
  }
  case_conditions <- list()
  case_conditions[[1]] <- glue::glue(
    '{ column } < cutoffs[1] ~ category_names[1]'
  )

  for (i in seq_along(cutoffs)[-1]) {
    case_conditions[[i]] <- glue::glue(
      '{ column } >= cutoffs[i - 1] & { column } < cutoffs[i] ~ category_names[i]'
    )
  }

  case_conditions[[length(cutoffs) + 1]] <- glue::glue(
    '{ column } >= cutoffs[length(cutoffs)] ~ category_names[length(category_names)]'
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


oxymetry_variables <- c(
  "avsao2_oxy", "minsao2_oxy", "t90_oxy", "odi_oxy", "record_duration"
)

level_three_variables <- c(
  "ahirdi_br", "avsao2_br", "minsao2_br", "t90_br", "odi_br", "ahirdi_br",
  "record_duration"
)

psg_variables <- c(
  "psg_trt", "psg_tst", "psg_sl", "psg_se", "psg_ai", "waso", "psg_n1", "psg_n2",
  "psg_n3", "psg_rem", "psg_snore", "psg_avsao2", "psg_minsao2", "psg_t90",
  "psg_odi", "psg_ahirdi"
)

sayy_continuous_variables <- c(
  "fvc_perc", "fev1_l",	"fev_perc", "fev1_fvc", "ph", "po2", "pco2", "h3co2",
  oxymetry_variables,
  level_three_variables,
  psg_variables
)

mv_patient_ventilation <- c("period_of_usage")

calculate_subgroup_percentage <- function(data, column) {
  sum(data[[column]], na.rm = T) / nrow(data) * 100
}

calculate_median_in_subgroup <- function(data, column) {
  density(data[[column]], na.rm = TRUE)
}

calculate_density <- function(data, column) {
  if (nrow(data) > 2) {
    if (column %in% c("fvc_perc", "fev_perc"))
      breaks_hist <- seq(0, 150, length.out = 26)
    else if (column == "fev1_fvc")
      breaks_hist <- seq(0, 150, length.out = 26)
    else if (column == "fv1_l")
      breaks_hist <- seq(0, 5, length.out = 26)
    else if (column == "ph")
      breaks_hist <- seq(0, 7, length.out = 20)
    else if (column == "h3co2")
      breaks_hist <- seq(0, 40, length.out = 21)
    else
      breaks_hist <- 26

    data <- data |>
      dplyr::filter(get(column) != 0)
    # result <- density(data[[column]], na.rm = T, breaks = 100)
    breaks_hist <- seq(0, max(data[[column]], na.rm = TRUE), length.out = 10)
    result <- hist(data[[column]], na.rm = T, breaks = 20, plot = FALSE)
    return(data.frame(x = result$mids, y = result$density))
  } else {
    return(data.frame(x = mean(data[[column]], y = 1)))
  }
}
