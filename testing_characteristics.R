source("connection.R")
source("helper.R")


patients <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patient")
centers <- DBI::dbGetQuery(conn, "SELECT * FROM centers_center")
gender <- DBI::dbGetQuery(conn, "SELECT * FROM patients_gender")
caregiver <- DBI::dbGetQuery(conn, "SELECT * FROM patients_caregiver")
condition <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientcondition")
profession <- DBI::dbGetQuery(conn, "SELECT * FROM patients_profession")
education <- DBI::dbGetQuery(conn, "SELECT * FROM patients_education")
visit <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientvisit") |>
  dplyr::mutate(
    visit_date = lubridate::as_date(visit_date)
  )
bad_habit <- DBI::dbGetQuery(conn, "SELECT * FROM patients_badhabit")
underlying_disease <- DBI::dbGetQuery(conn, "SELECT * FROM patients_underlyingdisease")
cardiopathy <- DBI::dbGetQuery(conn, "SELECT * FROM patients_cardiopathy")
device_testing_info <- DBI::dbGetQuery(conn, "SELECT * FROM patients_devicetestinginfo")
ventilation_type <- DBI::dbGetQuery(conn, "SELECT * FROM patients_ventilationtype")
breath_and_sleep_test <- DBI::dbGetQuery(conn, "SELECT * FROM patients_breathandsleeptest")



characteristics <- DBI::dbGetQuery(
  conn = conn,
  statement = "SELECT * FROM patients_patientcharacteristics"
)


breath_and_sleep_test_overall <- breath_and_sleep_test |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::relocate(pat_id_id, visit_id, visit_date)

characteristics_overall <- characteristics |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::relocate(pat_id_id, visit_id, visit_date) |>
  dplyr::mutate(
    bmi = weight / (height / 100) ** 2
  ) |>
  dplyr::relocate(bmi, .after = height) |>
  dplyr::left_join(
    patients_overall |> dplyr::select(id, age, gender),
    by = c("pat_id_id" = "id")
  ) |>
  dplyr::left_join(
    breath_and_sleep_test_overall |> dplyr::select(visit_id, ahirdi_br),
    by = "visit_id"
  )

# This returns a data frame with 6200 patients
characteristics_sayy <- characteristics_overall |>
  dplyr::filter(pat_id_id %in% patients_sayy$id)

characteristics_sayy |>
  dplyr::mutate(
    bmi_condition = dplyr::case_when(
      bmi >= 25 ~ "yes",
      bmi < 25 ~ "no"
    )
  ) |>
  dplyr::group_by(pat_id_id) |>
  tidyr::nest() |>
  dplyr::mutate(
    at_least_one_visit_overweight = as.numeric(sum(data[[1]]$bmi > 25) > 0)
  ) |>
  tidyr::unnest(cols = data) |>
  dplyr::group_by(at_least_one_visit_overweight) |>
  dplyr::summarise(n = dplyr::n())


characteristics_sayy |>
  dplyr::filter(age >= 18) |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice(1L) |>
  dplyr::mutate(
    bmi_condition = dplyr::case_when(
      bmi >= 25 ~ "BMI >= 25",
      bmi < 25 ~ "BMI < 25"
    )
  ) |>
  dplyr::group_by(gender, bmi_condition) |>
  dplyr::summarise(n = dplyr::n())


# ==============================================================================
# PA2 - BMI & Bad habits
# ==============================================================================

characteristics_sayy_first_visit <- characteristics_sayy |>
  dplyr::filter(age >= 18) |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice(1L)

# ------------------------------------------------------------------------------
# GENDER:BMI:SMOKER
# ------------------------------------------------------------------------------

bmi_subgroup_definition <- function(data, variable_name = "bmi") {
  data |>
    dplyr::mutate(
      result = dplyr::case_when(
        get(variable_name) >= 25 ~ "BMI >= 25",
        get(variable_name) < 25 ~ "BMI < 25"
      )
    )
}

ahirdi_subgroup_definition <- function(data, variable_name = "ahirdi_br") {
  data |>
    dplyr::mutate(
      result = dplyr::case_when(
        get(variable_name) < 15 ~ "AHI_RDI < 15",
        get(variable_name) < 30 ~ "15 <= AHI_RDI < 30",
        get(variable_name) >= 30 ~ "AHI_RDI > 30",
      )
    )
}

subgroup_settings <- list(
  gender = create_subgroup_settings(
    variable_name = "gender",
    subgroup_label = "gender"
  ),
  bmi = create_subgroup_settings(
    variable_name = "bmi",
    subgroup_label = "bmi_condition",
    subgroup_definition = bmi_subgroup_definition
  ),
  smoker = create_subgroup_settings(
    variable_name = "smoker_status",
    subgroup_label = "smoker_status"
  )
)

characteristics_sayy_first_visit |>
  dplyr::left_join(bad_habit, by = c("smoker_id" = "id")) |>
  dplyr::rename("smoker_status" = "bad_habit_status") |>
  count_subgroups(subgroup_settings) |>
  readr::write_csv("results/PA2_sayy_gender_bmi_smoker.csv")

# ------------------------------------------------------------------------------
# GENDER:BMI:ALCOHOL
# ------------------------------------------------------------------------------

subgroup_settings <- list(
  gender = create_subgroup_settings(
    variable_name = "gender",
    subgroup_label = "gender"
  ),
  bmi = create_subgroup_settings(
    variable_name = "bmi",
    subgroup_label = "bmi_condition",
    subgroup_definition = bmi_subgroup_definition
  ),
  alcohol = create_subgroup_settings(
    variable_name = "alcohol_status",
    subgroup_label = "alcohol_status"
  )
)

characteristics_sayy_first_visit |>
  dplyr::left_join(bad_habit, by = c("alcohol_id" = "id")) |>
  dplyr::rename("alcohol_status" = "bad_habit_status") |>
  count_subgroups(subgroup_settings) |>
  readr::write_csv("results/PA2_sayy_gender_bmi_alcohol.csv")

# ------------------------------------------------------------------------------
# GENDER:BMI:SMOKER:ALCOHOL
# ------------------------------------------------------------------------------

subgroup_settings <- list(
  gender = create_subgroup_settings(
    variable_name = "gender",
    subgroup_label = "gender"
  ),
  bmi = create_subgroup_settings(
    variable_name = "bmi",
    subgroup_label = "bmi_condition",
    subgroup_definition = bmi_subgroup_definition
  ),
  smoker = create_subgroup_settings(
    variable_name = "smoker_status",
    subgroup_label = "smoker_status"
  ),
  alcohol = create_subgroup_settings(
    variable_name = "alcohol_status",
    subgroup_label = "alcohol_status"
  )
)

characteristics_sayy_first_visit |>
  dplyr::left_join(bad_habit, by = c("alcohol_id" = "id")) |>
  dplyr::rename("alcohol_status" = "bad_habit_status") |>
  dplyr::left_join(bad_habit, by = c("smoker_id" = "id")) |>
  dplyr::rename("smoker_status" = "bad_habit_status") |>
  count_subgroups(subgroup_settings) |>
  readr::write_csv("results/PA2_sayy_gender_bmi_smoker_alcohol.csv")

# ==============================================================================
# PA3 - Underlying disease
# ==============================================================================

subgroup_settings <- list(
  gender = create_subgroup_settings(
    variable_name = "gender",
    subgroup_label = "gender"
  ),
  bmi = create_subgroup_settings(
    variable_name = "bmi",
    subgroup_label = "bmi_condition",
    subgroup_definition = bmi_subgroup_definition
  ),
  other_obstructive = create_subgroup_settings(
    variable_name = "other_obstructive",
    subgroup_label = "other_obstructive"
  )
)

characteristics_sayy_first_visit |>
  dplyr::left_join(underlying_disease, by = c("underlying_disease_id" = "id")) |>
  dplyr::rename("underlying_disease" = "disease") |>
  count_subgroups(subgroup_settings)

# ==============================================================================
# PA4 - Comorbidities
# ==============================================================================

subgroup_settings <- list(
  gender = create_subgroup_settings(
    variable_name = "gender",
    subgroup_label = "gender"
  ),
  bmi = create_subgroup_settings(
    variable_name = "bmi",
    subgroup_label = "bmi_condition",
    subgroup_definition = bmi_subgroup_definition
  ),
  age_groups = create_subgroup_settings(
    variable_name = "age_groups",
    subgroup_label = "age_groups"
  )
)
characteristics_sayy_first_visit_age <- characteristics_sayy_first_visit |>
  dplyr::mutate(
    age_groups = dplyr::case_when(
      is.na(age) ~ "NA",
      age <= 18 ~ "0-18",
      age < 30 ~ "19-30",
      age < 40 ~ "31-40",
      age < 50 ~ "41-50",
      age < 60 ~ "51-60",
      age < 70 ~ "61-70",
      TRUE ~ "71+"
    )
  )

# ------------------------------------------------------------------------------
# GENDER:BMI:AGE COUNT(SD)
# ------------------------------------------------------------------------------

characteristics_sayy_first_visit_age |>
  count_within_subgroups(subgroup_settings, "sd") |>
  readr::write_csv("results/PA4_sayy_gender_bmi_age_sd.csv")

# ------------------------------------------------------------------------------
# GENDER:BMI:AGE COUNT(aee)
# ------------------------------------------------------------------------------

characteristics_sayy_first_visit_age |>
  count_within_subgroups(subgroup_settings, "aee") |>
  readr::write_csv("results/PA4_sayy_gender_bmi_age_aee.csv")

# ------------------------------------------------------------------------------
# GENDER:BMI:AGE COUNT(ay)
# ------------------------------------------------------------------------------

characteristics_sayy_first_visit_age |>
  count_within_subgroups(subgroup_settings, "ay") |>
  readr::write_csv("results/PA4_sayy_gender_bmi_age_ay.csv")

# ------------------------------------------------------------------------------
# GENDER:BMI:AGE COUNT(pulmonary_hypertension)
# ------------------------------------------------------------------------------

characteristics_sayy_first_visit_age |>
  count_within_subgroups(subgroup_settings, "pulmonary_hypertension") |>
  readr::write_csv("results/PA4_sayy_gender_bmi_age_pulmonary_hypertension.csv")

# ------------------------------------------------------------------------------
# GENDER:BMI:AGE COUNT(cardiopathies)
# ------------------------------------------------------------------------------
subgroup_settings <- list(
  gender = create_subgroup_settings(
    variable_name = "gender",
    subgroup_label = "gender"
  ),
  bmi = create_subgroup_settings(
    variable_name = "bmi",
    subgroup_label = "bmi_condition",
    subgroup_definition = bmi_subgroup_definition
  ),
  cardiopathies = create_subgroup_settings(
    variable_name = "cardiopathies_id",
    subgroup_label = "cardiopathies_id"
  ),
  age_groups = create_subgroup_settings(
    variable_name = "age_groups",
    subgroup_label = "age_groups"
  )
)
characteristics_sayy_first_visit_age |>
  dplyr::filter(!is.na(cardiopathies_id)) |>
  count_subgroups(subgroup_settings) |>
  dplyr::left_join(cardiopathy, by = c("cardiopathies_id" = "id")) |>
  dplyr::select(-cardiopathies_id) |>
  dplyr::relocate(cardiopathy, .before = "age_groups") |>
  readr::write_csv("results/PA4_gender_bmi_age_cardiopathy.csv")


# ==============================================================================
# PA5 - NIV & DEVICE
# ==============================================================================

device_overall <- device_testing_info |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::relocate(pat_id_id, visit_id, visit_date) |>
  dplyr::left_join(
    patients_overall |> dplyr::select(id, age, gender),
    by = c("pat_id_id" = "id")
  ) |>
  dplyr::left_join(
    characteristics_overall |>
      dplyr::select(visit_id, bmi),
    by = "visit_id"
  )

device_sayy_first_visit <- device_overall |>
  dplyr::filter(age >= 18) |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice(1L) |>
  dplyr::filter(pat_id_id %in% patients_sayy$id) |>
  dplyr::left_join(ventilation_type, by = c("ma_type_id" = "id"))


subgroup_settings <- list(
  ventilation_type = create_subgroup_settings(
    variable_name = "ventilation_type",
    subgroup_label = "ventilation_type"
  )
)
device_sayy_first_visit |>
  count_subgroups(subgroup_settings)






ahirdi_subgroup_definition <- function(data, variable_name = "ahirdi_br") {
  data |>
    dplyr::mutate(
      result = dplyr::case_when(
        is.na(get(variable_name)) ~ "Missing",
        get(variable_name) < 15 ~ "AHI_RDI < 15",
        get(variable_name) < 30 ~ "15 <= AHI_RDI < 30",
        get(variable_name) >= 30 ~ "AHI_RDI > 30",
      )
    ) |>
    dplyr::mutate(
      result = factor(
        x = result,
        levels = c("Missing", "AHI_RDI < 15",
                   "15 <= AHI_RDI < 30", "AHI_RDI > 30")
      )
    )
}

subgroup_settings <- list(
  gender = create_subgroup_settings(
    variable_name = "gender",
    subgroup_label = "gender"
  ),
  bmi = create_subgroup_settings(
    variable_name = "bmi",
    subgroup_label = "bmi_condition",
    subgroup_definition = bmi_subgroup_definition
  ),
  ahi_rdi = create_subgroup_settings(
    variable_name = "ahirdi_br",
    subgroup_label = "ahi_rdi_condition",
    subgroup_definition = ahirdi_subgroup_definition
  )
)

characteristics_sayy_first_visit |>
  count_subgroups_with_percentage(subgroup_settings, target_variable = "ahi_rdi_condition")
