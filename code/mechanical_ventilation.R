source("code/connection.R")
source("code/load_tables.R")
source("code/helper.R")

characteristics_overall <- characteristics |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::relocate(pat_id_id, visit_id, visit_date)

patients_overall <- patients |>
  dplyr::left_join(gender, by = c("gender_id" = "id")) |>
  dplyr::left_join(profession, by = c("profession_id" = "id")) |>
  dplyr::left_join(education, by = c("education_id" = "id")) |>
  dplyr::left_join(condition, by = c("pat_condition_id" = "id")) |>
  dplyr::select(-c(education_id, gender_id, profession_id, pat_condition_id)) |>
  dplyr::mutate(
    ma_subscription_date = lubridate::as_date(ma_subscription_date),
    age = lubridate::year(ma_subscription_date) - birth_year
  )


patients_mv <- patients_overall |>
  dplyr::filter(pat_condition == "ΜΗΧΑΝΙΚΟΣ ΑΕΡΙΣΜΟΣ")

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
characteristics_mv <- characteristics_overall |>
  dplyr::filter(pat_id_id %in% patients_mv$id)

characteristics_mv_first_visit <- characteristics_mv |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice(1L)


patient_ventilation_overall <- patient_ventilation |>
  dplyr::left_join(visit, by = c("visit_id" = "id"))
