#!/usr/bin/env Rscript

source("code/connection.R")
source("code/load_tables.R")




patients_overall <- patients |>
  dplyr::left_join(gender, by = c("gender_id" = "id")) |>
  dplyr::left_join(profession, by = c("profession_id" = "id")) |>
  dplyr::left_join(education, by = c("education_id" = "id")) |>
  dplyr::left_join(condition, by = c("pat_condition_id" = "id")) |>
  dplyr::select(-c(education_id, gender_id, profession_id, pat_condition_id)) |>
  dplyr::mutate(
    ma_subscription_date = lubridate::as_date(ma_subscription_date),
    age = lubridate::year(ma_subscription_date) - birth_year
  ) |>
  dplyr::filter(!is.na(age))



patients_sayy <- patients_overall |>
  dplyr::filter(pat_condition == "ΣΑΥΥ") |>
  dplyr::mutate(
    gender = ifelse(is.na(gender), "ΑΓΝΩΣΤΟ", gender),
    gender = factor(
      gender,
      levels = c("ΑΝΔΡΑΣ", "ΓΥΝΑΙΚΑ", "ΑΛΛΟ", "ΑΓΝΩΣΤΟ")
    )
  )

patients_mv <- patients_overall |>
  dplyr::filter(pat_condition == "ΜΗΧΑΝΙΚΟΣ ΑΕΡΙΣΜΟΣ")

breath_and_sleep_test_overall <- breath_and_sleep_test |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::relocate(pat_id_id, visit_id, visit_date)


characteristics_overall_first_visit <- characteristics |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::relocate(pat_id_id, visit_id, visit_date) |>
  dplyr::mutate(
    bmi = weight / (height / 100) ** 2
  ) |>
  dplyr::relocate(bmi, .after = height) |>
  dplyr::left_join(
    breath_and_sleep_test_overall |> dplyr::select(visit_id, ahirdi_br, psg_ahirdi),
    by = "visit_id"
  ) |>
  dplyr::left_join(bad_habit, by = c("smoker_id" = "id")) |>
  dplyr::rename("smoker_status" = "bad_habit_status") |>
  dplyr::left_join(bad_habit, by = c("alcohol_id" = "id")) |>
  dplyr::rename("alcohol_status" = "bad_habit_status") |>
  dplyr::left_join(underlying_disease, by = c("underlying_disease_id" = "id")) |>
  dplyr::rename("underlying_disease" = "disease") |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice_head(n = 1)

characteristics_sayy_first_visit_adult <- patients_sayy |>
  dplyr::select(id, gender, age) |>
  dplyr::filter(age >= 18) |>
  dplyr::left_join(
    characteristics_overall_first_visit,
    by = c("id" = "pat_id_id")
  ) |>
  dplyr::mutate(
    smoker_status = tidyr::replace_na(smoker_status, "ΑΓΝΩΣΤΟ"),
    alcohol_status = tidyr::replace_na(alcohol_status, "ΑΓΝΩΣΤΟ"),
    underlying_disease = tidyr::replace_na(underlying_disease, "ΑΓΝΩΣΤΟ")
  ) |>
  dplyr::left_join(cardiopathy, by = c("cardiopathies_id" = "id")) |>
  dplyr::mutate(cardiopathy = tidyr::replace_na(cardiopathy, "ΑΓΝΩΣΤΟ"))


patient_ventilation_overall_first_visit <- patient_ventilation |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::select(-id) |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice_head(n = 1)

patient_ventilation_mv_first_visit <- patients_mv |>
  dplyr::select(id, gender, age) |>
  dplyr::left_join(patient_ventilation_overall_first_visit, by = c("id" = "pat_id_id")) |>
  dplyr::left_join(
    characteristics_overall_first_visit |>
      dplyr::select(pat_id_id, bmi),
    by = c("id" = "pat_id_id"))

characteristics_mv_first_visit <- patients_mv |>
  dplyr::select(id, gender, age) |>
  dplyr::left_join(characteristics_overall_first_visit, by = c("id" = "pat_id_id")) |>
  dplyr::mutate(
    smoker_status = tidyr::replace_na(smoker_status, "ΑΓΝΩΣΤΟ"),
    alcohol_status = tidyr::replace_na(alcohol_status, "ΑΓΝΩΣΤΟ"),
    underlying_disease = tidyr::replace_na(underlying_disease, "ΑΓΝΩΣΤΟ")
  )


device_testing_info_overall_first_visit<- device_testing_info |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice_head(n = 1)

device_testing_info_overall_first_visist_adult <- patients_sayy |>
  dplyr::select(id, gender, age) |>
  dplyr::filter(age >= 18) |>
  dplyr::left_join(
    device_testing_info_overall_first_visit,
    by = c("id" = "pat_id_id")
  ) |>
  dplyr::left_join(mask_type, by = c("mask_type_id" = "id")) |>
  dplyr::rename("mask_type" = "type") |>
  dplyr::left_join(mv_type, by = c("ma_type_id" = "id")) |>
  dplyr::rename("ma_type" = "type") |>
  dplyr::left_join(device_selection, by = c("dev_sel_id" = "id")) |>
  dplyr::rename("dev_sel_type" = "type") |>
  dplyr::left_join(check_cause, by = c("check_cause_id" = "id")) |>
  dplyr::rename("check_cause" = "cause") |>
  dplyr::left_join(checked_by, by = c("checked_by_id" = "id")) |>
  dplyr::rename("checked_by" = "by") |>
  dplyr::mutate(
    mask_type = tidyr::replace_na(mask_type, "ΑΓΝΩΣΤΟ"),
    ma_type = tidyr::replace_na(ma_type, "ΑΓΝΩΣΤΟ"),
    dev_sel_type = tidyr::replace_na(dev_sel_type, "ΑΓΝΩΣΤΟ"),
    checked_by = tidyr::replace_na(checked_by, "ΑΓΝΩΣΤΟ"),
    check_cause = tidyr::replace_na(check_cause, "ΑΓΝΩΣΤΟ")
  ) |>
  dplyr::select(
    -c(
      "mask_type_id", "ma_type_id", "dev_sel_id", "check_cause_id", "checked_by_id"
    )
  )


breath_and_sleep_test_overall_first_visit <- breath_and_sleep_test |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::group_by(pat_id_id) |>
  dplyr::arrange(visit_date) |>
  dplyr::slice_head(n = 1)

breath_and_sleep_test_overall_first_visit_adult <- patients_sayy |>
  dplyr::select(id, gender, age) |>
  dplyr::filter(age >= 18) |>
  dplyr::left_join(
    breath_and_sleep_test_overall_first_visit,
    by = c("id" = "pat_id_id")
  ) |>
  dplyr::left_join(
    characteristics_sayy_first_visit_adult |>
      dplyr::select(id, bmi),
    by = c("id" = "id")
  ) |>
  dplyr::relocate(
    bmi,
    .after = age
  ) |>
  dplyr::mutate(
    study = dplyr::case_when(
      overnight_oximetry == 1 ~ "overnight oxymetry",
      level_three_rec == 1 ~ "level three rec",
      polysomnography == 1 ~ "polysomnography",
      is.na(overnight_oximetry) | is.na(level_three_rec) | is.na(polysomnography) ~ "ΑΓΝΩΣΤΟ",
      TRUE ~ "none"
    )
  )

patients_with_two_or_more_studies <-
  breath_and_sleep_test_overall_first_visit_adult |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    n = sum(overnight_oximetry, level_three_rec, polysomnography)
  ) |>
  dplyr::filter(n > 1) |>
  dplyr::pull(id)

breath_and_sleep_test_overall_first_visit_adult <-
  breath_and_sleep_test_overall_first_visit_adult |>
  dplyr::filter(!(id %in% patients_with_two_or_more_studies))

readr::write_csv(
  patient_ventilation_overall_first_visit,
  "data/patient_ventilation_overall_first_visit.csv"
)
readr::write_csv(
  characteristics_mv_first_visit,
  "data/characteristics_mv_first_visit.csv"
)

readr::write_csv(
  characteristics_sayy_first_visit_adult,
  "data/characteristics_sayy_first_visit_adult.csv"
)

readr::write_csv(
  device_testing_info_overall_first_visist_adult,
  "data/device_testing_info_overall_first_visist_adult.csv"
)

readr::write_csv(
  breath_and_sleep_test_overall_first_visit_adult,
  "data/breath_and_sleep_test_overall_first_visit_adult.csv"
)
