source("connection.R")

patients <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patient")
centers <- DBI::dbGetQuery(conn, "SELECT * FROM centers_center")
gender <- DBI::dbGetQuery(conn, "SELECT * FROM patients_gender")
caregiver <- DBI::dbGetQuery(conn, "SELECT * FROM patients_caregiver")
condition <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientcondition")
profession <- DBI::dbGetQuery(conn, "SELECT * FROM patients_profession")
education <- DBI::dbGetQuery(conn, "SELECT * FROM patients_education")
visit <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientvisit")



characteristics <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientcharacteristics")

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

patients_sayy <- patients_overall |>
  dplyr::filter(pat_condition == "ΣΑΥΥ")

patients_sayy |>
  dplyr::group_by(gender) |>
  dplyr::summarise(n = dplyr::n())

characteristics_sayy <- characteristics |>
  dplyr::filter(id %in% patients_sayy$id)

patients_sayy |>
  dplyr::group_by(gender) |>
  dplyr::summarise(n = dplyr::n())

patients_sayy |>
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
) |>
  dplyr::group_by(gender,age_groups) |>
  dplyr::summarise(n = dplyr::n())

patients_mv <- patients_overall |>
  dplyr::filter(pat_condition == "ΜΗΧΑΝΙΚΟΣ ΑΕΡΙΣΜΟΣ")

patients_mv |>
  dplyr::group_by(gender) |>
  dplyr::summarise(n = dplyr::n())

patients_mv |>
  dplyr::group_by(gender) |>
  dplyr::summarise(m = mean(age))


patients_mv |>
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
) |>
  dplyr::group_by(gender,age_groups) |>
  dplyr::summarise(n = dplyr::n())


# NA counts same as ΑΓΝΩΣΤΟ
patients_mv |>
  dplyr::group_by(education) |>
  dplyr::summarise(n = dplyr::n())
