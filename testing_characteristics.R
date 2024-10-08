source("connection.R")

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



characteristics <- DBI::dbGetQuery(
  conn = conn,
  statement = "SELECT * FROM patients_patientcharacteristics"
)

characteristics_overall <- characteristics |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::relocate(pat_id_id, visit_id, visit_date) |>
  dplyr::mutate(
    bmi = weight / (height / 100) ** 2
  ) |>
  dplyr::relocate(bmi, .after = height)

# This returns a data frame with 6200 patients
characteristics_sayy <- characteristics_overall |>
  dplyr::filter(pat_id_id %in% patients_sayy$id)

characteristics_sayy |>
  dplyr::mutate(
    overweight = dplyr::case_when(
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
