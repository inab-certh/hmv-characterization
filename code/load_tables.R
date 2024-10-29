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
mv_type <- DBI::dbGetQuery(conn, "SELECT * FROM patients_mvtype")
mask_type <- DBI::dbGetQuery(conn, "SELECT * FROM patients_masktype")
device_selection <- DBI::dbGetQuery(conn, "SELECT * FROM patients_deviceselection")
check_cause <- DBI::dbGetQuery(conn, "SELECT * FROM patients_checkcause")
checked_by <- DBI::dbGetQuery(conn, "SELECT * FROM patients_checkedby")
ventilation_type <- DBI::dbGetQuery(conn, "SELECT * FROM patients_ventilationtype")
breath_and_sleep_test <- DBI::dbGetQuery(conn, "SELECT * FROM patients_breathandsleeptest")

characteristics <- DBI::dbGetQuery(
  conn = conn,
  statement = "SELECT * FROM patients_patientcharacteristics"
)

patient_ventilation <- DBI::dbGetQuery(
  conn = conn,
  statement = "SELECT * FROM patients_patientventilation"
)
