# ===
# ---- Helper function ----
# Joins the foreign keys
source("code/connection.R")
source("code/helper.R")

conn <- db_connect()

# --- Load tables ---
patients <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patient")
centers <- DBI::dbGetQuery(conn, "SELECT * FROM centers_center")
gender <- DBI::dbGetQuery(conn, "SELECT * FROM patients_gender")
caregiver <- DBI::dbGetQuery(conn, "SELECT * FROM patients_caregiver")
condition <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientcondition")
profession <- DBI::dbGetQuery(conn, "SELECT * FROM patients_profession")
education <- DBI::dbGetQuery(conn, "SELECT * FROM patients_education")
visit <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientvisit")
bad_habit <- DBI::dbGetQuery(conn, "SELECT * FROM patients_badhabit")
physical_activity  <- DBI::dbGetQuery(conn, "SELECT * FROM patients_physicalactivity")
extra_myopathy <- DBI::dbGetQuery(conn, "SELECT * FROM patients_extramyopathy")
nutrition <- DBI::dbGetQuery(conn, "SELECT * FROM patients_nutrition")
underlying_disease <- DBI::dbGetQuery(conn, "SELECT * FROM patients_underlyingdisease")
cardiopathy <- DBI::dbGetQuery(conn, "SELECT * FROM patients_cardiopathy")
characteristics <- DBI::dbGetQuery(conn, "SELECT * FROM patients_patientcharacteristics")
breath_and_sleep_test <- DBI::dbGetQuery(conn, "SELECT * FROM patients_breathandsleeptest")
characteristics_other_accomp <- DBI::dbGetQuery(
  conn,
  "SELECT * FROM patients_patientcharacteristics_other_accomp"
)
extra_accompanying_disease <- DBI::dbGetQuery(
  conn,
  "SELECT * FROM patients_extraaccompanyingdisease"
)


characteristics_overall <- characteristics |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  join_lookup(extra_myopathy, "extra_myopathy_id", "myopathy") |>
  join_lookup(bad_habit, "alcohol_id", "bad_habit_status", "alcohol") |>
  join_lookup(physical_activity, "physical_activity_id", "physical_activity") |>
  join_lookup(nutrition, "nutrition_id", "nutrition") |>
  join_lookup(cardiopathy, "cardiopathies_id", "cardiopathy") |>
  join_lookup(underlying_disease, "underlying_disease_id", "disease", "underlying_disease") |>
  join_lookup(bad_habit, "smoker_id", "bad_habit_status", "smoker") |>
  dplyr::relocate(pat_id_id, visit_id, visit_date) |>
  dplyr::mutate(bmi = weight / (height / 100)^2)

characteristics_overall <- characteristics_overall |>
  join_lookup(
    characteristics_other_accomp,
    "id",
    "patientcharacteristics_id",
    "extra_id"
  ) |>
  join_lookup(
    extra_accompanying_disease,
    "extraaccompanyingdisease_id",
    "acc_disease",
    "accompanying_disease"
  ) |>
  dplyr::select(-"extra_id") |>
  dplyr::rename(
    c(
      "duchenne_muscular_dystrophy" = "dmd",
      "ALS" = "nkn",
      "diabetes_type_II" = "sd",
      "stroke" = "aee",
      "arterial_hypertension" = "ay"
    )
  )

# --- Build patients ---
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

# --- SAYY subset ---
patients_sayy <- patients_overall |>
  dplyr::filter(pat_condition == "ΣΑΥΥ") |>
  dplyr::select(id, gender, profession, pat_condition, birth_year)

characteristics_sayy <- characteristics_overall |>
  dplyr::filter(pat_id_id %in% patients_sayy$id) |>
  dplyr::select(-myopathy) |>
  dplyr::right_join(patients_sayy, by = c("pat_id_id" = "id")) |>
  dplyr::mutate(visit_date = as.Date(visit_date)) |>
  dplyr::arrange(pat_id_id, visit_date) |>
  dplyr::distinct(pat_id_id, .keep_all = TRUE) |>
  dplyr::mutate(age = as.integer(format(visit_date, "%Y")) - birth_year) |>
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "ΑΝΔΡΑΣ" ~ "Male",
      gender == "ΓΥΝΑΙΚΑ" ~ "Female",
      gender == "ΑΛΛΟ" ~ "Other",
      TRUE ~ gender
    ),
    bmi_group = dplyr::case_when(
      bmi < 25              ~ "Normal BMI",
      bmi >= 25 & bmi < 30  ~ "Overweight",
      bmi >= 30             ~ "Obese",
      .default = NA_character_
    )
  ) |>
  dplyr::mutate(
    bmi_group = factor(bmi_group, levels = c("Normal BMI", "Overweight", "Obese"))
  )
# --- Variable lists ---
binary_vars <- c(
  "ALS", "other_limit_lung", "diabetes_type_II", "duchenne_muscular_dystrophy",
  "stroke", "arterial_hypertension", "pulmonary_hypertension", "xap"
)

cat_vars <- c(
  "alcohol", "physical_activity", "nutrition", "cardiopathy",
  "underlying_disease", "smoker", "profession", "pat_condition", "gender",
  "accompanying_disease", "bmi_group"
)

char_vars <- c(
  "gender", "age", "weight", "height", "bmi", "bmi_group" ,"ALS", "other_limit_lung",
  "diabetes_type_II", "stroke", "arterial_hypertension",
  "pulmonary_hypertension", "alcohol", "physical_activity", "nutrition",
  "cardiopathy", "underlying_disease", "accompanying_disease", "smoker",
  "profession", "pat_condition"
)

result_bmi <- characteristics_sayy |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(cat_vars), ~ forcats::fct_na_value_to_level(as.factor(.x), level = "(Missing)")
    ),
    dplyr::across(dplyr::all_of(binary_vars), as.integer),
    gender = factor(gender, levels = c("Female", "Male", "Other"))
  ) |>
  dplyr::select(dplyr::all_of(char_vars), gender) |>
  gtsummary::tbl_summary(
    by      = bmi_group,
    missing = "no",  # NAs are now explicit "(Missing)" levels, not hidden
    type    = purrr::map(binary_vars, ~ "dichotomous") |> purrr::set_names(binary_vars),
    value   = purrr::map(binary_vars, ~ 1)             |> purrr::set_names(binary_vars),
    statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})"),
    digits    = list(gtsummary::all_continuous() ~ 1)
  ) |>
  gtsummary::add_overall(last = TRUE) |>
  gtsummary::bold_labels()

result_gender_bmi <- characteristics_sayy |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(cat_vars), ~ forcats::fct_na_value_to_level(as.factor(.x), level = "(Missing)")
    ),
    dplyr::across(
      dplyr::all_of(binary_vars), ~ factor(.x, levels = c(0, 1))
    ),
    gender    = factor(gender, levels = c("Female", "Male", "Other")),
    bmi_group = factor(bmi_group, levels = c("Normal BMI", "Overweight", "Obese"))
  ) |>
  dplyr::select(dplyr::all_of(char_vars), gender, bmi_group) |>
  gtsummary::tbl_strata(
    strata = bmi_group,
    .tbl_fun = ~ .x |>
      gtsummary::tbl_summary(
        by      = gender,
        missing = "no",
        type    = purrr::map(binary_vars, ~ "dichotomous") |> purrr::set_names(binary_vars),
        value   = purrr::map(binary_vars, ~ "1")           |> purrr::set_names(binary_vars),
        statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})"),
        digits    = list(gtsummary::all_continuous() ~ 1)
      ) |>
      gtsummary::add_overall(last = TRUE) |>
      gtsummary::bold_labels()
  )

result_gender <- characteristics_sayy |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(cat_vars), ~ forcats::fct_na_value_to_level(as.factor(.x), level = "(Missing)")
    ),
    dplyr::across(dplyr::all_of(binary_vars), as.integer),
    gender = factor(gender, levels = c("Female", "Male", "Other"))
  ) |>
  dplyr::select(dplyr::all_of(char_vars), gender) |>
  gtsummary::tbl_summary(
    by      = gender,
    missing = "no",  # NAs are now explicit "(Missing)" levels, not hidden
    type    = purrr::map(binary_vars, ~ "dichotomous") |> purrr::set_names(binary_vars),
    value   = purrr::map(binary_vars, ~ 1)             |> purrr::set_names(binary_vars),
    statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})"),
    digits    = list(gtsummary::all_continuous() ~ 1)
  ) |>
  gtsummary::add_overall(last = TRUE) |>
  gtsummary::bold_labels()

save_gtsummary(result_gender, "characteristics_by_gender", "results")
save_gtsummary(result_bmi, "characteristics_by_bmi", "results")
save_gtsummary(result_gender_bmi, "characteristics_by_gender_bmi", "results")

# ===================================================================
# Age plots
# ===================================================================

characteristics_sayy_filtered <- characteristics_sayy |>
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "ΑΝΔΡΑΣ" ~ "Male",
      gender == "ΓΥΝΑΙΚΑ" ~ "Female",
      gender == "ΑΛΛΟ"    ~ "Other",
      .default = gender
    )
  ) |>
  dplyr::filter(gender %in% c("Male", "Female"))

pal <- c("Male" = "#378ADD", "Female" = "#D4537E")

plot_data <- characteristics_sayy_filtered |>
  dplyr::filter(!is.na(age)) |>
  dplyr::count(gender, age_group = cut(age, breaks = seq(0, 100, 5), right = FALSE)) |>
  dplyr::group_by(gender) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::mutate(prop = dplyr::if_else(gender == "Male", -prop, prop)) |>
  dplyr::ungroup()

max_prop <- max(abs(plot_data$prop), na.rm = TRUE)

p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = prop, y = age_group, fill = gender)) +
  ggplot2::geom_col(color = "white", linewidth = 0.2, width = 0.85) +
  ggplot2::scale_fill_manual(values = pal, guide = "none") + 
  ggplot2::scale_x_continuous(
    limits = c(-max_prop, max_prop),
    breaks = seq(-1, 1, 0.05),
    labels = \(x) scales::percent(abs(x), accuracy = 1)
  ) +
  ggplot2::labs(
    title = "Age distribution by gender",
    subtitle = paste0(
      "Comparing <span style='color:", pal["Male"], 
      "'>**males**</span> to <span style='color:", pal["Female"], 
      "'>**females**</span>"),
    x = "Relative Frequency",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
  ggplot2::theme(
    plot.subtitle = ggtext::element_markdown(size = 11, color = "grey30", margin = ggplot2::margin(b = 15)),
    plot.title = ggplot2::element_text(face = "bold", size = 16, margin = ggplot2::margin(b = 5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = "grey92", linewidth = 0.4),
    
    # Axis styling
    axis.text.y = ggplot2::element_text(face = "italic", color = "grey20"),
    axis.title.x = ggplot2::element_text(size = 9, color = "grey50", margin = ggplot2::margin(t = 10)),
    
    # Plot margins
    plot.margin = ggplot2::margin(20, 20, 20, 20)
  )

ggplot2::ggsave(
  "results/sayy_age_distribution_by_gender.png",
  p,
  dpi = 800,
)

# ===================================================================
# Breath and sleep tests
# ===================================================================

breath_and_sleep_test_sayy <- breath_and_sleep_test |>
  dplyr::select(-"id") |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::rename("pat_id" = "pat_id_id") |>
  dplyr::relocate(c("pat_id", "visit_id", "visit_date")) |>
  dplyr::filter(pat_id %in% patients_sayy$id) |>
  dplyr::right_join(
    patients_sayy |> dplyr::select(c("id", "gender")),
    by = c("pat_id" = "id")
  )



breath_and_sleep_test_sayy <- breath_and_sleep_test |>
  dplyr::select(-id) |>
  dplyr::left_join(visit, by = c("visit_id" = "id")) |>
  dplyr::rename(pat_id = pat_id_id) |>
  dplyr::relocate(c(pat_id, visit_id, visit_date)) |>
  dplyr::filter(pat_id %in% patients_sayy$id) |>
  dplyr::right_join(
    patients_sayy |> dplyr::select(c(id, gender)),
    by = c("pat_id" = "id")
  ) |>
  dplyr::mutate(visit_date = as.Date(visit_date)) |>
  dplyr::arrange(pat_id, visit_date) |>
  dplyr::distinct(pat_id, .keep_all = TRUE)

binary_vars_dx <- c(
  "daytime_hypercapnia", "nocturnal_hypoventilation", "hypoxemia",
  "overnight_oximetry", "level_three_rec", "polysomnography",
  "capnometry", "other"
)

continuous_vars_dx <- c(
  "fvc_l", "fvc_perc", "fev1_l", "fev_perc", "fev1_fvc", "fev25_75",
  "ph", "po2", "pco2", "h3co2", "pins", "pex", "snip", "pcf",
  "avsao2_oxy", "minsao2_oxy", "t90_oxy", "odi_oxy",
  "ahirdi_br", "avsao2_br", "minsao2_br", "t90_br",
  "psg_trt", "psg_tst", "psg_sl", "psg_se", "psg_ai", "waso",
  "psg_n1", "psg_n2", "psg_n3", "psg_rem", "psg_snore",
  "psg_avsao2", "psg_minsao2", "psg_t90", "odi_br", "psg_ahirdi", "psg_odi","record_duration"
)

char_vars_dx <- c(binary_vars_dx, continuous_vars_dx)

# breath_and_sleep_test_sayy |>
#   dplyr::mutate(
#     dplyr::across(dplyr::all_of(binary_vars_dx), as.integer),
#     gender = factor(gender, levels = c("ΓΥΝΑΙΚΑ", "ΑΝΔΡΑΣ", "ΑΛΛΟ"))
#   ) |>
#   dplyr::select(dplyr::all_of(char_vars_dx), gender) |>
#   gtsummary::tbl_summary(
#     by        = gender,
#     missing   = "no",
#     type      = purrr::map(binary_vars_dx, ~ "dichotomous") |> purrr::set_names(binary_vars_dx),
#     value     = purrr::map(binary_vars_dx, ~ 1)             |> purrr::set_names(binary_vars_dx),
#     statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})"),
#     digits    = list(gtsummary::all_continuous() ~ 1)
#   ) |>
#   gtsummary::add_overall(last = TRUE) |>
#   gtsummary::bold_labels()



# generate_visit_table <- function(data, visit_number) {
#   visit_data <- data |>
#     dplyr::arrange(pat_id, visit_date) |>
#     dplyr::group_by(pat_id) |>
#     dplyr::mutate(visit_rank = dplyr::row_number()) |>
#     dplyr::ungroup() |>
#     dplyr::filter(visit_rank == visit_number)
# 
#   # Drop binary vars that are all-zero or all-NA in this visit slice
#   valid_binary <- binary_vars_dx |>
#     purrr::keep(~ {
#       vals <- visit_data[[.x]]
#       !all(is.na(vals)) && length(unique(stats::na.omit(vals))) > 1
#     })
# 
#   visit_data |>
#     dplyr::mutate(
#       dplyr::across(dplyr::all_of(valid_binary), ~ factor(.x, levels = c(0, 1))),
#       gender = factor(gender, levels = c("ΓΥΝΑΙΚΑ", "ΑΝΔΡΑΣ", "ΑΛΛΟ"))
#     ) |>
#     dplyr::select(dplyr::all_of(c(valid_binary, continuous_vars_dx)), gender) |>
#     gtsummary::tbl_summary(
#       by        = gender,
#       missing   = "no",
#       type      = purrr::map(valid_binary, ~ "dichotomous") |> purrr::set_names(valid_binary),
#       value     = purrr::map(valid_binary, ~ "1")           |> purrr::set_names(valid_binary),
#       statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})"),
#       digits    = list(gtsummary::all_continuous() ~ 1)
#     ) |>
#     gtsummary::add_overall(last = TRUE) |>
#     gtsummary::bold_labels()
# }

labels_df <- readr::read_csv("data/breath_and_sleep_test_labels.csv")

label_map <- labels_df |>
  dplyr::select(legacy_name, measurement_name_en) |>
  tibble::deframe()

generate_visit_table <- function(data, visit_number) {
  visit_data <- data |>
    dplyr::arrange(pat_id, visit_date) |>
    dplyr::group_by(pat_id) |>
    dplyr::mutate(visit_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(visit_rank == visit_number)

  valid_binary <- binary_vars_dx |>
    purrr::keep(~ {
      vals <- visit_data[[.x]]
      !all(is.na(vals)) && length(unique(stats::na.omit(vals))) > 1
    })

  all_vars <- c(valid_binary, continuous_vars_dx)

  # Return column name itself if no mapping or mapping is NA
  var_labels <- all_vars |>
    purrr::map_chr(~ {
      lbl <- label_map[.x]
      if (is.na(lbl)) .x else lbl
    }) |>
    purrr::set_names(all_vars)

  visit_data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(valid_binary), ~ factor(.x, levels = c(0, 1))),
      gender = factor(gender, levels = c("ΓΥΝΑΙΚΑ", "ΑΝΔΡΑΣ", "ΑΛΛΟ"))
    ) |>
    dplyr::select(dplyr::all_of(all_vars), gender) |>
    gtsummary::tbl_summary(
      by        = gender,
      missing   = "no",
      type      = purrr::map(valid_binary, ~ "dichotomous") |> purrr::set_names(valid_binary),
      value     = purrr::map(valid_binary, ~ "1")           |> purrr::set_names(valid_binary),
      statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})"),
      digits    = list(gtsummary::all_continuous() ~ 1),
      label     = as.list(var_labels)
    ) |>
    gtsummary::add_overall(last = TRUE) |>
    gtsummary::bold_labels()
}
# Get max number of visits any patient has
max_visits <- breath_and_sleep_test_sayy |>
  dplyr::count(pat_id) |>
  dplyr::pull(n) |>
  max()

# Generate one table per visit number
visit_tables <- seq_len(max_visits) |>
  purrr::map(~ generate_visit_table(breath_and_sleep_test_sayy, .x)) |>
  purrr::set_names(paste0("visit_", seq_len(max_visits)))
