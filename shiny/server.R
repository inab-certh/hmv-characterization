shiny::shinyServer(function(input, output,session) {
  subgroup_settings <- shiny::reactive({
    result <- list()
    for (i in seq_along(input$subgroup_variables)) {
      subgroup_settings_name <- paste(
        input$subgroup_variables[i],
        "subgroup_settings",
        sep = "_"
      )
      result[[i]] <- get(subgroup_settings_name)
    }
    return(result)
  })

  current_base_dataset <- shiny::eventReactive(
    list(subgroup_settings(),input$target_variable, input$menu1), {
    if (input$menu1 == "sayy") {
      if (input$analysis_table == "characteristics")
        characteristics_sayy_first_visit_adult
      else if (input$analysis_table == "device")
        device_testing_info_overall_first_visist_adult
      else if (input$analysis_table == "breath_and_sleep_test") {
        breath_and_sleep_test_overall_first_visit_adult |>
          dplyr::filter(study != "ΑΓΝΩΣΤΟ")
      }
    } else if (input$menu1 == "mechanical_ventilation") {
      if (input$analysis_table == "characteristics")
        characteristics_mv_first_visit
      else if (input$analysis_table == "patient_ventilation")
        patient_ventilation_mv_first_visit
      else if (input$analysis_table == "device")
        device_testing_info_overall_mv_first_visit
    }
  })

  result_table <- shiny::eventReactive(
    list(subgroup_settings(),input$target_variable, input$menu1), {
      if (input$analysis_table == "breath_and_sleep_test" &
          input$target_variable %in% sayy_continuous_variables) {
        result <- count_subgroups_with_percentage(
          data = current_base_dataset(),
          subgroup_settings = subgroup_settings(),
          target_variable = input$subgroup_variables[1]
        ) |>
          dplyr::ungroup()
      } else {
        result <- count_subgroups_with_percentage(
          data = current_base_dataset(),
          subgroup_settings = subgroup_settings(),
          target_variable = input$target_variable
        ) |>
          dplyr::ungroup()
      }
      result
    }
  )

  output$subgroup_analysis <- DT::renderDataTable({
    form_table(result_table()) |>
      formattable::as.datatable(options = list(pageLength = 100))
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        paste(
          input$analysis_table,
          "subgroup",
          paste(input$subgroup_variables, collapse = "_"),
          "target",
          input$target_variable,
          sep = "_"),
        ".csv"
      )
    },
    content = function(file) {
      write.csv(result_table(), file, row.names = FALSE)
    }
  )

  observeEvent(input$subgroup_variables, {
    selected_options <- input$subgroup_variables
    if (input$analysis_table == "breath_and_sleep_test") {
      selected_options <- c(
        selected_options,
        sayy_continuous_variables
      )
    }
    updateSelectInput(
      session, "target_variable",
      choices = selected_options,
      selected = NULL
    )
  })

  observeEvent(input$menu1, {
    new_choices_analysis_table <- switch(
      input$menu1,
      sayy =  c("characteristics", "device", "breath_and_sleep_test"),
      mechanical_ventilation = c(
        "characteristics", "patient_ventilation", "device"
      )
    )
    shiny::updateSelectInput(
      session,
      "analysis_table",
      choices = new_choices_analysis_table,
      selected = new_choices_analysis_table[1]
    )
  })

  observeEvent(list(input$analysis_table, input$menu1), {
    if (input$menu1 == "sayy") {
      new_choices_subgroup_variables <- switch(
        input$analysis_table,
        characteristics = c(
          "gender", "bmi_condition", "bmi_25", "bmi_30", "age_groups",
          "ahirdi_br_condition", "psg_ahirdi_condition",
          "ahirdi_overall_condition", "smoker_status", "alcohol_status",
          "underlying_disease", "sd", "aee", "ay", "cardiopathy",
          "pulmonary_hypertension"
        ),
        device = c(
          "age_groups", "gender", "mask_type",
          "ma_type", "humidifier","dev_sel_type"
        ),
        breath_and_sleep_test = c(
          "study", "age_groups", "gender", "bmi_condition","bmi_25", "bmi_30",
          "nocturnal_hypoventilation", "hypoxemia", "symptom"
        )
      )
      new_choices_target_variable <- switch(
        input$analysis_table,
        characteristics = c(
          "gender", "bmi_condition", "bmi_25", "bmi_30", "age_groups",
          "ahirdi_br_condition", "psg_ahirdi_condition",
          "ahirdi_overall_condition", "smoker_status", "alcohol_status",
          "underlying_disease", "sd", "aee", "ay","cardiopathy",
          "pulmonary_hypertension"
        ),
        device = c(
          "age_groups", "gender", "mask_type",
          "ma_type", "humidifier", "dev_sel_type"
        ),
        breath_and_sleep_test = c(
          "study", "age_groups", "gender", "bmi_condition","bmi_25", "bmi_30",
          "nocturnal_hypoventilation", "hypoxemia",
          sayy_continuous_variables,
          "symptom"
        )
      )
    } else if (input$menu1 == "mechanical_ventilation") {
      new_choices_subgroup_variables <- switch(
        input$analysis_table,
        characteristics = c(
          "gender", "bmi_condition", "bmi_25", "bmi_30", "age_groups",
          "xap", "sayy", "obesity_subvent", "other_obstructive", "dmd",
          "myasthenia", "nkn", "other_neurological", "diaphragm_malfunction",
          "posttb", "kyphoscoliosis", "other_limit_lung"
        ),
        patient_ventilation = c(
          "gender", "period_of_usage", "xoth_hours_24_condition",
          "application_instructions", "physiotherapy_instructions",
          "emergency_instructions", "family_education", "certified_education",
          "invasive_ventilation", "tracheostomy_type", "ventilation_reason",
          "ventilation_status", "ventilation_type"
        ),
        device = c(
          "age_groups", "gender", "mask_type",
          "ma_type", "humidifier","dev_sel_type"
        )
      )
      new_choices_target_variable <- switch(
        input$analysis_table,
        characteristics = c(
          "gender", "bmi_condition", "bmi_25", "bmi_30", "age_groups",
          "xap", "sayy", "obesity_subvent", "other_obstructive", "dmd",
          "myasthenia", "nkn", "other_neurological", "diaphragm_malfunction",
          "posttb", "kyphoscoliosis", "other_limit_lung"
        ),
        patient_ventilation = c(
          "gender", "period_of_usage", "xoth_hours_24_condition",
          "application_instructions", "physiotherapy_instructions",
          "emergency_instructions", "family_education", "certified_education",
          "invasive_ventilation", "tracheostomy_type", "ventilation_reason",
          "ventilation_status", "ventilation_type"
        ),
        device = c(
          "age_groups", "gender", "mask_type",
          "ma_type", "humidifier","dev_sel_type"
        )
      )
    }
    shiny::updateSelectizeInput(
      session,
      "subgroup_variables",
      choices = new_choices_subgroup_variables,
      selected = new_choices_subgroup_variables[1]
    )
    shiny::updateSelectInput(
      session,
      "target_variable",
      choices = new_choices_target_variable,
      selected = new_choices_target_variable[1]
    )
  })

  output$dynamic_output <- shiny::renderUI({
    if (input$analysis_table == "breath_and_sleep_test" &
        input$target_variable %in% sayy_continuous_variables) {
      dd <- current_base_dataset() |>
        dplyr::filter(study != "ΑΓΝΩΣΤΟ")
      if (input$target_variable %in% oxymetry_variables) {
        dd <- dd |>
          dplyr::filter(study == "overnight oxymetry")
      } else if (input$target_variable %in% level_three_variables) {
        dd <- dd |>
          dplyr::filter(study == "level three rec")
      } else if (input$target_variable %in% psg_variables) {
        dd <- dd |>
          dplyr::filter(study == "polysomnography")
      }
      pp <- run_within_subgroups(
        data = dd,
        subgroup_settings = subgroup_settings(),
        target_variable = input$target_variable,
        fun = calculate_density,
        column = input$target_variable
      ) |>
        tidyr::unnest(percentage)
      x_index <- which(names(pp) == "x")
      result_plot <- pp |>
        dplyr::rowwise() |>
        dplyr::mutate(
          groups = paste(dplyr::c_across(1:(x_index - 1)), collapse = " + ")
        ) |>
        dplyr::ungroup() |>
        dplyr::select(c(x, y, groups)) |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = x,
            y = y,
            color = groups,
            fill = groups
            )
          ) +
        # ggplot2::geom_area(alpha = 0.5)
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw()
        # ggplot2::geom_line()
      shiny::renderPlot(result_plot)
    }
  })

  shiny::observe(
    print(input$analysis_table))

})


