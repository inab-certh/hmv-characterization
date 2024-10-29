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

  current_base_dataset <- shiny::reactive({
    if (input$analysis_table == "characteristics")
      characteristics_sayy_first_visit_adult
    else
      device_testing_info_overall_first_visist_adult
  })

  result_table <- shiny::reactive({


      result <- count_subgroups_with_percentage(
        data = current_base_dataset(),
        subgroup_settings = subgroup_settings(),
        target_variable = input$target_variable
      ) |>
        dplyr::ungroup()


    result
  })

  output$subgroup_analysis <- DT::renderDataTable({
    form_table(result_table()) |>
      formattable::as.datatable(options = list(pageLength = 100))
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        paste(
          "data",
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


  observeEvent(input$analysis_table, {
    new_choices_subgroup_variables <- switch(
      input$analysis_table,
      characteristics = c(
        "gender", "bmi", "age", "ahirdi_br", "psg_ahirdi", "smoker_status",
        "alcohol_status", "underlying_disease", "sd", "aee", "ay", "cardiopathy"
      ),
      device = c(
        "age", "gender", "mask_type", "ma_type", "humidifier", "dev_sel_type"
      )
    )

    new_choices_target_variable <- switch(
      input$analysis_table,
      characteristics = c(
        "gender", "bmi_condition", "age_groups","ahirdi_br_condition",
        "psg_ahirdi_condition", "smoker_status", "alcohol_status",
        "underlying_disease", "sd", "aee", "ay","cardiopathy"
      ),
      device = c(
        "age_groups", "gender", "mask_type", "ma_type", "humidifier", "dev_sel_type"
      )
    )
    # Update sub_choice with new choices based on main_choice
    shiny::updateSelectizeInput(
      session,
      "subgroup_variables",
      choices = new_choices_subgroup_variables
    )
    shiny::updateSelectInput(
      session,
      "target_variable",
      choices = new_choices_target_variable
    )
  })

  shiny::observe(
    print(result_table()))

})


