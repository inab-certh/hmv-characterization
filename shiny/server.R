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

  result_table <- shiny::reactive({
    result <- count_subgroups_with_percentage(
      data = characteristics_sayy_first_visit_adult,
      subgroup_settings = subgroup_settings(),
      target_variable = input$target_variable
    ) |>
      dplyr::ungroup()

    result
  })

  output$subgroup_analysis <- DT::renderDataTable({
    form_table(result_table()) |>
      formattable::as.datatable()
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

  shiny::observe(
    print(paste(input$subgroup_variables, sep = "_" )))

})


