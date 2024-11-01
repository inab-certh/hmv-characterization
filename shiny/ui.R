shiny::shinyUI(
  shinydashboardPlus::dashboardPage(
    skin = "black",
    title = "Characterization",
    shinydashboard::dashboardHeader(
      title = "Characterization"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "menu1",
        shinydashboard::menuItem(
          tabName = "sayy",
          text = "ΣΑΥΥ",
          icon = shiny::icon("bed")
        ),
        shinydashboard::menuItem(
          tabName = "mechanical_ventilation",
          text = "Μηχανικός αερισμός",
          icon = shiny::icon("mask-ventilator")
        )
      ),
      shinydashboard::sidebarMenu(
        id = "menu2",
        shiny::selectizeInput(
          inputId = "subgroup_variables",
          label = "Μεταβλητές ομαδοποίησης",
          choices = c(
            "gender", "bmi_condition", "age_groups", "ahirdi_br", "psg_ahirdi",
            "smoker_status", "alcohol_status", "underlying_disease", "sd",
            "aee", "ay", "cardiopathy"
          ),
          selected = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = "target_variable",
          label = "Μεταβλητή ενδιαφέροντος",
          choices = c(
            "gender", "bmi_condition", "age_groups", "ahirdi_br_condition",
            "psg_ahirdi_condition", "smoker_status", "alcohol_status",
            "underlying_disease", "sd", "aee", "ay", "cardiopathy"
          ),
          selected = NULL
        ),
        shiny::selectInput(
          inputId = "analysis_table",
          label = "Ανάλυση",
          choices = c("characteristics", "device", "breath_and_sleep_test"),
          selected = "characteristics"
        )
      )
    ),
    shinydashboard::dashboardBody(
      shiny::fluidRow(
        shinydashboard::box(
          title = "Ανάλυση σε υπο-ομάδες",
          status = "info",
          width = 12,
          DT::dataTableOutput("subgroup_analysis"),
          shiny::downloadButton("download_data")
        )
      ),
      shiny::fluidRow(
        shinydashboard::box(
          title = "Κατανομή τιμών",
          status = "info",
          width = 12,
          shiny::uiOutput("dynamic_output")
        )
      )
    )
  )
)
