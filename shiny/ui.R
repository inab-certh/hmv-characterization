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
            "gender", "bmi", "age", "ahirdi_br", "psg_ahirdi", "smoker_status",
            "alcohol_status", "underlying_disease", "sd", "aee", "ay",
            "cardiopathy"
          ),
          selected = "gender",
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = "target_variable",
          label = "Μεταβλητή ενδιαφέροντος",
          choices = c("gender", "bmi_condition", "age_groups", "ahirdi_br_condition", "psg_ahirdi_condition", "smoker_status",
                      "alcohol_status", "underlying_disease", "sd", "aee", "ay",
                      "cardiopathy"),
          selected = "gender"
        ),
        shiny::selectInput(
          inputId = "analysis_table",
          label = "Ανάλυση",
          choices = c("characteristics", "device"),
          selected = "characteristics"
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "sayy",
          DT::dataTableOutput("subgroup_analysis"),
          shiny::downloadButton("download_data")
        ),
        shinydashboard::tabItem(
          tabName = "mechanical_ventilation",
          shiny::h2("Under construction")
        )
      )
    )
  )
)
