db_connect <- function() {
  DBI::dbConnect(
    RMySQL::MySQL(),
    host = Sys.getenv("HOST", unset = NA_character_),
    port = as.integer(Sys.getenv("PORT", unset = "3306")),
    dbname = Sys.getenv("DB_NAME", unset = NA_character_),
    username = Sys.getenv("DB_USER", unset = NA_character_),
    password = Sys.getenv("DB_PASS", unset = NA_character_)
  )
}

