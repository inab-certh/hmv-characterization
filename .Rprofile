source("renv/activate.R")

options(HTTPUserAgent = sprintf(
  "R/%s R (%s)", 
  getRversion(), 
  paste(
    getRversion(), 
    R.version["platform"], 
    R.version["arch"], 
    R.version["os"]
  )
))

.ppm <- "https://packagemanager.posit.co/cran/__linux__/jammy/2024-10-07"
options(repos = c(CRAN = .ppm))
