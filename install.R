install.packages(
  c(
    'openxlsx',
    'readxl',
    'shinyjs',
    'tidyverse',
    'shiny'
  )
)
if(!require(devtools)) { install.packages("devtools") }
devtools::install_github("lifs-tools/rgoslin", ref="master")
