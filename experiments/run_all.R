### Create a data set excluding bot suspicious data and inappropriate data
rm(list = ls())
OUTPUTDIR <- "./output/without_suspicious/"

ROW_INVALID1 <- c(7, 22, 32, 44, 58, 88, 95, 96, 97, 113, 119, 135, 137, 138, 140)
ROW_INVALID2 <- c(6, 13, 21, 38, 45, 59, 92, 99, 100, 120, 136, 139, 140, 142, 146, 153, 155)
source("formatting.R")

### Confirmatory analysis
DATAID_INVALID <- c(21:25, 46:50)
FILEID <- "cnfm"
source("plot_data.R")
source("analysis.R")

### Exploratory analysis
DATAID_INVALID <- c()
FILEID <- "explr"
source("plot_data.R")
source("analysis.R")

### Create a data set excluding only inappropriate data
rm(list = ls())
OUTPUTDIR <- "./output/with_suspicious/"

ROW_INVALID1 <- c(32, 119, 137, 138, 140)
ROW_INVALID2 <- c(13, 21, 38, 45, 59, 92, 99, 100, 120, 136, 139, 140, 142, 146, 153, 155)
source("formatting.R")

### Check percent correct
DATAID_INVALID <- c(21:25, 46:50)
source("check_suspiciousdata.R")

### Confirmatory analysis
DATAID_INVALID <- c(21:25, 46:50)
FILEID <- "cnfm"
source("plot_data.R")
source("analysis.R")