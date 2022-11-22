###### Install necessary libraries if they are not installed yet ######
libset = c(
  "nparcomp",
  "nparLD",
  "MBESS",
  "MASS",
  "ggplot2",
  "ggpubr",
  "gridExtra",
  "RColorBrewer",
  "stringr"
)

for (i in 1:length(libset)) {
  check <- require(libset[i], character.only = TRUE)
  
  if(!check){
    install.packages(libset[i])
  }
}