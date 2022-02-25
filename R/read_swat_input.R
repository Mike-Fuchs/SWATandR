#### read_swat_input ####
# This routine simplifies the reading of swat model input files to R

read_swat_input <- function(path) {
  data_header                       <- read.csv(paste0(path), skip = 1, nrows = 1, header = F, sep = "")
  data                              <- read.csv(paste0(path), skip = 2, header = F, sep = "")
  names(data)  <- data_header
  return(data)
}