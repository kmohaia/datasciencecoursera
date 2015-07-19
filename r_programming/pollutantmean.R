#!/usr/bin/env Rscript

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
	
	# constructing a wild card path
	wc_path <- file.path(directory, "*.csv")
	# getting on the desired paths, using glob
	files <- (Sys.glob(wc_path))[id];
	
	pollutants <- c()

	for (file in files) {
		#reading each file
		data <- read.csv(file, sep=",")
		# extracting only the pollutant col
		target_col <- data[,pollutant]
		# removing na
		target_col <- target_col[!is.na(target_col)]
		# appending data to the existing pollutants vector
		pollutants <- c(pollutants, target_col)
	}
	return(mean(pollutants))
}




