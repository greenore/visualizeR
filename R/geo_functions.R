# Geographic Visualisations
#--------------------------

#' @title Load and prepare shape file
#' @export
#' 
#' @description \code{mergeShape} combines a shape file with a designated dataframe
#' according to an ID variable.
#' 
#' @param df Data that has to be merged with the shape file. 
#' @param path_shape Path to the shape file. 
#' @param shape_ID Unique ID of the shape file for the join.
#' @param df_ID Unique ID of the data file for the join.

mergeShape <- function(df, path_shape, df_ID, shape_ID){
	
  # Load Libraries
  require(maptools); require(sp)
  
  # Load shape file
  shape <- readShapePoly(path_shape)
  shape[[shape_ID]] <- as.character(shape[[shape_ID]])
  
  # Combine
  shape <- sp::merge(shape, df, by.x = shape_ID, by.y = df_ID, all = T, sort = T)
  shape
}

#' @title Plot data on a shape file (Geovisualisation)
#' @export
#' 
#' @description \code{plotMap} plots datapoints on a shape file. 
#' 
#' @param shape Data that has to be merged with the shape file. 
#' @param path_png Output path where the .png file is saved. 
#' @param var Name of the variable to be plotted.
#' @param labels Logical value to determine if labels are put onto the plot.
#' @param param Logical value to determine if the label values are transformed into percentages. 
#' @param col_shades What kind of colors shades are used. 
#' @param col_zero The color type for zeros.
#' @param col_missing The color type for missings (NA's).
#' 
#' @examples
#' plotMap(shp_obj, '/plots/schaeden.png', 'Anzahl')
#' 
#' plotMap(shp_obj, '/plots/kunden.png', 'Anzahl', perc = T, col_shades = 'Blues')

# Plot Function
plotMap <- function(shape, path_png, var, labels = T, perc = F, col_pos = 'Greens',
										col_neg = 'Reds', col_zero = '#999999', col_missing = 'black',
										numInterval = 10, numColLength = 4){
  
	# Load libraries
	require(maptools); require(RColorBrewer); require(classInt)
	
	# For Numeric variables
	if(is.numeric(shape[[var]]) == T){
		
		values <- shape[[var]]
		
		# Define Interval
		absMaxValue <- ceiling(max(abs(values), na.rm = T))
		minValue <- floor(min(values, na.rm = T))
		
		if(minValue < 0){
			interval <- c(-absMaxValue:absMaxValue)	
		}
		
		if(minValue >= 0){
			interval <- c(minValue:absMaxValue)	
		}
		
		# Breaks
		brks <- classIntervals(interval, n = numInterval, style = "quantile")$brks
		brks_pos <- brks[brks > 0]
		brks_neg <- brks[brks < 0]
		
		# Available Colors
		values <- ifelse(is.na(values), -9999, values)
		
		colors <- values
		col_pos <- brewer.pal(9, col_pos)
		col_neg <- brewer.pal(9, col_neg)
		
		col_neg <- col_neg[order(col_neg)]
		col_pos <- col_pos[(9 - numColLength):length(col_pos)]
		col_neg <- col_neg[1:(9 - numColLength)]
		
		# Set Colors
		colors[values > 0] <- col_pos[findInterval(values[values > 0], brks_pos, all.inside = T)]
		colors[values < 0] <- col_neg[findInterval(values[values < 0], brks_neg, all.inside = T)]
		colors[values == 0] <- col_zero
		colors[values == -9999] <- col_missing
		
		# Text
		shape$text <- as.character(round(values, 1))
		shape$text[shape$text == '-9999'] <- 'NV'
		shape$text <- as.character(round(values, 1))
		
		if(perc == T){
			shape$text <- paste(shape$text[shape$text != 'NV'], '%', sep = '')
		}
	}

	# For Character variables
	if(is.numeric(shape[[var]]) == F){
		shape$text <- as.character(shape[[var]])
		colors <- col_zero
	}
	
	
  # Plot the map
  png(path_png, bg = 'transparent', width = 1800, height = 1080)
  plot(shape, col = colors)
  if(labels == T){
    text(coordinates(shape), labels = shape$text, cex = 4, font = 1, col = 'black')
  }
  dev.off()
}

#' @title Kantone
#' @export
#' 
#' @description \code{geoCoding}
#'  
#' @param var
#'

geoCoding <- function(var){
  var <- as.character(var)
  var[var == 'AG'] <- 'Aargau'
  var[var == 'AI'] <- 'Appenzell Ausserrhoden'
  var[var == 'AR'] <- 'Appenzell Innerrhoden'
  var[var == 'BL'] <- 'Basel-Landschaft'
  var[var == 'BS'] <- 'Basel-Stadt'
  var[var == 'BE'] <- 'Bern / Berne'
  var[var == 'FR'] <- 'Fribourg / Freiburg'
  var[var == 'FL'] <- 'F\u00FCrstentum Liechtenstein'
  var[var == 'GE'] <- 'Gen\u00E8ve'
  var[var == 'GL'] <- 'Glarus'
  var[var == 'GR'] <- 'Graub\u00FCnden / Grigioni / Grischun'
  var[var == 'JU'] <- 'Jura'
  var[var == 'LU'] <- 'Luzern'
  var[var == 'NE'] <- 'Neuch\u00E2tel'
  var[var == 'NW'] <- 'Nidwalden'
  var[var == 'OW'] <- 'Obwalden'
  var[var == 'SH'] <- 'Schaffhausen'
  var[var == 'SZ'] <- 'Schwyz'
  var[var == 'SO'] <- 'Solothurn'
  var[var == 'SG'] <- 'St. Gallen'
  var[var == 'TG'] <- 'Thurgau'
  var[var == 'TI'] <- 'Ticino'
  var[var == 'UR'] <- 'Uri'
  var[var == 'VS'] <- 'Valais / Wallis'
  var[var == 'VD'] <- 'Vaud'
  var[var == 'ZG'] <- 'Zug'
  var[var == 'ZH'] <- 'Z\u00FCrich'
  var[var == 'ZHL'] <- 'Z\u00FCrich'
  var[var == 'ZHS'] <- 'Z\u00FCrich'
  var
}

#' @title Kantone2
#' @export
#'
#' @description \code{geoCoding2}
#'
#' @param var
#'

geoCoding2 <- function(var){
var <- as.character(var)
var[var == 'AG'] <- '19'
var[var == 'AI'] <- '16'
var[var == 'AR'] <- '15'
var[var == 'BL'] <- '13'
var[var == 'BS'] <- '12'
var[var == 'BE'] <- '2'
var[var == 'FR'] <- '10'
var[var == 'FL'] <- '27'
var[var == 'GE'] <- '25'
var[var == 'GL'] <- '8'
var[var == 'GR'] <- '18'
var[var == 'JU'] <- '26'
var[var == 'LU'] <- '3'
var[var == 'NE'] <- '24'
var[var == 'NW'] <- '7'
var[var == 'OW'] <- '6'
var[var == 'SH'] <- '14'
var[var == 'SZ'] <- '5'
var[var == 'SO'] <- '11'
var[var == 'SG'] <- '17'
var[var == 'TG'] <- '20'
var[var == 'TI'] <- '21'
var[var == 'UR'] <- '4'
var[var == 'VS'] <- '23'
var[var == 'VD'] <- '22'
var[var == 'ZG'] <- '9'
var[var == 'ZH'] <- '1'
var[var == 'ZHL'] <- '1'
var[var == 'ZHS'] <- '1'
as.numeric(var)
}
