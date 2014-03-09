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
#' 
#' @examples
#' prepGeo(my.df, '/shape/schweiz.shp', 'plz', 'PLZ')

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
	require(maptools); require(RColorBrewer); require(classInt); require(devEMF)
	
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

# Kantone
geoCoding <- function(var){
  var <- as.character(var)
  var[var == 'AG'] <- 'Aargau'
  var[var == 'AI'] <- 'Appenzell Ausserrhoden'
  var[var == 'AR'] <- 'Appenzell Innerrhoden'
  var[var == 'BL'] <- 'Basel-Landschaft'
  var[var == 'BS'] <- 'Basel-Stadt'
  var[var == 'BE'] <- 'Bern / Berne'
  var[var == 'FR'] <- 'Fribourg / Freiburg'
  var[var == 'FL'] <- iconv('FÃ¼rstentum Liechtenstein', 'utf-8')
  var[var == 'GE'] <- iconv('GenÃ¨ve', 'utf-8')
  var[var == 'GL'] <- 'Glarus'
  var[var == 'GR'] <- iconv('GraubÃ¼nden / Grigioni / Grischun', 'utf-8')
  var[var == 'JU'] <- 'Jura'
  var[var == 'LU'] <- 'Luzern'
  var[var == 'NE'] <- iconv('NeuchÃ¢tel', 'utf-8')
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
  var[var == 'ZH'] <- iconv('ZÃ¼rich', 'utf-8')
  var[var == 'ZHL'] <- iconv('ZÃ¼rich', 'utf-8')
  var[var == 'ZHS'] <- iconv('ZÃ¼rich', 'utf-8')
  var
}
