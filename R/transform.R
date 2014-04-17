# Transformation Functions
#-------------------------

#' @title Transformation of factor levels
#' @export
#' 
#' @description \code{changeLevels} 
#'  
#' @param data
#' @param var
#' @param old_level
#' @param new_level
#' 

changeLevels <- function(data, var, old_level, new_level){
  old <- old_level
  new <- new_level
  levels(data[, var])[levels(data[, var]) == old] <- new
  data[, var]
}

#' @title Transformation from factors to numeric variables
#' @export
#' 
#' @description \code{fact2Num} 
#'  
#' @param data
#' @param var
#' 

fact2Num <- function(data, var){
  as.numeric(as.character(data[, var]))
}

#' @title Transformation from factors to date variables
#' @export
#' 
#' @description \code{fact2Date} 
#'  
#' @param data
#' @param var
#' 

fact2Date <- function(data, var){
  as.Date(levels(data[, var])[data[, var]])
}

#' @title Assign NA for blank values ("") for factor variables
#' @export
#' 
#' @description \code{blank2Missing} 
#'  
#' @param data
#' 

blank2Missing <- function(data){
  
  for (i in names(data)){
    if (class(data[, i]) == 'factor'){
      data[, i][data[, i] == ""] <- NA
      levels(data[, i])[levels(data[, i]) == ""] <- NA
    }
  }
  
  return(data)
}

#' @title Transform a long function text into a formula
#' @export
#' 
#' @description \code{textFun} 
#'  
#' @param yvar
#' @param xvar
#'

textFun <- function(yvar, xvar){
  yvar <- paste(yvar, ' ~ ', sep = '')
  xvar <- paste(xvar, sep = '', collapse = ' + ')
  fun <- paste(yvar, xvar, sep = '')
  paste(fun)
}

#' @title Calculate a rank
#' @export
#' 
#' @description \code{rankFun} 
#'  
#' @param praemie
#' @param id
#' @param category
#' @param data
#'

rankFun <- function(praemie, id, category, data){
	
	df <- data[!is.na(data[, praemie]), ]
	df[, id] <- as.factor(df[, id])
	
	# Calculate Rank
	rank <- calcRank(id, category, praemie, df)
	names(rank)[4] <- paste('Rang_', praemie, sep = '')
	
	rank <- rank[, !names(rank) %in% praemie]
	rank <- rank[order(rank[, id], rank[, names(rank)[3]]), ]
	return(rank)
}

#' @title Data cleanup
#' @export
#' 
#' @description \code{cleanFun} 
#'  
#' @param praemie
#' @param id
#' @param category
#' @param data
#'

cleanFun <- function(praemie, id, category, data){
	df <- data[complete.cases(data[, praemie]), ]
	df[, category] <- as.factor(df[, category])
	
	df<- droplevels(df)
	
	# Drop Profiles with less than ... Offers
	num <- aggregate(df[, category], list(df[, id]), FUN = 'length')
	drop <- ifelse(num$x < (length(levels(df[, category])) - 2), as.character(num$Group.1), NA)
	df <- df[!df[, id] %in% drop, ]
	return(df)
}

#' @title Aggregating
#' @export
#' 
#' @description \code{aggFun} 
#'  
#' @param praemie
#' @param category
#' @param data
#' @param decreasing
#'

aggFun <- function(praemie, category, data, decreasing = F){
	df <- data[!is.na(data[, praemie]), ]
	
	median <- aggregate(df[, praemie], list(df[, category]), FUN = 'median')
	mean <- aggregate(df[, praemie], list(df[, category]), FUN = 'mean')
	sd <- aggregate(df[, praemie], list(df[, category]), FUN = 'sd')
	n <- aggregate(df[, praemie], list(df[, category]), FUN = 'length')
	min <- aggregate(df[, praemie], list(df[, category]), FUN = 'min')
	max <- aggregate(df[, praemie], list(df[, category]), FUN = 'max')
	
	# Rename
	names(median) <- c(category, 'Median')
	names(mean) <- c(category, 'Mean')
	names(sd) <- c(category, 'SDev')
	names(n) <- c(category, 'N')
	names(min) <- c(category, 'Min')
	names(max) <- c(category, 'Max')
	
	# Combine
	agg <- merge(merge(merge(merge(merge(median, mean), sd), min), max), n)
	
	# Order
	agg <- agg[order(agg[, category], decreasing = decreasing), ]
	rownames(agg) <- NULL
	agg
}

#' @title Cuting folder files
#' @export
#' 
#' @description \code{cutFiles} Import a file list from a folder and cut it into
#' pieces
#'  
#' @param path
#' @param cut_left
#' @param cut_right
#'

cutFiles <- function(path, cut_left, cut_right){
	files <- list.files(path, full.names = F)
	
	files <- substr(x = files,
									start = nchar(cut_left) + 1,
									stop = nchar(files) - nchar(cut_right))
	
	files <- as.Date(files, "%Y-%m-%d")
	
	return(files)
}

#' @title Return the newest file from a folder w the cutFiles function
#' @export
#' 
#' @description \code{newestFile}
#'  
#' @param path
#' @param cut_left
#' @param cut_right
#'

newestFile <- function(path, cut_left, cut_right){
	files <- cutFiles(path, cut_left, cut_right)
	
	x <- paste(cut_left, files[files %in% max(files, na.rm = T)], sep = '')
	x <- paste(x, cut_right, sep = '')
	
	return(x)
}
