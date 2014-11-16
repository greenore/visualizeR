# Summary Statistics
#-------------------
#' @title Calculate a rank
#' @export
#' 
#' @description \code{calcRank} 
#'  
#' @param id
#' @param num_var
#' @param cat_var
#' @param data
#' @param decreasing
#' @param ties.method
#'

calcRank <- function(id, num_var, cat_var, data, decreasing=FALSE, ties.method="random"){
  # Setup
  id_var <- unique(data[, id])
  var_name <- paste0("Rank_", num_var)
  nam_vec <- c(id, cat_var, num_var)
  df.rank <- createDf(nam_vec)
  
  # Calculate rank for every profile
  for(i in id_var){
    df.tmp <- data[data[, id] %in% i, nam_vec]
    df.tmp[, var_name] <- rank(as.numeric(df.tmp[, num_var]), ties.method=ties.method)
    if(decreasing){df.tmp[, var_name] <- rank(as.numeric(df.tmp[, num_var]) * -1, ties.method=ties.method)}
    df.rank <- rbind(df.rank, df.tmp)
  }
  
  # Transform rank to integer
  df.rank[, var_name] <- as.integer(df.rank[, var_name])
  
  # Remove num_var
  df.rank <- df.rank[, !names(df.rank) %in% num_var]
  
  df.rank
}

#' @export
createDf <- function(nam_vec){
  df <- data.frame(matrix(vector(), 0, length(nam_vec), dimnames=list(c(), nam_vec)))
  df
}

#' @title Data cleanup
#' @export
#' 
#' @description \code{cleanFun} 
#'  
#' @param id
#' @param num_var
#' @param cat_var
#' @param data
#'

cleanFun <- function(id, num_var, cat_var, data){
  df <- data[complete.cases(data[, num_var]), ]
  df[, cat_var] <- as.factor(df[, cat_var])
  
  df<- droplevels(df)
  
  # Drop Profiles with less than ... Offers
  num <- aggregate(df[, cat_var], list(df[, id]), FUN = 'length')
  drop <- ifelse(num$x < (length(levels(df[, cat_var])) - 2), as.character(num$Group.1), NA)
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

#' @export
# Frequency Table
freqTable <- function(data, xvar, yvar, n){
  x <- table(data[, yvar], data[, xvar])
  x <- x/n * 100
  x
}

#' @export
# Recalculate Stratum Frequencies for Cantons
stratumSize <- function(data, var, size){
  table <- table(data[, var])/sum(table(data[, var])) * 100
  
  t <- round(table * size/100, 0)
  t <- ifelse(table < 1, 1, t)
  
  t[t > 1] <- round(t[t > 1] * (size - sum(t[t == 1])) / sum(t[t > 1]), 0)
  
  t  
}

#' @export
# NA DETECTION of an overall dataframe
isNa <- function(data, var.number = 1:length(data)){
  v <- data.frame(cbind(names(data), var.number))
  v[, 2] <- as.numeric(v[, 2])
  names(v) <- c('Variable', 'Missings')
  
  for(i in var.number){
    v[i, 2] <- length(data[is.na(data[, i]), i])
  }
  
  v[v[, 2] != 0, ]
}

# CLASS DETECTION of an overall dataframe
varClass <- function(data, var.number = 1:length(data)){
  v <- NULL
  
  for(i in var.number){
    v[i] <- class(data[, i])
  }
  
  x <- cbind(names(data), v, 'x')
  
  # Numeric | Integer | Logical
  for(i in 1:nrow(x)){
    x[i, 3] <- ifelse(x[i, 2] != 'factor' | x[i, 2] != 'Date', sample(data[, as.character(x[i, 1])], size = 1), x[i, 3])
  }
  
  # Factor
  for(i in 1:nrow(x)){
    x[i, 3] <- ifelse(x[i, 2] == 'factor', sample(levels(data[, as.character(x[i, 1])]), size = 1), x[i, 3])
  }
  
  # Date
  for(i in 1:nrow(x)){
    x[i, 3] <- ifelse(x[i, 2] == 'Date', as.character(sample(as.Date(data[, as.character(x[i, 1])]), size = 1)), x[i, 3])
  }
  
  return(x)
}
