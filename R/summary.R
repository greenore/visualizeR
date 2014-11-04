# Summary Statistics
#-------------------
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

#' @export
calcRank <- function(Profil_Var, Firma_Var, PN_Var, data){
  d <- data
  var <- c(Profil_Var, Firma_Var, PN_Var)
  d[, Profil_Var] <- as.factor(d[, Profil_Var])
  lev <- levels(d[, Profil_Var])
  
  x <- d[d[, Profil_Var] == lev[1], var]
  x <- x[order(x[, PN_Var]), var[1:3]]
  Rang <- c(1:nrow(x[complete.cases(x[, PN_Var]), ]), rep(NA, nrow(x[!complete.cases(x[, PN_Var]), ])))
  x$Rang <- Rang[(length(Rang) - nrow(x) + 1):length(Rang)]
  
  for(i in lev[2:length(lev)]){
    y <- d[d[, Profil_Var] == i, var]
    y <- y[order(y[, PN_Var]), var[1:3]]
    Rang <- c(1:nrow(y[complete.cases(y[, PN_Var]), ]), rep(NA, nrow(y[!complete.cases(y[, PN_Var]), ])))
    y$Rang <- Rang[(length(Rang) - nrow(y) + 1):length(Rang)]
    
    x <- rbind(x, y)
  }
  return(x)
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
