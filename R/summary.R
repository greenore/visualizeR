
# Summary Statistics
#-------------------
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

# Frequency Table
freqTable <- function(data, xvar, yvar, n){
  x <- table(data[, yvar], data[, xvar])
  x <- x/n * 100
  x
}

# Recalculate Stratum Frequencies for Cantons
stratumSize <- function(data, var, size){
  table <- table(data[, var])/sum(table(data[, var])) * 100
  
  t <- round(table * size/100, 0)
  t <- ifelse(table < 1, 1, t)
  
  t[t > 1] <- round(t[t > 1] * (size - sum(t[t == 1])) / sum(t[t > 1]), 0)
  
  t  
}

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
