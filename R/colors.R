#' Baloise Colours
#'
#' \code{balCol} returns the 6 different official colour in use at Baloise.
#' This saves some time!
#' 
#' @export
#'
#' @details select one of the following colours:
#' \itemize{
#'  \item{"dbl"}{ Dunkelblau}
#'  \item{"hbl"}{ Hellblau}
#'  \item{"vio"}{ Violet}
#'  \item{"rot"}{ Rot}
#'  \item{"ora"}{ Orange}
#'  \item{"gel"}{ Gelb}
#' }
#' 
#' @examples
#' balCol("dbl")

balCol <- function (col, alpha=255){
  df.col <- list()
  df.col$dbl <- rgb(0, 51, 153, alpha=alpha, maxColorValue=255)
  df.col$hbl <- rgb(0, 165, 213, alpha=alpha, maxColorValue=255)
  df.col$vio <- rgb(183, 0, 122, alpha=alpha, maxColorValue=255)
  df.col$rot <- rgb(230, 67, 102, alpha=alpha, maxColorValue=255)
  df.col$ora <- rgb(246, 168, 0, alpha=alpha, maxColorValue=255)
  df.col$gel <- rgb(255, 220, 68, alpha=alpha, maxColorValue=255)
  as.character(df.col[col])
}

#' @title Add Color to a dataframe
#' @export
#' 
#' @description \code{colFun} Add a specific color according to the rank and the
#' category, i.e., Basler are blue etc. 
#'  
#' @param rang
#' @param id
#' @param category
#' @param rank_col
#' @param top
#' @param flop
#' @param nam_blue
#' @param nam_orange
#' @param col_neutral
#'

colFun <- function(rang, id, category, data, rank_col = T, top = c(1), flop = c(1),
                   nam_blue = 'Basler', nam_orange = 'TCS',
                   col_neutral = rgb(0, 0, 0, alpha = 180, maxColorValue = 255)){
  
  # Color Name
  col_name <- gsub('Rang_', '', rang)
  col_name <- paste('color_', col_name, sep = '')
  
  # seperate df
  df <- data[!is.na(data[, rang]), ]
  
  # Blue color for Baloise
  # ... (alpha = 180 is a good value)
  
  df[, col_name] <- ifelse(df[, category] %in% nam_blue, balCol('hbl'), col_neutral)
  df[, col_name] <- ifelse(df[, category] %in% nam_orange, balCol('ora'), df[, col_name])
  
  if (rank_col == T){
    # Green color for top profiles
    
    ifelse(df[, rang] %in% top, 1, 0)
    df[, col_name] <- ifelse(df[, rang] %in% top, rgb(0, 100, 0, alpha = 180, maxColorValue = 255), df[, col_name])
    
    # Red color for flop profiles
    num <- aggregate(df[, category], list(df[, id]), FUN = 'length')
    
    n4 <- ifelse(num$x == (length(unique(df[, category])) - 3), as.character(num$Group.1), NA)
    n5 <- ifelse(num$x == (length(unique(df[, category])) - 2), as.character(num$Group.1), NA)
    n6 <- ifelse(num$x == (length(unique(df[, category])) - 1), as.character(num$Group.1), NA)
    n7 <- ifelse(num$x == (length(unique(df[, category]))), as.character(num$Group.1), NA)
    
    n41 <- sort((length(unique(df[, category])) - 3) - (flop - 1))
    n51 <- sort((length(unique(df[, category])) - 2) - (flop - 1))
    n61 <- sort((length(unique(df[, category])) - 1) - (flop - 1))
    n71 <- sort((length(unique(df[, category])) ) - (flop - 1))
    
    df[, col_name] <- ifelse(df[, id] %in% n4 & df[, rang] %in% n41, rgb(139, 0, 0, alpha = 180, maxColorValue = 255), df[, col_name])
    df[, col_name] <- ifelse(df[, id] %in% n5 & df[, rang] %in% n51, rgb(139, 0, 0, alpha = 180, maxColorValue = 255), df[, col_name])
    df[, col_name] <- ifelse(df[, id] %in% n6 & df[, rang] %in% n61, rgb(139, 0, 0, alpha = 180, maxColorValue = 255), df[, col_name])
    df[, col_name] <- ifelse(df[, id] %in% n7 & df[, rang] %in% n71, rgb(139, 0, 0, alpha = 180, maxColorValue = 255), df[, col_name])  
  }
  
  df <- merge(data, df[, c(id, category, col_name)], by = c(id, category), all.x = T)
  df[, col_name] <- ifelse(is.na(df[, rang]), 'black', df[, col_name])
  
  df
  
}

