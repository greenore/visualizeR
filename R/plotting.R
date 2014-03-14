# Histogramm
#-----------
plotHist <- function(var, medianLine = T, col_bars = '#999999', col_median = 'purple',
										 border = NA, lwd = 0.4, las = 1, ...){

	# Histogram
	h <- hist(var, col = col_bars, border = border, lwd = lwd, las = las, ...)
	
	# White borders
	maxFreq <- max(h$counts)
	segments(x0 = h$breaks, y0 = rep(0, length(h$breaks)), x1 = h$breaks, y1 = maxFreq,
					 col = "white")

	# Median line
	if(medianLine == T){
		varMedian <- median(var)
		lines(c(varMedian, varMedian), c(-1, maxFreq), col = col_median, lwd = 2)
		
	}
}

# Lineplot
#---------
linePlot <- function(df, varYear, varValue, varCat, type = 'b', col = 'black', ...){
	
	i <- 1
	x <- df[, varYear][df[, varCat] ==	unique(df[, varCat])[i]]
	y <- df[, varValue][df[, varCat] ==	unique(df[, varCat])[i]]
	color <- ifelse(col == 'black', 'black', df[, col][df[, varCat] ==	unique(df[, varCat])[i]])	
	
	plot(x, y, type = type, ylim = c(0, max(df[, varValue])), xlim = c(min(df[, varYear]), max(df[, varYear]) + 1),
			 las = 1, ylab = '', xlab = '', col = color, ...)
	text(max(df[, varYear]) + .5, y[length(y)], label = unique(df[, varCat])[i],
			 col = color)
	
	for(i in 2:length(unique(df[, varCat]))){
		x <- df[, varYear][df[, varCat] ==	unique(df[, varCat])[i]]
		y <- df[, varValue][df[, varCat] ==	unique(df[, varCat])[i]]
  	color <- ifelse(col == 'black', 'black', df[, col][df[, varCat] ==	unique(df[, varCat])[i]])	
		
		lines(x, y, type = type, ylim = c(0, max(df[, varValue])), xlim = c(min(df[, varYear]), max(df[, varYear]) + 1),
					las = 1, ylab = '', xlab = '', col = color, ...)
		text(max(df[, varYear]) + .5, y[length(y)], label = unique(df[, varCat])[i],
				 col = color)
	}	
}


# Pie Charts
pieChart <- function(data, var, col = c(balcol$hbl, 'lightgrey'), change = F, main = ''){
  require(plotrix)
  
  prop <- table(data[, var])/sum(table(data[, var]))
  prop <- as.table(c(prop[ifelse(change, 2, 1)], prop[ifelse(change, 1, 2)]))
  
  # For 100 Percent in a category
  prop <- prop[!names(prop) %in% NA]
  
  lab <- paste(paste(names(prop), '\n', round(prop, 2) * 100, sep = ''), '%', sep = '')
  
  plot(1:5, type = "n", axes = F, ylab = '', xlab = '')
  pie.plot <- floating.pie(3, 3, prop, col = col, border = 'white', main = main,
                           radius = 1.7, edges = 2000, startpos = 4.94/pi)
  
  pie.labels(3, 3, pie.plot, lab, border = 'black', bg = 'white', cex = 2, radius = 1.7)
  
  title(main = main, cex.main = 2)
}

# Density Plot
densityPlot <- function(data, var, col = balcol$hbl, xlim = T, ylim = T){
  d <- density(data[, var])
  
  xlim1 <- ifelse(length(xlim) == 2, xlim[1], min(data[, var]))
  xlim2 <- ifelse(length(xlim) == 2, xlim[2], max(data[, var]))
  
  ylim1 <- ifelse(length(ylim) == 2, ylim[1], 0)
  ylim2 <- ifelse(length(ylim) == 2, ylim[2], max(d$y)*1.1)
  
  plot(d, main = paste(var), xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), ylab = '', xlab = '', axes = F,
       cex.main = 2.5)
  
  polygon(d, col = col, border = col)
  axis(side = 1, cex.axis = 2, las = 1); axis(side = 2, cex.axis = 2)
}

# HISTOGRAM and BOXPLOTS of NUMERIC variables
hiBoData <- function(data){
  v <- NULL
  
  for(i in 1:length(data)){
    v[i] <- ifelse(class(data[, i]) == 'numeric' | class(data[, i]) == 'integer', T, F)
  }
  
  d <- data[, v]
  n <- length(d)
  
  par(omi = c(0, 1, 0, 0), bg = 'transparent')
  if (n%%2 == 0 | n > 8){
    layout(matrix(c(1:16), 4, 4, byrow = T))
  } else {
    layout(matrix(c(1:12, 0, 13, 14, 0), 4, 4, byrow = T))
  }
  
  for (i in names(d)){
    par("plt" = c(.2,.95,.2,.8))
    hist(d[, i], main = i, col = "lightgray", xlab = "")
    par("plt" = c(.01,.20,.2,.8))
    boxplot(d[, i], col = "lightgray", axes = F)
  }
}

# BARPLOT of a FACTORIAL variable
#--------------------------------
barPlot <- function(data){
  v <- NULL
  
  for(i in 1:length(data)){
    v[i] <- ifelse(class(data[, i]) == 'factor' | class(data[, i]) == 'Date', T, F)
  }
  
  d <- data[, v]
  
  for (i in names(d)) {
    barplot(table(d[, i]), main = i, cex.names = 1,
            horiz = T)
  }
}

# BeePlot
#--------
beePlot <- function(id, category, praemie, rank, data, range = c(0, max),
										conf = T, median = T, mean = F, label = T, bee_plot = T,
                    cex = 6, cex.axis = 1.8, cex.lab = 1.4, cex.med = 2.2, 
                    cex.minmax = 1.3, lwd.med = 3){

	require(beeplot)
	
	# Ceiling
  df <- data[!is.na(data[, praemie]), ]
  max <- ceiling(max(df[, praemie])/100)*100
  
	# Aggregation
	aggPrime <- aggFun(praemie = praemie, category = category, data = data)
	aggRank <- aggFun(praemie = rank, category = category, data = data)
	names(aggRank)[names(aggRank) == 'Mean'] <- 'averageMean'
	
	# Merge
	df <- merge(df, aggRank[, c(category, 'averageMean')], by = category)
	aggPrime <- merge(aggPrime, aggRank[, c(category, 'averageMean')], by = category)

	# Order according to mean rank
	aggPrime[, category] <- reorder(aggPrime[, category], aggPrime$averageMean, FUN = 'mean')
	aggPrime <- aggPrime[order(aggPrime$averageMean), ]
	df[, category] <- reorder(df[, category], df$averageMean, FUN = 'mean')
	
	# Plotting
	par(mar = c(5, 6, 4, 2) + 0.1)
  
  if (bee_plot == F){
    boxplot(eval(parse(text = textFun(praemie, category))), data = df, border = F,
            cex = cex, las = 1, cex.axis = cex.axis, ylim = range)
  }
 
	# Beeplot
  if (bee_plot == T){
    
    set.seed(123)
    bee <- beeswarm(eval(parse(text = textFun(praemie, category))), data = df, 
                    vertical = T, method = 'center',  corral = "random",
                    spacing = 0.7, cex = cex,
                    pwcol = df[, 'Color'], pch = 18,
                    xlab = '', ylab = '', main = '',
                    ylim = range, las = 1, cex.axis = cex.axis,
                    pwbg = as.character(df[, id]))
    
    # Label: Numbers
    if (label == T){
      text(bee, labels = bee$bg, cex = cex.lab, col = 'white', font = 2)
    }

    # Line: Median
    if (median == T){
      boxplot(eval(parse(text = textFun('Median', category))), add = T, data = aggPrime,
      				xlab = "", ylab = "", axes = F, border = 'black', boxwex = .8, lwd = lwd.med, ylim = range)
    }

    if (mean == T){
    	boxplot(eval(parse(text = textFun('Mean', category))), add = T, data = aggPrime,
    					xlab = "", ylab = "", axes = F, border = balCol('rot'), boxwex = .8, lwd = lwd.med, ylim = range)
    }
    
    # Label: Median
    if (median == T){
    	lab <- paste('Median', ' = ', sep = '')
    	text(rep(0.5 * range[2]/100, length(levels(df[, category]))),
           labels = paste(lab, round(aggPrime$Median, 0), '.-', sep = ''),
           cex = cex.med, col = 'black', font = 2)
    }
    
    # Conf Intervall
    col <- ifelse(conf, rgb(0/255, 51/255, 153/255, alpha = 0.3), NA)
    box <- boxplot(eval(parse(text = textFun(praemie, category))), data = df, plot = F)
    rect((1:length(levels(df[, category]))) - 0.45, box$conf[1, ],
         (1:length(levels(df[, category]))) + 0.45, box$conf[2, ],
         col = col, border = NA)
    
    # Label: CI_L & CI_U
    if (conf == T){
      text(rep(-2.5 * range[2]/100, length(levels(df[, category]))),
           labels = paste('95% CI: [', round(box$conf[1, ], 0), '.- ; ',
                          round(box$conf[2, ], 0), '.-]', sep = ''),
           cex = cex.minmax, col = 'black', font = 2)
    } 
    
    # Label: Min Max
    if (conf == F & median == T){
      text(rep(-2.5 * range[2]/100, length(levels(df[, category]))),
           labels = paste('(Min = ', round(aggPrime$Min, 0), '.-',
                          '  Max = ', round(aggPrime$Max, 0), '.-)', sep = ''),
           cex = cex.minmax, col = 'black', font = 2)
    } 
  }
}

# Rank Plot
#----------
rankPlot <- function(rang, id, category, df, orderFun = 'Median', range = c(0, max), cex = 1.5,
										 nam_blue = 'Baloise', nam_orange = 'TCS', leg = T){
  
	# Calculate Frequency Table
  t <- freqTable(df, rang, category, length(unique(df[, id])))
  
  length(unique(df[, id]))
  for(i in 1:nrow(t)){
    t[i, nrow(t)-1] <- t[i, nrow(t)-1] + (100 - sum(t[i, ]))
  }
  
  # Transform Data
  x <- data.frame(t); names(x) <- c(category, 'Rang', 'Prozent')
  
  for(i in 1:length(levels(x$Rang))){
    levels(x$Rang)[i] <- paste('Rang', levels(x$Rang)[i])
  }
  
  # Color
  x[, 'Color'] <- ifelse(x[, category] %in% nam_blue, balCol('hbl'), '#999999')
  x[, 'Color'] <- ifelse(x[, category] %in% nam_orange, balCol('ora'), x[, 'Color'])
  
  # Maximum
  max <- ceiling(max(x[, 'Prozent'])/15)*15
  
  # Mean Rang
  mean_rang <- aggregate(eval(parse(text = textFun(rang, category))), data = df, FUN = 'mean')
  
  # Order the category according to Mean Rank
  x[, category] <- factor(x[, category], levels = levels(x[, category])[order(mean_rang[, 2])])
  
  # Plot
  for(i in levels(x[, category])){
    d <- x[x[, category] == i, ]
    
    if (prod(!levels(d$Rang) %in% 'Rang 1') == 1){
      tmp <- d[1, ]
      tmp$Rang <- 'Rang 1'; tmp$Prozent <- 0
      d <- rbind(tmp, d)
    }
    
    d <- d[order(d$Rang, decreasing = F), ]
    
    # Barplot
    barp <- barplot(d$Prozent, ylim = range, names.arg = d$Rang, axes = F,
                    col = d$Col, ylab = '', main = '', las = 3,
                    cex.axis = cex * 1.4, cex.names = cex * 1, cex.lab = cex,
                    border = F)
    
    # Format number
    a <- round(sum(d$Prozent[1:3]), digits = 1)
    a <- formatC(a, 1, format = "f")
    
    d$Prozent <- ifelse(d$Prozent <= 0, 0, d$Prozent)
    d$Prozent <- round(d$Prozent, digits = 1)

    # Percent
    text(x = barp, y = d$Prozent + 5 * range[2]/100,
         label = formatC(d$Prozent, digits = 1, format = "f"), cex = cex)
    
    # Title
#     legend('top', legend = d$VersName[1], box.col = 'transparent',
#            bg = 'transparent', cex = cex * 2, bty = 'o')
    
      title(main = d$VersName[1], cex = cex * 2)
    # Legend
    if(leg == T){
      temp <- legend('topright', legend = c(' ', ' '), xjust = 1, yjust = 1,
                     text.width = strwidth(paste('Durch.Rang =  ',
                        format(mean_rang[mean_rang[, category] == i, rang], digits = 1, nsmall = 1))),
                     box.col = 'transparent', bg = 'transparent', cex = cex, bty = 'o')
      text(temp$rect$left + temp$rect$w, temp$text$y, pos = 2,
           c(paste('Top 3 (%) = ', a, sep = ''),
             paste('Durch.Rang =  ', format(mean_rang[mean_rang[, category] == i, rang], digits = 1, nsmall = 1))),
             cex = cex, col = balCol('rot'))  
    }
  }
}

# Coeficient Plot
coefPlot <- function(object, df = NULL, level = 0.95, parm = NULL,
                     labels = TRUE, xlab = "Coefficient confidence intervals", ylab = "",
                     xlim = NULL, ylim = NULL,
                     las = 1, lwd = 1, lty = c(1, 2), pch = 19, col = 1,
                     length = 0, angle = 30, code = 3, cex.axis = 1, ...)
{
  cf <- coef(object)
  se <- sqrt(diag(vcov(object)))
  if(is.null(parm)) parm <- seq_along(cf)
  if(is.numeric(parm) | is.logical(parm)) parm <- names(cf)[parm]
  if(is.character(parm)) parm <- which(names(cf) %in% parm)
  cf <- cf[parm]
  se <- se[parm]
  k <- length(cf)
  
  if(is.null(df)) {
    df <- if(identical(class(object), "lm")) df.residual(object) else 0
  }
  
  critval <- if(df > 0 & is.finite(df)) {
    qt((1 - level)/2, df = df)
  } else {
    qnorm((1 - level)/2)
  }
  ci1 <- cf + critval * se
  ci2 <- cf - critval * se
  
  lwd <- rep(lwd, length.out = 2)
  lty <- rep(lty, length.out = 2)
  pch <- rep(pch, length.out = k)
  col <- rep(col, length.out = k)
  
  if(is.null(xlim)) xlim <- range(c(0, min(ci1), max(ci2)))
  if(is.null(ylim)) ylim <- c(1 - 0.05 * k, 1.05 * k)
  
  if(isTRUE(labels)) labels <- names(cf)
  if(identical(labels, FALSE)) labels <- ""
  labels <- rep(labels, length.out = k)
  
  plot(0, 0, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
       axes = FALSE, type = "n", las = las, ...)
  arrows(ci1, 1:k, ci2, 1:k, lty = lty[1], lwd = lwd[1], col = col,
         length = length, angle = angle, code = code)
  points(cf, 1:k, pch = pch, col = col)
  abline(v = 0, lty = lty[2], lwd = lwd[2])
  axis(1)
  axis(2, at = 1:k, labels = labels, las = las, cex.axis = cex.axis)
  box()
}
