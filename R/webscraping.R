# Internet Functions
#-------------------

# Get IP information
getIP <- function(src = 'offline'){
  if(src == 'offline'){
    x <- system("ipconfig", wait = F, intern = T)
    x <- x[grep("IPv4", x)]
    result <- gsub(".*? ([[:digit:]])", "\\1", x)
  }
  if(src == 'online'){
    require(XML)
    
    html <- htmlParse('http://ipaddress.com/')
    result <- data.frame(readHTMLTable(html, header = F))
    names(result) <- c('Object', 'Attribute')
  }
  result
}

# Open a new VPN instance
openVPN <- function(nr, protocol = 'UDP'){
  list <- list.files('C:/Program Files/OpenVPN/config')
  expr <- paste(protocol, '.ovpn', sep = '')
  expr <- paste('[[:print:]]{1, }.', expr, sep = '')
  list <- regmatches(list, regexpr(expr, list))
  path <- paste('openvpn-gui.exe --connect', list[nr], sep = ' ')
  system(path, wait = F, intern = F)  
}

# Web Scraping Functions
#-----------------------

# Post Captcha Code on www.deathbycaptcha.eu
postCaptcha <- function(url, user, pass, path){
  require(RCurl)
  
  result <- postForm(url, username = user, password = pass,
                     captchafile = fileUpload(filename = path, contentType = 'image/jpg'))
  result
}

# Get Captcha Code Result from www.deathbycaptcha.eu
getCaptcha <- function(url, id){
  require(RCurl)
  
  url <- paste(url, '/', sep = '')
  url <- paste(url, id, sep = '')
  
  result <- getURL(url)
  result
}

# Report Error on www.deathbycaptcha.eu
reportCaptcha <- function(url, user, pass){
  require(RCurl)
  
  url <- paste(url, '/', sep = '')
  url <- paste(url, id, sep = '')
  url <- paste(url, '/report', sep = '')
  
  result <- postForm(url, username = user, password = pass)
  result
}

#' @title Cut down text to required id
#' @export
#' 
#' @description \code{cutTxt} Cuts down the text to a required id
#' 
#' @param input Data
#' @param identifier
#' @param cut
#' @param regex
#' 

cutTxt <- function(input, identifier, cut = F, regex = '[[:alnum:]]{1, }'){
  
  expr <- paste(identifier, regex, sep = '')
  output <- regmatches(input, regexpr(expr, input))
  
  if(cut == T){
    output <- substr(x = output, start = nchar(identifier) + 1, stop = nchar(output))
  }
  
  output
}