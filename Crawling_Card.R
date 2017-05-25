library(XML)
library(stringr)

# get card images
getCardImgURL <- function(name, source) {
  card_img <- NULL
  for(i in 1:length(name)) {
    img_url <- xpathSApply(source, paste0(paste0("/html/body/li[",i),"]/div[2]/a[1]/img[@src]"), xmlGetAttr, "src")
    img_url <- str_replace_all(img_url, "\\\\\\\"", "")
    card_img <- c(card_img,img_url)
  }
  return (card_img)
}

# get card benefits
getBenefit <- function(name, source) {
  card_benefit <- NULL
  card_content <- NULL
  title <- NULL
  sub <- NULL
  for(i in 1:length(name)) {
    title <- xpathSApply(source, paste0(paste0("/html/body/li[",i),"]/div[3]/dl/dd[3]/ul/li/a/span/em"), xmlValue)
    sub <- xpathSApply(source, paste0(paste0("/html/body/li[",i),"]/div[3]/dl/dd[3]/ul/li/div/div/p"), xmlValue)
    card_content <- contentMake(title, sub)
    card_benefit <- c(card_benefit,card_content)
  }
  return (card_benefit)
}

# create card benefits content
contentMake <- function(title, sub) {
  content <- NULL
  for(i in 1:length(title)) {
    content[i] <- paste(title[i], sub[i*2-1], sep=": ")
    if(sub[i*2] != "") {
      content[i] <- paste(content[i], sub[i*2], sep=", ")
    }
  }
  result <- NULL
  for(item in content) {
    if(is.null(result)) {
      result <- paste(result, item, sep="")
    } else {
      result <- paste(result, item, sep="<br>")
    }
  }
  return (result)
}

# card data (columns: (numbering), classification, card_rank, name, img, benefit)
getCardData <- function (classification, source_url) {
  card_data <- NULL
  
  # encoding, parse html
  url <- paste0(source_url)
  doc <- readLines(url, encoding = "CP949")
  options(encoding = 'UTF-8') 
  doc<-c(iconv(doc, "CP949", "UTF-8"))
  print(doc)
  doc <- paste0(doc, collapse="\n")
  doc_parse <- htmlTreeParse(doc, useInternalNodes = TRUE, trim = TRUE, encoding = "UTF-8")
  
  # card name
  card_name <- xpathSApply(doc_parse, "//div[@class='\\\"detail\\\"']/dl/dt/a", xmlValue)
  print(card_name)
  # card img
  card_img <- getCardImgURL(card_name, doc_parse)
  print(card_img)
  # card benefit
  card_benefit<- getBenefit(card_name, doc_parse)
  print(card_benefit)
  # combine to data frame
  print(length(card_name))
  card_data <- data.frame(classification=rep(classification, length.out=length(card_name)), card_rank=c(1:length(card_name)),  name=card_name, img=card_img, benefit=card_benefit)
  
  return (card_data)
}

card_data1 <- getCardData("classification1" ,"url1")
card_data2 <- getCardData("classification2", "url2")
card_data3 <- getCardData("classification3", "url3")
card_data4 <- getCardData("classification4", "url4")

names(card_data1)
card_data <- rbind(card_data1, card_data2, card_data3, card_data4)
Encoding(as.vector(card_data$name))
write.csv(card_data, "card.csv", fileEncoding = "UTF-8")
