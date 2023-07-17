head.tail <-
function(data){
    print(head(data))
    print(tail(data))
}
load.data <-
function(rawdata, ticker) {
  data.raw <-  read.csv(rawdata, header = TRUE)
  Date <-  as.Date(data.raw$Date, format = "%Y−%m−%d")
  data.raw <-  cbind(Date, data.raw[, -1])
  data.raw <-  data.raw[order(data.raw$Date),]
  data.raw <-  xts(data.raw[, 2:7],order.by = data.raw$Date)
  A <-  paste(ticker, ".Open", sep = "")
  B <-  paste(ticker, ".High", sep = "")
  C <-  paste(ticker, ".Low", sep = "")
  D <-  paste(ticker, ".Close", sep = "")
  E <-  paste(ticker, ".Adjusted", sep="")
  F <-  paste(ticker, ".Volume", sep="")
  names(data.raw) <-  paste(c(A, B, C, D, E, F))
  data.raw <-  cbind(data.raw[, 1:4], data.raw[, 6], data.raw[, 5]) 
  return(data.raw)
}

load.fred <- function(data = "DGS3MO.csv", symbol = "DGS3MO") {
  temp <- read.csv(data, header = TRUE)
  temp <- subset(temp, temp[, 2]!=".")
  date <- ymd(temp$DATE)
  value <- as.numeric(as.character(temp[, 2]))
  temp2 <- xts(value, order.by = date)
  names(temp2) <- symbol
  temp2
}
