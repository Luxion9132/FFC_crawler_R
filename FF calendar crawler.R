library(XML)
library(RCurl)

url <-'http://www.forexfactory.com/calendar.php'
FFC <-crawlCalendar(url,5)

#step by step
# df.default <- getCalendarTable(url)
# url.last1 <- getLastPageUrl(url)
# df.last1 <- getCalendarTable(url.last1)
# url.last2 <- getLastPageUrl(url.last1)
# temp<- rbind(df.last1,df.default)


crawlCalendar <- function(url,pages){ 
  if(pages>10){
    #防呆, 修改最大頁數請自由心證
    stop("too many pages!")
  }
  #宣告empty dataframe,每頁資料貼至此df, 作為最終回傳資料
  df.result <- data.frame(Date=character(),
                          Time=character(), 
                          Currency=character(),
                          Event=character(),
                          Actual=character(),
                          Forecast=character(),
                          stringsAsFactors=F) 
  for(i in 1:pages){
    df.temp <- getCalendarTable(url)
    message(paste("擷取: ",url,"完成, ",nrow(df.temp),"筆record",sep=""))
    url <- getLastPageUrl(url)    
    df.result<- rbind(df.temp,df.result)
  }
  return(df.result)
}


getCalendarTable <- function(url){
  if(url.exists(url)){
    doc <-  htmlTreeParse(url, useInternalNodes = TRUE)
    #獲取html table
    calendar.xml <- getNodeSet(doc, "//div[@class='flexBox calendar']/table")
    if(length(calendar.xml)==1){
      calendar.df <- readHTMLTable(calendar.xml[[1]])#符合的只會有一個list 直接拆開readHTMLTable
      #table資料處理
      calendar.df <- calendar.df[-1:-2,]#刪除前兩筆record, 分別為thead與borderfix(內容值為NA)
      names(calendar.df)[2]<-'Time' #修改名稱預設為現時間的column 2, 以進行rbind
      names(calendar.df)[5]<-'Event' #補上沒有名稱的event欄位
      calendar.df<-calendar.df[,c('Date','Time','Currency','Event','Actual','Forecast')]#只留感興趣的欄位
      return(calendar.df)
    }else{
      stop('符合條件table不為1') #可能為0(找不到符合條件的table),或大於1(或許FF網站架構有變)
    }
  }else{
    stop(paste("找不到網址:",url))
  }
}

getLastPageUrl <- function(url){
  doc <-  htmlTreeParse(url, useInternalNodes = TRUE)
  #找出上一頁的網址, 在a的href屬性中
  lastpage.url <- getNodeSet(doc, "//div[@class='head']/ul/li[@class='left pagination']/a")
  #xmlGetAttr用於找出node的屬性值
  temp <- xmlGetAttr(lastpage.url[[1]],"href")
  lastpage.url <- paste('http://www.forexfactory.com/',temp,sep="")
  return(lastpage.url)
}