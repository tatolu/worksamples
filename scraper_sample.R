#SCRAPER SAMPLE
#Last edited March 30, 2015
#by Tato Lu
#contact @ tatolu818@gmail.com
  #This is sample code of a web scraper to serve as a work sample.
  #The scraper goes through a vector of "series" and compiles data on each series
  #into one data frame. As of March 26, 2015, the end result was a .csv file
  #with 2006 observations of 18 variables. 
  #I put together this program so that I could collect data for 
  #personal use, i.e. out of curiosty, from a certain website. 
  #At the time, the website in question had no API.

  #To respect the website's Terms of Services and their privacy, 
  #I've omitted chunks of the code that might be used to identify the website.
  #To indicate areas of missing code, I used the comment "#[OMITTED]".

library(stringr)

#parent site url
master.url<-#[OMITTED]

#FUNCTION: scheduleTime
  #INPUT: vector of dates
  #OUTPUT: vector of median wait time, mean wait time, and std deviation of wait time
    #("wait time" defined as time between each date)
scheduleTime<-function(dates){
  if(length(dates)==1){
      results<-c(0,0,0)
    }else{
      wait.time<-rep(0,length(dates)-1)
      for(i in 2:length(dates)){
        wait.time[i-1]<-dates[i]-dates[i-1]
      }
      std.dev<-sd(wait.time)
      if (is.na(std.dev)==T) std.dev<-0
      results<-c(median(wait.time)
                 ,mean(wait.time)
                 ,std.dev)
    }
  return(results)
}

#FUNCTION: trimRaw
  #INPUT: a line of string from the site's source code with HTML tags and blanks
  #OUTPUT: a line of string without HTML tags or any blanks at the beginning of the string
trimRaw<-function(raw.string) return(gsub("([[:blank:]]*)?<[^>]*>","",raw.string))

#FUNCTION: getData
  #INPUT: target URL
  #OUTPUT: a list of the data of interest obtained from the URL
getData<-function(test.url){
  #read in source code of series page
  raw.data<-readLines(test.url,warn="F")
  
  #get dates
    #target.key is the keyword we use to identify the line containing the dates
  target.key<-#[OMITTED]
  eplist<-raw.data[grep(paste("^([[:blank:]]*"
                              ,target.key
                              ,")"
                              ,sep="")
                        ,raw.data)]
  all.dates<-unlist(str_extract_all(eplist
                                    ,"[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"))
  all.dates<-as.Date(all.dates)
  
  #get the schedule
  schedule<-scheduleTime(all.dates)
  
  #check to see if the series is inactive
  #   NOTE: A series is considered inactive if their last update was more than t days ago,
  #   where t = average wait time + 2 * standard deviations of wait time.
  #   Take the series [OMITTED] as an example. On average, [OMITTED] published 
  #   an episode about every [OMITTED] days, with a standard deviation of about [OMITTED] days.
  #   As of [OMITTED], [OMITTED] last updated on [OMITTED], or [OMITTED] days ago.
  #   Since [OMITTED], we'd say [OMITTED] is inactive, so
  #   we would exclude it from the dataset. 
  #   I exclude inactive series from the dataset because they do not accurately represent
  #   the object of interest: active series on [OMITTED]. An inactive series can still 
  #   collect views over time but are likely to have far fewer subscribers than active series.
  #   Excluding inactive series should also speed up the data collection process.
  if(Sys.Date()-all.dates[length(all.dates)]>schedule[2]+2*schedule[3]) return(NA)
  
  #get the id
    #assign new keyword to target.key
  target.key<-#[OMITTED]
  series.id<-as.integer(gsub("[^0-9]"
                             ,""
                             ,raw.data[grep(paste(target.key
                                                  ," :"
                                                  ,sep="")
                                            ,raw.data)]))
  
  #[OMITTED: 2 variables of interest]
  
  
  #gets description body
  target.key<-#[OMITTED]
  descr.start<-grep(paste("id=\""
                          ,target.key
                          ,"\""
                          ,sep="")
                    ,raw.data)
  i<-1
  while(length(grep("</span>"
                    ,raw.data[descr.start+i]))!=1) i<-i+1
  descr.body<-gsub("^[[:blank:]]*|<br/?>",""
                   ,paste(raw.data[(descr.start+1):(descr.start+i-1)]
                          ,collapse=" "))
  
  #gets alt site URL out of description body
  sites<-unlist(str_extract_all(descr.body
                                ,"(https?)://(www\\.)?[[:alnum:][:punct:]]*/?([[:alnum:]]*)?/?"))
  if(length(sites)==0)sites<-NA
  
  #gets description out of description body
  descr<-gsub("((https?)://(www\\.)?[[:alnum:][:punct:]]*/?([[:alnum:]]*)?/?)|[[:space:]_]{3,}"
              ,""
              ,descr.body)
  if(length(grep("[^[:blank:]]",descr))==0)descr<-NA
  
  #get the average likes
  target.key<-#[OMITTED]
  likes<-as.integer(sapply(unlist(str_extract_all(eplist
                                                  ,paste(target.key
                                                               ,"\":[0-9]*")
                                                               ,sep=""))
                           ,function(x){gsub("[^0-9]"
                                            ,""
                                            ,x)}
                           ,USE.NAMES=F))
  avg.likes<-mean(likes)
  
  #[OMITTED: 8 more variables of interest]
  
  results<-list(series.id
                ,#[OMITTED]
                ,#[OMITTED]
                ,#[OMITTED]
                ,as.character(all.dates[length(all.dates)])
                ,schedule[1]
                ,schedule[2]
                ,schedule[3]
                ,avg.likes
                ,#[OMITTED]
                ,#[OMITTED]
                ,#[OMITTED]
                ,#[OMITTED]
                ,#[OMITTED]
                ,#[OMITTED]
                ,as.character(paste(sites,collapse=","))
                ,descr)

  return(results)
}

#FUNCTION: extractSeriesURL
  #INPUT: a chunk of source code
  #OUTPUT: a vector of strings without HTML tags
extractSeriesURL<-function(raw){
  raw<-raw[grep("<a class=\"title\""
                ,raw)]
  raw<-str_extract(raw
                   ,"/series/([a-zA-Z0-9\\_ -])*")
  return(raw)
}


#get a list of all series
raw.series<-readLines(paste(master.url
                            ,#[OMITTED]
                            ,sep=""),warn="F")
num.pages<-as.integer(gsub("^[^>]*>|</a>"
                           ,""
                           ,raw.series[grep("paging-btn dots"
                                            ,raw.series)+2]))
raw.series<-extractSeriesURL(raw.series)
n.entries<-num.pages*length(raw.series)
series.list<-rep(NA,n.entries)
index<-1
series.list[index:length(raw.series)]<-raw.series
for(i in 2:num.pages){
    index<-index+length(raw.series)
    raw.series<-readLines(paste(master.url
                                ,#[OMITTED]
                                ,i
                                ,#[OMITTED]
                                ,sep=""),warn="F")
  raw.series<-extractSeriesURL(raw.series)
  series.list[index:(index+length(raw.series)-1)]<-raw.series
  print(paste("Page"
              ,i))
}
series.list<-series.list[is.na(series.list)==F]

#preallocate an empty data frame
empty<-rep(NA,length(series.list))
batch2<-data.frame("series.id"=empty
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,"last.update"=empty
                   ,"med.update"=empty
                   ,"avg.update"=empty
                   ,"sd.update"=empty
                   ,"avg.likes"=empty
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,#[OMITTED]
                   ,"sites"=empty
                   ,"descr"=empty)

#go through each series in the series list and add the relevant data to the data frame
for(i in 2359:length(series.list)){
  batch2[i,]<-getData(paste(master.url
                            ,series.list[i]
                            ,sep=""))
  cat(i)
}
batch3<-batch2[is.na(batch2$series.id)==F,]

#save data as .csv file
write.csv(batch3,file=paste(#[OMITTED]
  ,Sys.Date()
  ,".csv"
  ,sep=""))