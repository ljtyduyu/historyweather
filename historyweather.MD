The emoji-weather visualization of beijing in 2016
=================================================
Use the emoji-icon to visualize weather state of beijing in 2016!
----------------------------------------------------------------


'''r
library(RCurl)
library(XML)
library(dplyr)
library(stringr)
library(tidyr)
library(plyr)
library(rvest)
library(ggimage)
library(Cairo)
library(showtext)
library(lubridate)
'''

'''r
url<-"http://lishi.tianqi.com/beijing/index.html"
myheader <-c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36")
webpage<-getURL(url,httpheader=myheader)
mymonthlink<-getHTMLLinks(url,externalOnly=TRUE)%>%grep(".*?2016\\d{2}.html",.,value=T)
'''

a failure attempt:
-----------------

'''r
####
#page1<-getURL(mymonthlink[2],.encoding="gbk")
#rd<-iconv(page1,"gbk","utf-8")
#rdhtml<-htmlParse(rd,encoding="UTF-8")
#cesh<-readHTMLList(rdhtml,trim=TRUE,elFun=xmlValue)%>%grep("\\d{4}-\\d{2}-\\d{2}",.,value=T)
#cesh<-cesh%>%sub("([a-z])(\\()(\\\)","",.)
#cesh<-cesh1%>%str_split(',')%>%plyr::ldply(.fun=NULL)
#cesh$V1<-cesh$V1%>%sub("[a-z]\\(","",.)%>%as.Date()
#names(cesh)<-c("date","high","low","state","wind","index")
####
ÒÔÉÏ´úÂëÐ´ÁËÒ»°ëÐ´²»ÏÂÈ¥ÁË£¬ÎÒÓÐrvestÎªÉ¶ÒªÓÃRCurl£¬¿Ï¶¨×Ô¼ºÄÔ³é½îÁË£¡
'''

then i find a batter way to get the target data.
-----------------------------------------------

'''r
mynewdata<-c()
for (i in mymonthlink){
mymonthdata<-read_html(i,encoding="gbk")%>%html_nodes("div.tqtongji2>ul")%>%html_text(trim=FALSE)%>%str_trim(.,side="right")%>%.[-1]
mynewdata<-c(mynewdata,mymonthdata)
}

mynewdata1<-mynewdata
mynewdata<-mynewdata1%>%gsub("\t\t\t|\t|\r\n","",.)%>%str_split('   ')%>%plyr::ldply(.fun=NULL)%>%.[,-2]
names(mynewdata)<-c("date","high","low","state","wind","index")
mynewdata$date<-as.Date(mynewdata$date)
mynewdata$high<-as.numeric(mynewdata$high)
mynewdata$low<-as.numeric(mynewdata$low)
'''


'''r
#cleanning the dirty data.  
unique(mynewdata$state)  
happy<-c("Çç","ÕóÓê~Çç","¶àÔÆ×ªÇç","¶àÔÆ~Çç","À×ÕóÓê~Çç","Òõ~Çç","ö²~Çç","¸¡³¾~Çç")  
depressed<-c("ö²","Òõ","¶àÔÆ","Çç~¶àÔÆ","ö²~¶àÔÆ","Çç~ö²","¶àÔÆ~ö²","ÕóÓê×ª¶àÔÆ","¶àÔÆ×ªÒõ","Òõ~¶àÔÆ","¶àÔÆ~Òõ","Çç~Òõ","ÕóÓê~¶àÔÆ","Ð¡Óê~¶àÔÆ","Ð¡Óê~Òõ","ö²~Îí","Ð¡Ñ©~Òõ","Òõ~Ð¡Ñ©","Ð¡Óê~Óê¼ÐÑ©")  
angry<-c("Ð¡Óê","Óê¼ÐÑ©","Ð¡Ñ©","À×ÕóÓê","ÕóÓê","ÖÐÓê","Ð¡µ½ÖÐÓê","À×ÕóÓê~Òõ","¶àÔÆ~À×ÕóÓê","Òõ~À×ÕóÓê","ö²~À×ÕóÓê","¶àÔÆ~ÕóÓê","Çç~ÕóÓê","Òõ~Ð¡Óê","ÕóÓê~Ð¡Óê")  
Terrified<-c("ÖÐµ½´óÓê","±©Óê","À×ÕóÓê~ÖÐµ½´óÓê")      
'''


'''r
#create a new factor[categorical] varibale.
mynewdata$mode<-NULL
mynewdata$mood<-ifelse(mynewdata$state%in% happy,"happy",ifelse(mynewdata$state%in% depressed,"depressed",ifelse(mynewdata$state%in% angry,"angry","Terrified")))    
mynewdata <- within(mynewdata,{
  mood_code <- NA
  mood_code[mood=="happy"]<-"1f604"
  mood_code[mood=="depressed"]<-"1f633"
  mood_code[mood=="angry"]<-"1f62d"
  mood_code[mood=="Terrified"]<-"1f621"
})                              
''' 

'''r
#tidy the time/date varibales.
mynewdata$month<-as.numeric(as.POSIXlt(mynewdata$date)$mon+1)
mynewdata$monthf<-factor(mynewdata$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)    
mynewdata$weekday<-as.POSIXlt(mynewdata$date)$wday
mynewdata$weekdayf<-factor(mynewdata$weekday,levels=rev(0:6),labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)
mynewdata$week <- as.numeric(format(mynewdata$date,"%W"))
mynewdata<-ddply(mynewdata,.(monthf),transform,monthweek=1+week-min(week))
mynewdata$day<-day(mynewdata$date)
'''


'''r
setwd("F:/Êý¾Ý¿ÉÊÓ»¯/R/RÓïÑÔÑ§Ï°±Ê¼Ç/¿ÉÊÓ»¯/ggplot2/ÉÌÎñÍ¼±í")
write.table(mynewdata,"historyweather.csv",sep=",",row.names=FALSE)
mynewdata<-read.csv("historyweather.csv",stringsAsFactors = FALSE,check.names = FALSE)  
'''

'''r
#first theme: 
mytheme<-theme(
         rect=element_blank(),
         axis.ticks=element_blank(),
         text=element_text(face="plain",lineheight=0.9,hjust=0.5,vjust=0.5,size=15),
         title=element_text(face="plain",lineheight=0.9,hjust=0,vjust=0.5,size=30),
         axis.title=element_blank(), 
         strip.text=element_text(size = rel(0.8)), 
         plot.margin = unit(c(5,2,5,2),"lines")
         )
'''

the first photo:
--------------
'''r
CairoPNG("emoji1.png",1000,870)
showtext.begin()
ggplot(mynewdata,aes(weekdayf,monthweek,fill=high))+
geom_tile(colour='white')+
scale_fill_gradient(low=NA, high=NA,guide=FALSE)+
ggtitle("The emoji-weather visualization of beijing in 2016")+
scale_y_reverse(breaks=seq(from=6,to=0,by=-1))+
ggimage::geom_emoji(aes(image=mood_code),size=.1)+
facet_wrap(~monthf ,nrow=3)+
mytheme
showtext.end()
dev.off()
'''

second theme:
-------------
'''r
mytheme2<-theme(
         rect=element_blank(),
         axis.ticks=element_blank(),
         text=element_text(face="plain",lineheight=0.9,hjust=0.5,vjust=0.5,size=15),
         title=element_text(face="plain",lineheight=0.9,hjust=0,vjust=0.5,size=30),
         axis.title=element_blank(), 
         strip.text=element_text(size = rel(0.8)), 
         plot.margin = unit(c(1,1,1,1),"lines")
         )
'''

second phone:
------------

'''r
CairoPNG("emoji2.png",1200,1200)
showtext.begin()
ggplot(mynewdata,aes(x=factor(day),y=monthf,fill=high))+
geom_tile(colour='white')+
expand_limits(y =c(-12,12))+
scale_x_discrete(position=c("bottom"))+
coord_polar(theta="x")+
scale_fill_gradient(low=NA, high=NA,guide=FALSE)+
ggimage::geom_emoji(aes(image=mood_code),size=.015)+
geom_image(aes(x=0,y=-12),image ="weather.png", size =.15)+
ggtitle("The emoji-weather visualization of beijing in 2016")+
mytheme2
showtext.end()
dev.off()
'''

