The emoji-weather visualization of beijing in 2016
=================================================
Use the emoji-icon to visualize weather state of beijing in 2016!
----------------------------------------------------------------


```r
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
```

```r
url<-"http://lishi.tianqi.com/beijing/index.html"
myheader <-c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36")
webpage<-getURL(url,httpheader=myheader)
mymonthlink<-getHTMLLinks(url,externalOnly=TRUE)%>%grep(".*?2016\\d{2}.html",.,value=T)
```

a failure attempt:
-----------------


```r
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
#以上代码写了一半写不下去了，我有rvest为啥要用RCurl，肯定自己脑抽筋了！
```

then i find a better way to get the target data.
-----------------------------------------------

```r
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
```


```r
#cleanning the dirty data.  
unique(mynewdata$state)  
happy<-c("晴","阵雨~晴","多云转晴","多云~晴","雷阵雨~晴","阴~晴","霾~晴","浮尘~晴")  
depressed<-c("霾","阴","多云","晴~多云","霾~多云","晴~霾","多云~霾","阵雨转多云","多云转阴","阴~多云","多云~阴","晴~阴","阵雨~多云","小雨~多云","小雨~阴","霾~雾","小雪~阴","阴~小雪","小雨~雨夹雪")  
angry<-c("小雨","雨夹雪","小雪","雷阵雨","阵雨","中雨","小到中雨","雷阵雨~阴","多云~雷阵雨","阴~雷阵雨","霾~雷阵雨","多云~阵雨","晴~阵雨","阴~小雨","阵雨~小雨")  
Terrified<-c("中到大雨","暴雨","雷阵雨~中到大雨")      
```


```r
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
```

```r
#tidy the time/date varibales.
mynewdata$month<-as.numeric(as.POSIXlt(mynewdata$date)$mon+1)
mynewdata$monthf<-factor(mynewdata$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)    
mynewdata$weekday<-as.POSIXlt(mynewdata$date)$wday
mynewdata$weekdayf<-factor(mynewdata$weekday,levels=rev(0:6),labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)
mynewdata$week <- as.numeric(format(mynewdata$date,"%W"))
mynewdata<-ddply(mynewdata,.(monthf),transform,monthweek=1+week-min(week))
mynewdata$day<-day(mynewdata$date)
```


```r
setwd("F:/数据可视化/R/R语言学习笔记/可视化/ggplot2/商务图表")
write.table(mynewdata,"historyweather.csv",sep=",",row.names=FALSE)
mynewdata<-read.csv("historyweather.csv",stringsAsFactors = FALSE,check.names = FALSE)  
```

```r
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
```

the first photo:
--------------
```r
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
```

<div  align="center">    
<img src="https://github.com/ljtyduyu/historyweather/blob/master/Image/emoji1.png" width = "7500" height = "652.5" alt="emoji1" align=center />
</div>


second theme:
-------------
```r
mytheme2<-theme(
         rect=element_blank(),
         axis.ticks=element_blank(),
         text=element_text(face="plain",lineheight=0.9,hjust=0.5,vjust=0.5,size=15),
         title=element_text(face="plain",lineheight=0.9,hjust=0,vjust=0.5,size=30),
         axis.title=element_blank(), 
         strip.text=element_text(size = rel(0.8)), 
         plot.margin = unit(c(1,1,1,1),"lines")
         )
```

second photo:
------------

```r
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
```

<div  align="center">    
<img src="https://github.com/ljtyduyu/historyweather/blob/master/Image/emoji2.png" width = "650" height = "650" alt="emoji2" align=center />
</div>


and this one is a small-background in the second photo  which i search on the web.<br>

<div  align="center">    
<img src="https://github.com/ljtyduyu/historyweather/blob/master/Image/weather.png" width = "500" height = "500" alt="wheather" align=center />
</div>


------------------------------------------------------------------------------------------------

联系方式：
----------------------------------------------------
wechat：ljty1991  <br>
Mail:578708965@qq.com <br>
个人公众号：数据小魔方（datamofang） <br>
团队公众号：EasyCharts <br>
qq交流群：[魔方学院]553270834

个人简介：
-------------------------------------------------
**杜雨** <br>
财经专业研究僧； <br>
伪数据可视化达人； <br>
文科背景的编程小白； <br>
喜欢研究商务图表与地理信息数据可视化，爱倒腾PowerBI、SAP DashBoard、Tableau、R ggplot2、Think-cell chart等诸如此类的数据可视化软件，创建并运营微信公众号“数据小魔方”。 <br>
Mail:578708965@qq.com <br>
![resume](https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/resume.png)

-------------------------------------------

备注信息：
----------------------------------------------------
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="知识共享许可协议" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />本作品采用<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">知识共享署名-非商业性使用 4.0 国际许可协议</a>进行许可。
