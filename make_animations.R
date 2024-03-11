require(transformr)
require(gganimate)
require(ggplot2)
rm(list=ls())
library(ggplot2)
library(forecast)
library(keras)
library(tensorflow)
library(kernlab)
library(ggplotify)
source("~/Downloads/Data_Viz/utils.R")

keep = c("BSX","PFE","JNJ","DE","DAL","EFX","ACN","NVDA","MSFT",
         "NDAQ","MS","PNC","WMB","HAL","XOM","MCD","NKE","SBUX","WMT","PEP","SYY",
         "VZ","NFLX","DIS","PNW","XEL","PEG","PSA","PLD","REG","PPG","VMC","IP")
type = c("health care","industrials","information tech","financials","energy",
         "consumer discretionary","consumer staples","communication services",
         "utilities","real estate","materials")
type = rep(type,each=3)

data = read.csv("~/Dropbox (University of Oregon)/stocks/all_stocks_5yr.csv")
data = data[data$Name %in% keep,]
length(unique(data$Name))

### assign stock type to dataframe
data$type = NA
for(i in 1:nrow(data)){
  idx = which(data$Name[i] == keep)
  data$type[i] = type[idx]
}

### market value for each stock
data$x = rep(1:1259,length(keep))
data_agg = aggregate(close ~ x + type,data,mean)
mycolors = rep("gray",nrow(data_agg))
mycolors[data_agg$type=="information tech"] = "orange"
mycolors[data_agg$type=="real estate"] = "blue"
names(mycolors) = data_agg$type
p <- ggplot(data_agg, aes(x = x, y = close, color = type)) +
  geom_point(size = 3,alpha = 0.5) +
  geom_line(size = 2) + 
  labs(x = "time (2012 - 2018)",y = "stock market value (US Dollars)",
       title="Information Technology Stocks Skyrocket Recently \n while Real Estate Drops")+
  transition_reveal(x) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=25),axis.ticks.x = element_blank(),
        legend.position = "None",axis.text.x = element_blank(),
        plot.title = element_text(vjust=-10,hjust=.4,size=30))+
  annotate("text",x = data_agg$x[data_agg$type=="information tech"],
           y = data_agg$close[data_agg$type=="information tech"]+20,
           label = "information tech",col="orange",size=8,fontface="bold")+
  annotate("text",x = data_agg$x[data_agg$type=="real estate"],
           y = data_agg$close[data_agg$type=="real estate"]+20,
           label = "real estate",col="blue",size=8,fontface="bold")+
  scale_color_manual(values = mycolors)

a = animate(p, 
        duration = 15, 
        fps = 20, 
        width = 800, 
        height = 550,
        renderer = gifski_renderer())
a

anim_save("info_ts.gif")

all500 = read.csv("all_stocks_5yr.csv")
all500 = all500[all500$Name %in% c(it_stocks,re_stocks,keep),]
all500$type = NA
all500$type[which(all500$Name %in% it_stocks)] = "information tech"
all500$type[which(all500$Name %in% re_stocks)] = "real estate"
all500$type[which(!all500$Name %in% c(it_stocks,re_stocks) & all500$Name %in% keep)] = "other"
all500$time = NA
all500$time[1] = 1
for(i in 2:nrow(all500)){
  if(all500$Name[i] != all500$Name[i-1]){
    all500$time[i] = 1
  }else{
    all500$time[i] = all500$time[i-1] + 1
  }
}
require(ggtext)
p2=ggplot(all500,aes(x = volume, y = close,col = type,group = Name, frame = date))+
  geom_point(alpha = 0.75,size = 10)+
  transition_reveal(time)+
  view_follow()+
  labs(y = "Market Value",x = "Trading Volume",
       title = "**<span style='color:orange'> Information Tech</span> Stocks Are as Healthy<br>as <span style='color:blue'>Real Estate</span> Stocks**",
       subtitle = "Week {frame} of {nframes} weeks",
       caption = "<span style='color:gray'> OTHER stock types in gray</span>")+
  scale_color_manual(values = c("orange","blue","gray"),name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=30),axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),axis.text.y = element_blank(),
        plot.title = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "None")
p2
a2 = animate(p2, 
             duration = 15, 
             fps = 20, 
             width = 800, 
             height = 550,
             renderer = gifski_renderer())
a2
anim_save("trades.gif")

