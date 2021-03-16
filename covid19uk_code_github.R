# R.Version() 
# 4.0.3
library(openxlsx)
data= read.xlsx( "/Users/leuzha/Desktop/COVID VACCINE/data_backlog.xlsx", sheet=1)
data$Date  = as.Date(data$date, origin = "1899-12-30") 
data$neg_percent=data$neg_p*100
data$pos_percent=data$pos_p*100

`%notin%` <- Negate(`%in%`)

library(ggplot2)
ylim.prim <- c(0, 80000) # will change the scale
ylim.sec <- c(0,100) # will change the position and size of negative sentiment (%)
b<- diff(ylim.prim)/diff(ylim.sec)
a<- b*(ylim.prim[1]-ylim.sec[1])
data$country="UK"
data$country2="UK"

###    PLOT  ################
opar <- par(lwd = 0.01)

p =ggplot(aes(x=Date), data=data) +
  
  geom_bar (aes(y=total_volume, fill=country2), size=0.15, color="grey50",stat = "identity",position="dodge")+
  geom_line(aes(y=a+b*neg_percent,color=country),size=0.4) +
  geom_point (aes(y=a+b*neg_percent,color=country),size=1.4) +
  
  geom_vline (xintercept =as.Date(c("2020-11-09", "2020-11-16", "2020-11-23", "2020-12-02", "2020-12-08", "2020-12-30")), linetype="dotted", size=0.5, color="grey50") + 
  geom_text (aes(x=as.Date("2020-11-09")), y=81000, label="Phase 3 P/BT", angle=90, vjust=-1, hjust= "top",size=3, color="grey30") +
  geom_text (aes(x=as.Date("2020-11-16")), y=81000, label="Phase 3 Moderna", angle=90, vjust=-1, hjust= "top", size=3, color="grey30") +
  geom_text (aes(x=as.Date("2020-11-23")), y=81000, label="Phase 3 Ox/AZ", angle=90, vjust=-1, hjust= "top", size=3, color="grey30") +
  geom_text (aes(x=as.Date("2020-12-02")), y=81000, label="UK approval P/BT", angle=90, vjust=-1, hjust= "top", size=3, color="grey30") +
  geom_text (aes(x=as.Date("2020-12-08")), y=81000, label="UK starts vaccine campaign", angle=90, vjust=-1, hjust= "top", size=3, color="grey30") +
  geom_text (aes(x=as.Date("2020-12-30")), y=81000, label="UK approval Ox/AZ", angle=90, vjust=-1, hjust= "top", size=3, color="grey30") +
  
  
  theme(panel.border = element_rect(colour = "black" ,fill=NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 75, hjust = 0, vjust=0, size=8),
        legend.position="bottom") +
  
  labs(title=" ", x = "", y ="")+ 
  
  scale_x_date(breaks = seq(as.Date('2020-11-02'), as.Date('2021-01-24'), by="2 days"), date_labels =  "%b %d",
               limits = c(as.Date('2020-10-31'), as.Date('2021-01-25')), expand = expansion(add = c(2,2))) +
  
  scale_y_continuous("Daily twitter volume", expand = expansion(mult = c(0.03,0)),limits = c(0,84000), breaks = c(0,20000,40000,60000,80000), 
                     sec.axis = sec_axis(~ (. - a)/b, breaks = c(0,20,40,60,80,100), name = "Negative sentiment (%)")) +
  
  scale_color_manual(name = "Negative sentiment (%)", values = rgb(68,112,110, maxColorValue = 255) ,labels = " ",) + #  47,108,66
  scale_fill_manual(name = "Daily twitter volume", values=rgb(212,215,192, maxColorValue = 255)  ,labels = " ") + #rgb(200,195,255, maxColorValue = 255)
  
  
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

p
pt =p+theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) #top, right, bottom, left #  1 pt = 0.35mm.
pt


### engagement by weeks ###########
dat= read.xlsx( "/Users/leuzha/Desktop/COVID VACCINE/data_backlog.xlsx", sheet=4)
#d[13,] = c("nov2-jan24","Total", colSums(d[,3:12]))
#d[,3:12]= lapply((d[,3:12]),as.numeric )
d[13,"poseng_t"] #6.446066
d[13,"negeng_t"] #4.709909

d[13,"poseng_c"] #0.6029395
d[13,"negeng_c"] #0.4751434

d[13,"poseng_s"] #0.9467254
d[13,"negeng_s"] #0.9170763

d[13,"poseng_l"] #4.896401
d[13,"negeng_l"] #3.31769


dat$poseng_t= (dat$pos_eng_t/ dat$pos_tweets)
dat$poseng_c= (dat$pos_eng_c/ dat$pos_tweets)
dat$poseng_s= (dat$pos_eng_s/ dat$pos_tweets)
dat$poseng_l= (dat$pos_eng_l/ dat$pos_tweets)


dat$negeng_t= (dat$neg_eng_t/ dat$neg_tweets)
dat$negeng_c= (dat$neg_eng_c/ dat$neg_tweets)
dat$negeng_s= (dat$neg_eng_s/ dat$neg_tweets)
dat$negeng_l= (dat$neg_eng_l/ dat$neg_tweets)



library(tidyr)
library(dplyr)

dat$week=as.factor(dat$week)

datalong_1 <- gather(dat, pos_engagetype, pos_measurement, poseng_t:poseng_l, factor_key=TRUE)
datalong_2 <- gather(dat, neg_engagetype, neg_measurement, negeng_t:negeng_l, factor_key=TRUE)

datalong_pos <- datalong_1 %>%
  mutate(pos_engagetype = recode(pos_engagetype, "poseng_t"= "total",  "poseng_c"="comments", "poseng_s"="shares", "poseng_l"="likes"))

datalong_neg <- datalong_2 %>%
  mutate(neg_engagetype = recode(neg_engagetype, "negeng_t"= "total",  "negeng_c"="comments", "negeng_s"="shares", "negeng_l"="likes"))

#datalong= cbind(datalong_1[,c("week","v","pos_engagetype","pos_measurement")], datalong_2[,c("neg_engagetype","neg_measurement")])
#d <- datalong

tmp1<- datalong_pos%>%
  rename (engagetype=pos_engagetype, measurement=pos_measurement)%>% 
  mutate(sentiment ="pos")  %>% 
  select(week, v,engagetype,measurement,sentiment)

tmp2<- datalong_neg%>%
  rename (engagetype=neg_engagetype, measurement=neg_measurement)%>% 
  mutate(sentiment ="neg") %>% 
  select(week, v,engagetype,measurement,sentiment)

data_long_long =rbind(tmp1,tmp2)
library(ggplot2)


##### data management #######
data_long_long =rbind(tmp1,tmp2)

data_long_long$week <- factor(data_long_long$week, levels=c('nov2-nov8','nov9-nov15','nov16-nov22',"nov23-nov29","nov30-dec6","dec7-dec13","dec14-dec20","dec21-dec27","dec28-jan3","jan4-jan10","jan11-jan17","jan18-jan24"),
                              labels=c("Ref","Week 1","Week 2 ","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10","Week 11"))

data_long_long = data_long_long %>%
  mutate(sentiment = recode(sentiment, "pos"= "Pos",  "neg"="Neg"))

data_long_long$sentiment <- factor(data_long_long$sentiment, levels=c("Pos","Neg"))



library(tidyverse)
library(ggsci)
v=
  data_long_long %>%  
  filter(engagetype!="total")  %>%  
  unite("vweek", v:week, na.rm = TRUE, remove = FALSE) %>%  
  ggplot(aes(x=sentiment, y=measurement, fill=engagetype)) + 
  geom_col(width=0.8,size=0.15, color="grey40",) + 
  facet_grid(.~week) + 
  theme(legend.position="bottom", 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.spacing = unit(1.2, "lines"))+
  labs (x="Sentiment", y="No. of engagement messages per tweet")+
  scale_fill_brewer(palette = "BrBG",name = "", labels = c("Comments", "Shares", "Likes"))

#scale_fill_manual(name = "Engagement type", labels = c("Comments", "Shares", "Likes"),values = c("#374E5599","#79AF9799","#80796B99") )#c("#d8b365", "#7E6148B2", "#5ab4ac"))#91d9c2B2   4dbbd5B2
vt=v +theme(plot.margin = unit(c(1.25, 0.5, 0.5, 0.5), "cm")) #top, right, bottom, left #  1 pt = 0.35mm.

vt
