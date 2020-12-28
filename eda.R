library(ggplot2)
library(dplyr)
library(lubridate)
library(here)
#setwd('/Users/padhokshaja/Downloads/trivago')
here::here()

test <- read.csv('marketplace_anon_2019.csv',stringsAsFactors = FALSE)

test$date <- lubridate::ymd(test$date)
### Conversion Rate (Slide 7)
test %>% 
  mutate(total_clicks = clicks_A+clicks_B+clicks_C,
         bookings_total = bookings_A+bookings_B+bookings_C) %>%
  mutate(month_ = month(date,label = TRUE),year_=year(date)) %>%
  mutate(date_modify = stringr::str_c(month_,'-',year_,sep=' ')) %>%
  group_by(date_modify,month_) %>%
  summarise(bookings_total = sum(bookings_total,na.rm = TRUE),
            total_clicks = sum(total_clicks,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(month_ = factor(month_,levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))) %>%
  mutate(conversion_rate = bookings_total/total_clicks) %>%
  arrange((month_))
### Total Number of booking by market Type (slide 8)

test %>% 
  mutate(total_clicks = clicks_A+clicks_B+clicks_C,
         bookings_total = bookings_A+bookings_B+bookings_C) %>%
  mutate(month_ = month(date,label = F),year_=year(date)) %>%
  mutate(quarter_= lubridate::quarter(month_)) %>%
  mutate(date_modify = stringr::str_c(month_,'-',year_,sep=' ')) %>%
  group_by(quarter_,ttt_group) %>%
  summarise(bookings_total = sum(bookings_total,na.rm = TRUE),
            total_clicks = sum(total_clicks,na.rm=TRUE)) %>%
  ungroup() %>%
  select(quarter_,ttt_group,bookings_total) %>%
  tidyr::spread(ttt_group,bookings_total) %>%
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% View()


#### Average Consumer Value By Market Segment (Slide 8)

test %>%
  mutate(total_revenue = booking_rev_A+booking_rev_B+booking_rev_C,
         bookings_total = bookings_A+bookings_B+bookings_C)  %>%
  group_by(ttt_group) %>%
  summarise(total_revenue = sum(as.numeric(total_revenue)),bookings_total = sum(bookings_total)) %>%
  ungroup() %>%
  mutate(acv = total_revenue/bookings_total)

### Conversion Rate by Market Type


test %>% 
  mutate(total_clicks = clicks_A+clicks_B+clicks_C,
         bookings_total = bookings_A+bookings_B+bookings_C) %>%
  mutate(month_ = month(date,label = TRUE),year_=year(date)) %>%
  mutate(date_modify = stringr::str_c(month_,'-',year_,sep=' ')) %>%
  group_by(date_modify,month_,ttt_group) %>%
  summarise(bookings_total = sum(bookings_total,na.rm = TRUE),
            total_clicks = sum(total_clicks,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(month_ = factor(month_,levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))) %>%
  mutate(rates = bookings_total/total_clicks) %>%
  select(month_,ttt_group,rates) %>%
  tidyr::spread(ttt_group,rates) %>% View()


### Total Number of Clicks By Market Type

test %>%
  mutate(total_clicks = clicks_A+clicks_B+clicks_C,
         bookings_total = bookings_A+bookings_B+bookings_C) %>%
  mutate(month_ = month(date,label = TRUE),year_=year(date)) %>%
  group_by(ttt_group,month_) %>%
  summarise(n=sum(total_clicks)) %>%
  ungroup() %>%
  tidyr::spread(ttt_group,n) %>% View()

### Q3 deep dive

test %>% 
  mutate(total_clicks = clicks_A+clicks_B+clicks_C,
         bookings_total = bookings_A+bookings_B+bookings_C) %>%
  filter(ttt_group=='short') %>%
  mutate(month_ = month(date,label = TRUE),year_=year(date),weeknum= week(date)) %>%
  filter(month_ %in% c('Jun','Jul','Aug','Sep','Oct')) %>%
  group_by(weeknum) %>%
  summarise(n=sum(bookings_total)) %>%
  ggplot(aes(x=weeknum,y=n))+geom_bar(stat='identity')

### Advertiser
#### How does CPA vary with time by advertiser type?
test %>% 
 select(date,bookings_A,bookings_B,bookings_C,cost_A,cost_B,cost_C,ttt_group) %>%
  mutate(CPA_a = cost_A/bookings_A,CPA_b= cost_B/bookings_B,CPA_c=cost_C/bookings_C) %>%
  select(date,ttt_group,contains('CPA')) %>%
  mutate(month_=month(date,label=TRUE)) %>%
  tidyr::gather(type,value,3:5) %>%
  ggplot(aes(x=month_,y=value,fill=ttt_group))+geom_bar(stat='identity',position = position_dodge())+facet_wrap(~type)
##### How does ROI vary by advertiser type?
test %>% 
  mutate(month_=month(date,label=TRUE)) %>%
  group_by(ttt_group) %>%
  summarise(ROI_a = sum(booking_rev_A)/sum(cost_A),ROI_b = sum(booking_rev_B)/sum(cost_B),ROI_c=sum(booking_rev_C)/sum(cost_C))
  
##### How does booking revenue vary with time by advertiser type?
test %>% 
  select(date,ttt_group,contains('booking_rev')) %>%
  mutate(month_=month(date,label=TRUE)) %>%
  tidyr::gather(type,value,3:5) %>%
  ggplot(aes(x=month_,y=value,fill=ttt_group))+geom_bar(stat='identity',position = position_dodge())+facet_wrap(~type)

##### Profit across advertisers and market segments

test %>% 
  select(date,ttt_group,contains('booking_rev')) %>%
  tidyr::gather(advertiser,booking_rev,3:5) %>%
  mutate(advertiser = gsub('booking_rev_','',advertiser)) %>%
  group_by(ttt_group,advertiser) %>%
  summarise(total_profit = sum(booking_rev)*0.15) %>% arrange(desc(advertiser))
### Profit to Cost Ratio Across Adervtisers and Market Segments
test %>% 
  mutate(month_ = month(date)) %>%
  select(month_,ttt_group,contains('booking_rev')) %>%
  tidyr::gather(advertiser,booking_rev,3:5) %>%
  mutate(advertiser = gsub('booking_rev_','',advertiser)) %>%
  group_by(ttt_group,advertiser,month_) %>%
  summarise(total_profit = sum(booking_rev)*0.15) %>%
  inner_join(test %>% 
               mutate(month_=month(date)) %>%
               select(month_,ttt_group,contains('cost')) %>%
               tidyr::gather(advertiser,cost,3:5) %>%
               mutate(advertiser = gsub('cost_','',advertiser)) %>%
               group_by(ttt_group,advertiser,month_) %>%
               summarise(total_cost = sum(cost))) %>%
  mutate(ratio = total_profit/total_cost) %>%
  ggplot(aes(x=month_,y=ratio,fill='ttt_group'))+geom_bar(stat = 'identity')+facet_wrap(~advertiser)

### Booking Profit to Cost Ratio

test %>% 
  select(date,ttt_group,contains('booking_rev')) %>%
  tidyr::gather(advertiser,booking_rev,3:5) %>%
  mutate(advertiser = gsub('booking_rev_','',advertiser)) %>%
  group_by(ttt_group,advertiser) %>%
  summarise(total_profit = sum(booking_rev)*0.15) %>%
  inner_join(test %>% 
               select(date,ttt_group,contains('cost')) %>%
               tidyr::gather(advertiser,cost,3:5) %>%
               mutate(advertiser = gsub('cost_','',advertiser)) %>%
               group_by(ttt_group,advertiser) %>%
               summarise(total_cost = sum(cost))) %>%
  View()
### Revenue by Month Across Advertisers

test %>% 
  mutate(ROI_a = booking_rev_A/cost_A,ROI_b= booking_rev_B/cost_B,ROI_c=booking_rev_C/cost_C) %>%
  select(date,ttt_group,contains('booking_rev')) %>%
  mutate(month_=month(date,label=TRUE)) %>%
  tidyr::gather(type,value,3:5) %>%
  ggplot(aes(x=month_,y=value,fill=ttt_group))+geom_bar(stat='identity',position = position_dodge())+facet_wrap(~type)

### Profit to Cost Per Month
test %>% 
  mutate(month_=month(date,label=FALSE)) %>%
  mutate(quarter_ = quarter(month_)) %>%
  select(quarter_,contains('booking_rev')) %>%
  tidyr::gather(advertiser,profit,2:4) %>%
  mutate(advertiser = gsub('booking_rev_','',advertiser)) %>%
  mutate(profit=profit*0.15) %>%
  group_by(quarter_,advertiser) %>%
  summarise(profit=sum(profit)) %>%
  inner_join(test %>% 
               mutate(month_=month(date,label=FALSE)) %>%
               mutate(quarter_=quarter(month_)) %>%
               select(quarter_,contains('cost')) %>%
               tidyr::gather(advertiser,cost,2:4) %>%
               mutate(advertiser = gsub('cost_','',advertiser)) %>%
               group_by(quarter_,advertiser) %>%
               summarise(cost=sum(cost))) %>%
  mutate(ratio = profit/cost) %>%
  ggplot(aes(x=quarter_,y=ratio,fill))+geom_bar(stat='identity')+facet_wrap(~advertiser)

### For advertiser Profit % & Cost % A,B,C 

test %>%
  mutate(month_ = month(date,label = FALSE)) %>%
  mutate(quarter_ = quarter(month_)) %>%
  select(quarter_,contains('A'),ttt_group) %>%
  mutate(profit = booking_rev_A*0.15) %>%
  group_by(quarter_,ttt_group) %>%
  summarise(n=sum(profit)) %>% mutate(per_profit=n/sum(n)) %>%
  ungroup() %>%
  select(quarter_,ttt_group,per_profit) %>%
inner_join(
test %>%
  mutate(month_ = month(date,label = FALSE)) %>%
  mutate(quarter_ = quarter(month_)) %>%
  select(quarter_,contains('A'),ttt_group) %>%
  mutate(profit = booking_rev_A*1) %>%
  group_by(quarter_,ttt_group) %>%
  summarise(n=sum(cost_A))  %>% mutate(per_cost=n/sum(n)) %>%
  ungroup() %>%
  select(quarter_,ttt_group,per_cost)) %>%
  arrange((quarter_)) %>%
  tidyr::gather(percentage_type,value,3:4) %>%
  mutate(percentage_type=factor(percentage_type,levels=c('per_profit','per_cost'))) %>%
  ggplot(aes(x=as.factor(quarter_),y=value,fill=ttt_group))+scale_fill_manual(values=c('long'='#0088BC','short'='#FF8B00','medium'='#D83F1F'))+geom_bar(stat='identity',position = position_dodge())+
  facet_wrap(~percentage_type,nrow=2) +theme(text = element_text(family = "Arial"),
                                             plot.background = element_rect(fill = "transparent",colour = NA),
                                             panel.background = element_rect(fill = "transparent",colour = NA),
                                             legend.background = element_rect(fill = "transparent",colour = "black"),
                                             panel.grid.minor = element_line(colour="grey", size=0),
                                             panel.grid.major = element_line(colour="grey", size=0),
                                             axis.title.y=element_text(margin=margin(0,20,0,0), size = 22, face = "bold"),
                                             axis.title.x=element_text(margin=margin(10,0,0,0), size = 22, face  = "bold"),
                                             legend.position="right", legend.text = element_text(angle = 0, hjust = 1,size=16) ,
                                             legend.key = element_blank(),
                                             legend.title = element_blank(),
                                             axis.text.y = element_text(size=20),
                                             axis.text.x = element_text(angle = 50, hjust = 1, size=20),
                                             plot.title = element_text(size=20, face = "bold"),
                                             strip.text.x = element_text(size = 20, colour = "black"),
                                             axis.ticks = element_blank(),strip.background = element_blank())+
  scale_y_continuous(labels = scales::percent)+
  labs(x='',y='') -> p_c
ggsave(p_c, file = "profit_cost_a.png", width = 18, height = 15, bg="transparent")

### CPA for A,B,C
  test %>%
  mutate(month_ = month(date,label = FALSE)) %>%
  mutate(quarter_ = quarter(month_)) %>%
  select(quarter_,contains('A'),ttt_group) %>%
  select(quarter_,cost_A,bookings_A,ttt_group) %>%
  group_by(quarter_,ttt_group) %>%
  summarise(cost_A = sum(cost_A),bookings_A=sum(bookings_A)) %>%
    mutate(cpa = cost_A/bookings_A)
  
  ### Booking Rate for A,B,C
  test %>%
    mutate(month_ = month(date,label = FALSE)) %>%
    mutate(quarter_ = quarter(month_)) %>%
    select(quarter_,contains('B'),ttt_group) %>%
    select(quarter_,cost_B,bookings_B,ttt_group,clicks_B) %>%
    group_by(quarter_,ttt_group) %>%
    summarise(clicks_B = sum(clicks_B),bookings_B=sum(bookings_B)) %>%
    mutate(rate = bookings_B/clicks_B)
  


  