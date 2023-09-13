####2023 Data#### 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
#McD
ggplot(data = McDermott_littoral_trim) + 
  geom_point(mapping = aes(x = date, y =temp)) +
  facet_wrap(~common_ID)

#SP
ggplot(data = Sparkling_littoral_trim) + 
  geom_point(mapping = aes(x = date, y =temp)) +
  facet_wrap(~common_ID)

#ES
ggplot(data = Escanaba_littoral_trim) + 
  geom_point(mapping = aes(x = date, y =temp)) +
  facet_wrap(~common_ID)

#Litpel comp McD
#Raw values 
ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5, mapping = aes(x = sample, y =pel_1_temp), col="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_temp), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_temp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_temp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_temp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_temp), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_temp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_temp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_temp), color="cyan") #+
  #geom_vline(xintercept = seq(from=1, to=10000, by=96)) 


#Comp to pelagic temp 
#2 and 6 are half meter
ggplot(data = McDermott_litpel_comp) + 
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="black") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(0,4000) 

#Highlight 
ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.01,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.01,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.01,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.01,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.01,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(mapping = aes(x = sample, y =McD_10_comp), color="cyan") #+
#xlim(0,1000) 

#Zooming in 
ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(400,500) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 
#Now we're talking


#Cool stuff 
ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(2000,2500)+
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Highlight cool stuff 
ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.1,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.1,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.1,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.1,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.1,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(2000,2500)+
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Also very cool 
ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(2500,3000)+
  geom_vline(xintercept = seq(from=1, to=3000, by=96)) 

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(3000,3500)

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(3000,4000)

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(4000,5000)

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(5000,6000)

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(6000,7000)

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(7000,8000)

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(8000,9000)
