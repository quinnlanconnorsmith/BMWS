####2023 Data#### 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
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

####McDermott####
###Litpel comp McD###
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
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="black") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(0,1000)

#One at a time 
ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="black")

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
  xlim(2000,3500) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(9000,10000) 

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
#Half and 1m comps 
ggplot(data = McDermott_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_1_temp), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_temp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_5_temp), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_temp), color="black") +
  xlim(2000,2500) 


#####Escanaba####
#Plot it all 
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_1_temp), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_3_temp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_4_temp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_5_temp), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_temp), color="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_temp), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_temp), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_temp), color="darkgreen") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_10_temp), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_11_temp), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_12_temp), color="lightyellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =pel_avg_temp), color="black") 

#Making comp values 
class(Escanaba_litpel_comp)
#No idea why I get errors, but it works 
Escanaba_litpel_comp$ES_1_compt <- (Escanaba_litpel_comp$ES_1_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_3_compt <- (Escanaba_litpel_comp$ES_3_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_4_compt <- (Escanaba_litpel_comp$ES_4_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_5_compt <- (Escanaba_litpel_comp$ES_5_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_6_compt <- (Escanaba_litpel_comp$ES_6_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_7_compt <- (Escanaba_litpel_comp$ES_7_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_8_compt <- (Escanaba_litpel_comp$ES_8_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_9_compt <- (Escanaba_litpel_comp$ES_9_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_10_compt <- (Escanaba_litpel_comp$ES_10_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_11_compt <- (Escanaba_litpel_comp$ES_11_temp - Escanaba_litpel_comp$pel_avg_temp)
Escanaba_litpel_comp$ES_12_compt <- (Escanaba_litpel_comp$ES_12_temp - Escanaba_litpel_comp$pel_avg_temp)

#Comp to pelagic
#7 and 9 are half meter
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_1_compt), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_3_compt), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_4_compt), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_5_compt), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="black") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="darkgreen") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_10_compt), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_11_compt), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_12_compt), color="lightyellow") 

#let's focus a bit 
#Start of the year 
ggplot(data = Escanaba_litpel_comp) + 
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_1_compt), col="blue") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_3_compt), color="yellow") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_4_compt), color="purple") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_5_compt), color="orange") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="black") +
    #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="brown") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="cyan") +
    #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="darkgreen") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_10_compt), color="pink") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_11_compt), col="green") +
    geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_12_compt), color="lightyellow") +
  xlim(2800,2900) 

#Big swing
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_1_compt), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_3_compt), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_4_compt), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_5_compt), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="black") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="darkgreen") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_10_compt), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_11_compt), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_12_compt), color="brown") +
  xlim(2500,3000) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96))  

#Goofin here looking at 1000 sampling periods 
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_1_compt), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_3_compt), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_4_compt), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_5_compt), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="black") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="brown") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="darkgreen") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_10_compt), color="pink") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_11_compt), col="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_12_compt), color="brown") +
  xlim(5500,6500) 

#Looking at meter/halfmeter 6 & 7
#Big difs! 
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="red") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="brown") 
  xlim(1000,1250) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Some REALLY cool diel cycles at half/meter - don't think this is shading because they're offset, but 
#can test with lux - weird part is it matches up for other days 
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="red") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="brown") 
  xlim(3500,4000) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Matching perfectly 
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="red") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="brown") 
  xlim(5000,6000) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Change this one around
ggplot(data = Escanaba_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="red") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="brown") 
  xlim(8000,9000) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

##Pivot to 8 & 9
#Kinda interesting here
ggplot(data = Escanaba_litpel_comp) + 
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="black") +
  xlim(2000,3000) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Most of the other data matches up pretty well
ggplot(data = Escanaba_litpel_comp) + 
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="black") +
  xlim(3500,4000) +
  ylim(-2,4) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 


####Sparkling####
#Plot it all 

ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_temp), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_3_temp), color="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_4_temp), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_5_temp), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_6_temp), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_temp), color="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_temp), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_temp), color="black") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =pel_1_temp), color="pink") +
  xlim(0,10000) 

#Making comp values 
Sparkling_litpel_comp$SP_2_compt <- (Sparkling_litpel_comp$SP_2_temp - Sparkling_litpel_comp$pel_1_temp)
Sparkling_litpel_comp$SP_3_compt <- (Sparkling_litpel_comp$SP_3_temp - Sparkling_litpel_comp$pel_1_temp)
Sparkling_litpel_comp$SP_4_compt <- (Sparkling_litpel_comp$SP_4_temp - Sparkling_litpel_comp$pel_1_temp)
Sparkling_litpel_comp$SP_5_compt <- (Sparkling_litpel_comp$SP_5_temp - Sparkling_litpel_comp$pel_1_temp)
Sparkling_litpel_comp$SP_6_compt <- (Sparkling_litpel_comp$SP_6_temp - Sparkling_litpel_comp$pel_1_temp)
Sparkling_litpel_comp$SP_7_compt <- (Sparkling_litpel_comp$SP_7_temp - Sparkling_litpel_comp$pel_1_temp)
Sparkling_litpel_comp$SP_8_compt <- (Sparkling_litpel_comp$SP_8_temp - Sparkling_litpel_comp$pel_1_temp)
Sparkling_litpel_comp$SP_D1_compt <- (Sparkling_litpel_comp$SP_D1_temp - Sparkling_litpel_comp$pel_1_temp)


#Compare to pelagic 
#2 and 8 are half meter
ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_3_compt), color="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_4_compt), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_5_compt), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_6_compt), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_compt), color="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_compt), color="black") +
  xlim(0,10000) 

#Removing hald meters and deep stuff 
ggplot(data = Sparkling_litpel_comp) + 
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_3_compt), color="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_4_compt), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_5_compt), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_6_compt), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_compt), color="green") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_compt), color="black") +
  xlim(0,10000) 

#Closer look 
ggplot(data = Sparkling_litpel_comp) + 
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_3_compt), color="red") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_4_compt), color="purple") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_5_compt), color="orange") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_6_compt), color="yellow") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_compt), color="green") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_compt), color="black") +
  xlim(0000,20000) 
#Really this is the only period when things are wacky 

#Comp half m and 1m sites
ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_temp), color="green") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_temp), color="cyan") +
  xlim(2000,3000) 


#Compare the 3m plants to 0.5m on the point 
ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_3_compt), color="red") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_4_compt), color="purple") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_5_compt), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_6_compt), color="yellow") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_compt), color="green") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_compt), color="black") +
  xlim(0,10000) 

#Closer look
ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_3_compt), color="red") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_4_compt), color="purple") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_5_compt), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_6_compt), color="yellow") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_compt), color="green") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_compt), color="black") +
  xlim(2500,5000) 

#Wait, what's going on here: 
ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_3_compt), color="red") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_4_compt), color="purple") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_5_compt), color="orange") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_6_compt), color="yellow") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_7_compt), color="green") +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_compt), color="black") +
  xlim(4250,4700) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Whoa, temp is cooler in the plants compared to pelagic at night 
#Another way to viz

ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_temp), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_temp), color="black") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =pel_1_temp), color="red") +
  xlim(4250,4700) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) +
  ylim(21,26)
#So not too surprising 

#All summer - 3m plants, 0.5m and littoral
ggplot(data = Sparkling_litpel_comp) + 
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_2_temp), col="blue") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =SP_D1_temp), color="black") +
  geom_point(alpha=0.5,mapping = aes(x = sample, y =pel_1_temp), color="red")

####Using Lines#### 

ggplot(data = Sparkling_litpel_comp) + 
  geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_3_compt), color="red") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_4_compt), color="purple") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_5_compt), color="orange") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_6_compt), color="yellow") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_7_compt), color="green") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  xlim(0,10000) +
  ylim(-3,4)

ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_1_comp), col="blue") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_2_comp), col="green") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_3_comp), color="yellow") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_4_comp), color="purple") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_5_comp), color="orange") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_6_comp), color="black") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_7_comp), color="brown") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =McD_10_comp), color="cyan") +
  xlim(0,10000)+
  ylim(-3,4)

ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_1_compt), col="blue") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_3_compt), color="yellow") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_4_compt), color="purple") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_5_compt), color="orange") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_6_compt), color="black") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="brown") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_8_compt), color="cyan") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="darkgreen") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_10_compt), color="pink") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_11_compt), col="green") +
  geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_12_compt), color="lightyellow") +
  ylim(-3,4)

####For Presentations####

#McD raw 
mcdr <- ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_1_temp), col="#1B9E77", lwd=2) +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_temp), col="") +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_3_temp), color="skyblue", lwd=2) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_4_temp), color="#7570B3", lwd=2) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_5_temp), color="#E7298A", lwd=2) +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_temp), color="pink") +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_7_temp), color="#66A61E", lwd=2) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_10_temp), color="tomato", lwd=2) +
  geom_line(alpha=1, mapping = aes(x = sample, y =pel_1_temp), col="black") +
  ylab("Temperature(°C)")+
  xlab("Sample") + 
  geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  geom_vline(xintercept=3649,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=6625,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("McDermott 2023 - 1m") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  ylim(14,29)
#geom_vline(xintercept = seq(from=1, to=10000, by=96)) 
mcdr
tiff("mcdr.tiff", units="in", width=10, height=8, res=600)
mcdr
dev.off()

#ES raw
esr <- ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 0.35, mapping = aes(x = sample, y =ES_1_temp), col="#1B9E77", lwd=2) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_3_temp), color="skyblue", lwd=2) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_4_temp), color="#7570B3", lwd=2) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_5_temp), color="#E7298A", lwd=2) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_6_temp), color="#66A61E", lwd=2) +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_7_temp), color="brown") +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_8_temp), color="tomato", lwd=2) +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_9_temp), color="darkgreen") +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_10_temp), color="thistle", lwd=2) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_11_temp), color="lightgoldenrod3", lwd=2) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_12_temp), color="indianred", lwd=2) +
  geom_line(mapping = aes(x = sample, y =pel_avg_temp), color="black") +
  ylab("Temperature(°C)")+
  xlab("Sample") + 
  geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  geom_vline(xintercept=3649,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=6625,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Escanaba 2023 - 1m") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  ylim(14,29)
#geom_vline(xintercept = seq(from=1, to=10000, by=96)) 
esr
tiff("esr.tiff", units="in", width=10, height=8, res=600)
esr
dev.off()


#SP raw 
spr <- ggplot(data = Sparkling_litpel_comp) + 

  #geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_2_temp), col="blue") +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_3_temp), col="#1B9E77", lwd=2) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_4_temp), color="skyblue", lwd=2) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_5_temp), color="#E7298A", lwd=2) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_6_temp), color="#66A61E", lwd=2) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_7_temp), color="tomato", lwd=2) +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_8_temp), color="cyan") +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_D1_temp), color="black") +
  geom_line(mapping = aes(x = sample, y =pel_1_temp), color="black") +
  ylab("Temperature(°C)")+
  xlab("Sample") + 
  geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  geom_vline(xintercept=3649,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=6625,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Sparkling 2023 - 1m") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  ylim(14,29)
#geom_vline(xintercept = seq(from=1, to=10000, by=96)) 
  xlim(0,10000) 
  
spr 
tiff("spr.tiff", units="in", width=10, height=8, res=600)
spr
dev.off()

#Highlight some McD goodies 
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


mcd_c1 <- ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_1_comp), col="#1B9E77", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_3_comp), color="skyblue", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_4_comp), color="#7570B3", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_5_comp), color="#E7298A", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_7_comp), color="#66A61E", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_10_comp), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
mcd_c1

mcd_c2 <- ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_1_comp), col="#1B9E77", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_3_comp), color="skyblue", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_4_comp), color="#7570B3", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_5_comp), color="#E7298A", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_7_comp), color="#66A61E", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_10_comp), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
mcd_c2

mcd_c3 <- ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_1_comp), col="#1B9E77", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_3_comp), color="skyblue", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_4_comp), color="#7570B3", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_5_comp), color="#E7298A", lwd=1) +
  geom_line(alpha =1,mapping = aes(x = sample, y =McD_7_comp), color="#66A61E", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_10_comp), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
mcd_c3


mcd_c4 <- ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_1_comp), col="#1B9E77", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_3_comp), color="skyblue", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_4_comp), color="#7570B3", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =McD_5_comp), color="#E7298A", lwd=1) +
  geom_line(alpha =1,mapping = aes(x = sample, y =McD_7_comp), color="#66A61E", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =McD_10_comp), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
mcd_c4



tiff("mcd_c1.tiff", units="in", width=10, height=8, res=600)
mcd_c1
dev.off()
tiff("mcd_c2.tiff", units="in", width=10, height=8, res=600)
mcd_c2
dev.off()
tiff("mcd_c3.tiff", units="in", width=10, height=8, res=600)
mcd_c3
dev.off()
tiff("mcd_c4.tiff", units="in", width=10, height=8, res=600)
mcd_c4
dev.off()


sp_c1 <- ggplot(data = Sparkling_litpel_comp) + 
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_3_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_4_compt), color="skyblue", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_7_compt), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
sp_c1


sp_c2 <- ggplot(data = Sparkling_litpel_comp) + 
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_3_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_4_compt), color="skyblue", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_7_compt), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
sp_c2

sp_c3 <- ggplot(data = Sparkling_litpel_comp) + 
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_3_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_4_compt), color="skyblue", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_7_compt), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
sp_c3

sp_c4 <- ggplot(data = Sparkling_litpel_comp) + 
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_3_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_4_compt), color="skyblue", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha=0.2,mapping = aes(x = sample, y =SP_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha=1,mapping = aes(x = sample, y =SP_7_compt), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
sp_c4


tiff("sp_c1.tiff", units="in", width=10, height=8, res=600)
sp_c1
dev.off()
tiff("sp_c2.tiff", units="in", width=10, height=8, res=600)
sp_c2
dev.off()
tiff("sp_c3.tiff", units="in", width=10, height=8, res=600)
sp_c3
dev.off()
tiff("sp_c4.tiff", units="in", width=10, height=8, res=600)
sp_c4
dev.off()

#Escanaba
es_c1 <- ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 1, mapping = aes(x = sample, y =ES_1_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_3_compt), color="skyblue", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_4_compt), color="#7570B3", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_8_compt), color="tomato", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_10_compt), color="thistle", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_11_compt), color="lightgoldenrod3", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_12_compt), color="indianred", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
es_c1


es_c2 <- ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 0.2, mapping = aes(x = sample, y =ES_1_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_3_compt), color="skyblue", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_4_compt), color="#7570B3", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_8_compt), color="tomato", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_10_compt), color="thistle", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_11_compt), color="lightgoldenrod3", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_12_compt), color="indianred", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
es_c2

es_c3 <- ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 0.2, mapping = aes(x = sample, y =ES_1_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_3_compt), color="skyblue", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_4_compt), color="#7570B3", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_8_compt), color="tomato", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_10_compt), color="thistle", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_11_compt), color="lightgoldenrod3", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_12_compt), color="indianred", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
es_c3


es_c4 <- ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 0.2, mapping = aes(x = sample, y =ES_1_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_3_compt), color="skyblue", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_4_compt), color="#7570B3", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_8_compt), color="tomato", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_10_compt), color="thistle", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_11_compt), color="lightgoldenrod3", lwd=1) +
  geom_line(alpha = 0.2,mapping = aes(x = sample, y =ES_12_compt), color="indianred", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  xlim(2000,2500)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96), color = "darkgrey", lwd=2) +
  geom_hline(yintercept=0)
es_c4



tiff("es_c1.tiff", units="in", width=10, height=8, res=600)
es_c1
dev.off()
tiff("es_c2.tiff", units="in", width=10, height=8, res=600)
es_c2
dev.off()
tiff("es_c3.tiff", units="in", width=10, height=8, res=600)
es_c3
dev.off()
tiff("es_c4.tiff", units="in", width=10, height=8, res=600)
es_c4
dev.off()

#Now let's do fancy comps 

esc <- ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 0.35, mapping = aes(x = sample, y =ES_1_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_3_compt), color="skyblue", lwd=1) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_4_compt), color="#7570B3", lwd=1) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_6_compt), color="#66A61E", lwd=1) +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_7_compt), color="brown") +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_8_compt), color="tomato", lwd=1) +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =ES_9_compt), color="darkgreen") +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_10_compt), color="thistle", lwd=1) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_11_compt), color="lightgoldenrod3", lwd=1) +
  geom_line(alpha = 0.35,mapping = aes(x = sample, y =ES_12_compt), color="indianred", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  geom_vline(xintercept=3649,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=6625,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Escanaba 2023 - 1m Comparison ") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_hline(yintercept=0) + 
  ylim(-3,5)

esc


mcdc <- ggplot(data = McDermott_litpel_comp) + 
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_1_comp), col="#1B9E77", lwd=1) +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_2_compt), col="") +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_3_comp), color="skyblue", lwd=1) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_4_comp), color="#7570B3", lwd=1) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_5_comp), color="#E7298A", lwd=1) +
  #geom_point(alpha=0.5,mapping = aes(x = sample, y =McD_6_compt), color="pink") +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_7_comp), color="#66A61E", lwd=1) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =McD_10_comp), color="tomato", lwd=1) +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  geom_vline(xintercept=3649,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=6625,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("McDermott 2023 - 1m Comparison") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_hline(yintercept=0) +
  ylim(-3,5)
mcdc

spc <- ggplot(data = Sparkling_litpel_comp) + 
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_2_compt), col="blue") +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_3_compt), col="#1B9E77", lwd=1) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_4_compt), color="skyblue", lwd=1) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_5_compt), color="#E7298A", lwd=1) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha=0.35,mapping = aes(x = sample, y =SP_7_compt), color="tomato", lwd=1) +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_8_compt), color="cyan") +
  #geom_line(alpha=0.5,mapping = aes(x = sample, y =SP_D1_compt), color="black") +
  ylab("Temperature Comparison to Pelagic (°C)")+
  xlab("Sample") + 
  geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  geom_vline(xintercept=3649,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=6625,lwd=2,colour="darkgrey") +
  geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Sparkling 2023 - 1m Comparison") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) +
  geom_hline(yintercept=0) +
  ylim(-3,5)
spc


tiff("mcdc.tiff", units="in", width=10, height=8, res=600)
mcdc
dev.off()
tiff("esc.tiff", units="in", width=10, height=8, res=600)
esc
dev.off()
tiff("spc.tiff", units="in", width=10, height=8, res=600)
spc
dev.off()



#ES 1/2 and 1m 

ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_7_compt), color="#E7298A", lwd=1) +
  xlim(3500,4000) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 

#Matching perfectly 
ggplot(data = Escanaba_litpel_comp) + 
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_6_compt), color="#66A61E", lwd=1) +
  geom_line(alpha = 1,mapping = aes(x = sample, y =ES_7_compt), color="#E7298A", lwd=1) +
  xlim(5000,6000) +
  geom_vline(xintercept = seq(from=1, to=10000, by=96)) 
