####Exploratory####
getwd()
library(tidyverse)
library(ggplot2)
library(dplyr)
read.csv(hobo_tempsum.csv)
#I guess you need to import the dataset the manual way?

#Some exploratory analysis 

ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp))
#This is big and ugly, but that's ALL of the data 
ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp, color=depth.m))
#Here's one way to break it down - still a lot of data 
#Further breaking things down 

#Getting into the goodies - here's both lakes broken into temp/day per meter measured 
ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~ depth.m)

#Separated out by lakes 
ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_grid(lake ~ depth.m)

#Lets filter this down to just McD and head forward with that 
hobo_mcd <-hobo_tempsum[hobo_tempsum$lake =="MCD",]

#Now heading back to temp/day per meter 

ggplot(data = hobo_mcd) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~ depth.m)

#Extra filters, let's focus on 0.5m and 1m using dplyr 

hobo_mcd_up<-filter(hobo_mcd, depth.m =='0.5'| depth.m =='1')
ggplot(data = hobo_mcd_up) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~ depth.m)

#Let's trey getting even more complex, diel cycles 
#Also, we still have to separate out littoral and pelagic 

ggplot(data = hobo_mcd_up) + 
  geom_point(mapping = aes(x = yday, y =mean.temp, color =location)) +
  facet_wrap(hour ~ depth.m)

#This is pretty ugly, and it looks like there's only littoral temp for 1m 
#Let's continue filtering - don't actually do this for the regression model 
#hobo_mcd_up<-filter(hobo_mcd,depth.m =='1')
#Now this is just data from 1m 
ggplot(data = hobo_mcd_up) + 
  geom_point(mapping = aes(x = yday, y =mean.temp, color =location)) +
  facet_wrap(~hour)

#Still a little ugly due to overlap, but now it's only 1m! 
#Maybe we start with just one hour, and then look around from there. 
#It may make sense to do an early morning, noon, and late hour when all is said and done. 

hobo_mcd_12<-filter(hobo_mcd_up,hour =='12')
ggplot(data = hobo_mcd_12) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~location)

#Let's see what kind of bogus we can get up to with filtering
hobo_mcd_4x<-filter(hobo_mcd_up, hour =='0'| hour =='6'|hour =='12'|hour =='18')
ggplot(data = hobo_mcd_4x) + 
  geom_point(mapping = aes(x = yday, y =log(mean.temp))) +
  facet_wrap(hour~location, nrow=4)
#Based on looking at these data, the littoral does seem warmer than the pelagic
#Maybe we can bind this from days 125-260?
#What happened around day 175? Looks like pelagic was warmer than littoral

write.csv(hobo_mcd_up,"C:\\Users\\Quinn\\Documents\\R\\BMWS", row.names = FALSE)

#Trying simple way

mcd_lit <- filter(hobo_mcd_up,location =='littoral')
mcd_pel <- filter(hobo_mcd_up,location =='pelagic')

mod1 <- lm(mcd_lit$mean.temp ~ mcd_pel$mean.temp)

#Differing lengths here

#gotta work with this data 
#Lets go back to hobo_mcd_up and make some more columns, let's turn littoral and 0.5m pelagic into a column

hobo_mcd_up = as_tibble(hobo_mcd_up)
hobo_mcd_up

#For MDY things 
library(lubridate)

wide_hobo_mcd_up = hobo_mcd_up %>%
  pivot_wider(names_from = 'location', values_from = mean.temp) %>%
  rename(littoral_meantemp = littoral, 
         pelagic_meantemp = pelagic)
wide_hobo_mcd_up

hobo_mcd_up_w = wide_hobo_mcd_up %>%
  pivot_wider(names_from = 'depth.m', values_from = pelagic_meantemp)
  

lm <- lm(1~littoral_meantemp, hobo_mcd_up_w)

#write.csv(hobo_mcd_up_w, "C:\\Users\\Quinn\\Documents\\R\\BMWS\\hobo_mcd_up_w.csv", row.names = FALSE)

####mcd_lit_pel dataset####

mcd_lit_pel_temp$hourcos <- cos(mcd_lit_pel_temp$hour)

ggplot(data=mcd_lit_pel_temp) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  geom_abline(y=1)

ggplot(data=mcd_lit_pel_temp) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral)) +
  geom_abline(y=1) +
  facet_wrap(~ hour)

hobo_mcd_4x


ggplot(data = mcd_lit_pel_temp) + 
  geom_point(mapping = aes(x = yday, y =littoral, color=year))

ggplot(data = mcd_lit_pel_temp) + 
  geom_point(mapping = aes(x = yday, y = pelagic_05, color=year))

ggplot(data = mcd_lit_pel_temp) + 
  geom_point(mapping = aes(x = yday, y =pelagic_1, color=year))

#Hella ella 
#The above are standardized to same days/year 
#JD 146-252 for 2017, 2018, and 2019 

#Beginning with linear models 

#Littoral
lm_lit <- lm(littoral~yday+year, data=mcd_lit_pel_temp)
summary(lm_lit)
plot(lm_lit$residuals)

#Pelagic 0.5
lm_pel05 <- lm(pelagic_05~yday+year, data=mcd_lit_pel_temp)
summary(lm_pel05)
plot(lm_pel05$residuals)

#Pelagic 
lm_pel1 <- lm(pelagic_1~yday+year, data=mcd_lit_pel_temp)
summary(lm_pel1)
plot(lm_pel1$residuals)

#Fairly poor r2 all around, lets try some quadratic regressions 

#mcd_lit_pel_temp$littoral2 <- mcd_lit_pel_temp$littoral^2
#ggplot(data = mcd_lit_pel_temp) + 
#  geom_point(mapping = aes(x = yday, y =littoral2, color=year))
#lm_lit_q <- lm(littoral~yday+littoral2+year, data=mcd_lit_pel_temp)
#summary(lm_lit_q)
#plot(lm_lit_q$residuals)

#Major screw around time...

test_lm <- lm(yday~littoral+littoral2+year, data=mcd_lit_pel_temp)
summary(test_lm)
#Not what I'm looking for ^ 

mcd_lit_pel_temp$yday2 <- mcd_lit_pel_temp$yday^2

#Try applying quadratic to yday? 
#I think this is the appropriate way to do it 
test_lm2 <- lm(littoral~yday+yday2+year, data=mcd_lit_pel_temp)
summary(test_lm2)
plot(test_lm2)
plot(test_lm2$residuals)

#Quadratic regression to littoral, pelagic 0.5, and pelagic 1 

#Littoral
lm_lit_q <-lm(littoral~yday+yday2+year, data=mcd_lit_pel_temp)
summary(lm_lit_q)
plot(lm_lit_q)
plot(lm_lit_q$residuals)

#Pelagic 0.5
lm_pel05_q <- lm(pelagic_05~yday+yday2+year, data=mcd_lit_pel_temp)
summary(lm_pel05_q)
plot(lm_pel05_q)
plot(lm_pel05_q$residuals)

#Pelagic 1 
lm_pel1_q <- lm(pelagic_1~yday+yday2+year, data=mcd_lit_pel_temp)
summary(lm_pel1_q)
plot(lm_pel1_q)
plot(lm_pel1_q$residuals)

#Predicting with models 
 
lm_lit_q2 <-lm(littoral~yday+yday2, data=mcd_lit_pel_temp)
dayvals <-seq(146,252,1)

day_predict <- predict(lm_lit_q2, list(yday=dayvals, yday2=dayvals^2))

plot(mcd_lit_pel_temp$yday, mcd_lit_pel_temp$pelagic_1, col=mcd_lit_pel_temp$year)
lines(dayvals,day_predict, col='blue', lwd=5)

#Delving into specific loggers 


#ALL 
ggplot(data=mcd_lit_pel_temp_update) +
  geom_point(mapping = aes(x = pelagic_1, y =mean_lit, color=hour)) +
  geom_abline(y=1)


#Logger 6 
ggplot(data=subset(hobo_mcd6_lit_pel, !is.na(year))) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  facet_wrap(~year)+
  geom_abline(y=1)


#Logger 7 
ggplot(data=hobo_mcd7_lit_pel) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  facet_wrap(~year)+
  geom_abline(y=1)


#Logger 8 
ggplot(data=hobo_mcd8_lit_pel) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  facet_wrap(~year)+
  geom_abline(y=1)

#Log 8 in 2019

ggplot(data=hobo_mcd8_lit_pel_2019) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  facet_wrap(~month)+
  geom_abline(y=1)

#SAme site, logger 8 in 2020, logger 2 in 2021
#NOTE: For 2020 only 0.5m pelagic exists, for 2021 only 1m pelagic exists 
#This is just explroatory 
ggplot(data=hobo_mcd8_2_post_panini) +
  geom_point(mapping = aes(x = pelagic, y =littoral_meantemp, color=hour)) +
  facet_wrap(~year)+
  geom_abline(y=1)

#Site 2 (same as 6) logger 1 for 2021 

ggplot(data=hobo_mcd1_lit_pel_2021) +
  geom_point(mapping = aes(x = pelagic, y =littoral_meantemp, color=hour)) +
  facet_wrap(~month)+
  geom_abline(y=1)
