####Exploratory####
getwd()
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
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
hobo_mcd6_lit_pel$comp <- hobo_mcd6_lit_pel$littoral -hobo_mcd6_lit_pel$pelagic_1
ggplot(data=hobo_mcd6_lit_pel) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)

  #log 6 residuals 
rmod6 <- lm(pelagic_1~littoral, data=hobo_mcd6_lit_pel)
summary(rmod6)
hobo_mcd6_lit_pel <-  filter(hobo_mcd6_lit_pel, rowSums(is.na(hobo_mcd6_lit_pel)) != ncol(hobo_mcd6_lit_pel))
residuals6 <- rstandard(rmod6)
hobo_mcd6_lit_pel$residuals <- residuals6
ggplot(data=hobo_mcd6_lit_pel) +
  geom_point(mapping = aes(x = yday, y =residuals)) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)


#Logger 7 
ggplot(data=hobo_mcd7_lit_pel) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  facet_wrap(~year)+
  geom_abline(y=1)
hobo_mcd7_lit_pel$comp <- hobo_mcd7_lit_pel$littoral -hobo_mcd7_lit_pel$pelagic_1
ggplot(data=hobo_mcd7_lit_pel) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)

#Logger 8 
ggplot(data=hobo_mcd8_lit_pel) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  facet_wrap(~year)+
  geom_abline(y=1)
hobo_mcd8_lit_pel$comp <- hobo_mcd8_lit_pel$littoral -hobo_mcd8_lit_pel$pelagic_1
ggplot(data=hobo_mcd8_lit_pel) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) 
  

#Log 8 in 2019 - deep dive

ggplot(data=hobo_mcd8_lit_pel_2019) +
  geom_point(mapping = aes(x = pelagic_1, y =littoral, color=hour)) +
  facet_wrap(~month)+
  geom_abline(y=1)

#SAme site, logger 8 in 2020, logger 2 in 2021
#NOTE: For 2020 only 0.5m pelagic exists, for 2021 only 1m pelagic exists 
#This is just exploratory 
ggplot(data=hobo_mcd8_2_post_panini) +
  geom_point(mapping = aes(x = pelagic, y =littoral_meantemp, color=hour)) +
  facet_wrap(~year)+
  geom_abline(y=1)
hobo_mcd8_2_post_panini$comp <- hobo_mcd8_2_post_panini$littoral_meantemp -hobo_mcd8_2_post_panini$pelagic
ggplot(data=hobo_mcd8_2_post_panini) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)


#Site 2 (same as 6) logger 1 for 2021 

ggplot(data=hobo_mcd1_lit_pel_2021) +
  geom_point(mapping = aes(x = pelagic, y =littoral_meantemp, color=hour)) +
  facet_wrap(~month)+
  geom_abline(y=1)


#Making comparison graphs 
mcd_lit_pel_temp_update$comp <- mcd_lit_pel_temp_update$mean_lit -mcd_lit_pel_temp_update$pelagic_1
             
ggplot(data=mcd_lit_pel_temp_update) +
  geom_point(mapping = aes(x = yday, y =comp, color=as.factor())) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)

ggplot(data=mcd_lit_pel_temp_update) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)

ggplot(data=mcd_lit_pel_temp_update) +
  geom_point(mapping = aes(x = yday, y =comp, color=(id))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)


#Littoral temp residuals 

rmod <- lm(pelagic_1~mean_lit, data=mcd_lit_pel_temp_update)
summary(rmod)
residuals <- rstandard(rmod)

mcd_lit_pel_temp_update <-  filter(mcd_lit_pel_temp_update, rowSums(is.na(mcd_lit_pel_temp_update)) != ncol(mcd_lit_pel_temp_update))

mcd_lit_pel_temp_update$residuals <- residuals

ggplot(data=mcd_lit_pel_temp_update) +
  geom_point(mapping = aes(x = yday, y =residuals, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)

ggplot(data=mcd_lit_pel_temp_update) +
  geom_point(mapping = aes(x = pelagic_1, y =residuals, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0)


#Zoom in on some daily goodies 
#Log 8

#Big view 

ggplot(data=subset(hobo_mcd8_lit_pel, !is.na(year))) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) + 
  ggtitle("Logger 8")

#Small window
ggplot(data=subset(hobo_mcd8_lit_pel, !is.na(year))) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) + 
  xlim(160,180) + 
  ggtitle("Logger 8")
#Log 7

#Big view 
#Small window 
ggplot(data=subset(hobo_mcd7_lit_pel, !is.na(year))) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) +
  xlim(175, 195) +
  ggtitle("Logger 7")

#Big view 
ggplot(data=subset(hobo_mcd6_lit_pel, !is.na(year))) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) +
  ggtitle("Logger 6")
#Small window 
#Log 6
ggplot(data=subset(hobo_mcd6_lit_pel, !is.na(year))) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) +
  xlim(160,180) + 
  ggtitle("Logger 6")
#Just 2019 
ggplot(data=subset(hobo_mcd6_lit_pel,hobo_mcd6_lit_pel$year =="2019", !is.na(year))) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) +
  xlim(140,160) + 
  ggtitle("Logger 6")

#Just 2019 logger 8

ggplot(data=subset(hobo_mcd8_lit_pel, hobo_mcd8_lit_pel$year =="2019", !is.na(year))) +
  geom_point(mapping = aes(x = yday, y =comp, color=(hour))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) + 
  xlim(160,180) + 
  ggtitle("Logger 8")

#Some site overlay 

hobo_mcd6_lit_pel$logr <- 6
hobo_mcd7_lit_pel$logr <- 7
hobo_mcd8_lit_pel$logr <- 8

#All years, comp per julian day 
ggplot()+
  geom_point(data=subset(hobo_mcd6_lit_pel, !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd7_lit_pel, !is.na(year)),alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd8_lit_pel, !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) +
  ggtitle("Title")

#Log 6/7/8/ 2017 
ggplot()+
  geom_point(data=subset(hobo_mcd6_lit_pel, hobo_mcd6_lit_pel$year =="2017", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd7_lit_pel, hobo_mcd7_lit_pel$year =="2017", !is.na(year)),alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd8_lit_pel, hobo_mcd8_lit_pel$year =="2017",!is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  facet_wrap(~year)+
  geom_hline(yintercept=0) +
  ggtitle("2017 for all 3 logger sites")

#Log6&8 2019 
ggplot()+
  geom_point(data=subset(hobo_mcd6_lit_pel, hobo_mcd6_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd8_lit_pel, hobo_mcd8_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_hline(yintercept=0) +
  ggtitle("Logger 6&8 2019")

#xlim
ggplot()+
  geom_point(data=subset(hobo_mcd6_lit_pel, hobo_mcd6_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd8_lit_pel, hobo_mcd8_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_hline(yintercept=0) +
  xlim(170,210) +
  ggtitle("Logger 6&8 2019")

#Pretty plot for the whole year 
ggplot()+
  geom_point(data=subset(hobo_mcd6_lit_pel, hobo_mcd6_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd8_lit_pel, hobo_mcd8_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_hline(yintercept=0) +
  xlab("Julian Day")+
  ylab("Temperature Comparison(°C)") +
  guides (color = guide_legend(title="Littoral Logger")) +
  ggtitle("Comparison of Littoral Temperature and Pelagic Temperature - 2019")


ggplot()+
  geom_point(data=subset(hobo_mcd6_lit_pel, hobo_mcd6_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_point(data=subset(hobo_mcd8_lit_pel, hobo_mcd8_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = yday, y =comp, color=(as.factor(logr)))) +
  geom_hline(yintercept=0) +
  xlab("Julian Day")+
  ylab("Temperature Comparison(°C)") +
  theme(legend.position = "none")

#Further exploration 
#Make a comp - date/hour per year 

hobo_mcd6_lit_pel$day_hr <- paste(hobo_mcd6_lit_pel$yday, hobo_mcd6_lit_pel$hour, sep= ".")
hobo_mcd8_lit_pel$day_hr <- paste(hobo_mcd8_lit_pel$yday, hobo_mcd8_lit_pel$hour, sep= ".")

hobo_mcd6_lit_pel$diel_seq <- seq(from=1, to=17043)
hobo_mcd8_lit_pel$diel_seq <- seq(from=1, to=17043)

write.csv(hobo_mcd6_lit_pel,"C:\\Users\\Quinn\\Documents\\R\\BMWS\\mcd6_lit_pel_2.csv", row.names = FALSE)
write.csv(hobo_mcd8_lit_pel,"C:\\Users\\Quinn\\Documents\\R\\BMWS\\mcd8_lit_pel_2.csv", row.names = FALSE)

ggplot()+
  geom_point(data=subset(hobo_mcd6_lit_pel, hobo_mcd6_lit_pel$year =="2019", !is.na(year)), alpha= 0.1, mapping = aes(x = day_hr, y =comp, color=(as.factor(logr)))) +
  geom_hline(yintercept=0) +
  xlab("x")+
  ylab("Temperature Comparison(°C)") +
  theme(legend.position = "none")

#Goofin 

ggplot()+
  geom_point(data=mcd6_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =littoral, color=(as.factor(logr)))) +
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =littoral, color=(as.factor(logr)))) +
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =pelagic_1)) + 
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =pelagic_3.5), col="blue") + 
  ylab("Temperature")+
  xlab("Hours") + 
  geom_vline(xintercept=385,lwd=1,colour="darkgrey") + 
  geom_vline(xintercept=1105,lwd=1,colour="darkgrey") +
  geom_vline(xintercept=1849,lwd=1,colour="darkgrey") +
  geom_vline(xintercept=2593,lwd=1,colour="darkgrey") 
#+
#  scale_color_manual(name = 'Logger', 
#                      values =c("red", "lightblue", "black", "blue"), 
#                      labels = c("Littoral6 - 1m", "Littoral8 - 1m", "Pelagic - 1m", "test"))
#  
#"red"="red", "lightblue"="lightblue", "black"="black", "blue"="blue"
#"red", "lightblue", "black", "blue"


ggplot()+
  geom_point(data=mcd6_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =littoral, color=(as.factor(logr)))) +
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =littoral, color=(as.factor(logr)))) +
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =pelagic_1)) +
  geom_vline(xintercept = seq(from=1, to=2808, by=24)) +
  xlim(0,500) +
  #ylim(13,23) +
  ylab("Temperature")+
  xlab("Hours") 

ggplot()+
  geom_point(data=mcd6_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =littoral, color=(as.factor(logr)))) +
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =littoral, color=(as.factor(logr)))) +
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =pelagic_1)) +
  geom_vline(xintercept = seq(from=1, to=2808, by=24)) +
  xlim(900,1100)+
  ylim(16,25) +
  ylab("Temperature")+
  xlab("Hours") 


ggplot()+
  geom_point(data=mcd6_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =comp, color=(as.factor(logr)))) +
  geom_point(data=mcd8_lit_pel_2, alpha= 0.5, mapping = aes(x = seq, y =comp, color=(as.factor(logr)))) + 
  geom_vline(xintercept = seq(from=1, to=2808, by=24)) +
  xlim(1300,1500)+
  geom_hline(yintercept=0) +
  ylab("Temperature comparison to pelagic ")+
  xlab("Hours")

#CCF testing 
# Time lag may be indicative of internal movements, if not possibly general site characteristics 

lit8<- mcd8_lit_pel_2$littoral
lit6<- mcd6_lit_pel_2$littoral

lit8_comp<- mcd8_lit_pel_2$comp
lit6_comp<- mcd6_lit_pel_2$comp

ccf(lit8, lit6, lag=10000, type="correlation")
ccf(lit8, lit6, lag=3000, type="correlation")

ccf(lit8_comp, lit6_comp, lag=1000)



####Annual thermocline viz####
#2017
hobo_mcd_pel_2017$date <- paste(hobo_mcd_pel_2017$year, "-0",hobo_mcd_pel_2017$month, "-",hobo_mcd_pel_2017$day)

write.csv(hobo_mcd_pel_2017,"C:\\Users\\Quinn\\Documents\\R\\BMWS\\hobo_mcd_pel_2017_date.csv", row.names = FALSE)


pel_2017 <- as.tibble(hobo_mcd_pel_2017_time)



ggplot(pel_2017, aes(x = date, y = depth.m, colour = temp.c)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint = 20, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )


#Step 1
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- pel_2017 %>% 
    filter(date == target_date) %>%
    arrange(depth.m)
  approx(data_for_date$depth.m, data_for_date$temp.c, xout = target_depth)$y
}
estimate_temp_by_date(ymd("2017-06-29"), c(0.5, 2, 2.5))  


#Step 2
temp_interp_depth <- crossing(
  # the same dates as sonde_tbl_1993
  tibble(date = unique(pel_2017$date)),
  # depths can now be any value
  tibble(depth = seq(1, 3, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date(date[1], depth))

#Step 3
estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y
}

estimate_temp_by_depth(
  target_depth = 1, 
  target_date = seq(ymd("2017-06-01"), ymd("2017-06-29"), by = 1)
)



temp_raster <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2017-05-26"), ymd("2017-09-01"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(temp_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(depth[1], date))

ggplot(temp_raster, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 20, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  ) +
  coord_cartesian(expand = FALSE)


#2018 
hobo_mcd_pel_2018$date <- paste(hobo_mcd_pel_2018$year, "-0",hobo_mcd_pel_2018$month, "-",hobo_mcd_pel_2018$day)

write.csv(hobo_mcd_pel_2018,"C:\\Users\\Quinn\\Documents\\R\\BMWS\\hobo_mcd_pel_2018_date.csv", row.names = FALSE)


pel_2018 <- as.tibble(hobo_mcd_pel_2018_date)



ggplot(pel_2018, aes(x = date, y = depth.m, colour = temp.c)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint = 20, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )


#Step 1
estimate_temp_by_date_2 <- function(target_date, target_depth) {
  data_for_date <- pel_2018 %>% 
    filter(date == target_date) %>%
    arrange(depth.m)
  approx(data_for_date$depth.m, data_for_date$temp.c, xout = target_depth)$y
}
estimate_temp_by_date_2(ymd("2018-06-29"), c(1, 2))  


#Step 2
temp_interp_depth_2 <- crossing(
  # the same dates as sonde_tbl_1993
  tibble(date = unique(pel_2018$date)),
  # depths can now be any value
  tibble(depth = seq(1, 3.5, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date_2(date[1], depth))

#Step 3
estimate_temp_by_depth_2 <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth_2 %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y
}

estimate_temp_by_depth_2(
  target_depth = 1, 
  target_date = seq(ymd("2018-06-01"), ymd("2018-06-29"), by = 1)
)



temp_raster_2 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2018-05-04"), ymd("2018-09-11"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(temp_interp_depth_2$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth_2(depth[1], date))

ggplot(temp_raster_2, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 20, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  ) +
  coord_cartesian(expand = FALSE)


#2019
hobo_mcd_pel_2019$date <- paste(hobo_mcd_pel_2019$year, "-0",hobo_mcd_pel_2019$month, "-",hobo_mcd_pel_2019$day)

write.csv(hobo_mcd_pel_2019,"C:\\Users\\Quinn\\Documents\\R\\BMWS\\hobo_mcd_pel_2019_date.csv", row.names = FALSE)


pel_2019 <- as.tibble(hobo_mcd_pel_2019_date)



ggplot(pel_2019, aes(x = date, y = depth.m, colour = temp.c)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint = 20, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )


#Step 1
estimate_temp_by_date_3 <- function(target_date, target_depth) {
  data_for_date <- pel_2019 %>% 
    filter(date == target_date) %>%
    arrange(depth.m)
  approx(data_for_date$depth.m, data_for_date$temp.c, xout = target_depth)$y
}
estimate_temp_by_date_3(ymd("2019-06-29"), c(1, 2))  


#Step 2
temp_interp_depth_3 <- crossing(
  # the same dates as sonde_tbl_1993
  tibble(date = unique(pel_2019$date)),
  # depths can now be any value
  tibble(depth = seq(1, 3.5, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date_3(date[1], depth))

#Step 3
estimate_temp_by_depth_3 <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth_3 %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y
}

estimate_temp_by_depth_3(
  target_depth = 1, 
  target_date = seq(ymd("2019-06-01"), ymd("2019-06-29"), by = 1)
)



temp_raster_3 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2019-05-16"), ymd("2019-09-09"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(temp_interp_depth_3$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth_3(depth[1], date))

ggplot(temp_raster_3, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 15, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  ) +
  coord_cartesian(expand = FALSE)

####2023 Data#### 

#McD
ggplot(data = McDermott_littoral_trim) + 
  geom_point(mapping = aes(x = date, y =temp)) +
  facet_wrap(~common_ID)

#SP
ggplot(data = McDermott_littoral_trim) + 
  geom_point(mapping = aes(x = date, y =temp)) +
  facet_wrap(~common_ID)
