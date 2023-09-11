####2023 Data#### 

#McD
ggplot(data = McDermott_littoral_trim) + 
  geom_point(mapping = aes(x = date, y =temp)) +
  facet_wrap(~common_ID)

#SP
ggplot(data = Sparkling_littoral_trim) + 
  geom_point(mapping = aes(x = date, y =temp)) +
  facet_wrap(~common_ID)

#ES
