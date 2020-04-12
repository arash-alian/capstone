##### mars########

mars <- read.csv("mars-weather.csv")
str(mars)
summary(mars)
head(mars,250)
tail(mars,250)

################### backing up the data set#####

mars_backup <- mars

################### removing unwanted columns from the data#########

mars$id <- NULL
mars$sol <- NULL
mars$ls <- NULL
mars$pressure <- NULL
mars$wind_speed < NULL
mars$atmo_opacity <- NULL
mars$wind_speed <- NULL

str(mars)


################### cleaning the data
mars <- mars[-c(1:32),]
head(mars,10)
mars <- mars[-c(1849:1894),]
tail(mars,10)
rownames(mars) <- NULL
head(mars,25)
tail(mars,25)


### 1 year on Mars is 687 days on Earth
### in order to make month by month graph for the temperature
### we need to transfer Earth months and years to Mars months and years

year1 <- mars[c(1816:1546),]
year2 <- mars[c(1545:914),]
year3 <- mars[c(913:257),]
year4 <- mars[c(256:1),]

################### Removing NA values for the 4 years and using median imputation method to proxy the missing values

#### year1


year1[!complete.cases(year1),]

maxmediany1m9 <- median(year1[year1$month=="Month 9","max_temp"],na.rm=TRUE)
year1[is.na(year1$max_temp) & year1$month=="Month 9",] 
year1[is.na(year1$max_temp) & year1$month=="Month 9","max_temp"] <- maxmediany1m9

minmediany1m9 <- median(year1[year1$month=="Month 9","min_temp"],na.rm=TRUE)
year1[is.na(year1$min_temp) & year1$month=="Month 9",] 
year1[is.na(year1$min_temp) & year1$month=="Month 9","min_temp"] <- minmediany1m9




maxmediany1m10 <- median(year1[year1$month=="Month 10","max_temp"],na.rm=TRUE)
year1[is.na(year1$max_temp) & year1$month=="Month 10",] 
year1[is.na(year1$max_temp) & year1$month=="Month 10","max_temp"] <- maxmediany1m10

minmediany1m10 <- median(year1[year1$month=="Month 10","min_temp"],na.rm=TRUE)
year1[is.na(year1$min_temp) & year1$month=="Month 10",] 
year1[is.na(year1$min_temp) & year1$month=="Month 10","min_temp"] <- minmediany1m10




maxmediany1m11 <- median(year1[year1$month=="Month 11","max_temp"],na.rm=TRUE)
year1[is.na(year1$max_temp) & year1$month=="Month 11",] 
year1[is.na(year1$max_temp) & year1$month=="Month 11","max_temp"] <- maxmediany1m11

minmediany1m11 <- median(year1[year1$month=="Month 11","min_temp"],na.rm=TRUE)
year1[is.na(year1$min_temp) & year1$month=="Month 11",] 
year1[is.na(year1$min_temp) & year1$month=="Month 11","min_temp"] <- minmediany1m11


##### year 2

year2[!complete.cases(year2),]

maxmediany2m1 <- median(year2[year2$month=="Month 1","max_temp"],na.rm=TRUE)
year2[is.na(year2$max_temp) & year2$month=="Month 1",] 
year2[is.na(year2$max_temp) & year2$month=="Month 1","max_temp"] <- maxmediany2m1

minmediany2m1 <- median(year2[year2$month=="Month 1","min_temp"],na.rm=TRUE)
year2[is.na(year2$min_temp) & year2$month=="Month 1",] 
year2[is.na(year2$min_temp) & year2$month=="Month 1","min_temp"] <- minmediany2m1



maxmediany2m2 <- median(year2[year2$month=="Month 2","max_temp"],na.rm=TRUE)
year2[is.na(year2$max_temp) & year2$month=="Month 2",] 
year2[is.na(year2$max_temp) & year2$month=="Month 2","max_temp"] <- maxmediany2m2

minmediany2m2 <- median(year2[year2$month=="Month 2","min_temp"],na.rm=TRUE)
year2[is.na(year2$min_temp) & year2$month=="Month 2",] 
year2[is.na(year2$min_temp) & year2$month=="Month 2","min_temp"] <- minmediany2m2



maxmediany2m3 <- median(year2[year2$month=="Month 3","max_temp"],na.rm=TRUE)
year2[is.na(year2$max_temp) & year2$month=="Month 3",] 
year2[is.na(year2$max_temp) & year2$month=="Month 3","max_temp"] <- maxmediany2m3

minmediany2m3 <- median(year2[year2$month=="Month 3","min_temp"],na.rm=TRUE)
year2[is.na(year2$min_temp) & year2$month=="Month 3",] 
year2[is.na(year2$min_temp) & year2$month=="Month 3","min_temp"] <- minmediany2m3



maxmediany2m10 <- median(year2[year2$month=="Month 10","max_temp"],na.rm=TRUE)
year2[is.na(year2$max_temp) & year2$month=="Month 10",] 
year2[is.na(year2$max_temp ) & year2$month=="Month 10","max_temp"] <- maxmediany2m10

minmediany2m10 <- median(year2[year2$month=="Month 10","min_temp"],na.rm=TRUE)
year2[is.na(year2$min_temp) & year2$month=="Month 10",] 
year2[is.na(year2$min_temp) & year2$month=="Month 10","min_temp"] <- minmediany2m10



maxmediany2m11 <- median(year2[year2$month=="Month 11","max_temp"],na.rm=TRUE)
year2[is.na(year2$max_temp) & year2$month=="Month 11",] 
year2[is.na(year2$max_temp) & year2$month=="Month 11","max_temp"] <- maxmediany2m11

minmediany2m11 <- median(year2[year2$month=="Month 11","min_temp"],na.rm=TRUE)
year2[is.na(year2$min_temp) & year2$month=="Month 11",] 
year2[is.na(year2$min_temp) & year2$month=="Month 11","min_temp"] <- minmediany2m11




#### year 3

year3[!complete.cases(year3),]


maxmediany3m11 <- median(year3[year3$month=="Month 11","max_temp"],na.rm=TRUE)
year3[is.na(year3$max_temp) & year3$month=="Month 11",] 
year3[is.na(year3$max_temp) & year3$month=="Month 11","max_temp"] <- maxmediany3m11

minmediany3m11 <- median(year3[year3$month=="Month 11","min_temp"],na.rm=TRUE)
year3[is.na(year3$min_temp) & year3$month=="Month 11",] 
year3[is.na(year3$min_temp) & year3$month=="Month 11","min_temp"] <- minmediany3m11



### year 4

year4[!complete.cases(year4),]

### no NA values

################### graph the years

### Year 1


amax <- ggplot(data=year1,aes(x=date, y=max_temp,colour=month))
amax + geom_line() + geom_point()

amin <- ggplot(data=year1,aes(x=date, y=min_temp,colour=month))
amin + geom_line() + geom_point()


### Year 2


b <- ggplot(data=year2,aes(x=date, y=max_temp,colour=month))
b + geom_line() + geom_point()

c <- ggplot(data=year2,aes(x=date, y=min_temp,colour=month))
c + geom_line() + geom_point()


### Year 3


dmax <- ggplot(data=year3,aes(x=date, y=max_temp,colour=month))
dmax + geom_line() + geom_point()

dmin <- ggplot(data=year3,aes(x=date, y=min_temp,colour=month))
dmin + geom_line() + geom_point()


### Year 4


emax <- ggplot(data=year4,aes(x=date, y=max_temp,colour=month))
emax + geom_line() + geom_point()

emin <- ggplot(data=year4,aes(x=date, y=min_temp,colour=month))
emin + geom_line() + geom_point()


################### Changing terrestrial_date from factor to timestamp

year1$date<- as.POSIXct(year1$terrestrial_date, format="%Y-%m-%d")
typeof(date)
year1$terrestrial_date <- NULL
year1 <- year1[,c(4,2,1,3)]

year2$date<- as.POSIXct(year2$terrestrial_date, format="%Y-%m-%d")
typeof(date)
year2$terrestrial_date <- NULL
year2 <- year2[,c(4,2,1,3)]

year3$date<- as.POSIXct(year3$terrestrial_date, format="%Y-%m-%d")
typeof(date)
year3$terrestrial_date <- NULL
year3 <- year3[,c(4,1,2,3)]

year4$date<- as.POSIXct(year4$terrestrial_date, format="%Y-%m-%d")
typeof(date)
year4$terrestrial_date <- NULL
year4 <- year4[,c(4,1,2,3)]









