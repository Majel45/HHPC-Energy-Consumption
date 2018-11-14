# 1.1 Load libraries & Dataset ####
library(dplyr)

library(tidyr)

library(doMC)

library(xtable)

library(ggplot2)

library(lubridate)

library(reshape2)

library(TTR)

library(forecast)

library(gapminder)

#Setting Time Zone
Sys.setenv(TZ='Europe/Berlin') 

#Uploading Dataset
HHPC <- read.csv("household_power_consumption.txt",
sep = ";", header = TRUE)

#Rename column names
names(HHPC)<-c("Date","Time","GAP", 'GRP', 'Voltage', 'GI', 'Kitchen',
               'Laundry_Room', 'Heater_AC') 

# Factor trap: Factor - Character, Character -Numeric
cols = c(3, 4, 5, 6, 7, 8, 9)
HHPC[,cols] = apply(HHPC[,cols], 2, function(x) as.numeric(as.character(x)))
str(HHPC)

#Create Other (submetering 4)
HHPC$Other <- (HHPC$GAP*1000/60 - 
     HHPC$Kitchen - HHPC$Laundry_Room - HHPC$`Heater_AC`)

#Unit Conversion from Kilowatt per minute to Watt per minute
HHPC$GAP <- ((HHPC$GAP*1000/60))
HHPC$GRP <- ((HHPC$GRP*1000/60))

# Remove NA's 
is.na(HHPC)
HHPC <- na.omit(HHPC)

#Convert Date string to Date format
HHPC$Date <- as.Date(HHPC$Date,"%d/%m/%Y")
str(HHPC)

# 1.2 Extract & Create Month ####
HHPC <-cbind(HHPC, month(HHPC$Date, label = TRUE, 
       abbr = TRUE), stringsAsFactors=FALSE)
colnames(HHPC)[11] <-"Month"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]
head(HHPC)

#Extract & Create Year
HHPC <-cbind(HHPC, year(HHPC$Date), stringsAsFactors=FALSE)
colnames(HHPC)[12] <-"Year"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]
head(HHPC)

#Extract & Create Day
HHPC <-cbind(HHPC, day(HHPC$Date), stringsAsFactors=FALSE)
colnames(HHPC)[13] <-"Day"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]
head(HHPC)

#Extract & Create Weekday
HHPC <-cbind(HHPC, weekdays.POSIXt(HHPC$Date), stringsAsFactors=FALSE)
colnames(HHPC)[14] <-"Weekday"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]
head(HHPC)

#Convert Weekday to Factor
HHPC$Weekday<-as.factor(HHPC$Weekday)

#Convert Time(string) to time
strptime(HHPC$Time,"%H:%M:%S")

#Extract & Create Hour
HHPC <-cbind(HHPC, min(hms(HHPC$Time)), stringsAsFactors=FALSE)
colnames(HHPC)[15] <-"Hour"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]
head(HHPC)
str(HHPC)

#Get Season
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice (extreme)
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox (middle)
  
  # Convert dates from any year to 2012 dates (2012 is a leap year: every 4 years)
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter", # between the first WS & first SE
          ifelse (d >= SE & d < SS, "Spring", 
                  ifelse (d >= SS & d < FE, "Summer", "Autumn")))
}
 
HHPC <-cbind(HHPC, getSeason(HHPC$Date), stringsAsFactors=FALSE)
colnames(HHPC)[16] <-"Season"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]
head(HHPC)

# 1.3 Create simplified data & remove columns ####
HHPCsml <- HHPC
head(HHPCsml, 5)

#default main dataframe
HHPCMain <- HHPCsml

#Store Training set to create samples
HHPCsml <- HHPCsml[,-c(1,2,3)]
head(HHPCsml)
HHPCTrain <- HHPCsml

#Compressing Data to create Year & Month
HHPCsml <- group_by(HHPCsml, Year, Month)
HHPCsml

#Reducing dataset by using mean instead of observations
HHPCsml <- summarise(HHPCsml, MeanGAP = mean(GAP, na.rm = TRUE),
                     MeanGRP = mean(GRP, na.rm = TRUE),
                     MeanVolt = mean(Voltage, na.rm = TRUE),
                     MeanGI = mean(GI, na.rm = TRUE),
                     MeanKitchen = mean(Kitchen, na.rm = TRUE),
                     MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                     MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                     MeanOther = mean(Other))
HHPCsml <- arrange(HHPCsml, Year, Month)
head(HHPCsml)

#Calculate mean GAP, Kitchen, Laundry Room, Heater & AC, Other time series
GAP_vector <- vector(mode ="numeric")
GAP_vector <- HHPCsml$MeanGAP

Kitchen_vector <- vector(mode ="numeric")
Kitchen_vector <- HHPCsml$MeanKitchen

Laundry_Room_vector <- vector(mode ="numeric")
Laundry_Room_vector <- HHPCsml$MeanLaundry_Room

Heater_AC_vector <- vector(mode ="numeric")
Heater_AC_vector <- HHPCsml$MeanHeater_AC

Other_vector <- vector(mode ="numeric")
Other_vector <- HHPCsml$MeanOther
#GAP_vector <- select(HHPCsml, MeanKitchen, MeanLaundry_Room, MeanHeater_AC, MeanOther)

#Plot GAP, Kitchen, Laundry Room, Heater & AC, Other ts
GAP_ts <- ts(GAP_vector, frequency = 12, start = c(2006,12), end = c(2010,11))
plot.ts(GAP_ts)

Kitchen_ts <- ts(Kitchen_vector, frequency = 12, start = c(2006,12), end = c(2010,11))
plot.ts(Kitchen_ts)

Laundry_Room_ts <- ts(Laundry_Room_vector, frequency = 12, start = c(2006,12), 
                          end = c(2010,11))
plot.ts(Laundry_Room_ts)

Heater_AC_ts <- ts(Heater_AC_vector, frequency = 12, start = c(2006, 12),
                       end = c(2010, 11))
plot.ts(Heater_AC_ts)

Other_ts <- ts(Other_vector, frequency = 12, start = c(2006, 12), 
                   end = c(2010,11))
plot.ts(Other_ts)

#Smoothing GAP time series #no. of periods 
GAP_ts_SMA3 <- SMA(GAP_ts, n=3)
plot(GAP_ts_SMA3)

Kitchen_ts_SMA3 <- SMA(Kitchen_ts, n=3)
plot(Kitchen_ts_SMA3)

Laundry_Room_ts_SMA3 <- SMA(Laundry_Room_ts, n=3)
plot(Laundry_Room_ts_SMA3)

Heater_AC_ts_SMA3 <- SMA(Heater_AC_ts, n=3)
plot(Heater_AC_ts_SMA3)

Other_ts_SMA3 <- SMA(Other_ts, n=3)
plot(Other_ts_SMA3)

#Calculate time series components
GAP_ts_components <- decompose(GAP_ts)
Kitchen_ts_components <- decompose(Kitchen_ts)
Laundry_Room_ts_components <- decompose(Laundry_Room_ts)
Heater_AC_ts_components <- decompose(Heater_AC_ts)
Other_ts_components <- decompose(Other_ts)

#Mean GAP Seasonal component
GAP_ts_components$seasonal
autoplot(GAP_ts_components)

Kitchen_ts_components$seasonal
autoplot(Kitchen_ts_components)

Laundry_Room_ts_components$seasonal
autoplot(Laundry_Room_ts_components)

Heater_AC_ts_components$seasonal
autoplot(Heater_AC_ts_components)

Other_ts_components$seasonal
autoplot(Other_ts_components)

ggseasonplot(Laundry_Room_ts_components$trend, polar = T) +
  ylab("") +
  ggtitle("")

# 1.4 Reset main data frame 2008/July ####
HHPCsml <- HHPCMain 
head(HHPCsml)

#Create Sample dataframe 
HHPCSample <- subset(HHPCsml, HHPCsml$Date >= "2008-07-01" & HHPCsml$Date <= "2008-08-01")
head(HHPCSample)

#Compressing Data to create Month & Day
HHPCSample1 <- group_by(HHPCSample, Month, Day)
head(HHPCSample1)

#Reducing dataset by using mean instead of observations
HHPCSample1 <- summarise(HHPCSample1, MeanGAP = mean(GAP, na.rm = TRUE),
                     MeanGRP = mean(GRP, na.rm = TRUE),
                     MeanVolt = mean(Voltage, na.rm = TRUE),
                     MeanGI = mean(GI, na.rm = TRUE),
                     MeanKitchen = mean(Kitchen, na.rm = TRUE),
                     MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                     MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                     MeanOther = mean(Other))
head(HHPCSample1)
HHPCSample1 <- arrange(HHPCSample1, Day)
HHPCSample1

#Calculate mean GAP time series
GAP_vector <- vector(mode ="numeric")
GAP_vector <- select(HHPCSample1, MeanKitchen, MeanLaundry_Room, MeanHeater_AC, 
                     MeanOther)

GAP_ts <- ts(GAP_vector, frequency = 32, start = c(07, 01), end = c(08, 01))
plot.ts(GAP_ts)

# 1.5 Reset main data frame 2010/January ####
HHPCsml <- HHPCMain 
View(HHPCsml)

#Create Sample dataframe 
HHPCSample2 <- subset(HHPC, HHPC$Date >= "2010-01-01" & HHPC$Date <= "2010-02-01")

#Compressing Data to create Year & Month
HHPCSample2 <- group_by(HHPCSample2, Month, Day)
HHPCSample2

#Reducing dataset by using mean instead of observations
HHPCSample2 <- summarise(HHPCSample2, MeanGAP = mean(GAP, na.rm = TRUE),
                        MeanGRP = mean(GRP, na.rm = TRUE),
                        MeanVolt = mean(Voltage, na.rm = TRUE),
                        MeanGI = mean(GI, na.rm = TRUE),
                        MeanKitchen = mean(Kitchen, na.rm = TRUE),
                        MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                        MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                        MeanOther = mean(Other))
HHPCSample2 <- arrange(HHPCSample2, Month, Day)
HHPCSample2

#Calculate mean GAP time series
GAP_vector <- vector(mode ="numeric")
GAP_vector <- select(HHPCSample2, MeanKitchen, MeanLaundry_Room, MeanHeater_AC, 
                     MeanOther)

GAP_ts <- ts(GAP_vector, frequency = 32, start = c(01, 01), end = c(02, 01))

str(GAP_ts)

ts.plot(GAP_ts, gpars = list(col=rainbow(6)))

# 1.6 Reset main data frame Summer of 2008  ####
HHPCsml <- HHPCMain 
View(HHPCsml)

#Create Sample dataframe: Summer Time June til August 2008
HHPCSample3 <- subset(HHPC, HHPC$Date >= "2008-06-01" & HHPC$Date <= "2008-09-01")

#Compressing Data to create Weekday
HHPCSample3 <- group_by(HHPCSample3, Weekday)
HHPCSample3

#Reducing dataset by using mean instead of observations
HHPCSample3 <- summarise(HHPCSample3, MeanGAP = mean(GAP, na.rm = TRUE),
                         MeanGRP = mean(GRP, na.rm = TRUE),
                         MeanVolt = mean(Voltage, na.rm = TRUE),
                         MeanGI = mean(GI, na.rm = TRUE),
                         MeanKitchen = mean(Kitchen, na.rm = TRUE),
                         MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                         MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                         MeanOther = mean(Other))
HHPCSample3 <- arrange(HHPCSample3, Weekday)
HHPCSample3

#Order Weekday
HHPCSample3$Weekday <- factor(HHPCSample3$Weekday, levels= c("Monday", "Tuesday", 
                       "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

HHPCSample3 <- HHPCSample3[order(HHPCSample3$Weekday), ]

ggplot(HHPCSample3, aes(Weekday, group = 1)) + 
  geom_line(aes(y = MeanKitchen, colour = "MeanKitchen")) + 
  geom_line(aes(y = MeanLaundry_Room, colour = "MeanLaundry_Room")) +
  geom_line(aes(y = MeanHeater_AC, colour = "MeanHeater_AC")) +
  geom_line(aes(y = MeanOther, colour = "MeanOther")) + labs(y= 'Avg Watt/hour',
  title = ' 2008 Summer (06-08) Weekdays')


# 1.7 Reset main data frame: Winter of 2008 ####
HHPCsml <- HHPCMain 
View(HHPCsml)

#Create Sample dataframe: Winter Dec til February 2008/2009
HHPCSample4 <- subset(HHPC, HHPC$Date >= "2008-12-01" & HHPC$Date <= "2009-03-01")

#Compressing Data to create Year & Month
HHPCSample4 <- group_by(HHPCSample4, Weekday)
HHPCSample4

#Reducing dataset by using mean instead of observations
HHPCSample4 <- summarise(HHPCSample4, MeanGAP = mean(GAP, na.rm = TRUE),
                         MeanGRP = mean(GRP, na.rm = TRUE),
                         MeanVolt = mean(Voltage, na.rm = TRUE),
                         MeanGI = mean(GI, na.rm = TRUE),
                         MeanKitchen = mean(Kitchen, na.rm = TRUE),
                         MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                         MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                         MeanOther = mean(Other))
HHPCSample4 <- arrange(HHPCSample4, Weekday)
HHPCSample4

#Order Weekday
HHPCSample4$Weekday <- factor(HHPCSample4$Weekday,
                        levels= c("Monday",  "Tuesday", "Wednesday", 
                                  "Thursday", "Friday", "Saturday", "Sunday"))

HHPCSample4 <- HHPCSample4[order(HHPCSample3$Weekday), ]

ggplot(HHPCSample4, aes(Weekday, group = 1)) + 
  geom_line(aes(y = MeanKitchen, colour = "MeanKitchen")) + 
  geom_line(aes(y = MeanLaundry_Room, colour = "MeanLaundry_Room")) +
  geom_line(aes(y = MeanHeater_AC, colour = "MeanHeater_AC")) +
  geom_line(aes(y = MeanOther, colour = "MeanOther")) + labs(y= 'Avg Watt/hour',
  title = '2008/2009 Winter (12-02) Weekdays')

# 1.8 Reset main data frame: 2009 Winter Hours of Weekend (Saturday) ####
HHPCsml <- HHPCMain 
View(HHPCsml)

#Create Sample dataframe Hours of Saturday
HHPCSample5 <- subset(HHPC, HHPC$Date == "2009-01-24")

#Compressing Data to create Year & Month
HHPCSample5 <- group_by(HHPCSample5, Hour)
HHPCSample5

#Reducing dataset by using mean instead of observations
HHPCSample5 <- summarise(HHPCSample5, MeanGAP = mean(GAP, na.rm = TRUE),
                         MeanGRP = mean(GRP, na.rm = TRUE),
                         MeanVolt = mean(Voltage, na.rm = TRUE),
                         MeanGI = mean(GI, na.rm = TRUE),
                         MeanKitchen = mean(Kitchen, na.rm = TRUE),
                         MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                         MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                         MeanOther = mean(Other))
HHPCSample5 <- arrange(HHPCSample5, Hour)
HHPCSample5

ggplot(HHPCSample5, aes(Hour, group = 1)) + 
  geom_line(aes(y = MeanKitchen, colour = "MeanKitchen")) + 
  geom_line(aes(y = MeanLaundry_Room, colour = "MeanLaundry_Room")) +
  geom_line(aes(y = MeanHeater_AC, colour = "MeanHeater_AC")) +
  geom_line(aes(y = MeanOther, colour = "MeanOther")) + labs(y= 'Avg Watt/hour',
  title = 'January 2009 Weekend (Saturday)')

#Pie Chart
HHPCSample5 <- subset(HHPC, HHPC$Date == "2009-01-24")
HHPCSample5_WHourSat <- summarise(HHPCSample5, MeanGAP = mean(GAP, na.rm = TRUE),
                         MeanGRP = mean(GRP, na.rm = TRUE),
                         MeanVolt = mean(Voltage, na.rm = TRUE),
                         MeanGI = mean(GI, na.rm = TRUE),
                         MeanKitchen = mean(Kitchen, na.rm = TRUE),
                         MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                         MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                         MeanOther = mean(Other))
head(HHPCSample5_WHourSat)
HHPCSample5_WHourSat = melt(HHPCSample5_WHourSat, id.vars = c("MeanGAP",
                                                          "MeanGRP", "MeanVolt", "MeanGI")) # melt means to convert columns into rows
head(HHPCSample5_WHourSat)
HHPCSample5_WHourSat <- HHPCSample5_WHourSat[, -c(1:4)]

#Percentage out of total
HHPCSample5_WHourSat$value <- HHPCSample5_WHourSat$value /sum(HHPCSample5_WHourSat$value)*100

head(HHPCSample5_WHourSat)

ggplot(HHPCSample5_WHourSat, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+ labs(x= "", y="Saturday (24) January 2009: Appliance Average")

# 1.9 Reset main data frame: 2009 Winter Hours of Weekday (Thursday) ####
HHPCsml <- HHPCMain 
View(HHPCsml)

#Create Sample dataframe Hours of Thursday in Winter
HHPCSample6 <- subset(HHPC, HHPC$Date == "2009-01-29")

#Compressing Data to create Year & Month
HHPCSample6 <- group_by(HHPCSample6, Hour)
HHPCSample6

#Reducing dataset by using mean instead of observations
HHPCSample6 <- summarise(HHPCSample6, MeanGAP = mean(GAP, na.rm = TRUE),
                         MeanGRP = mean(GRP, na.rm = TRUE),
                         MeanVolt = mean(Voltage, na.rm = TRUE),
                         MeanGI = mean(GI, na.rm = TRUE),
                         MeanKitchen = mean(Kitchen, na.rm = TRUE),
                         MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                         MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                         MeanOther = mean(Other))
HHPCSample6 <- arrange(HHPCSample6, Hour)
HHPCSample6

ggplot(HHPCSample6, aes(Hour, group = 1)) + 
  geom_line(aes(y = MeanKitchen, colour = "MeanKitchen")) + 
  geom_line(aes(y = MeanLaundry_Room, colour = "MeanLaundry_Room")) +
  geom_line(aes(y = MeanHeater_AC, colour = "MeanHeater_AC")) +
  geom_line(aes(y = MeanOther, colour = "MeanOther")) + labs(y= 'Avg Watt/hour',
  title = 'January 2009 Weekday (Thursday)')

#Pie Chart
HHPCSample6 <- subset(HHPC, HHPC$Date == "2009-01-29")
HHPCSample6_WHourThur <- summarise(HHPCSample6, MeanGAP = mean(GAP, na.rm = TRUE),
                                  MeanGRP = mean(GRP, na.rm = TRUE),
                                  MeanVolt = mean(Voltage, na.rm = TRUE),
                                  MeanGI = mean(GI, na.rm = TRUE),
                                  MeanKitchen = mean(Kitchen, na.rm = TRUE),
                                  MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                                  MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                                  MeanOther = mean(Other))
head(HHPCSample6_WHourThur)
HHPCSample6_WHourThur = melt(HHPCSample6_WHourThur, id.vars = c("MeanGAP",
                                                              "MeanGRP", "MeanVolt", "MeanGI")) # melt means to convert columns into rows
head(HHPCSample6_WHourThur)
HHPCSample6_WHourThur <- HHPCSample6_WHourThur[, -c(1:4)]

#Percentage out of total
HHPCSample6_WHourThur$value <- HHPCSample6_WHourThur$value /sum(HHPCSample6_WHourThur$value)*100

head(HHPCSample6_WHourThur)

ggplot(HHPCSample6_WHourThur, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+ labs(x= "", y="Thursday (29) January 2009: Appliance Average")

# 1.10 Reset main data frame: Create Seasons  ####
HHPCsml <- HHPCMain 
View(HHPCsml)

#Compressing Data to create Weekday
HHPCSample7 <- group_by(HHPCsml, Season)
HHPCSample7

#Reducing dataset by using mean instead of observations
HHPCSample7 <- summarise(HHPCSample7, MeanGAP = mean(GAP, na.rm = TRUE),
                         MeanGRP = mean(GRP, na.rm = TRUE),
                         MeanVolt = mean(Voltage, na.rm = TRUE),
                         MeanGI = mean(GI, na.rm = TRUE),
                         MeanKitchen = mean(Kitchen, na.rm = TRUE),
                         MeanLaundry_Room = mean(Laundry_Room, na.rm = TRUE),
                         MeanHeater_AC = mean(Heater_AC, na.rm = TRUE),
                         MeanOther = mean(Other))
HHPCSample7 <- arrange(HHPCSample7, Season)
HHPCSample7

#Order Season
HHPCSample7$Season <- factor(HHPCSample7$Season, levels= c("Winter", 
                      "Autumn","Summer", "Spring"))

HHPCSample7 <- HHPCSample7[order(HHPCSample7$Season), ]

ggplot(HHPCSample7, aes(Season, group = 1)) + 
  geom_line(aes(y = MeanKitchen, colour = "MeanKitchen")) + 
  geom_line(aes(y = MeanLaundry_Room, colour = "MeanLaundry_Room")) +
  geom_line(aes(y = MeanHeater_AC, colour = "MeanHeater_AC")) +
  geom_line(aes(y = MeanOther, colour = "MeanOther")) + labs(y= 'Avg Watt/hour',
  title = 'Seasons of Dec 2006- Nov 2010')

# 1.11 Create Season Pie Charts ####
#Winter
HHPCSample7_winter <- subset(HHPCSample7, HHPCSample7$Season == "Winter")
HHPCSample7_winter = melt(HHPCSample7_winter, id.vars = c("Season","MeanGAP",
  "MeanGRP", "MeanVolt", "MeanGI")) # melt means to convert columns into rows
HHPCSample7_winter <- HHPCSample7_winter[, -c(1:5)]
head(HHPCSample7_winter)

#Percentage out of total
HHPCSample7_winter$value <- HHPCSample7_winter$value /sum(HHPCSample7_winter$value)*100
ggplot(HHPCSample7_winter, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+ labs(x= '',
                                    y="Winter Appliance Average")

#Spring
HHPCSample7_spring <- subset(HHPCSample7, HHPCSample7$Season == "Spring")
HHPCSample7_spring = melt(HHPCSample7_spring, id.vars = c("Season","MeanGAP",
                                                          "MeanGRP", "MeanVolt", "MeanGI")) # melt means to convert columns into rows
HHPCSample7_spring <- HHPCSample7_spring[, -c(1:5)]
head(HHPCSample7_spring)

#Percentage out of total
HHPCSample7_spring$value <- HHPCSample7_spring$value /sum(HHPCSample7_spring$value)*100
ggplot(HHPCSample7_spring, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+ labs(x= '', y="Spring Appliance Average")

#Summer
HHPCSample7_summer <- subset(HHPCSample7, HHPCSample7$Season == "Summer")
HHPCSample7_summer = melt(HHPCSample7_summer, id.vars = c("Season","MeanGAP",
                                                          "MeanGRP", "MeanVolt", "MeanGI")) # melt means to convert columns into rows
HHPCSample7_summer <- HHPCSample7_summer[, -c(1:5)]
head(HHPCSample7_summer)

#Percentage out of total
HHPCSample7_summer$value <- HHPCSample7_summer$value /sum(HHPCSample7_summer$value)*100
ggplot(HHPCSample7_summer, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+ labs(x= '', y="Summer Appliance Average")

#Autumn
HHPCSample7_autumn <- subset(HHPCSample7, HHPCSample7$Season == "Autumn")
HHPCSample7_autumn = melt(HHPCSample7_autumn, id.vars = c("Season","MeanGAP",
                                                          "MeanGRP", "MeanVolt", "MeanGI")) # melt means to convert columns into rows
HHPCSample7_autumn <- HHPCSample7_autumn[, -c(1:5)]
head(HHPCSample7_autumn)
#Percentage out of total
HHPCSample7_autumn$value <- HHPCSample7_autumn$value /sum(HHPCSample7_autumn$value)*100
ggplot(HHPCSample7_autumn, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+ labs(x= '', y="Autumn Appliance Average")

# 1.12 Forecasting using HoltWinters #### Seasonal gamma=T ####
GAPforecasts_noSeason <- HoltWinters(GAP_ts, beta=FALSE, gamma=F)
GAPforecasts_noSeason

GAPforecasts_Season <- HoltWinters(GAP_ts, beta = F, gamma = T)
GAPforecasts_Season

#Kitchen ts: Non-seasonal and seasonal
Kitchen_forecasts_noSeason <- HoltWinters(Kitchen_ts, beta = F, gamma = F)
Kitchen_forecasts_noSeason

Kitchen_forecasts_Season <- HoltWinters(Kitchen_ts, beta = F, gamma = T)
Kitchen_forecasts_Season

#Laundry room ts: Non seasonal and seasonal
LR_forecasts_nonSeason <- HoltWinters(Laundry_Room_ts, beta = F, gamma = F)
LR_forecasts_nonSeason

LR_forecasts_Season <- HoltWinters(Laundry_Room_ts, beta = F, gamma = T)
LR_forecasts_Season

#Heater & AC ts: Non seasonal and seasonal
HAC_forecasts_nonSeason <- HoltWinters(Heater_AC_ts, beta = F, gamma = F)
HAC_forecasts_nonSeason

HAC_forecasts_Season <- HoltWinters(Heater_AC_ts, beta = F, gamma = T)
HAC_forecasts_Season

#Other ts: Non seasonal and seasonal
Other_forecasts_nonSeason <- HoltWinters(Other_ts, beta = F, gamma = F)
Other_forecasts_nonSeason

Other_forecasts_Season <- HoltWinters(Other_ts, beta = F, gamma = T)
Other_forecasts_Season

# alpha = 0.9999472; beta = FALSE; gamma = FALSE; Coefficients: [,1] a 1.196853

# Mean Global Active Power fitted (training the model)
GAPforecasts_noSeason$fitted
plot(GAPforecasts_noSeason)

GAPforecasts_Season$fitted
plot(GAPforecasts_Season)

#Kitchen fitted
Kitchen_forecasts_noSeason$fitted
plot(Kitchen_forecasts_noSeason)

Kitchen_forecasts_Season$fitted
plot(Kitchen_forecasts_Season)

#Laundry fitted
LR_forecasts_nonSeason$fitted
plot(LR_forecasts_nonSeason)

LR_forecasts_Season$fitted
plot(LR_forecasts_Season)

#Water Heater & AC fitted
HAC_forecasts_nonSeason$fitted
plot(HAC_forecasts_nonSeason)

HAC_forecasts_Season$fitted
plot(HAC_forecasts_Season)

#Other fitted
Other_forecasts_nonSeason$fitted
plot(Other_forecasts_nonSeason)

Other_forecasts_Season$fitted
plot(Other_forecasts_Season)

#Sum of squared errors = 2.318598
GAPforecasts$SSE

#Forecasting for further time plots
GAPforecasts2 <- forecast(GAPforecasts, h=1)
GAPforecasts2
plot(GAPforecasts2)

#Forecasting Kitchen time plots
Kitchen_forecasts2 <- forecast(Kitchen_forecasts, h=1)
plot(Kitchen_forecasts2)

#Forecasting Laundry Room time plots
Laundry_Room_forecasts2 <- forecast(Laundry_Room_forecasts, h=1)
plot(Laundry_Room_forecasts2)

#Forecasting Water Heater & AC time plots
Heater_AC_forecasts2 <- forecast(Heater_AC_forecasts, h=1)
plot(Heater_AC_forecasts2)

#Forecasting Othertime plots
Other_forecasts2 <- forecast(Other_forecasts, h=1)
plot(Other_forecasts2)

#Holt's exponential smoothing
GAPforecastsS <- HoltWinters(GAP_ts, gamma=FALSE)
plot(GAPforecastsS)

Kitchen_forcastS <- HoltWinters(Kitchen_ts, gamma = F)
plot(Kitchen_forcastS)

Laundry_Room_forcastS <- HoltWinters(Laundry_Room_ts, gamma = F)
plot(Laundry_Room_forcastS)

Heater_AC_forcastS <- HoltWinters(Heater_AC_ts, gamma = F)
plot(Heater_AC_forcastS)

Other_forcastS <- HoltWinters(Other_ts, gamma = F)
plot(Laundry_Room_forcastS)

# GAP forecast's smoothing parameters:
GAPforecastsS
Laundry_Room_forcastS

# Smoothing parameters: alpha: 1; beta : 0.2295635; gamma: FALSE
# Coefficients:[,1] ; a 1.19685447

# GAP forecast sum of squared errors = 2.696654
GAPforecastsS$SSE

#Forecasting for further time plots
GAPforecastsS2 <- forecast(GAPforecastsS, h=1)
plot(GAPforecastsS2)

# 1.13 Forecasting using linear models ####
GAPforecast_tslm <- tslm(GAP_ts ~ trend + season)
summary(GAPforecast_tslm)
GAPforecast_tslm_forecast <- forecast(GAPforecast_tslm, h=1, level = 0.95)
plot(GAPforecast_tslm_forecast)

Kitchen_forecast_tslm = tslm(Kitchen_ts ~ trend + season)
summary(Kitchen_forecast_tslm)
Kitchen_tslm_forecast <- forecast(Kitchen_forecast_tslm, h=1, level = 0.95)
plot(Kitchen_forecast_tslm)

Laundry_Room_forecast_tslm = tslm(Laundry_Room_ts ~ trend + season)
summary(Laundry_Room_forecast_tslm)
Laundry_Room_tslm_forecast <- forecast(Laundry_Room_forecast_tslm, h=1, level = 0.95)
plot(Laundry_Room_tslm_forecast)
