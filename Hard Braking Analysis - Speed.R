setwd("/Users/mac/Desktop/Dash")
library('dplyr')
s
#import data
Total25 <- read.csv("Total_25s.csv")

#get time
x = as.POSIXct(Total25$datetime, origin = "1970-01-01")
Total25$datetimehuman <- x
Total25$datetimehuman <- format(as.POSIXct(strptime(Total25$datetimehuman,"%Y-%m-%d %H:%M:%S", tz="")), format = "%H:%M:%S")

sample <- Total25[c(1:1000),]

#>10 to 0 in 1 second
Total25_brake_1 <- Total25 %>% 
  filter((datetime - lag(datetime)<2) & (lag(mph) - mph >6) & mph < 1)

#?20 to 0 in 2 to 5 seconds
Total25_brake_2 <- Total25 %>% 
  filter((datetime - lag(datetime) >2) & (datetime - lag(datetime) <5) & (lag(mph) - mph >20) & mph < 1)

write.csv(Total25_brake_1, "Total25_Brake_1.csv")

#merging rows
HB_Streets <- read.csv("HB_Streets.csv")
HB_Streets <- HB_Streets[c(1,13,16)]
HB_Streets_1 <- HB_Streets %>% group_by(field_1) %>% summarise(val=paste(street, collapse=" AND "))

require(data.table)
setDT(HB_Streets); setDT(HB_Streets_1) # convert to data.tables by reference
HB_Streets_2 <- HB_Streets[HB_Streets_1, mult = "first", on = "field_1", nomatch=0L]

write.csv(HB_Streets_2, "Hard Braking Streets.csv")

#demograhpics
HardBrakes <- read.csv("HardBrakesSolved.csv")   
All_IDs <- read.csv("All_IDS.csv")

unique(HardBrakes$id)    

HardBrakesID <- merge(x=HardBrakes, y=All_IDs, by="id",all.x = TRUE)

Cars <-aggregate(HardBrakesID, by=list(HardBrakesID$vehiclemake,HardBrakesID$vehiclemodel), 
                    FUN=NROW)
Cars <- Cars[c(1,2,3)]
write.csv(Cars, "Hard Brakes - Car.csv")      


Driver <-aggregate(HardBrakesID, by=list(HardBrakesID$id), 
                 FUN=NROW)
Driver <- Driver[c(1,2)]
write.csv(Driver, "Hard Brakes - Driver.csv")      
