setwd("~/Documents/Bike_Share/bikeshare/src")

data = read.csv("metro-bike-share-trip-data.csv")
startingID <- data[ ,5]
startingID.t1 <- table(startingID)
startingID.t1
write.table(endingID.t1, "~/Documents/Bike_Share/bikeshare/src/table5.txt", sep="\t",  row.names = TRUE, col.names = TRUE)

endingID <- data[ ,8]
endingID.t1 <- table(endingID)
endingID.t1
write.table(startingID.t1, "~/Documents/Bike_Share/bikeshare/src/table6.txt", sep="\t",  row.names = TRUE, col.names = TRUE)

pass <- data[ ,14]
pass.t1 <- table(pass)
pass.t1

duration <- data[ ,2]
sum = 0;
for(i in 1:132427)
{
  sum = sum + ((duration[i] / 3600 )* 10)
}
avgD <- sum/132427
avgD

m1 <- mean(duration)
dur <- (m1 /3600) * 10
dur

date <- data[ ,3]
dats <- vector()

for(i in 1:132427)
{
  dats[i] = substring(date[i], 1, 10)
}
dats.t1 <- table(dats)
dats.t1
hist(dats)

meanDuration <- vector()

count = 1

for(i in 1:268)
{
  meanDuration[i] = 0;
  for(j in 1:dats.t1[[i]])
  {
    meanDuration[i] = meanDuration[i] + (duration[count] / 60)
    count = count +1
  }
  meanDuration[i] = meanDuration[i] / dats.t1[[i]]
}

dates <- vector()
counter = 1

for(i in 1:268)
{
  for(j in 1:dats.t1[[i]])
  {
    dates[i] = dats[counter]
    counter = counter +1
  }
}

write.table(dats.t1, "~/Documents/Bike_Share/bikeshare/src/table1.txt", sep="\t",  row.names = TRUE, col.names = TRUE)

write.table(dates, "~/Documents/Bike_Share/bikeshare/src/table2.txt", sep="\t",  row.names = FALSE, col.names = FALSE)
write.table(meanDuration, "~/Documents/Bike_Share/bikeshare/src/table3.txt", sep="\t",  row.names = FALSE, col.names = FALSE)

trip <- data[ ,13]
pass <- data[ ,14]

trip.flex <- vector()
iCount = 1
for(i in 1:132427)
{
  if(pass[i] == "Flex Pass")
  {
    trip.flex[iCount] = trip[i]
    iCount = iCount + 1
  }
}

flexType <- vector()

for(i in 1:9517)
{
  if(trip.flex[i] == 1)
  {
    flexType[i] = "One Way"
  }
  else
  {
    flexType[i] = "Round Trip"
  }
}

flexType.t1 <- table(flexType)
flexType.t1

trip.monthly <- vector()
iCounter = 1
for(i in 1:132427)
{
  if(pass[i] == "Monthly Pass")
  {
    trip.monthly[iCounter] = trip[i]
    iCounter = iCounter + 1
  }
}

monthlyType <- vector()

for(i in 1:81304)
{
  if(trip.monthly[i] == 1)
  {
    monthlyType[i] = "One Way"
  }
  else
  {
    monthlyType[i] = "Round Trip"
  }
}

monthlyType.t1 <- table(monthlyType)
monthlyType.t1

trip.daily <- vector()
jCounter = 1
for(i in 1:132427)
{
  if(pass[i] == "Walk-up")
  {
    trip.daily[jCounter] = trip[i]
    jCounter = jCounter + 1
  }
}

dailyType <- vector()

for(i in 1:41224)
{
  if(trip.daily[i] == 1)
  {
    dailyType[i] = "One Way"
  }
  else
  {
    dailyType[i] = "Round Trip"
  }
}

dailyType.t1 <- table(dailyType)
dailyType.t1

dayFlexV <- vector()
dayMonthV <- vector()
dayWalkV <- vector()
dayStaffV <- vector()

kCounter = 1
for(i in 1:268)
{
  dayFlexV[i] = 0
  dayMonthV[i] = 0
  dayWalkV[i] = 0
  dayStaffV[i] = 0
  for(j in 1:dats.t1[[i]])
  {
    if(pass[kCounter] == "Flex Pass")
    {
      dayFlexV[i] = dayFlexV[i] + 1
    }
    else if(pass[kCounter] == "Monthly Pass")
    {
      dayMonthV[i] = dayMonthV[i] + 1
    }
    else if(pass[kCounter] == "Walk-up")
    {
      dayWalkV[i] = dayWalkV[i] + 1
    }
    else
    {
      dayStaffV[i] = dayStaffV[i] + 1
    }
    kCounter = kCounter + 1
  }
}

passType.table = data.frame(dates, dayFlexV, dayMonthV, dayWalkV, dayStaffV)
write.table(passType.table, "~/Documents/Bike_Share/bikeshare/src/table3.txt", sep="\t",  row.names = FALSE, col.names = FALSE)

bikeID <- data[ ,11]
bikeID.t1 <- table(bikeID)
bikeID.t1

startDayOne <- vector()

for(i in 1:190)
{
  startDayOne[i] = startingID[i]
}

startDayOne.t1 <- table(startDayOne)
startDayOne.t1

mat<-matrix(NA, ncol = 2, nrow = )
count = 0

for(j in 1:268)
{
  endDayOne <- vector()
  for(i in 1:dates[i])
  {
    endDayOne[i] = endingID[count]
  }
  endDayOne.t1 <- table(endDayOne)
  endDayOne.t1
  for(k in 1:nrow(endDayOne.t1))
  {
    
  }
}
nrow(endDayOne.t1)

endDayOne.t1[[1]]


