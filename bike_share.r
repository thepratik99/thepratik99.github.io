"Put the path of your working directory in the quotes"
setwd("../src")

"Read the csv file and store it in the variable data"
data = read.csv("metro-bike-share-trip-data.csv")
"number of rows in data"
row <- nrow(data)

startingID <- data[ ,5]
"make a table of the vector it will give the frequency of each Station"
startingID.t1 <- table(startingID)
"make a csv file of the table so that we can sort it and find most popular stations"
write.table(startingID.t1, "~/Documents/BikeShare/startingID.csv", sep="\t",  row.names = TRUE, col.names = TRUE)

endingID <- data[ ,8]
"make a table of the vector it will give the frequency of each Station"
endingID.t1 <- table(endingID)
"make a csv file of the table so that we can sort it and find most popular stations"
write.table(endingID.t1, "~/Documents/BikeShare/endingID.csv", sep="\t",  row.names = TRUE, col.names = TRUE)

"get the duration column in the duration vector"
duration <- data[ ,2]
"taking a mean of the vector"
m1 <- mean(duration)
"Suppose that the average biking speed is 10kmph
  Since time we have given is in seconds divide the mean by 3600 and multiply 10
  multiply by 5/8 to get the distance in miles"
distance <- ((m1 /3600) * 10) * (5/8)
distance

date <- data[ ,3]
"make a substring of each string in the date vector to only get the date"
dates <- vector()
for(i in 1:row)
{
  dates[i] = substring(date[i], 1, 10)
}
"making a table will help us figure out the frequency of each date"
dates.t1 <- table(dates)
numberOfDays <- nrow(dates.t1)

"make a vector to store the number of trips on each day"
numberOfTrips <- vector()
for(i in 1:numberOfDays)
{
  numberOfTrips[i] = dates.t1[[i]]
}

"make a vector for the dates that will be same size of numberOfTrips vector"
dats <- vector()
counter = 1
for(i in 1:numberOfDays)
{
  dats[i] = dates[counter]
  counter = counter + dates.t1[[i]]
}

trip <- data[ ,13]
pass <- data[ ,14]

"make vectors to store the number of trips done with certain type"
dayFlexV <- vector()
dayMonthV <- vector()
dayWalkV <- vector()
dayStaffV <- vector()

"Edit the vector according to given data set"
count = 1
for(i in 1:numberOfDays)
{
  dayFlexV[i] = 0
  dayMonthV[i] = 0
  dayWalkV[i] = 0
  dayStaffV[i] = 0
  for(j in 1:dates.t1[[i]])
  {
    if(pass[count] == "Flex Pass")
    {
      dayFlexV[i] = dayFlexV[i] + 1
    }
    else if(pass[count] == "Monthly Pass")
    {
      dayMonthV[i] = dayMonthV[i] + 1
    }
    else if(pass[count] == "Walk-up")
    {
      dayWalkV[i] = dayWalkV[i] + 1
    }
    else
    {
      dayStaffV[i] = dayStaffV[i] + 1
    }
    count = count + 1
  }
}

"make a data frame and export it as a csv file to analysize the data and graph it"
passType.table = data.frame(dats,numberOfTrips, dayFlexV, dayMonthV, dayWalkV, dayStaffV)
write.table(passType.table, "~/Documents/BikeShare/typesOfPasses.csv", sep="\t",  row.names = TRUE, col.names = TRUE)

"make a vector to store the type of the trip only with flex pass"
trip.flex <- vector()
flexCount = 1
for(i in 1:row)
{
  if(pass[i] == "Flex Pass")
  {
    trip.flex[flexCount] = trip[i]
    flexCount = flexCount + 1
  }
}

"convert 1 and 2's to One Way or Round Trip and make a table to get frequency"
flexType <- vector()
for(i in 1:(flexCount-1))
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

"make a vector to store the type of the trip only with monthly pass"
trip.monthly <- vector()
monthCount = 1
for(i in 1:row)
{
  if(pass[i] == "Monthly Pass")
  {
    trip.monthly[monthCount] = trip[i]
    monthCount = monthCount + 1
  }
}

"convert 1 and 2's to One Way or Round Trip and make a table to get frequency"
monthlyType <- vector()
for(i in 1:(monthCount-1))
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

"make a vector to store the type of the trip only with walk-ups"
trip.daily <- vector()
dailyCount = 1
for(i in 1:row)
{
  if(pass[i] == "Walk-up")
  {
    trip.daily[dailyCount] = trip[i]
    dailyCount = dailyCount + 1
  }
}

"convert 1 and 2's to One Way or Round Trip and make a table to get frequency"
dailyType <- vector()
for(i in 1:(dailyCount-1))
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

"make a data frame of the frequency table and export it as a csv file so that we can make a graph"
type <- data.frame(flexType.t1, monthlyType.t1, dailyType.t1)
write.table(type, "~/Documents/BikeShare/TripRoute-Pass.csv", sep="\t",  row.names = FALSE, col.names = TRUE)
