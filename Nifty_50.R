library(Quandl)

data<- Quandl("NSE/NIFTY_50", api_key="KMKRiKFsPNez6Xg-LUby", 
              start_date = "2017-08-01", end_date="2018-07-31")
typeof(data)
head(data)

data_df <- as.data.frame(data)

write.csv(data_df, "/home/abhishek/Documents/DataScience/Projects/Nifty_50_Prediction/Data Sets/Nifty50_aug_july.csv")

#After writing this data to csv, i'll prefer csv for further operations instead of Quandl
getwd()
setwd("/home/abhishek/Documents/DataScience/Projects/Nifty_50_Prediction/Data Sets/")
nifty50<- read.csv("Nifty50_aug_july.csv")

#Backup for safty purpose
backup <- nifty50
nifty50 <- backup

#Convert to Date
nifty50$Date <- as.Date(nifty50$Date, "%Y-%m-%d")

#Convert to double using POSIXlt
nifty50$Date <-as.POSIXct(nifty50$Date,format='%Y-%m-%d')

today_close <- nifty50$Close
today_open <- nifty50$Open
prev_close <- nifty50$Close[2:length(nifty50$Close)]
high <- nifty50$High
low  <- nifty50$Low

today_close[length(today_close)] <- 0
today_open[length(today_open)] <- 0

#For Make below vectors of same length for doing operations
prev_close[length(prev_close)+1] <- 0

nifty50$diff_today_prev <- (today_close - prev_close)

#Set last value to 1 becoz we if take log(0/1) : it will be -Inf
today_close[length(today_close)] <- 1
today_open[length(today_open)] <- 1
prev_close[length(prev_close)] <- 1
nifty50$ln_rate_of_int <- round(log(today_close/prev_close),4)
nifty50$ln_high_low  <- round(log(high/low),4)
nifty50$ln_openToday_closePrev  <- round(log(today_open/prev_close),4)

head(nifty50)

#test of Data Normallity
shapiro.test(nifty50$Open)  #p<0.05 Accept H1: Not Normally Distributed
shapiro.test(nifty50$High)  #p<0.05 Accept H1: Not Normally Distributed
shapiro.test(nifty50$Low)   #p<0.05 Accept H1: Not Normally Distributed
shapiro.test(nifty50$Close) #p<0.05 Accept H1: Not Normally Distributed
shapiro.test(nifty50$diff_today_prev) #p<0.05 Accept H1: Not Normally Distributed
shapiro.test(nifty50$ln_rate_of_int) #p>0.05 Accept H0:Normally Distributed
shapiro.test(nifty50$ln_high_low) #p>0.05 Accept H0:Normally Distributed
shapiro.test(nifty50$ln_openToday_closePrev) #p>0.05 Accept H0:Normally Distributed
#test

#updating csv file
write.csv(nifty50, "/home/abhishek/Documents/DataScience/Projects/Nifty_50_Prediction/Data Sets/Nifty50_aug_july.csv")


