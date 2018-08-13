library(Quandl)

data<- Quandl("NSE/NIFTY_50", api_key="KMKRiKFsPNez6Xg-LUby", start_date = "2017-08-01", end_date="2018-08-11")
typeof(data)
head(data)

data_df <- as.data.frame(data)
is.data.frame(data_df)

#logarithmic rate of return : log(today close/pre.day close)
fun_log_roi <- function(x){
  val <- 0
  if(is.na(log(data_df[x,5]/data_df[x+1,5]))){
    val <- 0
  }else{
    val <- round(log(data_df[x,5]/data_df[x+1,5]),4)
  } 
}

for(i in 1:length(data_df$Close)){
  data_df[i,8] <- fun_log_roi(i)
}

colnames(data_df)[8] <- "Rate of Interest"

nrow(data_df)
#test of Data Normallity
shapiro.test(data_df$Open)  #p>0.05 Accept H0:Normally Distributed
shapiro.test(data_df$High)  #p>0.05 Accept H0:Normally Distributed
shapiro.test(data_df$Low)   #p>0.05 Accept H0:Normally Distributed
shapiro.test(data_df$Close) #p>0.05 Accept H0:Normally Distributed
shapiro.test(data_df$`Rate of Interest`) #p>0.05 Accept H0:Normally Distributed

c<- data_df$Open
c<-sort(c, decreasing = F)
shapiro.test(c) #p>0.05 Accept H0:Normally Distributed

#test

