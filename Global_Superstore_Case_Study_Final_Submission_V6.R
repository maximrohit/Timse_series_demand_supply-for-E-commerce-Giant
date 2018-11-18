#####################################################################################
#Loading required library
library(forecast)
library(tseries)
require(graphics)
library(ggplot2)
library(reshape)
library(gridExtra)
library(scales)
library(gsubfn)
#####################################################################################
#Table of Content
#1.Data loading,Business Understanding,EDA and Data prep
#1.1.Loading data & Intial Business analysis
#1.2.EDA
#1.3.Dividing the data into 21 sets
#1.4.Finding out most profitable sectors
#1.5.Common functions for reusable code- used in analysis
###################################################################
#2. Consumer APAC Sales Analysis & Predictions
#2.1 Auto Arima modelling
#2.2 Moving avg based smoothing for the Time series
#2.3 classical decomposition modelling for Time series
#2.4 Checking the Local autoregressive part of Time series
#2.5 Prediction and Accuracy mesurements
###################################################################
#3. Consumer APAC Quantity/Demand Analysis & Predictions
#3.1 Auto Arima modelling
#3.2 Moving avg based smoothing for the Time series
#3.3 classical decomposition modelling for Time series
#3.4 Checking the Local autoregressive part of Time series
#3.5 Prediction and Accuracy mesurements
###################################################################
#4. Consumer EU Sales Analysis & Predictions
#4.1 Auto Arima modelling
#4.2 Moving avg based smoothing for the Time series
#4.3 classical decomposition modelling for Time series
#4.4 Checking the Local autoregressive part of Time series
#4.5 Prediction and Accuracy mesurements
###################################################################
#5. Consumer EU Quantity/Demand Analysis & Predictions
#5.1 Auto Arima modelling
#5.2 Moving avg based smoothing for the Time series
#5.3 classical decomposition modelling for Time series
#5.4 Checking the Local autoregressive part of Time series
#5.5 Prediction and Accuracy mesurements
###################################################################
#6.Conclusion
#####################################################################################

#####################################################################################
#1.Data loading,Business Understanding,EDA and Data prep
#####################################################################################

###################################################################
#1.1. Loading data & Intial analysis
###################################################################
#Please set correct working directory before execution
#setwd("C:/08_PGDDS/14_Time_Series/graded_assignment")

retail_giant_data<-read.csv("Global Superstore.csv", header = T, sep = ',',stringsAsFactors = F,check.names = F)
str(retail_giant_data)
#51290 obs. of  24 variables
summary(retail_giant_data)
#from the summary  Postal.Code 41296 NA's. We will analysis this in more detail later 
#View was used to view various objects throughout the code
#View(retail_giant_data)

#uniquness check for data
length(unique(retail_giant_data$'Order ID')) #25035-seems like an order is spread across multiple rows
length(unique(retail_giant_data$'Row ID')) #51290-all unique

#undertsaning one order
retail_giant_data[which(retail_giant_data$'Order ID'=="CA-2012-124891"),] ##an order is divided based on the products. this doesn't impact our analysis

#check for NA
sapply(retail_giant_data,function(x) sum(is.na(x)))
#Postal code any ways shows 41296 NA's - we don't need additional vars
#postal codes bear no relevance in our analysis ,
#as the analysis level is at region and we have those values
#so we will leave this data as is.

###################################################################
#1.2.EDA
###################################################################
retail_giant_data$Segment <- as.factor(retail_giant_data$Segment)
retail_giant_data$Market <- as.factor(retail_giant_data$Market)
retail_giant_data$Category <- as.factor(retail_giant_data$Category)
retail_giant_data$'Order Date'<-as.Date(retail_giant_data$'Order Date',format='%d-%m-%Y')


## extract month and year detail into separate columns for analysis
retail_giant_data$Month<- as.Date(cut(retail_giant_data$`Order Date`,breaks ="month"))
retail_giant_data$Year<- as.Date(cut(retail_giant_data$`Order Date`,breaks ="year"))

## Plot to understand sales, profit and Quantity trends over months

chart1 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Month, retail_giant_data$Sales)) + xlab("Order Date") + ylab("Sales") +ggtitle("Aggregate Sales by Month") + stat_summary(fun.y = sum, geom = "bar") + scale_x_date(labels = date_format("%b-%Y"), date_breaks = "3 months")
chart2 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Month, retail_giant_data$Quantity)) + xlab("Order Date") + ylab("Quantity") +ggtitle("Aggregate Quantity by Month") + stat_summary(fun.y = sum, geom = "bar")+scale_x_date(labels = date_format("%b-%Y"), date_breaks = "3 months")
chart3 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Month, retail_giant_data$Profit)) + xlab("Order Date") + ylab("Profit") +ggtitle("Aggregate Profit by Month") + stat_summary(fun.y = sum, geom = "bar")+scale_x_date(labels = date_format("%b-%Y"), date_breaks = "3 months")
grid.arrange(chart1, chart2,chart3)

chart4 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Month, retail_giant_data$Sales)) + xlab("Order Date") + ylab("Sales") +ggtitle("Aggregate Sales by Month") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Segment) + scale_fill_discrete(name = "Segment") + theme(legend.position="top")
chart5 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Month, retail_giant_data$Sales)) + xlab("Order Date") + ylab("Sales") +ggtitle("Aggregate Sales by Month") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Market) + scale_fill_discrete(name = "Market")+ theme(legend.position="top")
chart6 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Month, retail_giant_data$Sales)) + xlab("Order Date") + ylab("Sales") +ggtitle("Aggregate Sales by Month") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Category) + scale_fill_discrete(name = "Category")+ theme(legend.position="top")
grid.arrange(chart4, chart5,chart6, nrow=1)

## Plot to understand Sales and Quantity by Segment, Market and Category over yearly data

chart7 <- ggplot(data = retail_giant_data, aes(x=retail_giant_data$Year, y=retail_giant_data$Sales)) + xlab("Order Date") + ylab("Sales") +ggtitle("Aggregate Sales by Segment") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Segment) + scale_fill_discrete(name = "Segment") + theme(legend.position="top")
chart8 <- ggplot(data = retail_giant_data, aes(x=retail_giant_data$Year, y=retail_giant_data$Sales)) + xlab("Order Date") + ylab("Sales") +ggtitle("Aggregate Sales by Market") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Market) + scale_fill_discrete(name = "Market")+ theme(legend.position="top")
chart9 <- ggplot(data = retail_giant_data, aes(x=retail_giant_data$Year, y=retail_giant_data$Sales)) + xlab("Order Date") + ylab("Sales") +ggtitle("Aggregate Sales by Category") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Category) + scale_fill_discrete(name = "Category")+ theme(legend.position="top")

chart10 <- ggplot(data = retail_giant_data, aes(x=retail_giant_data$Year, y=retail_giant_data$Quantity)) + xlab("Order Date") + ylab("Quantity") +ggtitle("Aggregate Quantity by Segment") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Segment) + scale_fill_discrete(name = "Segment") + theme(legend.position="top")
chart11 <- ggplot(data = retail_giant_data, aes(x=retail_giant_data$Year, y=retail_giant_data$Quantity)) + xlab("Order Date") + ylab("Quantity") +ggtitle("Aggregate Quantity by Market") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Market) + scale_fill_discrete(name = "Market")+ theme(legend.position="top")
chart12 <- ggplot(data = retail_giant_data, aes(x=retail_giant_data$Year, y=retail_giant_data$Quantity)) + xlab("Order Date") + ylab("Quantity") +ggtitle("Aggregate Quantity by Category") + geom_bar(fun.y = sum, stat = "summary") + aes(fill = retail_giant_data$Category) + scale_fill_discrete(name = "Category")+ theme(legend.position="top")
grid.arrange(chart7, chart8,chart9,chart10, chart11,chart12, nrow=2)

## Plot to understand which are the major segments, markets and categories by Sales
chart13 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Segment, retail_giant_data$Sales)) + xlab("Segment") + ylab("Sales") +ggtitle("Sales by Segment") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Segment) +theme(legend.position="none")
chart14 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Market, retail_giant_data$Sales)) + xlab("Market") + ylab("Sales") +ggtitle("Sales by Market") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Market) +theme(legend.position="none")
chart15 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Category, retail_giant_data$Sales)) + xlab("Category") + ylab("Sales") +ggtitle("Sales by Category") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Category) +theme(legend.position="none")

## Plot to understand which are the major segments, markets and categories by Quantity
chart16 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Segment, retail_giant_data$Quantity)) + xlab("Segment") + ylab("Quantity") +ggtitle("Quantity by Segment") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Segment) +theme(legend.position="none")
chart17 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Market, retail_giant_data$Quantity)) + xlab("Market") + ylab("Quantity") +ggtitle("Quantity by Market") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Market) +theme(legend.position="none")
chart18 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Category, retail_giant_data$Quantity)) + xlab("Category") + ylab("Quantity") +ggtitle("Quantity by Category") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Category) +theme(legend.position="none")

## Plot to understand which are the major segments, markets and categories by Profit
chart19 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Segment, retail_giant_data$Profit)) + xlab("Segment") + ylab("Profit") +ggtitle("Profit by Segment") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Segment) +theme(legend.position="none")
chart20 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Market, retail_giant_data$Profit)) + xlab("Market") + ylab("Profit") +ggtitle("Profit by Market") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Market) +theme(legend.position="none")
chart21 <- ggplot(data = retail_giant_data, aes(retail_giant_data$Category, retail_giant_data$Profit)) + xlab("Category") + ylab("Profit") +ggtitle("Profit by Category") + stat_summary(fun.y = sum, geom = "bar")+ aes(fill = retail_giant_data$Category) +theme(legend.position="none")

## Calculating Profit Margin for Data by Segment, Market and Category

## By Segment
sales_segment <- aggregate(retail_giant_data$Sales, by=list(Category=retail_giant_data$Segment), FUN = sum)
profit_segment <- aggregate(retail_giant_data$Profit, by=list(Category=retail_giant_data$Segment), FUN = sum)
profit_segment_margin <- merge(sales_segment, profit_segment, by.x="Category",by.y="Category")
profit_segment_margin$profitmargin <- profit_segment_margin$x.y/profit_segment_margin$x.x
chart22 <- ggplot(data = profit_segment_margin, aes(profit_segment_margin$Category, profit_segment_margin$profitmargin)) + xlab("Segment") + ylab("Profit Margin") +ggtitle("Profit Margin by Segment") + geom_bar(stat="identity") + aes(fill = profit_segment_margin$Category) +theme(legend.position="none")
#profit_segment_margin

## By Market
sales_market <- aggregate(retail_giant_data$Sales, by=list(Category=retail_giant_data$Market), FUN = sum)
profit_market <- aggregate(retail_giant_data$Profit, by=list(Category=retail_giant_data$Market), FUN = sum)
profit_market_margin <- merge(sales_market, profit_market, by.x="Category",by.y="Category")
profit_market_margin$profitmargin <- profit_market_margin$x.y/profit_market_margin$x.x
chart23 <- ggplot(data = profit_market_margin, aes(profit_market_margin$Category, profit_market_margin$profitmargin)) + xlab("Market") + ylab("Profit Margin") +ggtitle("Profit Margin by Market") + geom_bar(stat="identity") + aes(fill = profit_market_margin$Category) +theme(legend.position="none")

#profit_market_margin

## By Category
sales_category <- aggregate(retail_giant_data$Sales, by=list(Category=retail_giant_data$Category), FUN = sum)
profit_category <- aggregate(retail_giant_data$Profit, by=list(Category=retail_giant_data$Category), FUN = sum)
profit_category_margin <- merge(sales_category, profit_category, by.x="Category",by.y="Category")
profit_category_margin$profitmargin <- profit_category_margin$x.y/profit_category_margin$x.x
chart24 <- ggplot(data = profit_category_margin, aes(profit_category_margin$Category, profit_category_margin$profitmargin)) + xlab("Category") + ylab("Profit Margin") +ggtitle("Profit Margin by Category") + geom_bar(stat="identity") + aes(fill = profit_category_margin$Category) +theme(legend.position="none")

#profit_category_margin

## let's arrange charts together
grid.arrange(chart13, chart14,chart15, chart16,chart17, chart18,chart19, chart20, chart21,chart22,chart23, chart24, ncol=3, nrow=4)

#Cleanup-Optional-can be removed if not needed
rm(list=setdiff(ls(),"retail_giant_data"))

##  Observations from above charts
##  Among the three segments, Consumer is the largest segment for Global Mart contributing over 50% of revenue
##  Profitability of segments indicate all three Consumer(11.5%), Corporate (11.5%) and Home Office (12.0%) are all nearly same profit margins
##  Among the seven markets, APAC and EU remain the large two sectors with high Sales and Profit contribution
##  Quantity supplied to EU, LATAM and US markets remain near same but sales and profit remain small for LATAM and US, which indicate avg. selling prices of goods in these markets are low
##  Although Canada remains high in profit margin basis, volumes and sales from this region remain very small
##  After Canada, EU  is the most profitable (with profit margin of 12.7%) followed by US (12.5%) and APAC (12.2%)
##  Among the three product categories, Technology products contribute highest to sales and profit but remain very low in terms of quantity - this indicate low volume high value products compared to Furniture and Office supplies
##  Interms of profitability, Techology remains top at 14.0%, closely followed by Office Supplies at (13.7%) and Furniture productshave low profit margin of (6.9%)

###################################################################
#1.3.Dividing the data into 21 sets based on Segment & Market
###################################################################
#check length and convert to factors
length(unique(retail_giant_data$Segment))#3
length(unique(retail_giant_data$Market))#7

##converting the order date in right format before making the split
retail_giant_data$date_order<-format(retail_giant_data$'Order Date',"%Y%m")

#Code below converts YYYYMM to numbered months for timeseries
order_data<-data.frame(1:48,sort(unique(retail_giant_data$date_order)))
retail_giant_data$date_order<-as.numeric(factor(rank(retail_giant_data$date_order)))

##splitting data into 21 parts
data_21segment<-split(retail_giant_data[,c('Profit','Quantity','Sales','date_order')], with(retail_giant_data, interaction(Segment,Market)))
length(data_21segment)#21

###################################################################
#1.4.Finding out most profitable sectors
###################################################################
str(data_21segment[])
#overall analysis w/o time series formation
agg_data_21segment<-sapply(data_21segment,function (x) { c(sum(x$Profit),sum(x$Quantity),sum(x$Sales)) })
rownames(agg_data_21segment)<-c('Profit','Quantity','Sales')
agg_data_21segment<-t(agg_data_21segment)

#Let's draw some plots
a <-as.data.frame(agg_data_21segment) # dummy data frame just for plots
chart25 <- ggplot(a, aes(x= reorder(rownames(a),a$Sales),y= a$Sales)) + geom_bar(stat="identity") + coord_flip() + xlab(NULL) + ylab("Sales") + aes(fill = rownames(a)) +theme(legend.position="none")
chart26 <- ggplot(a, aes(x= reorder(rownames(a),a$Sales),y= a$Profit)) + geom_bar(stat="identity") + coord_flip() + xlab(NULL) + ylab("Profit") + aes(fill = rownames(a)) +theme(legend.position="none")
chart27 <- ggplot(a, aes(x= reorder(rownames(a),a$Sales),y= a$Quantity)) + geom_bar(stat="identity") + coord_flip() + xlab(NULL) + ylab("Quantity") + aes(fill = rownames(a)) +theme(legend.position="none")
grid.arrange(chart25, chart26,chart27, nrow=1)


#from the graphs we can see that consumer section in apac and Eu are the best performing sector
#in terms of all three factor of Profit,Quantity,Sales
# But, are they the consistently profitable ones? Let's see


matplot(agg_data_21segment, type = c("b"),pch=1,col = 1:3,ann=FALSE,xaxt="n") #plot
axis(side=1, at=1:21, labels=rownames(agg_data_21segment),las=2,cex.axis=.6)
legend("topright", legend = colnames(agg_data_21segment),fill=1:3,text.width=.4,bg=8,cex = 0.7,horiz=T, col=1:3, pch=1,box.lty=1,box.lwd=1) # optional legend
#From the plot we can see that consumer section in apac and Eu are the best performing sector
#in terms of all three factor of Profit,Quantity,Sales

#creating aggregated data for all 21 segments for COV analysis
agg_data_21segment<-sapply(data_21segment,function (x) { aggregate(x, by=list(x$date_order), FUN=sum, na.rm=TRUE,simplify=T) },simplify=T)
#calcualting  coefficient of variation of the Profit for all 21 market segments
cov_profit<-sapply(agg_data_21segment[c('Profit'),],function (x) { sd(x)/mean(x)})
cov_profit[order(cov_profit)]
# Consumer.EU      Consumer.APAC
# 0.6243052          0.6321323
#Consumer segment in APAC and EU  remaisn the most consistantly profitale accoung to this measure as well

##plot coefficient of variance to highlight most consistently profitable segments
df <- melt(cov_profit)
df$name <- rownames(df)
rownames(df) <- NULL

chart28 <- ggplot(data=df,aes(x=reorder(df$name,-df$value), y=df$value, fill = df$name))+ geom_bar(stat = "identity") + coord_flip() + geom_text(aes(label=round(df$value,4),hjust=0)) + xlab("Market Segment") + ylab("Coefficient of Variation (CoV) in Profit") +ggtitle("CoV by Market Segment (Lower the better)") +theme(legend.position="none")
chart28
#optional-cleanup
rm(a,chart25,chart26,chart27,chart28,df)

###################################################################
#1.5.Common functions for reusable code- used in analysis
###################################################################
#Function for auto arima analysis and plot
auto_arima_analysis<-function(x){
  
  autoarima <- auto.arima(x)
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  resi_auto_arima <- x - fitted(autoarima)
  
  print(autoarima)
  print(adf.test(resi_auto_arima,alternative = "stationary"))
  print(kpss.test(resi_auto_arima))
  
  return(autoarima) 
}

#Function for mape analysis and plot
mape_analysis<-function(x,y,z){
  fcast_auto_arima <- forecast(x, h = 6)
  print(fcast_auto_arima$method)
  MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,y)
  print(MAPE_auto_arima)
  auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$mean))
  plot(ts(z), col = "black")
  lines(auto_arima_pred, col = "red")
  
}

#Function to get residual in additive model
get_residual_additive<- function (x,y){
  
  lmfit <- lm(y ~ poly(date_order,options[x,]$i)+sin(options[x,]$j*date_order)
              +   cos((1-options[x,]$j)*date_order)
              , data=train)
  globalpred <- predict(lmfit, Month=train$date_order)      
  localpred <- timeser - globalpred
  sum(localpred^2)
}


#Function to find best additive model

get_additive_models<- function (c,x,y,predvar,test,testpred)
{
  lmfit <- lm(predvar ~ poly(date_order,x[c])+sin(y[c]*date_order)
              +   cos(y[c]*date_order)
              , data=train)
  
  globalpred <- predict(lmfit, Month=train$date_order)      
  localpred <- timeser - globalpred
  ar<-auto.arima(localpred) #return
  armafit_local <- auto.arima(localpred)
  resi <- localpred-fitted(armafit_local)
  t1<-adf.test(resi, alternative="stationary")
  t1$p.value
  t2<-kpss.test(resi)
  t2$p.value
  
  global_pred_out <- predict(lmfit,data.frame(date_order = test$date_order))
  auto_arima_pred_out <- predict(armafit_local, n.ahead = 6)
  global_pred_out<-global_pred_out+auto_arima_pred_out$pred
  acc<-accuracy(global_pred_out,testpred)
  retval=list(autoarima=ar,adf_pval=t1$p.value,kpss_pval=t2$p.value,MAPE_val=acc)
  return(retval)
}

#Function to get residual in multiplicative model
get_residual_multiplicative<- function (x,y){
  
  lmfit <- lm(y ~ poly(date_order,options[x,]$i)*sin(options[x,]$j*date_order)
              +   poly(date_order,options[x,]$i)*cos((1-options[x,]$j)*date_order)
              , data=train)
  globalpred <- predict(lmfit, Month=train$date_order)      
  localpred <- timeser - globalpred
  sum(localpred^2)
  
}

#Function to find best multiplicative model

get_multiplicative_models<- function (c,x,y,predvar,test,testpred){
  lmfit <- lm(predvar ~ poly(date_order,x[c])+sin(y[c]*date_order)*poly(date_order,x[c])
              +   cos(y[c]*date_order)*poly(date_order,x[c])
              , data=train)
  
  globalpred <- predict(lmfit, Month=train$date_order)      
  localpred <- timeser - globalpred
  ar<-auto.arima(localpred) #return
  armafit_local <- auto.arima(localpred)
  resi <- localpred-fitted(armafit_local)
  t1<-adf.test(resi, alternative="stationary")
  t1$p.value
  t2<-kpss.test(resi)
  t2$p.value
  
  global_pred_out <- predict(lmfit,data.frame(date_order = test$date_order))
  auto_arima_pred_out <- predict(armafit_local, n.ahead = 6)
  global_pred_out<-global_pred_out+auto_arima_pred_out$pred
  acc<-accuracy(global_pred_out,testpred)
  retval=list(autoarima=ar,adf_pval=t1$p.value,kpss_pval=t2$p.value,MAPE_val=acc)
  return(retval)
}

#Function to get smoothened series
get_smoothened_series<-function(timeser,j){
  
  plot(timeser)
  width<-c(2,6,8)
  cols <- c("red", "blue", "green", "black")
  labels <- c(paste("width =", width), "Original")
  for (i in 1:length(width)){
    smoothedseries <- stats::filter(timeser, filter=rep(1/width[i], width[i]),
                             method="convolution", sides=2)
    
    lines(smoothedseries, col=cols[i], lwd=2)
  }
  legend("topleft", labels, col=cols, lwd=2, cex=.5,bg=8)
  smoothedseries <- stats::filter(timeser, filter=rep(1/j, j),
                           method="convolution", sides=2)
  return(smoothedseries)
  
}


#Function to implode NA values in smoothened series
get_na_replaced<-function(smoothedseries,f,l){
  
  sl<-length(smoothedseries)-l
  #first f values
  diff<-smoothedseries[f+2]-smoothedseries[f+1]
  for (i in 1:f){
    smoothedseries[f+1-i]<-smoothedseries[f+1]-(i*diff)
    
  }
  #last l values
  diff<-smoothedseries[sl]-smoothedseries[sl-1]
  for (i in 1:l){
    smoothedseries[sl+i]<-smoothedseries[sl]+(i*diff)
    
  }
  return(smoothedseries)
}

#####################################################################################
#2. Consumer APAC Sales Analysis & Predictions
#####################################################################################
#Seperate APAC Data for sales and quantity/demand
Consumer.APAC_Data<-as.data.frame(data_21segment['Consumer.APAC'])
Consumer.APAC_Data
str(Consumer.APAC_Data)
agg_Consumer.APAC_Data<-aggregate(Consumer.APAC_Data, by=list(Consumer.APAC_Data$Consumer.APAC.date_order), FUN=sum, 
                                  na.rm=TRUE,simplify=T)
str(agg_Consumer.APAC_Data)
names(agg_Consumer.APAC_Data)[1]<-"date_order"
names(agg_Consumer.APAC_Data)[3]<-"Quantity"
names(agg_Consumer.APAC_Data)[4]<-"Sales"

###################APAC Sales Analysis and predictions
train_apac_sales<-agg_Consumer.APAC_Data[1:42,c("date_order","Sales")]
test_apac_sales<-agg_Consumer.APAC_Data[43:48,c("date_order","Sales")]

#using generic train/test for use of functions
train<-train_apac_sales
test<-test_apac_sales

timeser <- ts(train$Sales,frequency = 12)# representing one year as the aggregation is monthly
plot(timeser)
#from the graph seems like trend and seasonality have a multiplicative relationship
#but seasoanlity is not consistant

###################################################################
#2.1 Auto Arima modelling for APAC
###################################################################
#lets used auto arima to model first to get a baseline for APAC consumer
autoarima<-auto_arima_analysis(timeser)
#Results:
#Autoarima
#AIC=644.15   AICc=645.08   BIC=648.36
#Augmented Dickey-Fuller Test p-value = 0.0571 - Fail to reject  - Series is not stationary
#KPSS Test for Level Stationarity p-value = 0.1 - Series is stationary
#Review plot to fit of predicted values

#Let's do MAPE analysis for accuracy check
mape_analysis(autoarima,test_apac_sales$Sales,agg_Consumer.APAC_Data$Sales)
#Series - "ARIMA(0,0,0)(1,1,0)[12] with drift"

#                ME     RMSE      MAE      MPE     MAPE
#Test set 5202.242 9498.391 7454.207 6.967048 11.77618
#Less MAPE value #11.7761 hence accuracy is good
#Review plot to fit of predicted values
###################################################################
#2.2 Moving avg based smoothing for the Time series
###################################################################
smoothedseries<-get_smoothened_series(timeser,8)
sum(is.na(smoothedseries))
smoothedseries
#first 3 and last 4 are na, since we don't have the range, 
#we will use the last steps as differnce to fill in these values
#intial 3 values
smoothedseries<-get_na_replaced(smoothedseries,3,4)
#we have all the values
sum(is.na(smoothedseries))

#repalcing with the fitted one
train<-cbind.data.frame(date_order=1:length(smoothedseries),
                        Sales=as.vector(smoothedseries))
str(train)                

###################################################################
#2.3 classical decomposition modelling for Time series
###################################################################
options <- expand.grid(i=1:3, j=seq(.1,1,by=.1))
###############
#running regression for additive model to check possible polynomial options
approx_residual<-unlist(lapply(1:nrow(options),get_residual_additive,y=train$Sales), use.names=FALSE)
plot(approx_residual)
axis(side=1, at=1:30)
top10<-head(options[order(approx_residual),],10)
top10
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_additive_models,x=top10$i,y=top10$j,predvar=train$Sales,test=test,testpred=test$Sales)

#top10results
#Analysis of various models details in top10results to pick the decent one
#Option 1-High AIC/BIC-AIC=893.67/BIC=897.14, sigma^2 estimated as 89393910
#Option 2-sigma^2 estimated as 99091978, MAPE - 17.60357
#Option 3 onwards, MAPE and sigma^2 increase and/or adf p value test fails
#We should check multiplicative model for fit

#running regression for multiplicative model to find suitable model coeff
approx_residual<-unlist(lapply(1:nrow(options),get_residual_multiplicative,y=train$Sales), use.names=FALSE)
plot(approx_residual)
axis(side=1, at=1:30)
top10
top10<-head(options[order(approx_residual),],10)
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_multiplicative_models,x=top10$i,y=top10$j,predvar=train$Sales,test=test,testpred=test$Sales)
#top10results
#Analysis of various models details in top10results to pick the decent one
#Option 1- High MAPE -189.9605
#Option 2- High ADF test P Value, non stationary element
#Option 3,4,5 - High MAPE 29.247,91.78398,AIC 630+
#MAPE is drastically reduced at 7th option - 14.97376
#8th option also gives good MAPE 14.57363 however since it has more local remnant component,
#we go with 7th option as final model
#both adf and kpss test show that remaining series after modelling is white noise
#This option is 12 - 2 0.5 which we will use for our final model
#Final model

lmfit <- lm(Sales ~ poly(date_order,2)+sin(.5*date_order)*poly(date_order,2)
            +   cos(.5*date_order)*poly(date_order,2)
            , data=train)

globalpred <- predict(lmfit, Month=train$date_order)
plot(train_apac_sales$date_order,train_apac_sales$Sales,col='green', type = "l")#actual time series
lines(train$date_order, globalpred, col='red', type = "l")#trend line
#our prediction is a decent fit

#lets calculate local autoregressive component of the model
localpred <- timeser - globalpred
lines(train$date_order, localpred, col='blue', type = "l")

###################################################################
#2.4 Checking the Local autoregressive part of Time series
###################################################################
# Compute and plot the ACF for the time remaining series
acf(localpred)

# Compute and plot the PACF for the time series
pacf(localpred)
#couple of values toucing the lines

## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
#ARIMA is ARIMA(0,0,0)(1,0,0)[12]
#AIC=644.67   AICc=645.12   BIC=647.48
resi <- localpred-fitted(armafit_local)

# Compute and plot the ACF for the time series
acf(resi)
#seems fine for residual- less points crossing threshold

# Compute and plot the PACF for the residual time series
pacf(resi)
# there is improvement in the pacf graph as well

##Lets perform the stationary test
adf.test(resi, alternative="stationary")
#p-value = 0.02443, null hypothesis of not stationary can be rejected - residual is white noise
kpss.test(resi)
#p-value = 0.1, hence the null hypothesis of series being stationary can't be rejected
#no change in the result, hence we will ignore the residual componet ascomplete noise

###################################################################
#2.5 Prediction and Accuracy mesurements
###################################################################
#let's predict and see the mape value

global_pred_out <- predict(lmfit,data.frame(date_order = test$date_order))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 6)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_sales <- accuracy(global_pred_out,test$Sales)
MAPE_sales
#ME    RMSE      MAE      MPE     MAPE
#Test set 7977.991 11618.9 9522.642 11.50086 14.97376
#MAPE -14.97376

#six month predcition plot
plot(test$date_order,test$Sales,col='green', type = "l")#actual time series
lines(test$date_order, global_pred_out, col='red', type = "l")#predcited time series

# complete time series plot
pred <- c((globalpred+fitted(armafit_local)),global_pred_out)
plot(ts(agg_Consumer.APAC_Data$Sales), col = "black")
lines(pred, col = "red")

#####################################################################################
#3. Consumer APAC Quantity/Demand Analysis & Predictions
#####################################################################################
#Seperate apac quantity/demand data
train_apac_qty<-agg_Consumer.APAC_Data[1:42,c("date_order","Quantity")]
test_apac_qty<-agg_Consumer.APAC_Data[43:48,c("date_order","Quantity")]

#Use train/test for use of functions
train<-train_apac_qty
test<-test_apac_qty

timeser <- ts(train$Quantity,frequency = 12)# representing one year as the aggregation is monthly
plot(timeser)
#from the graph appears that trend and seasonality have a multiplicative relationship


###################################################################
#3.1 Auto Arima modelling for APAC
###################################################################
#lets used auto arima to model first in order to get a baseline for APAC consumer
autoarima<-auto_arima_analysis(timeser)
#Results
#Autoarima
#AIC=365.28   AICc=366.2   BIC=369.48
#Augmented Dickey-Fuller Test p-value = 0.1573 - Series is not stationary - there seems to be some
#auto regressive behaviour
#KPSS Test for Level Stationarity p-value = 0.1 - Series is stationary
#Residual is white noise
#Review plot to fit of predicted values

mape_analysis(autoarima,test_apac_qty$Quantity,agg_Consumer.APAC_Data$Quantity)
#Series - "ARIMA(0,0,0)(1,1,0)[12] with drift"

#                ME     RMSE      MAE      MPE     MAPE
#Test set 102.191 138.9728 109.6619 11.70521 13.36779
#Less MAPE value #13.36779
#Review plot to fit of predicted values

###################################################################
#3.2 Moving avg based smoothing for the Time series
###################################################################
#moving avg method
smoothedseries<-get_smoothened_series(timeser,6)
sum(is.na(smoothedseries))
smoothedseries
#first 2 and last 3 are na, since we don't have the range, 
#we will use the last steps as differnce to fill in these values
#intial 2 values
smoothedseries<-get_na_replaced(smoothedseries,2,3)
#we have all the values
sum(is.na(smoothedseries))

#replacing with the fitted one
train<-cbind.data.frame(date_order=1:length(smoothedseries),
                        Quantity=as.vector(smoothedseries))
str(train)                

###################################################################
#3.3 Classical decomposition modelling for Time series
###################################################################

options <- expand.grid(i=1:3, j=seq(.1,1,by=.1))
#running regression for additive model
approx_residual<-unlist(lapply(1:nrow(options),get_residual_additive,y=train$Quantity), use.names=FALSE)
plot(approx_residual)
axis(side=1, at=1:30)
top10<-head(options[order(approx_residual),],10)
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_additive_models,x=top10$i,y=top10$j,predvar=train$Quantity,test=test,testpred=test$Quantity)

#top10results
#Analysis of various models details in top10results to pick the decent one
#Option-1 through 10 - ADF test shows non stationary series.Some options have high MAPE as well hence we can not use additive model

#running regression for multiplicative model
approx_residual<-unlist(lapply(1:nrow(options),get_residual_multiplicative,y=train$Quantity), use.names=FALSE)
plot(approx_residual)
axis(side=1, at=1:30)
top10<-head(options[order(approx_residual),],10)
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_multiplicative_models,x=top10$i,y=top10$j,predvar=train$Quantity,test=test,testpred=test$Quantity)

#top10results
#Analysis of various models details in top10results to pick the decent one
#Option 1-high adf p val -0.08829152, option2 adf p val reduces to 0.04858445
#sigma^2 estimated as 6349:  log likelihood=-176.17
#AIC=356.35   AICc=356.79   BIC=359.15
#Option 2 - 15 3 0.5 - We will select this as model

lmfit <- lm(Quantity ~ poly(date_order,3)+sin(.5*date_order)*poly(date_order,3)
            +   cos(.5*date_order)*poly(date_order,3)
            , data=train)
globalpred <- predict(lmfit, Month=train_apac_qty$date_order)
plot(train_apac_qty$date_order,train_apac_qty$Quantity,col='green', type = "l")#actual time series
lines(train_apac_qty$date_order, globalpred, col='red', type = "l")#trend line
#our prediction seems to have captured the trend well

#lets calculate local autoregressive component of the model
localpred <- timeser - globalpred
lines(train_apac_qty$date_order, localpred, col='blue', type = "l")

###################################################################
#3.4 Checking the Local autoregressive part of Time series
###################################################################
# Compute and plot the ACF for the time remaining series
acf(localpred)
#few values crossing out, This seems to be AR the series type

# Compute and plot the PACF for the time series
pacf(localpred)

## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
#ARIMA(0,0,0)(1,1,0)[12] 
#AIC=356.35   AICc=356.79   BIC=359.15
resi <- localpred-fitted(armafit_local)

# Compute and plot the ACF for the residual time series
acf(resi)
#seems fine - lot of improvement after modeling AR portion

# Plot the PACF for the time series
pacf(resi)# This shows improvement as well
##Lets perform teh stationary test
adf.test(resi, alternative="stationary")
# p-value = 0.04858, null hypothesis of not being stationary can be rejected
kpss.test(resi)
#p-value = 0.1, hence the null hypothesis of series being stationary can not be rejected

###################################################################
#3.5 Prediction and Accuracy mesurements
###################################################################
#lets predict and see the mape value
global_pred_out <- predict(lmfit,data.frame(date_order = test$date_order))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 6)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_qty <- accuracy(global_pred_out,test$Quantity)
MAPE_qty
# ME     RMSE      MAE      MPE     MAPE
#Test set 106.496 137.7615 107.7154 14.23774 14.38483
#MAPE#14.38483

#six month predcition plot
plot(test$date_order,test$Quantity,col='green', type = "l")#actual time series
lines(test$date_order, global_pred_out, col='red', type = "l")#predcited time series

# complete time series plot
pred <- c((globalpred+fitted(armafit_local)),global_pred_out)
plot(ts(agg_Consumer.APAC_Data$Quantity), col = "black")
lines(pred, col = "red")

#####################################################################################
#4.Consumer EU Sales Analysis & Predictions
#####################################################################################
#Let's  get data seperated for Consumer EU amalysis 
Consumer.EU_Data<-as.data.frame(data_21segment['Consumer.EU'])
Consumer.EU_Data
str(Consumer.EU_Data)
agg_Consumer.EU_Data<-aggregate(Consumer.EU_Data, by=list(Consumer.EU_Data$Consumer.EU.date_order), FUN=sum, 
                                na.rm=TRUE,simplify=T)
str(agg_Consumer.EU_Data)
names(agg_Consumer.EU_Data)[1]<-"date_order"
names(agg_Consumer.EU_Data)[3]<-"Quantity"
names(agg_Consumer.EU_Data)[4]<-"Sales"

############Sales Modelling/Analysis##################
train_eu_sales<-agg_Consumer.EU_Data[1:42,c("date_order","Sales")]
test_eu_sales<-agg_Consumer.EU_Data[43:48,c("date_order","Sales")]

#We'll use generic train/test for use of functions
train<-train_eu_sales
test<-test_eu_sales

timeser <- ts(train$Sales,frequency = 12)# representing one year as the aggregation is monthly
plot(timeser)
#from the graph seems like trend and seasonality have a multiplicative relationship
#seasoanlity is more consistant as copared to the apac time series

###################################################################
#4.1 Auto Arima modelling for EU
###################################################################
#let's used auto arima to model first to get a baseline for EU consumer
autoarima<-auto_arima_analysis(timeser)
#Results
#Autoarima
#AIC=636.53   AICc=637.45   BIC=640.73

#Augmented Dickey-Fuller Test p-value = 0.09775 - Series is not stationary
#KPSS Test for Level Stationarity p-value = 0.1 - Series is stationary
#Residual is white noise is not substantiated by adf test however KPSS series says so
#Review plot to fit of predicted values

mape_analysis(autoarima,test$Sales,agg_Consumer.EU_Data$Sales)
#Series - "ARIMA(0,0,0)(1,1,0)[12] with drift"

#                ME     RMSE      MAE      MPE     MAPE
#Test set 7920.493 10109.39 8538.623 13.81862 15.75223
#Reasonably low  MAPE value #15.75223
#Review plot to fit of predicted values

###################################################################
#4.2 Moving avg based smoothing for the Time series
###################################################################
#moving avg method
smoothedseries<-get_smoothened_series(timeser,6)
smoothedseries
sum(is.na(smoothedseries))
#first 2 and last 3 are na, since we don't have the range, 
#we will use the last steps as differnce to fill in these values
smoothedseries<-get_na_replaced(smoothedseries,2,3)
#we have all the values
sum(is.na(smoothedseries))

#repalcing with the fitted one
train<-cbind.data.frame(date_order=1:length(smoothedseries),
                        Sales=as.vector(smoothedseries))
str(train)                

###################################################################
#4.3 Classical decomposition modelling for Time series
###################################################################
options <- expand.grid(i=1:3, j=seq(.1,1,by=.1))

#running regression for additive model to check options
approx_residual<-unlist(lapply(1:nrow(options),get_residual_additive,y=train$Sales), use.names=FALSE)
plot(approx_residual)
axis(side=1, at=1:30)
top10<-head(options[order(approx_residual),],10)
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_additive_models,x=top10$i,y=top10$j,predvar=train$Sales,test=test,testpred=test$Sales)

#top10results
#Analysis of various models details in top10results to pick the decent one
#All models have high AIC,BIC values and any ways, it indicates polynomial of high degree is required.
#Instead we will check multiplicative models.


#running regression for multiplicative model
# approx_residual<-sapply(1:nrow(options),get_residual_multiplicative)
approx_residual<-unlist(lapply(1:nrow(options),get_residual_multiplicative,y=train$Sales), use.names=FALSE)
plot(approx_residual)
axis(side=1, at=1:30)

top10<-head(options[order(approx_residual),],10)
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_multiplicative_models,x=top10$i,y=top10$j,predvar=train$Sales,test=test,testpred=test$Sales)

#top10results
#Analysis of various models details in top10results to pick the decent one
#Option 1 or 2 offers decent MAPE ~30/20 and same AIC/BIC
#We will use 12 3 0.4
lmfit <- lm(Sales ~ poly(date_order,3)+sin(.4*date_order)*poly(date_order,3)
            +   cos(.4*date_order)*poly(date_order,3)
            , data=train)
globalpred <- predict(lmfit, Month=train$date_order)
plot(train_eu_sales$date_order,train_eu_sales$Sales,col='green', type = "l")#actual time series
lines(train$date_order, globalpred, col='red', type = "l")#trend line

#lets calculate local autoregressive component of the model
localpred <- timeser - globalpred
lines(train$date_order, localpred, col='blue', type = "l")

###################################################################
#4.4 Checking the Local autoregressive part of Time series
###################################################################
# Compute and plot the ACF for the time remaining series
acf(localpred)
#AR(1) seems to be the series type

# Compute and plot the PACF for the time series
pacf(localpred)
## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
#ARIMA(0,0,0)(1,1,0)[12]
#AIC=626.99   AICc=627.43   BIC=629.79
resi <- localpred-fitted(armafit_local)

# Compute and plot the ACF for the time series
acf(resi)
#seems better now

# Compute and plot the PACF for the time series
pacf(resi)
# there is  some improvement in the pacf graph


##Lets perform the stationary test
adf.test(resi, alternative="stationary")
# p-value = 0.01, null hypothesis of not being stationary can be rejected
kpss.test(resi)
#p-value = 0.1, hence the null hypothesis of series being stationary can be rejected
###################################################################
#4.5 Prediction and Accuracy mesurements
###################################################################
#lets predict and see the mape value

global_pred_out <- predict(lmfit,data.frame(date_order = test$date_order))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 6)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_sales <- accuracy(global_pred_out,test$Sales)
MAPE_sales
# ME     RMSE      MAE      MPE     MAPE
# Test set 6504.57 12259.11 10347.92 14.05211 20.13542
# MAPE#20.13542

#six month predcition plot
plot(test$date_order,global_pred_out,col='red', type = "l")#predcited time series
lines(test$date_order,test$Sales , col='green', type = "l")#actual time series


# complete time series plot
pred <- c((globalpred+fitted(armafit_local)),global_pred_out)
plot(ts(agg_Consumer.EU_Data$Sales), col = "black")
lines(pred, col = "red")

#####################################################################################
#5.Consumer EU Quantity/Demand Analysis & Predictions
#####################################################################################
train_eu_qty<-agg_Consumer.EU_Data[1:42,c("date_order","Quantity")]
test_eu_qty<-agg_Consumer.EU_Data[43:48,c("date_order","Quantity")]

#Use train/test for use of functions
train<-train_eu_qty
test<-test_eu_qty

timeser <- ts(train$Quantity,frequency = 12)# representing one year as the aggregation is monthly
plot(timeser)
#from the graph seems like trend and seasonality have a multiplicative relationship

###################################################################
#5.1 Auto Arima modelling for EU
###################################################################
#lets used auto arima to model first to get a baseline for EU consumer
autoarima<-auto_arima_analysis(timeser)
#Results
#Autoarima
#AIC=361.45   AICc=362.37   BIC=365.65

#Augmented Dickey-Fuller Test p-value = 0.1801 - Series is not stationary
#KPSS Test for Level Stationarity p-value = 0.1 - Series is stationary
#ADF suggests that residual is not pure white noise 
#Review plot to fit of predicted values

mape_analysis(autoarima,test$Quantity,agg_Consumer.EU_Data$Quantity)
#Series - "ARIMA(0,0,0)(1,1,0)[12] with drift"

#                ME     RMSE      MAE      MPE     MAPE
#Test set 127.7511 153.3188 129.7701 17.32507 17.6185
#Reasonably low  MAPE value #17.6185
#Review plot to fit of predicted values

###################################################################
#5.2 Moving avg based smoothing for the Time series
###################################################################
#moving avg method
smoothedseries<-get_smoothened_series(timeser,6)
smoothedseries

sum(is.na(smoothedseries))
#first 2 and last 3 are na, since we don't have the range, 
#we will use the last steps as differnce to fill in these values

smoothedseries<-get_na_replaced(smoothedseries,2,3)
#we have all the values
sum(is.na(smoothedseries))


#replacing with the fitted one
train<-cbind.data.frame(date_order=1:length(smoothedseries),
                        Quantity=as.vector(smoothedseries))
str(train)                

###################################################################
#5.3 Classical decomposition modelling for Time series
###################################################################
options <- expand.grid(i=1:3, j=seq(.1,1,by=.1))

#running regression for additive model to check options
approx_residual<-unlist(lapply(1:nrow(options),get_residual_additive,y=train$Quantity), use.names=FALSE)

plot(approx_residual)
axis(side=1, at=1:30)
top10<-head(options[order(approx_residual),],10)
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_additive_models,x=top10$i,y=top10$j,predvar=train$Quantity,test=test,testpred=test$Quantity)

#top10results
#Analysis of various models details in top10results to pick the decent one
#Option 1 or 2 offer same results MAPE ~ 28.4 AIC=357.93   AICc=358.38   BIC=360.74
#ADF test P val is 0.03925274
#This is expected to be multiplicative as well hence let's check it. 

#running regression for multiplicative model
approx_residual<-unlist(lapply(1:nrow(options),get_residual_multiplicative,y=train$Quantity), use.names=FALSE)
plot(approx_residual)
axis(side=1, at=1:30)
top10<-head(options[order(approx_residual),],10)
top10results=list[10]
top10results<-lapply(1:nrow(top10),get_multiplicative_models,x=top10$i,y=top10$j,predvar=train$Quantity,test=test,testpred=test$Quantity)

#top10results
#Analysis of various models details in top10results to pick the decent one
#Option 1 has higher MAPE-58.9360 and 2,3 have high ADF P value and/or MAPE value
#Option 5 has MAPE 19.71775 and gives ADF pvalue of 0.01
#Rest other models have higher MAPE or adf pval for remnant model.
#Hence we will select 11 2 0.4

#final model for EU Quantiry/Demand
lmfit <- lm(Quantity ~ poly(date_order,2)+sin(.4*date_order)*poly(date_order,2)
            +   cos(.4*date_order)*poly(date_order,2)
            , data=train)

globalpred <- predict(lmfit, Month=train_eu_qty$date_order)
plot(train_eu_qty$date_order,train_eu_qty$Quantity,col='green', type = "l")#actual time series
lines(train_eu_qty$date_order, globalpred, col='red', type = "l")#trend line
#our prediction is a decent fit

#lets calculate local autoregressive component of the model
localpred <- timeser - globalpred
lines(train_eu_qty$date_order, localpred, col='blue', type = "l")

###################################################################
#5.4 Checking teh Local autoregressive part of Time series
###################################################################
# Compute and plot the ACF for the time remaining series
acf(localpred)

# Compute and plot the PACF for the time series
pacf(localpred)

## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
#ARIMA(0,0,2)(1,1,0)[12]
#AIC=349.57   AICc=351.17   BIC=355.17
resi <- localpred-fitted(armafit_local)

# Compute and plot the ACF for the residual time series
acf(resi)

# Plot the PACF for the time series
pacf(resi)
##Lets perform teh stationary test
adf.test(resi, alternative="stationary")
# p-value = 0.01, null hypothesis of not being stationary can be rejected
kpss.test(resi)
#p-value = 0.1, hence the null hypothesis of series being stationary can't be rejected
#Remnant is white noise

###################################################################
#5.5 Prediction and Accuracy mesurements
###################################################################
#lets predict and see the mape value
global_pred_out <- predict(lmfit,data.frame(date_order = test$date_order))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 6)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_qty <- accuracy(global_pred_out,test$Quantity)
MAPE_qty
# ME     RMSE      MAE      MPE     MAPE
# Test set 126.551 149.9429 135.6421 18.39637 19.71775
#MAPE#19.71775

#six month predcition plot
plot(test$date_order,test$Quantity,col='green', type = "l")#actual time series
lines(test$date_order, global_pred_out, col='red', type = "l")#predcited time series

# complete time series plot
pred <- c((globalpred+fitted(armafit_local)),global_pred_out)
plot(ts(agg_Consumer.EU_Data$Quantity), col = "black")
lines(pred, col = "red")
#####################################################################################
#6.Conclusion
#####################################################################################
# Consumer APAC & Consumer EU segments demonstrated most & consistent profitability
#A. 
# Timeseries plots shows strong growing trend and for both segments in sales as well 
# as demand along with seasonality. 
#B. 
# ADF tests on Auto Arima models on Sales & Quantity for both segments indicate that there
# may be some local regressive behaviour.
#C. 
# Consumer APAC timeseries required smoothing over a longer term of 8 periods, 
# as compared to the simpler Consumer EU time series which we smoothed with 6 periods
#D. 
# With Classic decomposition models on smoothened series, ADF & KPSS test results indicate that  
# we are better able to separate white noise, thus prove to be better choice for forecast modelling