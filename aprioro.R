#Libraries
library(plotly)
library(hrbrthemes)
library(arules)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(viridis)
library(arulesViz)
 
#read excel 
retail <- read_excel('C:/Users/Vidhi Punjabi/Documents/Online Retail.xlsx')
retail <- retail[complete.cases(retail), ]
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
cbind(retail,TransTime)
cbind(retail,InvoiceNo)

glimpse(retail)


#transaction
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
transactionData
transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL
colnames(transactionData) <- c("items")
transactionData


write.csv(transactionData,"C:/Users/Vidhi Punjabi/Documents/market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
tran <- read.transactions('C:/Users/Vidhi Punjabi/Documents/market_basket_transactions.csv', format = 'basket', sep=',')
tran
summary(tran)

itemFrequencyPlot(tran,topN=20,type="absolute",col=viridis(20, option = "A"), main="Absolute Item Frequency Plot")

asrules <- apriori(tran, parameter = list(supp=0.001, conf=0.8,maxlen=10))
inspect(asrules[1:10])

ploting<-asrules[quality(asrules)$confidence>0.4, col="NA" ]
#Plot Scatter
plot(ploting,col = grey.colors(10))

plotly_arules(ploting)

top10subRules <- head(ploting, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")


subRules2<-head(ploting, n=20, by="lift")
plot(subRules2, method="paracoord")



