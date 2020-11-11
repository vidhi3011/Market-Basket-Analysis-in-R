
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(purrr)
library(NbClust)
library(factoextra)
library(purrr)

#Reading the Data Set
sales <- read_excel("C:/Users/Vidhi Punjabi/Documents/supermarket_sales Updated.xlsx")
fORincome<-sales[1:200,3]
print(fORincome)
View(sales)
glimpse(sales)

table(sales$`Product line`)
summary(sales$`Customer type`)
view(forcluster)

#Cleaning the Data
any(is.na(sales))
is.na(sales)
colSums(is.na(sales))
sales$`Unit price`[sales$`Unit price`==""] <- NA
sales$Rating[sales$Rating==""] <- NA
colSums(is.na(sales))
which(is.na(sales$`Unit price`))
which(is.na(sales$Rating))
sales$`Unit price`<- ifelse(is.na(sales$`Unit price`), mean(sales$`Unit price`, na.rm=TRUE), sales$`Unit price`)
sales$Rating<- ifelse(is.na(sales$Rating), mean(sales$Rating, na.rm=TRUE), sales$Rating)
colSums(is.na(sales))

#new_data=data.frame(sales$Age,sales$City,sales$Income)
#print(tail(new_data))
#new_data<-new_data[-c(201:1000),]
#print(tail(new_data))
#any(is.na(new_data))



#Categorizing Data
sales$Gender = factor(sales$Gender, levels = c('Male', 'Female'), labels = c(0,1))
head(sales)
summary(sales)

#Splitting the Data
sample <- sample(c(TRUE, FALSE), nrow(sales), replace = T, prob = c(0.7,0.3))
train <- sales[sample,]
test <- sales[!sample,]


#Visualization

a=table(sales$Gender)
barplot(a,main="Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=magma.colors(2),
        legend=rownames(a))

b=table(sales$Payment)
barplot(b,main="Payment Comparision",
        ylab="Count",
        xlab="Payement",
        col=heat.colors(3),
        legend=rownames(b),legend.position="top")

c=table(sales$Branch)

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Gender")

pct=round(c/sum(a)*100)
lbs=paste(c("Yangon","Mandalay","Naypyitaw")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(c,labels=lbs,
      main="Pie Chart Depicting Branch")

hist(sales$Rating,
     col="gold",
     main="Histogram to Show Count of Ratings",
     xlab="Rating",
     ylab="Frequency",
     labels=TRUE)

qplot(x=sales$`Tax 5%`, y=sales$`Unit price`, data=sales, geom = "point")

qplot(x=sales$`Unit price`, y=sales$Quantity, data=sales, geom = "point")

qplot(x=sales$Gender, y=sales$`Product line`, data=sales, geom = "point")

qplot(sales$Quantity,y=sales$`Unit price`, data = sales, colour = sales$Gender, shape = sales$Gender)

qplot(sales$`Unit price` ,sales$Quantity,data = sales,geom= "boxplot", fill = sales$Gender)

qplot(sales$`Product line` ,sales$`Customer type`,data = sales,geom= "boxplot", fill = sales$`Customer type`)

qplot(sales$Quantity,sales$`Product line` , data = sales,geom= "boxplot", fill = sales$`Product line`)

qplot(sales$`Tax 5%`,sales$`Product line` , data = sales,geom= "boxplot", fill = sales$`Product line`)

qplot(sales$`Unit price`,sales$`Product line` , data = sales,geom= "boxplot", fill = sales$`Product line`)

qplot(sales$City ,sales$`Tax 5%` , data = sales,geom= "boxplot", fill = sales$`Tax 5%` )

ggplot(sales, aes(x = sales$`Unit price`, y = sales$`Tax 5%`)) +
  geom_point()

#determining the number of clusters
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")


set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(fORincome,k,iter.max=10,nstart=10,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")
k<-4
#sales$Income sales$SpendingScore


set.seed(125)
stat_gap <- clusGap(fORincome, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

#Plotting
ggplot(sales, aes(x =Income, y = SpendingScore)) + 
  geom_point(stat = "identity", ) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
