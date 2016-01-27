#brupoon 2015, MIT license
#install.packages("doBy")
#install.packages("ggplot2")
library(doBy)
library(ggplot2)
library(gdata)
library(plyr)
#data
dat <- read.xls("~/dev/dsci/rollingsales/rollingsales_manhattan.xls", pattern="BOROUGH")
dat$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",dat$SALE.PRICE)) #clean using regex
sale.price.count <- count(is.na(dat$SALE.PRICE.N))
names(dat) <- tolower(names(dat)) #lowercase all names in the data

#clean again using regex
dat$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",dat$gross.square.feet))
dat$land.sqft <- as.numeric(gsub("[^[:digit:]]","",dat$land.square.feet))

dat$sale.date <- as.Date(dat$sale.date)
dat$year.built <- as.numeric(as.character(dat$year.built))

#use attach to make things easier
attach(dat)
#we want to make sure our sale data isn't messed up in some weird way
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0]) 
hist(gross.sqft[sale.price.n==0])
detach(dat)

#remove non-sales
dat.sale <- dat[dat$sale.price.n!=0,]

plot.sqft.price <- plot(dat.sale$gross.sqft,dat.sale$sale.price.n)
plot.sqft.price.log <- plot(log(dat.sale$gross.sqft),log(dat.sale$sale.price.n))

#what if we want to look at 1- 2- and 3- family homes?
dat.homes <- dat.sale[which(grepl("FAMILY",dat.sale$building.class.category)),]
plot.sqft.price.fam <- plot(log(dat.homes$gross.sqft,log(dat.homes$sale.price.n)))

dat.homes[which(dat.homes$sale.price.n<100000),]
	[order(dat.homes[which(dat.homes$sale.price.n<100000),]
	$sale.price.n),]

#remove outliers that weren't sales

dat.homes$outliers <- (log(dat.homes$sale.price.n) <=5) + 0
dat.homes <- dat.homes[which(dat.homes$outliers==0),] #adjust dat.homes to not include these

plot.sqft.price.fam.clean <- plot(log(dat.homes$gross.sqft),log(dat.homes$sale.price.n))

#now we can actually do some linear regression
model.a <- lm(log(dat.homes$sale.price.n) ~ log(dat.homes$gross.sqft))

