#Copyright (c) 2015 Brunston Poon (@brupoon) - MIT License
#install.packages("doBy")
#install.packages("ggplot2")
library(doBy)
library(ggplot2)
#data
nyt = read.csv(path) #change for different computer

#view information
summary(nyt)
head(nyt)

ageGroupings <- c(-Inf,0,18,24,34,44,54,64,Inf)
nyt$age <- cut(nyt$Age, breaks = ageGroupings)

#Let's create one vector which can store all of our summaries.
vsum <-c()

siteRange <- function(x){c(length(x), min(x), mean(x), max(x))}
#Creates a function siteRange returning a vector of x with above attr.

#brackets
vsum$byAge <- summaryBy(Age~age, data = nyt, FUN=siteRange)
#groupwise summary stats

#only signed in users have ages and genders
#impressions / click-thru rate for each age category
vsum$byAge_imp_ctr<- summaryBy(Gender+Signed_In+Impressions+Clicks~age, data = nyt)

#plotting
hst_imp_age <- ggplot(nyt, aes(x=Impressions, fill=age)) +
    geom_histogram(binwidth=1)
box_imp_age <- ggplot(nyt, aes(x=age, y=Impressions, fill=age)) +
    geom_boxplot()

