nyt = read.csv("D:\\github/dsci/nyt/nyt1.csv") #change for different computer
summary(nyt)
ageGroupings <- c(-Inf,18,24,34,44,54,64,Inf)
ageGroup <- cut(nyt$Age, breaks = ageGroupings)
ctrLess18 <- cut(nyt$Impressions)