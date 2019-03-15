#Import Data
CustData <- read.csv("cust_training_data.csv", header = TRUE)
histData <- read.csv("cust__training_hist_data.csv", header = TRUE)
ChurnData <- read.csv("churn_training_output.csv", header = TRUE)

testCustData <- read.csv("cust_test_data.csv", header = TRUE)
testhistData <- read.csv("cust_test_hist_data.csv", header = TRUE)
testChurnData <- read.csv("churn_test_output.csv", header = TRUE)

#Import Packages
library(mlogit)
library(caTools)
library(e1071)
library(ROSE)
library(ROCR)
library(rpart)

#Add Churn data to custdata
CustData$churn <- ChurnData$churn
head(Cust)

#Preliminary Analysis
str(CustData)
summary(CustData)
str(ChurnData)
summary(ChurnData)
sum(ChurnData$churn)/NROW(ChurnData$id)

summary(CustData$channel_sales)
tapply(CustData$churn,CustData$channel_sales,sum,na.rm=TRUE)
str(histData)
summary(histData)

#find average price for person
CustData$AvgP1_var <- 0
for (i in 1:16096) {
  CustData$AvgP1_var[i] <- mean(histData$price_p1_var[which(histData$id == CustData$id[i])],na.rm = TRUE)
}
which(CustData$id=='24011ae4ebbe3035111d65fa7c15bc57')
mean(histData$price_p1_var[which(histData$id == CustData$id[2])],na.rm = TRUE)

head(CustData$AvgP1_var)

CustData$AvgP2_var <- 0
for (i in 1:16096) {
  CustData$AvgP2_var[i] <- mean(histData$price_p2_var[which(histData$id == CustData$id[i])],na.rm = TRUE)
}
head(CustData$AvgP2_var)
summary(CustData$AvgP1_var)
which(is.na(CustData$AvgP1_var))
CustData$id[9688]

CustData$AvgP3_var <- 0
for (i in 1:16096) {
  CustData$AvgP3_var[i] <- mean(histData$price_p3_var[which(histData$id == CustData$id[i])],na.rm = TRUE)
}

CustData$AvgP1_fix <- 0
for (i in 1:16096) {
  CustData$AvgP1_fix[i] <- mean(histData$price_p1_fix[which(histData$id == CustData$id[i])],na.rm = TRUE)
}

CustData$AvgP2_fix <- 0
for (i in 1:16096) {
  CustData$AvgP2_fix[i] <- mean(histData$price_p2_fix[which(histData$id == CustData$id[i])],na.rm = TRUE)
}

CustData$AvgP3_fix <- 0
for (i in 1:16096) {
  CustData$AvgP3_fix[i] <- mean(histData$price_p3_fix[which(histData$id == CustData$id[i])],na.rm = TRUE)
}

#Categorise dates to month
str(CustData$date_end)
summary(CustData$date_end)
format(as.Date(CustData$date_end[1], "%d/%m/%Y"), "%Y")
as.numeric(format(as.Date(CustData$date_end[1],"%Y-%m-%d"),"%m"))

CustData$date_end_class <- 0
for (i in 1:16096) {
  CustData$date_end_class[i] <- as.numeric(format(as.Date(CustData$date_end[i],"%Y-%m-%d"),"%Y%m"))
}
head(CustData$date_end_class)
table(CustData$date_end_class)
tapply(CustData$churn,CustData$date_end_class,sum)

CustData$date_end_class <- as.factor(CustData$date_end_class)
str(CustData$date_end_class)


#See initially if Churn is affected by elect consumption
CustData$con_12_Class <- ifelse(CustData$cons_12m < 0, -1, ifelse(CustData$cons_12m == 0, 0, 1))
table(CustData$con_12_Class,CustData$churn)
plot(jitter(CustData$con_12_Class),CustData$churn)
tapply(CustData$churn,CustData$con_12_Class,sum)
#20% of customers of negative consumption tend to churn compared to only 10% when consumption 0 or positive.

#See gas consumption of those who do not need electricity
table(CustData$con_12_Class,CustData$has_gas)
6/NROW(subset(CustData$con_12_Class,CustData$con_12_Class<0))
20/NROW(subset(CustData$con_12_Class,CustData$con_12_Class==0))
2938/NROW(subset(CustData$con_12_Class,CustData$con_12_Class>0))
#not much difference in number of ppl who subscribe to gas (within different elec consumption)
summary(CustData$cons_gas_12m)
CustData$cons_gas_12_Class <- ifelse(CustData$cons_gas_12m < 0, -1, ifelse(CustData$cons_gas_12m == 0, 0, 1))
CustData$cons_gas_12_Class[which(CustData$cons_gas_12m<0)]
table(CustData$has_gas,CustData$cons_gas_12_Class)
table(CustData$con_12_Class,CustData$cons_gas_12_Class)

#see churn rates of people who do not use gas or elect in last 12 months
table(CustData$churn[which(CustData$con_12_Class==0 & CustData$cons_gas_12_Class==0)])
10/sum(table(CustData$churn[which(CustData$con_12_Class==0 & CustData$cons_gas_12_Class==0)]))

#see forecasted data of churn
summary(CustData$forecast_bill_12m)
summary(CustData$forecast_cons_12m)

#how much net margin churn take from power subscription? 12% of net revenue
head(CustData$margin_gross_pow_ele,10)
head(CustData$margin_net_pow_ele,10)
tapply(CustData$margin_net_pow_ele,CustData$churn,sum,na.rm=TRUE)
42708.78/sum(tapply(CustData$margin_net_pow_ele,CustData$churn,sum,na.rm=TRUE))



#Analysis of ppl who churn in 3 months
ChurnP <- subset(CustData, CustData$churn==1)
NoChurn <- subset(CustData, CustData$churn==0)
summary(ChurnP)
str(ChurnP)

summary(ChurnP$num_years_antig)
hist(ChurnP$num_years_antig)
hist(NoChurn$num_years_antig)
hist(CustData$num_years_antig)
#Most People who chrun were customers less than 6 yrs
ChurnP$antig_class <- ifelse(ChurnP$num_years_antig <= 6, 1, 2)
table(ChurnP$antig_class)
104/sum(table(ChurnP$antig_class)) #6.5% are above 6 years
table(ChurnP$num_years_antig)

## Antiquity of customers overall
CustData$antig_class <- ifelse(CustData$num_years_antig <= 6, 1, 2)
table(CustData$antig_class)
1447/sum(table(CustData$antig_class))
table(CustData$num_years_antig)

NROW(which(ChurnP$antig_class == 2))/NROW(which(CustData$antig_class == 2))
NROW(which(ChurnP$antig_class == 1))/NROW(which(CustData$antig_class == 1))
104/1595 #% of those above 7 yrs among those who leave
NROW(which(ChurnP$num_years_antig==6))
20000/1595  # Average amount loss from discount per year per customer
(0.94*368*12.5)/20000 # % of savings back if stop discount to those once they are in 7th year

NoChurn$antig_class <- ifelse(NoChurn$num_years_antig <= 6, 1, 2)
table(NoChurn$antig_class)
1343/sum(table(NoChurn$antig_class))
table(NoChurn$num_years_antig)
summary(NoChurn$num_years_antig[which(NoChurn$antig_class==2)]) #Customers who stay beyond 6yrs stay for at least 2 more years on average

#Average elect usage of Churns are lower
summary(ChurnP$cons_12m/12)
summary(CustData$cons_12m/12)
summary(NoChurn$cons_12m/12)
88760/194800 #average usage
200/15130 #diff in median usage
plot(ChurnP$cons_12m)
plot(CustData$cons_12m)


summary(ChurnP$cons_last_month)
summary(CustData$cons_last_month)
summary(NoChurn$cons_last_month)

#Similar Profile for date end and renewal for those churn and did not churn: they do not care about their contract
summary(ChurnP$date_end)
plot(ChurnP$date_end)
plot(ChurnP$date_renewal)
plot(NoChurn$date_end)
plot(NoChurn$date_renewal)
modelEnd <- lm(churn~date_end-1, data = CustData)
summary(modelEnd)
head(sort(table(ChurnP$date_end),decreasing = TRUE))/sum(table(ChurnP$date_end))
head(sort(table(NoChurn$date_end),decreasing = TRUE))/sum(table(NoChurn$date_end))
NROW(which(NoChurn$date_end=='2016-06-07'))/sum(table(NoChurn$date_end))

head(sort(table(ChurnP$date_renewal[which(ChurnP$date_end=='2016-06-07')]),decreasing = TRUE)) #renewal date 1 yr before
# head(sort(table(NoChurn$date_renewal[which(NoChurn$date_end=='2016-06-07')]),decreasing = TRUE))

summary(ChurnP$date_renewal)
head(sort(ChurnP$date_renewal,decreasing = TRUE))
head(sort(NoChurn$date_renewal,decreasing = TRUE))
head(histData$price_date)
modelRW <- lm(churn~date_renewal-1, data = CustData)
summary(modelRW)
head(sort(table(ChurnP$date_renewal),decreasing = TRUE))/sum(table(ChurnP$date_renewal))
head(sort(table(NoChurn$date_renewal),decreasing = TRUE))/sum(table(NoChurn$date_renewal))
#top 5 profile for date renewal is similar for both who churn and did not.
head(sort(table(NoChurn$date_end[which(NoChurn$date_renewal=='2015-06-23')]),decreasing = TRUE))
#Check if ppl who do not churn actually renew their contract
ChurnP$date_end_class <- 0
ChurnP$date_renewal_class <- 0
for (i in 1:1595) {
  ChurnP$date_end_class[i] <- as.numeric(format(as.Date(ChurnP$date_end[i],"%Y-%m-%d"),"%Y%m"))
  ChurnP$date_renewal_class[i] <- as.numeric(format(as.Date(ChurnP$date_renewal[i],"%Y-%m-%d"),"%Y%m"))
}

NoChurn$date_end_class <- 0
NoChurn$date_renewal_class <- 0
for (i in 1:14501) {
  NoChurn$date_end_class[i] <- as.numeric(format(as.Date(NoChurn$date_end[i],"%Y-%m-%d"),"%Y%m"))
  NoChurn$date_renewal_class[i] <- as.numeric(format(as.Date(NoChurn$date_renewal[i],"%Y-%m-%d"),"%Y%m"))
}

head(sort(table(ChurnP$date_end_class[which(ChurnP$date_renewal_class=='201505')]),decreasing = TRUE))
head(sort(table(NoChurn$date_end_class[which(NoChurn$date_renewal_class=='201505')]),decreasing = TRUE))

sort(table(NoChurn$date_renewal_class),decreasing = TRUE)
sort(table(Churn$date_end_class),decreasing = TRUE)
table(CustData$date_end_class,CustData$churn)
#need additional time spactial data to confirm

summary(ChurnP$nb_prod_act)
summary(NoChurn$nb_prod_act)
plot(ChurnP$forecast_base_bill_ele)
plot(NoChurn$forecast_base_bill_ele)
#forecast usage for churners higher than non churners
summary(ChurnP$forecast_cons_12m)
summary(NoChurn$forecast_cons_12m)

#Those who churn were mainly in different company's activity compared to those who did not
plot(ChurnP$activity_new)
NoCat <- subset(ChurnP$activity_new,!ChurnP$activity_new=='')
plot(NoCat)
NoCatNC <- subset(NoChurn$activity_new,!NoChurn$activity_new=='')
plot(sort(NoCatNC,decreasing = TRUE))
sort(table(NoCatNC),decreasing = TRUE)
head(sort(table(NoCat),decreasing = TRUE))/sum(table(NoCat))
head(sort(table(NoCatNC),decreasing = TRUE))/sum(table(NoCatNC))

NROW(which(NoCatNC=='fmwdwsxillemwbbwelxsampiuwwpcdcb'))/sum(table(NoCatNC)) #2% more than didnt churn
NROW(which(NoCatNC=='kwuslieomapmswolewpobpplkaooaaew'))/sum(table(NoCatNC)) #1.5% more than didnt churn
NROW(which(NoCatNC=='wxemiwkumpibllwklfbcooafckufkdlm'))/sum(table(NoCatNC)) #1% more 
NROW(which(NoCatNC=='cluecxlameloamldmasudocsbmaoamdw'))/sum(table(NoCatNC)) #1% more
head(sort(ChurnP$date_renewal,decreasing = TRUE))

plot(NoChurn$activity_new)
summary(NoChurn$activity_new)

#Little link between overall Channel sales and Churn. Only able to explain 10% of Churning effect
#While that sales channel has lots of sales, most of the customers are also leaving from there and there is a larger than propotion leaving
summary(ChurnP$channel_sales) #mostly belonging from "foosdfpfkusacimwkcsosbicdxkicaua" sales channel
plot(ChurnP$channel_sales)
plot(NoChurn$channel_sales)
summary(ChurnP$channel_sales)
summary(NoChurn$channel_sales)
summary(ChurnP$channel_sales)+ summary(NoChurn$channel_sales)
NROW(which(ChurnP$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'))/sum(table(ChurnP$channel_sales)) #much higher proportion
NROW(which(NoChurn$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'))/sum(table(NoChurn$channel_sales))

model2 <- lm(churn~channel_sales-1, data = CustData1)
summary(model2)

modelCS <- glm(churn~channel_sales-1, data = CustData1, family = binomial)
summary(modelCS)
step(model1)

table(ChurnP$channel_sales)/ table(CustData$channel_sales) #highest turnover within sales channel "foosdfpfkusacimwkcsosbicdxkicaua" with 12% loss
mean(table(ChurnP$channel_sales)/ table(CustData$channel_sales))  #More than twice the average rate of 5%
#identify big impact danger customer

#Correlation between subscribed power and consumption
#Initial analysis
plot(CustData$pow_max,CustData$cons_12m)
lm1 <-  lm(pow_max~cons_12m+cons_gas_12m, data = CustData)
abline(lm1)
summary(lm1)
Subpow <- subset(CustData, CustData$pow_max <=100)
plot(Subpow$pow_max,Subpow$cons_12m)
summary(Subpow$cons_12m)
Subpow$con_adjust <- log(Subpow$cons_12m+125301)
plot(Subpow$pow_max,Subpow$con_adjust)
lm2 <-  lm(pow_max~con_adjust, data = Subpow)
abline(lm2)
summary(lm2)
cor(Subpow$pow_max,Subpow$con_adjust)
cor(Subpow$pow_max,Subpow$cons_12m)

Subpow1 <- subset(CustData, CustData$pow_max !='')
cor(Subpow1$pow_max,Subpow1$cons_12m)
summary(Subpow1$pow_max)

lm3 <- lm(pow_max~poly(cons_12m,3), data = Subpow)
summary(lm3)
abline(lm3)
plot(fitted(lm3),residuals(lm3))

plot(CustData$pow_max,CustData$cons_12m)
Custcon <- subset(CustData, select = c("cons_12m","pow_max"))
which(Custcon$cons_12m>1.5*10^7)
which(Custcon$cons_12m>5*10^6 & Custcon$pow_max>200)
Custcon <- Custcon[-c(1119,4371,12742),]
str(Custcon)
plot(Custcon$pow_max,Custcon$cons_12m)
plot(Custcon$pow_max,log(Custcon$cons_12m))
Custcon <- subset(Custcon, !is.na(Custcon$pow_max))
cor(Custcon$pow_max,Custcon$cons_12)
cor(Custcon$pow_max,Custcon$cons_12)
lm4 <-  lm(pow_max~cons_12m, data = Custcon)
abline(lm4)
summary(lm4)
lm5 <- lm(pow_max~poly(cons_12m,2), data = Custcon)
abline(lm5)
summary(lm5)

#Get rid of outlier and NA data for power and consumption data
con_pow <- CustData[-c(1119,4371,12742),]
con_pow <- subset(con_pow, !is.na(con_pow$pow_max))
str(con_pow)
cor(con_pow$pow_max,con_pow$cons_12m)

hist(con_pow$num_years_antig)
plot(con_pow$pow_max[which(con_pow$num_years_antig<=2)],con_pow$cons_12m[which(con_pow$num_years_antig<=2)])
plot(con_pow$pow_max[which(con_pow$num_years_antig>=8)],con_pow$cons_12m[which(con_pow$num_years_antig>=8)])
plot(con_pow$pow_max[which(con_pow$num_years_antig==10)],con_pow$cons_12m[which(con_pow$num_years_antig==10)])

cor(con_pow$pow_max[which(con_pow$num_years_antig<=2)],con_pow$cons_12m[which(con_pow$num_years_antig<=2)])
cor(con_pow$pow_max[which(con_pow$num_years_antig==10)],con_pow$cons_12m[which(con_pow$num_years_antig==10)])
cor(con_pow$pow_max[which(con_pow$num_years_antig>=7)],con_pow$cons_12m[which(con_pow$num_years_antig>=7)])

#Correlation for churner with 3 yrs and less with Power co = 0.38, non churners with =<3 yrs = 0.1, =<2 yrs = 0.38
plot(con_pow$pow_max[which(con_pow$churn==1&con_pow$num_years_antig<=3)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$num_years_antig<=3)])
plot(con_pow$pow_max[which(con_pow$churn==0&con_pow$num_years_antig<=3)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$num_years_antig<=3)])
cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$num_years_antig<=3)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$num_years_antig<=3)])
cor(con_pow$pow_max[which(con_pow$churn==0&con_pow$num_years_antig<=3)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$num_years_antig<=3)])

#Correlation for churner 4 to 6 yrs with Power co = 0.11, non churners with 4 to 6 yrs = 0.1
cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$churn==0&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)])

#Correlation for churner 7 to 9 yrs with Power co = 0.04, non churners with 3 -yrs = 0.11
cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$num_years_antig>6&con_pow$num_years_antig<=9)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$num_years_antig>6&con_pow$num_years_antig<=9)])
cor(con_pow$pow_max[which(con_pow$churn==0&con_pow$num_years_antig>6&con_pow$num_years_antig<=9)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$num_years_antig>6&con_pow$num_years_antig<=9)])

#Correlation for churner with more than 9 yrs with Power co = 0.58, non churners with >9 yrs = 0.17, <6 yrs = 0.13
plot(con_pow$pow_max[which(con_pow$churn==1&con_pow$num_years_antig>9)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$num_years_antig>9)])
plot(con_pow$pow_max[which(con_pow$churn==0&con_pow$num_years_antig>9)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$num_years_antig>9)])
cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$num_years_antig>9)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$num_years_antig>9)])
cor(con_pow$pow_max[which(con_pow$churn==0&con_pow$num_years_antig>9)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$num_years_antig>9)])

cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$num_years_antig==9)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$num_years_antig==9)])

#Does forecast power price affect correlation pattern? No
summary(CustData$forecast_price_pow_p1)
cor(con_pow$pow_max[which(con_pow$forecast_price_pow_p1<=43.5&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$forecast_price_pow_p1<=43.5&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$forecast_price_pow_p1>43.5&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$forecast_price_pow_p1>43.5&con_pow$num_years_antig>3&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$forecast_price_pow_p1>43.5)],con_pow$cons_12m[which(con_pow$forecast_price_pow_p1>43.5)])

#Does Sales channel price affect correlation pattern? Only for 'food... the most popular sales channel'
summary(CustData$channel_sales)
plot(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua')],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua')],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig<3)],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig<3)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>6)])

cor(con_pow$pow_max[which(con_pow$channel_sales=='')],con_pow$cons_12m[which(con_pow$channel_sales=='')])
cor(con_pow$pow_max[which(con_pow$channel_sales==''&con_pow$num_years_antig<3)],con_pow$cons_12m[which(con_pow$channel_sales==''&con_pow$num_years_antig<3)])
cor(con_pow$pow_max[which(con_pow$channel_sales==''&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)],con_pow$cons_12m[which(con_pow$channel_sales==''&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)])
cor(con_pow$pow_max[which(con_pow$channel_sales==''&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$channel_sales==''&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$channel_sales==''&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$channel_sales==''&con_pow$num_years_antig>6)])

plot(con_pow$pow_max[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu')],con_pow$cons_12m[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu')],con_pow$cons_12m[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig<3)],con_pow$cons_12m[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig<3)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)],con_pow$cons_12m[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$channel_sales=='epumfxlbckeskwekxbiuasklxalciiuu'&con_pow$num_years_antig>6)])

plot(con_pow$pow_max[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema')],con_pow$cons_12m[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema')],con_pow$cons_12m[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig<3)],con_pow$cons_12m[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig<3)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)],con_pow$cons_12m[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$channel_sales=='lmkebamcaaclubfxadlmueccxoimlema'&con_pow$num_years_antig>6)])

plot(con_pow$pow_max[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci')],con_pow$cons_12m[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci')],con_pow$cons_12m[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig<3)],con_pow$cons_12m[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig<3)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)],con_pow$cons_12m[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$channel_sales=='ewpakwlliwisiwduibdlfmalxowmwpci'&con_pow$num_years_antig>6)])

plot(con_pow$pow_max[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds')],con_pow$cons_12m[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds')],con_pow$cons_12m[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig<3)],con_pow$cons_12m[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig<3)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)],con_pow$cons_12m[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$channel_sales=='sddiedcslfslkckwlfkdpoeeailfpeds'&con_pow$num_years_antig>6)])

plot(con_pow$pow_max[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf')],con_pow$cons_12m[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf')],con_pow$cons_12m[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf')])
cor(con_pow$pow_max[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig<3)],con_pow$cons_12m[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig<3)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)],con_pow$cons_12m[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig>=3&con_pow$num_years_antig<=4)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig>=5&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$channel_sales=='usilxuppasemubllopkaafesmlibmsdf'&con_pow$num_years_antig>6)])

#Combine sales channel and churns
str(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$churn==1)])
plot(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$churn==1)],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$churn==1)])
cor(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$churn==1)],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$churn==1)])
#0.47
cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig<4)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig<4)])
#0.12
cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=4&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=4&con_pow$num_years_antig<=6)])
#0.42
cor(con_pow$pow_max[which(con_pow$churn==1&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$churn==1&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>6)])

#only above 6 yrs good with 0.58
cor(con_pow$pow_max[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$churn==0)],con_pow$cons_12m[which(con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$churn==0)])
cor(con_pow$pow_max[which(con_pow$churn==0&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig<4)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig<4)])
cor(con_pow$pow_max[which(con_pow$churn==0&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=4&con_pow$num_years_antig<=6)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>=4&con_pow$num_years_antig<=6)])
cor(con_pow$pow_max[which(con_pow$churn==0&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>6)],con_pow$cons_12m[which(con_pow$churn==0&con_pow$channel_sales=='foosdfpfkusacimwkcsosbicdxkicaua'&con_pow$num_years_antig>6)])

#Clean data to remove NA from channel_sale
# CustData1 <- subset(CustData,!is.na(CustData$channel_sales))
# CustData1 <- subset(CustData,CustData$channel_sales!='')
# str(CustData1)


#Create Initial logistic Regression model to see significant  variables
#split data into training and test
set.seed(1)
split <- sample.split(CustData$churn,SplitRatio = 0.65) #split data up into 65% for training set and 35% for test/valid set
training1 <- subset(CustData,split == TRUE)
test <- subset(CustData,split == FALSE)
split2 <- sample.split(test$churn,SplitRatio = 0.5) #split test/valid data up into 50% for validate set and 50% for test set
valid1 <- subset(test,split2 == TRUE)
test1 <- subset(test,split2 == FALSE)
#str(split2)

model1 <- glm(churn~activity_new-1,data = training1,family = binomial)
summary(model1)


modelConE <- glm(churn~con_12_Class-1, data = training1, family = binomial)
summary(modelConE) #elec consumption good indicator for predicting churn

modelCon <- glm(churn~con_12_Class+cons_gas_12_Class-1, data = training1, family = binomial)
summary(modelCon) #elec & gas consumption good indicator for predicting churn

model3 <- glm(churn~con_12_Class+cons_gas_12_Class+num_years_antig-1, data = training1, family = binomial)
summary(model3) #elec, gas consumption and anti good indicator for predicting churn

model4 <- glm(churn~con_12_Class+cons_gas_12_Class+num_years_antig+nb_prod_act+channel_sales-1, data = training1, family = binomial)
summary(model4)

model5 <- glm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales-1, data = training1, family = binomial)
summary(model5)

model6 <- glm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales+pow_max+date_end_class-1, data = training1, family = binomial)
summary(model6)
model7 <- glm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales+AvgP3_fix+AvgP1_var+forecast_meter_rent_12m-1, data = training1, family = binomial)
# model7 <- glm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales+AvgP3_fix+AvgP1_var+forecast_meter_rent_12m+date_end-1, data = CustData, family = binomial)
summary(model7)
predtrain <- predict(model7,newdata=training1,type="response")
summary(predtrain)
Qtrain <- as.numeric(predtrain>0.5)
table(Qtrain,training1$churn)
# model8 <- glm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales+AvgP3_fix+AvgP1_var+forecast_meter_rent_12m+activity_new+cons_last_month+date_activ-1, data = training1, family = binomial)
# summary(model8)

#training Accuracy
predtrain <- predict(model8,newdata=training1,type="response")
summary(predtrain)
Qtrain <- as.numeric(predtrain>0.5)
table(Qtrain,training1$churn)

#determine cutoff using validation
pred <- predict(model7,newdata=valid1,type="response")
max(pred)
acc <- list()
for (i in 1:50) {
  v <- as.numeric(pred>(i/100))
  # v <- as.numeric(pred>(1/100))
  accuracy <- (table(v,valid1$churn)[1]+table(v,valid1$churn)[4])/sum(table(v,valid1$churn))
  acc[i] <- accuracy
}
Threshold <- which.max(acc)/100

#test accuracy
predictTest <- predict(model7,newdata=test1,type="response")
head(predictTest)
max(predictTest)
summary(predictTest)
head(sort(predictTest, decreasing = TRUE),10)
test1$predicted <- predictTest
head(test1$predicted)
top10 <- test1[order(-test1$predicted),]
head(top10$predicted)
table(top10$churn[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)])
table(top10$churn[c(seq(1,2816))])
str(test1$churn)
str(predictTest)
Q2 <- as.numeric(predictTest>Threshold)
Q2 <- as.numeric(predictTest>0.18)
max(Q2)
table(Q2,test1$churn)


#test initial model
predict1 <- predict(model7,newdata=test1,type="response")
head(predict1)
str(predict1)
summary(predict1)
Q <- as.numeric(predict1>0.5)
max(predict1)
max(Q)
table(Q,test1$churn)
(1830+105)/sum(table(test2$churn))

table(test2$churn) #baseline predicing everything 0
2523/sum(table(test1$churn))


predict2 <- predict(model4,newdata=test1,type="response")
Q2 <- as.numeric(predict2>0.11)
head(predict2)
max(predict2)
summary(predict2)
max(Q2)
table(Q2,test1$churn)
1672+119

#ROCR Curve
#library(ROCR)
ROCRpred <- prediction(predict1, test1$churn)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


#Create clean dataset with sig variables only
#cons_12m cons_gas_12m num_years_antig channel_sales AvgP3_fix AvgP1_var forecast_meter_rent_12m forecast_price_energy_p1 date_end_class

summary(CustData$date_end_class)
summary(CustData1$date_end_class)
#Clean data to remove NA
CustData1 <- subset(CustData,!is.na(CustData$AvgP3_fix))
CustData2 <- subset(CustData1,!is.na(CustData1$date_end_class))
str(CustData1)

Data3 <- data.frame(CustData2$cons_12m,CustData2$cons_gas_12m,CustData2$num_years_antig,CustData2$channel_sales,CustData2$AvgP3_fix,CustData2$AvgP1_var,CustData2$forecast_meter_rent_12m,CustData2$forecast_price_energy_p1,CustData2$date_end_class)
Data3 <- subset(CustData2, select=c("cons_12m", "cons_gas_12m","num_years_antig","channel_sales","AvgP3_fix","AvgP1_var","forecast_meter_rent_12m","forecast_price_energy_p1","date_end_class","churn"))
str(Data3)
write.csv(Data3, file="cust_data.csv")

#split Clean data into training and test
set.seed(1)
split <- sample.split(Data3$churn,SplitRatio = 0.65) #split data up into 65% for training set and 35% for test/valid set
training2 <- subset(Data3,split == TRUE)
test <- subset(Data3,split == FALSE)
split2 <- sample.split(test$churn,SplitRatio = 0.5) #split test/valid data up into 50% for validate set and 50% for test set
valid2 <- subset(test,split2 == TRUE)
test2 <- subset(test,split2 == FALSE)

#SVM Model
modelsvm <- svm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales+AvgP3_fix+AvgP1_var+forecast_meter_rent_12m+forecast_price_energy_p1, data = training2)
pred <- predict(modelsvm,test2)
summary(pred)
S <- as.numeric(pred>0.04)
max(S)
table(S,test2$churn)
2348/sum(table(test1$churn))

table(test1$churn) #baseline predicing everything 0
2537/sum(table(test1$churn))

#evaluating if custermers are price sensitive
#Overall prices charged are similar for both groups, which seems to suggest that they are not price sensitive.
tapply(CustData$AvgP1_var,CustData$churn,mean,na.rm = TRUE) #not much diff
tapply(CustData$AvgP2_var,CustData$churn,mean,na.rm = TRUE) #not much diff
tapply(CustData$AvgP3_var,CustData$churn,mean,na.rm = TRUE) #not much diff
tapply(CustData$AvgP1_fix,CustData$churn,mean,na.rm = TRUE) #not much diff
tapply(CustData$AvgP2_fix,CustData$churn,mean,na.rm = TRUE) #slightly lower avg price for non churners
tapply(CustData$AvgP3_fix,CustData$churn,mean,na.rm = TRUE) #slightly lower avg price for non churners

summary(CustData$AvgP3_fix[which(CustData$churn==1)])
summary(CustData$AvgP3_fix[which(CustData$churn==0)])
hist(CustData$AvgP3_fix[which(CustData$churn==1)])
hist(CustData$AvgP3_fix[which(CustData$churn==0)])

#Customer defection margins analysis
CustData$Avg_Yr_margin <- CustData$net_margin/CustData$num_years_antig
tapply(CustData$Avg_Yr_margin,CustData$churn,sum,na.rm=TRUE)
tapply(CustData$Avg_Yr_margin,CustData$churn,sum,na.rm=TRUE)[2]/sum(tapply(CustData$Avg_Yr_margin,CustData$churn,sum,na.rm=TRUE))
head(sort(ChurnP$Avg_Yr_margin, decreasing =TRUE))
hist(ChurnP$Avg_Yr_margin, breaks=500, main='Breaks=500')
summary(ChurnP$Avg_Yr_margin)

hist(CustData$Avg_Yr_margin)
summary(CustData$Avg_Yr_margin)
CustData$Avg_Yr_margin_Class <- ifelse(CustData$Avg_Yr_margin < 15, 1, ifelse(CustData$Avg_Yr_margin < 30, 2, ifelse(CustData$Avg_Yr_margin < 45, 3, ifelse(CustData$Avg_Yr_margin < 60, 4, 5))))
summary(CustData$Avg_Yr_margin_Class)
hist(CustData$Avg_Yr_margin_Class)
summary(CustData$Avg_Yr_margin_Class[which(CustData$churn==1)])
hist(CustData$Avg_Yr_margin_Class[which(CustData$churn==1)])
#greater proportion of higer spenders in ppl who churn
table(CustData$Avg_Yr_margin_Class)[5]/sum(table(CustData$Avg_Yr_margin_Class))
table(CustData$Avg_Yr_margin_Class[which(CustData$churn==1)])[5]/sum(table(CustData$Avg_Yr_margin_Class[which(CustData$churn==1)]))

#evaluating 20% discount
summary(ChurnP$net_margin)
0.2*sum(ChurnP$net_margin)
sum(ChurnP$net_margin)/ sum(CustData$net_margin,na.rm = TRUE)
sum(ChurnP$Avg_Yr_margin)/sum(CustData$Avg_Yr_margin,na.rm = TRUE) #Churners take 12.7% average net margin
losingM <- subset(ChurnP, ChurnP$net_margin<0)
summary(losingM)
ChurnP$Avg_Yr_margin <- ChurnP$net_margin/ChurnP$num_years_antig
summary(ChurnP$Avg_Yr_margin)
0.2*sum(ChurnP$Avg_Yr_margin)/sum(CustData$Avg_Yr_margin,na.rm = TRUE)
sum(ChurnP$Avg_Yr_margin) #Total Net margin per year on average
plot(ChurnP$num_years_antig,ChurnP$Avg_Yr_margin)
sum(ChurnP$Avg_Yr_margin[which(ChurnP$num_years_antig == 6)])
NROW(which(ChurnP$antig_class == 2))/NROW(which(CustData$antig_class == 2))
NROW(which(ChurnP$antig_class == 1))/NROW(which(CustData$antig_class == 1))
104/1595 #% of those above 7 yrs among those who leave
NROW(which(ChurnP$num_years_antig==6))
20000/1595  # Average amount loss from discount per year per customer
(0.94*368*12.5)/20000 # % of savings back if stop discount to those once they are in 7th year
1595/16096 #10% of overall customer churning

#Random analysis
summary(test1$forecast_bill_12m)
summary(test1$pow_max)
head(test1$forecast_bill_12m,20)
head(test1$imp_cons,20)

#Oversampling
data_balanced_over <- ovun.sample(churn~., data = training1, method = "over",N = 18852)$data

str(training1)
table(training1$churn)

training1$churn <- as.factor(training1$churn)
test1$churn <- as.factor(test1$churn)
str(test1$churn)
Add_sample <- subset(training1, training1$churn==1) 
data.balanced.ou <- rbind(training1,Add_sample)
data.balanced.ou <- rbind(data.balanced.ou,Add_sample)  #must do 4 times to get equal proportion
table(data.balanced.ou$churn)
str(data.balanced.ou)

model7_Bal <- glm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales+AvgP3_fix+AvgP1_var+forecast_meter_rent_12m-1, data = data.balanced.ou, family = binomial)
# model7 <- glm(churn~cons_12m+cons_gas_12m+num_years_antig+channel_sales+AvgP3_fix+AvgP1_var+forecast_meter_rent_12m+date_end-1, data = CustData, family = binomial)
summary(model7_Bal)

predict_bal <- predict(model7_Bal,newdata=test1,type="response")
head(predict_bal)
max(predict_bal)
summary(predict_bal)
Q_b <- as.numeric(predict_bal>0.68)
max(Q_b)
table(Q_b,test1$churn)
table(test1$churn)

head(sort(predict_bal, decreasing = TRUE),10)
test1$predicted_bal <- predict_bal
head(test1$predicted_bal)
top10_bal <- test1[order(-test1$predicted_bal),]
head(top10_bal$predicted_bal)
table(top10_bal$churn[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)])
# table(top10_bal$churn[c(seq(1,2816*0.1))])
# (table(top10_bal$churn[c(seq(1,2816*0.3))])/sum(table(top10_bal$churn[c(seq(1,2816*0.3))])))[2]

#getting accuracy for different percentile range
prd_list <- list()
for (i in 1:10) {
  accuracy <- table(top10_bal$churn[c(seq(1,2816*(i/10)))])/sum(table(top10_bal$churn[c(seq(1,2816*(i/10)))]))
  #accuracy <- (table(v,valid1$churn)[1]+table(v,valid1$churn)[4])/sum(table(v,valid1$churn))
  prd_list[i] <- accuracy[2]
}
prd_list

#Tree model
library(rpart)
tree_1 <- rpart(churn~cons_12m+num_years_antig+channel_sales+AvgP3_fix+AvgP1_var+forecast_meter_rent_12m, data = data.balanced.ou)
summary(tree_1)
pred.tree_1 <- predict(tree_1,newdata = test1)
roc.curve(test1$churn,pred.tree_1[,2])
head(pred.tree_1[,2])

test1$predicted_bal_tree <- pred.tree_1[,2]
head(test1$predicted_bal_tree)
top10_bal_tree <- test1[order(-test1$predicted_bal_tree),]
head(top10_bal_tree$predicted_bal_tree)
table(top10_bal_tree$churn[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)])

tree.prd_list <- list()
for (i in 1:10) {
  accuracy <- table(top10_bal_tree$churn[c(seq(1,2816*(i/10)))])/sum(table(top10_bal_tree$churn[c(seq(1,2816*(i/10)))]))
  #accuracy <- (table(v,valid1$churn)[1]+table(v,valid1$churn)[4])/sum(table(v,valid1$churn))
  tree.prd_list[i] <- accuracy[2]
}
tree.prd_list
tree_1$variable.importance
summary(tree_1)

tree_2 <- rpart(churn~date_end_class, data = data.balanced.ou,method = 'class')
tree_2 <- rpart(churn~AvgP3_fix+cons_12m+num_years_antig+AvgP1_var+forecast_meter_rent_12m+cons_gas_12m, data = data.balanced.ou,method = 'class')
# tree_2 <- rpart(churn~AvgP3_fix+con_12_Class+num_years_antig+AvgP1_var+forecast_meter_rent_12m+cons_gas_12m, data = data.balanced.ou)
summary(tree_2)
summary(tree_1)
tree_2$variable.importance
rpart.plot::rpart.plot(tree_2)
tree_3 <- rpart(churn~AvgP3_fix+cons_12m+num_years_antig+AvgP1_var+forecast_meter_rent_12m+cons_gas_12m+channel_sales, data = data.balanced.ou,method = 'class')
summary(tree_3)
rpart.plot::rpart.plot(tree_3)
pred.tree_2 <- predict(tree_2,newdata = test1)
#library(ROSE)
roc.curve(test1$churn,pred.tree_2[,2])
roc.curve(test1$churn,pred.tree_1[,2])

test1$predicted_bal_tree2 <- pred.tree_2[,2]
head(test1$predicted_bal_tree2)
top10_bal_tree2 <- test1[order(-test1$predicted_bal_tree2),]
head(top10_bal_tree2$predicted_bal_tree2)
table(top10_bal_tree2$churn[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)])

tree.prd_list2 <- list()
for (i in 1:10) {
  accuracy <- table(top10_bal_tree2$churn[c(seq(1,2816*(i/10)))])/sum(table(top10_bal_tree2$churn[c(seq(1,2816*(i/10)))]))
  #accuracy <- (table(v,valid1$churn)[1]+table(v,valid1$churn)[4])/sum(table(v,valid1$churn))
  tree.prd_list2[i] <- accuracy[2]
}
tree.prd_list2

#Q1
NROW(which(ChurnP$antig_class==2))
NROW(which(ChurnP$antig_class==1))
table(CustData$antig_class)

#Q2
NROW(which(CustData$num_years_antig<=4))/NROW(CustData$id)
NROW(which(ChurnP$num_years_antig<=4))/NROW(ChurnP$id)
NROW(which(NoChurn$num_years_antig<=4))/NROW(NoChurn$id)

#Factsheet
sum(CustData$net_margin, na.rm = TRUE)
sum(CustData$Avg_Yr_margin, na.rm = TRUE)
mean(CustData$num_years_antig)

#populate test template with result Logit
testCustData$AvgP1_var <- 0
for (i in 1:4024) {
  testCustData$AvgP1_var[i] <- mean(testhistData$price_p1_var[which(testhistData$id == testCustData$id[i])],na.rm = TRUE)
}

testCustData$AvgP3_fix <- 0
for (i in 1:4024) {
  testCustData$AvgP3_fix[i] <- mean(testhistData$price_p3_fix[which(testhistData$id == testCustData$id[i])],na.rm = TRUE)
}

pre.logit_test <- predict(model7_Bal, newdata = testCustData, type = "response")
str(pre.logit_test)
head(pre.logit_test)
summary(pre.logit_test)
testChurnData$Logit_probability <- pre.logit_test
testChurnData$Logit_prediction <- as.numeric(pre.logit_test>0.5)

#populate test template with result Tree
pred.tree_Test <- predict(tree_2,newdata = testCustData)
str(pred.tree_Test)
head(as.numeric(pred.tree_Test[,2]>0.5))

head(testChurnData)
str(testChurnData)
testChurnData$Tree_probability <- pred.tree_Test[,2]
testChurnData$Tree_prediction <- as.numeric(pred.tree_Test[,2]>0.5)

write.csv(testChurnData, file="Output_test_Benny.csv")
