####### Didier Bouba Ndengue    ############
### Data Analysis Excercise   
##      9/20/2018
###  Load and analyse the data to create Fraud Rules
##############################################

install.packages("RMySQL")
install.packages("lubridate")
install.packages("party")
install.packages("rpart")
install.packages("rpart.plot")
library(RMySQL) 
library(lubridate) 
library(rpart)
library(rpart.plot)
mydb = dbConnect(MySQL(), user='didier', password='password', dbname='INTUIT', host='localhost')
dbListTables(mydb)

#Transaction info data
rs = dbSendQuery(mydb, " select * from transactionInfo")
TxnInfo = fetch(rs, n=-1)
dbClearResult(dbListResults(mydb)[[1]])

#Merchant info data
rs = dbSendQuery(mydb, " select * from merchantInfo")
MerchInfo = fetch(rs, n=-1)
dbClearResult(dbListResults(mydb)[[1]])

#Rules info data
rs = dbSendQuery(mydb, " select * from RuleInfo")
RuleInfo = fetch(rs, n=-1)
dbClearResult(dbListResults(mydb)[[1]])

#Merging Transactional and merchant info
# merge two data frames by ID
Transactions <- merge(TxnInfo, MerchInfo,by="merchID")

#try to convert date
#Application date
Transactions$application_dt <- parse_date_time(Transactions$app_dt, c('mdy'))
#Authorization date in String
Transactions$authorization_dt<-gsub("(/15 ).*", "\\1", Transactions$AuthDate)
Transactions$auth_dt1 <- parse_date_time(Transactions$authorization_dt, c('mdy'))
##Days on book = Authorization date  - application date
Transactions$Days_On_Books<- difftime(Transactions$auth_dt1 ,Transactions$application_dt , units = c("days"))

##Get the authorization date Hours
Transactions$authDt <-sub(".*/15 ", "", Transactions$AuthDate)
Transactions$authDt <-format(strptime(Transactions$authDt, "%I:%M %p"), format="%H:%M:%S")

Transactions$authDt1 <- strptime(Transactions$authDt, "%H:%M:%S")
Transactions$auth_hour <- format(round(Transactions$authDt1, "hour"),format="%H:%M")
Transactions$auth_hour2 <- format(round(Transactions$authDt1, "hour"),format="%H")


#Convert Amount to double
Transactions$Amount <- as.double(Transactions$Amount)

##Tag Merchants that already have fraud rules

FraudMerchant <- data.frame(unique(RuleInfo$merchID))
FraudMerchant$Fraud = "T"
colnames(FraudMerchant)[1] <- "merchID"
colnames(FraudMerchant)[2] <- "Fraud"

NewData<-merge(x=Transactions,y=FraudMerchant ,by="merchID",all.x=TRUE)
#change NA to False in new dataset
NewData$Fraud[is.na(NewData$Fraud)] <- "F"

#get the month from the authorization date
NewData$Auth_mth_in_digit <-format(NewData$auth_dt1,"%m")
NewData$Auth_mth_in_letter <- format(NewData$auth_dt1,"%B")

### Analyze the Data in Hands
table(NewData$badMerchant)

#Get Amount bad
badData <- subset(NewData, NewData$badMerchant == 1)
sum(badData$Amount)

#Get Good bad
goodData <- subset(NewData, NewData$badMerchant == 0)
sum(goodData$Amount)

#how many data have NA as month --- This should be flagged and review
No_auth_date_merchat <- NewData[is.na(NewData$Auth_mth_in_letter),]

#Analysis Dataset without NAs as Authorization Date
AnalysisData <- subset(NewData, !is.na(NewData$Auth_mth_in_letter))

##Add Region to dataset
AnalysisData$regions <- ifelse(AnalysisData$state %in% c("CT","ME","MA","NH","RI","VT","NJ","NY","PA"), "Northeast",
                               ifelse(AnalysisData$state %in% c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
                                                                "ND","SD"), "Midwest",
                                ifelse(AnalysisData$state %in% c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
                                                                 "KY","MS","TN","AR","LA","OK","TX"), "South","West")))
                         
##Add Period
AnalysisData$Period <- ifelse(AnalysisData$Auth_mth_in_letter %in% c("January", "February", "March"), "Q1",
                              ifelse(AnalysisData$Auth_mth_in_letter %in% c("April", "May", "June"), "Q2", "Q3"))
#Get the first substring of the merchat
AnalysisData$First_Mcc <- substring(AnalysisData$mcc, 1, 1)
AnalysisData$Category <- ifelse(AnalysisData$First_Mcc == 1, "Contractor Services",
                         ifelse(AnalysisData$First_Mcc == 2, "miscellaneous",
                         ifelse(AnalysisData$First_Mcc == 4, "Transportation Services",
                         ifelse(AnalysisData$First_Mcc==5, "Food Services",
                          ifelse(AnalysisData$First_Mcc == 6, "Specialty Cleaning",
                          ifelse(AnalysisData$First_Mcc==7, "Business Services", "Professional/Doctor Services"))))))


#start with BadMerchant
AnalysisData$badMerchant <- as.factor(AnalysisData$badMerchant)
AnalysisData$Transactiontype <- as.factor(AnalysisData$Transactiontype)
AnalysisData$Swiped <- as.factor(AnalysisData$Swiped)
AnalysisData$CardType <- as.factor(AnalysisData$CardType)
AnalysisData$Fraud <- as.factor(AnalysisData$Fraud)
AnalysisData$mcc <- as.factor(AnalysisData$mcc)
AnalysisData$mcc_description <- as.factor(AnalysisData$mcc_description)
AnalysisData$state <- as.factor(AnalysisData$state)
AnalysisData$auth_hour <- as.factor(AnalysisData$auth_hour)
AnalysisData$Days_On_Books <- as.numeric(AnalysisData$Days_On_Books)
AnalysisData$DOB <- as.factor(AnalysisData$Days_On_Books)
AnalysisData$Auth_mth_in_letter1 <- as.factor(AnalysisData$Auth_mth_in_letter)
AnalysisData$Period <- as.factor(AnalysisData$Period)
AnalysisData$regions <- as.factor(AnalysisData$regions)
AnalysisData$Category <- as.factor(AnalysisData$Category)
AnalysisData$auth_hour2 <- as.numeric(AnalysisData$auth_hour2)

##Extract dataset for Florida and Doctor Services
temp_data <-AnalysisData[(AnalysisData$state =="FL"),]
Doctor_data <- AnalysisData[(AnalysisData$First_Mcc==8),]
temp_xx <-AnalysisData[(AnalysisData$First_Mcc==8 & AnalysisData$state =="FL"),]
constructorData <- AnalysisData[(AnalysisData$First_Mcc==1),]
summary(constructorData$badMerchant)
##useful data
#Get Amount bad
badsample <- subset(Doctor_data, Doctor_data$badMerchant == 1)
sum(badsample$Amount)

#Get Good bad
goodSample <- subset(Doctor_data, Doctor_data$badMerchant == 0)
sum(goodSample$Amount)

##Decision Tree Model
#try a little decision tree
library(party)

tree <- ctree(badMerchant ~Days_On_Books+auth_hour2+Category+Amount+state+Swiped+Transactiontype+CardType, data = Doctor_data, controls = ctree_control(mincriterion = 0.9, minsplit = 200))
plot(tree)
tree

#prediction
predict(tree, Q2_Dataset, type="prob")

####RPART  MCC 8 Merchant
tree1 <- rpart(badMerchant ~Days_On_Books+auth_hour2+Category+Amount+state+Swiped+Transactiontype+CardType, data = Doctor_data)
rpart.plot(tree1)
tree1

#look at our rules
rule1MCC8 <- Doctor_data[(Doctor_data$Days_On_Books < 20.5 & Doctor_data$Amount < 69.45 & Doctor_data$state %in% c("FL","HI","MD","NY")),]
rule2MCC8 <- Doctor_data[(Doctor_data$Days_On_Books > 20.5 & Doctor_data$Amount >=2334.435 & Doctor_data$state %in% c("FL","HI","MD","NY")),]
summary(rule2MCC8$Fraud)
##left Join the rules
summary(rule1MCC8$Fraud)
summary(rule2MCC8$Fraud)
#data for each rules
#Get Amount bad
badsamplerule1 <- subset(rule1MCC8, rule1MCC8$badMerchant == 1)
sum(badsamplerule1$Amount)

#Get Good bad
goodSamplerule1 <- subset(rule1MCC8, rule1MCC8$badMerchant == 0)
sum(goodSamplerule1$Amount)
summary(rule1MCC8$badMerchant)

###Rule 2
badsamplerule2 <- subset(rule2MCC8, rule2MCC8$badMerchant == 1)
sum(badsamplerule2$Amount)

#Get Good bad
goodSamplerule2 <- subset(rule2MCC8, rule2MCC8$badMerchant == 0)
sum(goodSamplerule2$Amount)
summary(rule2MCC8$badMerchant)

###Florida Dataset
####RPART  Constructor
tree2 <- rpart(badMerchant ~Days_On_Books+auth_hour2+Category+Amount+state+Swiped+Transactiontype+CardType, data = constructorData)
rpart.plot(tree2)
tree2


