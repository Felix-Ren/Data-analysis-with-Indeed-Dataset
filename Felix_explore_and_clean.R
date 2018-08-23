# load data ---------------------------------------------------------------
load(file = "IndeedSample.rda")


# (Vileena) clean first col 1 - col 8 ---------------------------------
library(dplyr) 

##Optional: split df into 3 df based on country
# indeedusa<-filter(IndeedSample,country='US')
# indeedca<-filter(IndeedSample,country='CA')
# indeedde<-filter(IndeedSample,country='DE')
# unique(indeedusa$stateProvince) 
##with Guam, Puerto Rico, Virgin Islands and Northern Mariana Islands
# unique(indeedca$stateProvince) 
# unique(indeedde$stateProvince)

##Convert character to numeric vector
IndeedSample$avgOverallRating<-as.numeric(IndeedSample$avgOverallRating)
IndeedSample$numReviews<-as.numeric(IndeedSample$numReviews)

#Making factors
IndeedSample$companyId<-factor(IndeedSample$companyId)
IndeedSample$jobId<-factor(IndeedSample$jobId)
IndeedSample$country<-factor(IndeedSample$country)
IndeedSample$stateProvince<-factor(IndeedSample$stateProvince)
IndeedSample$city<-factor(IndeedSample$city)

#date as Date object
library(lubridate)
IndeedSample$date<-as_date(IndeedSample$date)



# Cleaning Claudia Perez ####################
#columns I am cleaning =  industry - salary Currency
# mid3 <- IndeedSample[,9:15]
# head(mid3)

## 1) Changing industry(9) blanks to NAs
head(IndeedSample$industry)
index_indus <- which(IndeedSample$industry == "")
IndeedSample[index_indus,9] <- NA
head(IndeedSample$industry)

## 2) Changing normTitle(10) blanks to NAs
#IndeedSample[,10]
head(IndeedSample$normTitle)
index_normTitle <- which(IndeedSample$normTitle == "")
IndeedSample[index_normTitle,10] <- NA
head(IndeedSample$normTitle)

## 3) Changing normTitleCategory(11) blanks to NAs
#IndeedSample[,11]
head(IndeedSample$normTitleCategory)
index_normTitlecat <- which(IndeedSample$normTitleCategory == "")
IndeedSample[index_normTitlecat,11] <- NA
head(IndeedSample$normTitleCategory)

# 4) Changing descriptionCharacterLength to numeric
#head(IndeedSample$descriptionCharacterLength)
IndeedSample$descriptionCharacterLength <- as.numeric(IndeedSample$descriptionCharacterLength)

#head(IndeedSample$descriptionCharacterLength)
#IndeedSample$descriptionCharacterLength %>% class

#summary(IndeedSample)

## 5) Changing descriptionWordCount to numeric 
#head(IndeedSample$descriptionWordCount)
IndeedSample$descriptionWordCount <- as.numeric(IndeedSample$descriptionWordCount)

#head(IndeedSample$descriptionWordCount)
#IndeedSample$descriptionWordCount %>% class

#summary(IndeedSample)


## 6) Changing experienceRequired to numeric 
#head(IndeedSample$experienceRequired)
IndeedSample$experienceRequired <- as.numeric(IndeedSample$experienceRequired)

#head(IndeedSample$experienceRequired)
#IndeedSample$experienceRequired %>% class

#summary(IndeedSample)


## 7) Changing estimatedSalary to numeric 
#head(IndeedSample$estimatedSalary)
IndeedSample$estimatedSalary <- as.numeric(IndeedSample$estimatedSalary)

#head(IndeedSample$estimatedSalary)
#IndeedSample$estimatedSalary %>% class

#summary(IndeedSample)


# clean columns 16 - 23: ---------------------------------
# salaryCurrency
# jobLanguage
# supervisingJob
# licenseRequiredJob
# educationRequirement
# jobAgeDays
# clicks
# localClicks

# fix variable types
toFactor <- apply(X = IndeedSample[,16:20], MARGIN = 2, FUN = as.factor)
toNumeric <- apply(X = IndeedSample[,21:23], MARGIN = 2, FUN = as.numeric)
IndeedSample <- as.data.frame(cbind(IndeedSample[,1:15], toFactor, toNumeric))

# change yes/no factor levels to 1/0
levels(IndeedSample$supervisingJob) <- c("", "0", "1") 
levels(IndeedSample$licenseRequiredJob) <- c("", "0", "1") 

# fill blanks of salaryCurrency according to the country feature
# table(IndeedSample$country, IndeedSample$salaryCurrency)  # shows that CA - CAD, DE - EUR, US - USD
CADRow <- which(IndeedSample$salaryCurrency == "" & IndeedSample$country == "CA") 
IndeedSample[CADRow,16] <- "CAD"
EURRow <- which(IndeedSample$salaryCurrency == "" & IndeedSample$country == "DE") 
IndeedSample[EURRow,16] <- "EUR"
USDRow <- which(IndeedSample$salaryCurrency == "" & IndeedSample$country == "US") 
IndeedSample[USDRow,16] <- "USD"

# fill blanks in supervisingJob, licenseRequiredJob, and educationRequirement with NA
index_sj <- which(IndeedSample$supervisingJob == "")
IndeedSample[index_sj,18] <- NA
index_lrj <- which(IndeedSample$licenseRequiredJob == "")
IndeedSample[index_lrj,19] <- NA
index_er <- which(IndeedSample$educationRequirements == "")
IndeedSample[index_er,20] <- NA

# fix levels of factors
# IndeedSample[,16:20] <- apply(X = IndeedSample[,16:20], MARGIN = 2, FUN = as.factor)
IndeedSample$salaryCurrency <- factor(IndeedSample$salaryCurrency)
IndeedSample$supervisingJob <- factor(IndeedSample$supervisingJob)
IndeedSample$educationRequirements <- factor(IndeedSample$educationRequirements)
IndeedSample$licenseRequiredJob <- factor(IndeedSample$licenseRequiredJob)


# data Exploration -----------------------------------------------------
setwd(dir = "E:/Programs and Activities/Datafest 2018/Data")
load(file = "IndeedSampleclean")
indeedClean <- IndeedSample
rm(IndeedSample)
library(dplyr)

library(skimr)
skim(indeedClean)

## explore whether there is relationship between educationRequired, license, supervising
ggplot(data = indeedClean, mapping = aes(x = educationRequirements, y = licenseRequiredJob )) + geom_jitter()

# make a word cloud for trending job titles (for employer) ----------------
library(SnowballC)
library(wordcloud)
library(tm)
library(dplyr)
library(RColorBrewer)

numWord <- 60

tmp <- VectorSource(indeedClean$normTitle[indeedClean$jobAgeDays == 0]) %>% 
  Corpus() %>% 
  # tm_map(PlainTextDocument) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, c('the', 'this', stopwords('english'))) %>% 
  # tm_map(stemDocument) %>% 
  TermDocumentMatrix() %>% 
  as.matrix()

tmp2 <- sort(rowSums(tmp),decreasing=TRUE)

titleKeywordFreq <- data.frame(word = names(tmp2),freq=tmp2)

wordcloud(words = titleKeywordFreq$word,
          freq = titleKeywordFreq$freq,
          min.freq = 800,
          max.words=numWord, 
          random.order=FALSE,
          scale = c(3,0.4),
          rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))

# output title keywords freq table
write.table(x = titleKeywordFreq[1:numWord,], file = "freqTable_employer.txt",sep = ":",row.names = F, col.names = F, quote = F)

# then use this url:https://worditout.com/word-cloud/create

# make a word cloud for trending job titles (for job seeker) ----------------

# jobPostDay <- indeedClean %>% group_by(jobId) %>% summarize(job_len = max(jobAgeDays))
# 
# jobPostDay$job_len <- as.integer(jobPostDay$job_len)

indeedSorted <- dplyr::arrange(.data = indeedClean, jobId, jobAgeDays)
jobLastPostInd <- indeedSorted$jobId %>% 
  table() %>% 
  cumsum()
uniqueJob <- indeedSorted[jobLastPostInd,]

# aggregate normTitle by averaging clicks
titleKeywordFreq_applicant <- uniqueJob %>% 
  group_by(normTitle) %>% 
  summarise(avgClick = mean(clicks))

# process the keywords
tmp <- titleKeywordFreq_applicant %>% 
  VectorSource() %>% 
  Corpus() %>% 
  # tm_map(PlainTextDocument) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, c('the', 'this', stopwords('english'))) %>% 
  # tm_map(stemDocument) %>% 
  TermDocumentMatrix() %>% 
  as.matrix()

tmp2 <- sort(rowSums(tmp),decreasing=TRUE)

titleKeywordFreq_applicant <- data.frame(word = names(tmp2),freq=tmp2)

# output title keywords freq table
write.table(x = titleKeywordFreq_applicant[1:numWord,], file = "freqTable_applicant.txt",sep = ":",row.names = F, col.names = F, quote = F)

# # find the last day record of a given job id
# # @param fullData has replicates in terms of job ids
# # @param maxDay  2 cols: 1st unique job ids; 2nd number of days posted
# findLastDayPost <- function(fullData, maxDay){
#   
#   return(filter(.data = fullData, jobId == id, jobAgeDays == maxDay))
# }

# Predict number of clicks ----------------
# for US, and jobAgeDays <= 7
us1Week <- indeedClean %>% filter(country == "US", jobAgeDays <= 7)
us1Week$industry <- as.factor(us1Week$industry)
us1Week$normTitle <- as.factor(us1Week$normTitle)
us1Week$normTitleCategory <- as.factor(us1Week$normTitleCategory)

us1Week$stateProvince <- factor(us1Week$stateProvince)
# us1Week <- filter(.data = us1Week, jobLanguage == "EN")  # get rid of 2 jobs required Franch

# baseline model
m0 <- lm(formula = clicks ~ stateProvince+avgOverallRating+numReviews+normTitleCategory+descriptionCharacterLength+descriptionWordCount+experienceRequired+estimatedSalary+supervisingJob+licenseRequiredJob+educationRequirements+jobAgeDays, data = us1Week)  # R_adj = 13.15%
# the residues are not normal 

# # cross validation
# set.seed(810253237)
# predfun.lm = function(train.x, train.y, test.x, test.y)
# {
#   lm.fit = lm(train.y ~ . , data=train.x, na.action = na.exclude)
#   ynew = predict(lm.fit, test.x )
#   
#   # compute squared error risk (MSE)
#   out = mean( (ynew - test.y)^2 )
#   
#   return( out )
# }
# crossval::crossval(predfun.lm, us1Week[,c(7,8,12:15,18:21)], us1Week$clicks, K=5, B=10)
# boot::cv.glm(data = us1Week, glmfit = m0, K = 5)

# model1: glm
m1 <- glm(formula = clicks ~ stateProvince+avgOverallRating+numReviews+normTitleCategory+descriptionCharacterLength+descriptionWordCount+experienceRequired+estimatedSalary+supervisingJob+licenseRequiredJob+educationRequirements+jobAgeDays, data = us1Week)  # ~ 3%

# model2: in model0, too many levels for stateProvince, group them together
levels(us1Week$stateProvince)[47] <- NA
lvls <- levels(us1Week$stateProvince)
lvls[c(2,3,9:11,19,20,27,29,43,45,48,52)] <- "SE"
lvls[c(4,31,34,38,46)] <- "SW"
lvls[c(5,6,15,28,35,39,47,50,53)] <- "W"
lvls[c(7,8,21:23,32,33,36,40,42,49)] <- "NW"
lvls[c(14,16:18,24:26,30,37,44,51)] <- "MidW"
levels(us1Week$stateProvince) <- lvls

m2 <- lm(formula = clicks ~ stateProvince+avgOverallRating+numReviews+normTitleCategory+descriptionCharacterLength+descriptionWordCount+experienceRequired+estimatedSalary+supervisingJob+licenseRequiredJob+educationRequirements+jobAgeDays, data = us1Week)  # 13.3%. 
# not normal

# scale all relevant numeric variables
us1Week[,c("avgOverallRating","numReviews","descriptionCharacterLength","descriptionWordCount","experienceRequired","estimatedSalary","jobAgeDays","clicks")] <- scale(x = us1Week[,c("avgOverallRating","numReviews","descriptionCharacterLength","descriptionWordCount","experienceRequired","estimatedSalary","jobAgeDays","clicks")])

library(car)
m2 <- update(object = m2, data = us1Week)  # 13.31%

# model3: in model2, too many insignificant predictors
step(object = m2, scope = list(upper = clicks ~ stateProvince + avgOverallRating + numReviews + 
                                 normTitleCategory + descriptionCharacterLength + descriptionWordCount + 
                                 experienceRequired + estimatedSalary + supervisingJob + licenseRequiredJob + 
                                 educationRequirements + jobAgeDays, lower = clicks ~ 1), direction = "backward", data = us1Week)
m3 <- lm(formula = clicks ~ stateProvince + avgOverallRating + normTitleCategory + descriptionCharacterLength + experienceRequired + estimatedSalary + supervisingJob + educationRequirements + jobAgeDays, data = us1Week)  # 13.4%
step(object = m3, scope = list(upper = clicks ~ stateProvince + avgOverallRating + normTitleCategory + descriptionCharacterLength + experienceRequired + estimatedSalary + supervisingJob + educationRequirements + jobAgeDays, lower = clicks ~ 1), direction = "backward", data = us1Week)


# Create job title (demand) word cloud for full data ----------------
# load full data freq files (txt)
library(readr)
demandFreqFull1 <- read_delim(file = "freqTable_applicant_full_data1.txt", delim = ":", col_names = F)
colnames(demandFreqFull1) <-c("canonTitle", "freq_part1")
demandFreqFull2 <- read_delim(file = "freqTable_applicant_full_data2.txt", delim = ":", col_names = F)
colnames(demandFreqFull2) <-c("canonTitle", "freq_part2")

demandFreqFull <- full_join(x = demandFreqFull1, y = demandFreqFull2)
demandFreqFull$freq <- (demandFreqFull$freq_part1 + demandFreqFull$freq_part2) / 2

# output freq table
write.table(x = demandFreqFull[1:numWord,c(1,4)], file = "freqTable_job_applicant_full.txt",sep = ":",row.names = F, col.names = F, quote = F)
