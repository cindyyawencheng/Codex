# eda

if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

library(ggplot2)

# -----------------------------------
# Load Data
# -----------------------------------
load(file = paste0(pathMain, '/participation_development/codexWide.rda'))

# remove proposals for which we have no data for
codexWide = codexWide[-which(codexWide$cac_start == 'CACNA'),]


 
# ----------------------------------- 
# check distribution of years/cacAfter Proposal by committee by proposal adoption
# ----------------------------------- 
table(codexWide[which(codexWide$adopted == 1), 'yearsAfterProposal']) 
table(codexWide[which(codexWide$adopted == 0), 'yearsAfterProposal']) 

table(codexWide[which(codexWide$Reference != 0), 'cacAfterProposal']) 
table(codexWide[which(codexWide$Reference == 0), 'cacAfterProposal']) 
 
 
ggplot(codexWide,aes(x=yearsAfterProposal)) + 
  geom_density(data=subset(codexWide,adopted == 1),fill = "red", color = 'black', alpha = 0.6) +
  geom_density(data=subset(codexWide,adopted == 0),fill = "blue", color = 'black', alpha = 0.6)  

ggplot(codexWide,aes(x=cacAfterProposal)) + 
  geom_density(data=subset(codexWide,adopted == 1),fill = "red", color = 'black', alpha = 0.6) +
  geom_density(data=subset(codexWide,adopted == 0),fill = "blue", color = 'black', alpha = 0.6)  

t.test(codexWide[which(codexWide$adopted == 1), 'yearsAfterProposal'], codexWide[which(codexWide$adopted == 0), 'yearsAfterProposal'])
t.test(codexWide[which(codexWide$adopted == 1), 'cacAfterProposal'], codexWide[which(codexWide$adopted == 0), 'cacAfterProposal'])

# ----------------------------------- 
# check distribution of years/cacAfter Proposal by committee
# -----------------------------------
codexWide2 = codexWide[-which(is.na(codexWide$committee )),]
 
ggplot(codexWide2)+
  geom_boxplot(aes(x = reorder(committee, yearsAfterProposal, FUN = median), y = yearsAfterProposal))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(codexWide2)+
  geom_boxplot(aes(x = reorder(committee, cacAfterProposal, FUN = median), y = yearsAfterProposal))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# by meeting type

ggplot(codexWide2)+
  geom_boxplot(aes(x = reorder(committee, yearsAfterProposal, FUN = median), y = yearsAfterProposal, colour = meeting_type))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(codexWide2)+
  geom_boxplot(aes(x = reorder(committee, cacAfterProposal, FUN = median), y = yearsAfterProposal, colour = meeting_type))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# by meeting status
ggplot(codexWide2)+
  geom_boxplot(aes(x = reorder(committee, yearsAfterProposal, FUN = median), y = yearsAfterProposal, colour = meeting_status))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(codexWide2)+
  geom_boxplot(aes(x = reorder(committee, cacAfterProposal, FUN = median), y = yearsAfterProposal, colour = meeting_status))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# ----------------------------------- 
# check distribution of years/cacAfter Proposal by committee by whether data handcoded or not
# ----------------------------------- 
codexWide$step %>% table() %>% sum()
dim(codexWide)
520-length(which(codexWide$Reference == 0))

codexCheck = rbind( data.frame(codexWide[which(!is.na(codexWide$step)), ], stepDummy = 1),
                    data.frame(codexWide[which(is.na(codexWide$step)), ], stepDummy = 0))
b = 1
ggplot(codexCheck,aes(x=yearsAfterProposal)) + 
  geom_histogram(data=subset(codexCheck,stepDummy == 1),fill = "red", color = 'black', alpha = 0.6, binwidth = b) +
  geom_histogram(data=subset(codexCheck,stepDummy == 0),fill = "blue", color = 'black', alpha = 0.6, binwidth = b)  

ggplot(codexCheck,aes(x=cacAfterProposal)) + 
  geom_histogram(data=subset(codexCheck,stepDummy == 1),fill = "red", color = 'black', alpha = 0.6, binwidth = b) +
  geom_histogram(data=subset(codexCheck,stepDummy == 0),fill = "blue", color = 'black', alpha = 0.6, binwidth = b)  

ggplot(codexCheck,aes(x=yearsAfterProposal)) + 
  geom_density(data=subset(codexCheck,stepDummy == 1),fill = "red", color = 'black', alpha = 0.6) +
  geom_density(data=subset(codexCheck,stepDummy == 0),fill = "blue", color = 'black', alpha = 0.6)  

ggplot(codexCheck,aes(x=cacAfterProposal)) + 
  geom_density(data=subset(codexCheck,stepDummy == 1),fill = "red", color = 'black', alpha = 0.6) +
  geom_density(data=subset(codexCheck,stepDummy == 0),fill = "blue", color = 'black', alpha = 0.6)  

t.test(codexWide[which(!is.na(codexWide$step)), 'yearsAfterProposal'], codexWide[which(is.na(codexWide$step)), 'yearsAfterProposal'])
t.test(codexWide[which(!is.na(codexWide$step)), 'cacAfterProposal'], codexWide[which(is.na(codexWide$step)), 'cacAfterProposal'])


# ----------------------------------- 
# check distribution of number of proposals by committee
# -----------------------------------

codexWide2$numProposal = 1
names(codexWide2)
codexWide2$stepDummy = ifelse(codexWide2$step %>% is.na(), 0, 1)

codexWideAggCommittee = codexWide2 %>% group_by(year_start, committee) %>% summarise(numProposal = sum(numProposal), numStep = sum(stepDummy)) %>% data.frame()

ggplot(codexWideAggCommittee )+
  geom_boxplot(aes(x = reorder(committee, numProposal, FUN = median), y = numProposal))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

head(codexWideAggCommittee)

codexWideAggCAC = codexWide2 %>% group_by(endCac) %>% summarise(numProposal = sum(numProposal), 
                                                                droppedNum = sum(droppedDummy),
                                                                supersededNum = sum(supersededDummy)) %>% data.frame()


ggplot(codexWideAggCAC)+
  geom_line(aes(x = endCac, y = numProposal -droppedNum - supersededNum))+ 
  geom_line(aes(x = endCac, y = droppedNum, color = 'red'))+ 
  geom_line(aes(x = endCac, y = supersededNum, color = 'blue'))


# ----------------------------------- 
# check distribution of number of proposals by committee
# -----------------------------------
codexExpand = codexExpand[-which(codexExpand$committee_num %>% is.na()),]  
codexExpand$yearsAfterProposal = lapply(split(codexExpand$committee_num, codexExpand$Title), function(x){
  1:length(x)
}) %>% unlist()



# ----------------------------------- 
# check which countries tend to have highest deleg concentration
# ----------------------------------- 

load(file = paste0(pathMain, '/participation_development/codexWideParticip.rda'))


findMaxDeleg = codexWideAll[-496,which(names(codexWideAll) == 'delegates_Afghanistan'):which(names(codexWideAll) == 'delegates_Zimbabwe')]

maxDeleg = data.frame(delegConc = codexWideAll$delegConc[-496], t(apply(findMaxDeleg, 1, function(x){
  as.matrix(c(gsub('delegates_','', names(which.max(x))), max(x, na.rm = TRUE)), nrow = 1)
})))
names(maxDeleg)[2:3] = c('country', 'maxDeleg')
maxDeleg$delegConc = as.numeric(maxDeleg$delegConc)
maxDeleg$maxDeleg = as.numeric(maxDeleg$maxDeleg)
maxDeleg$committee = codexWideAll$committee[-96]


#maxDeleg = maxDeleg[-which(maxDeleg$committee %in% c('CCAFRICA', 'CCASIA', 'CCEURO', 'CCNEA', 'CCLAC')),]
 
# order barplot
maxDeleg <- within(maxDeleg, 
                   country <- factor(country, 
                                      levels=names(sort(table(country), 
                                                        decreasing=TRUE))))


ggplot(maxDeleg)+
  geom_bar(aes(country))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(maxDeleg) +
  geom_boxplot(aes(x = reorder(country, maxDeleg, FUN = median), y = maxDeleg))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(maxDeleg) +
  geom_boxplot(aes(x = reorder(country, delegConc, FUN = median), y = delegConc))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  


names(codexWideAll)
 

library(survival)
install.packages('survminer')
library(survminer)
library(dplyr)
