## the following committess met multiple times in the same year and are collapsed in the particip dataset
# uncollapse/replace them with the 'extra' dataset
# CCFICS11"  "CCFL14"    "CCGP19"    "CCGP21"    "CCM4" "CCEXEC" (multiple)

rm(list = ls())

if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

extra = read.csv(paste0(pathMain, '/participation_development/codex_committee_meetings_participants_long.csv'), stringsAsFactors = FALSE)
names(extra)[which(names(extra) %in% c('Year', 'Participants'))] = c('year', 'delegates')
extra[which(extra$Delegation == 'EU'), 'Delegation_Name'] = c('European Union (IGO)')
extra = extra[which(extra$Meeting %in% c('CCFICS10', 'CCFICS11',
                                         'CCFL13', 'CCFL14', 
                                         'CCGP18', 'CCGP19',
                                         'CCGP20', 'CCGP21',
                                         'CCM3', 'CCM4',
                                         paste0('CCEXEC', c(2:9, 11:14, 16, 17, 53:57, 59, 60, 71:76))
)),]

extra = extra[-which(duplicated(extra)),]

extra$deleg_type_short = paste0(extra$Delegation_Type, '_', extra$Delegation)


extra = extra[-c(which(extra$deleg_type_short == 'IGO_EU' & extra$Meeting == 'CCFL13' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_EU' & extra$Meeting == 'CCFL14' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_EU' & extra$Meeting == 'CCFICS10' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_EU' & extra$Meeting == 'CCFICS11' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_OIV' & extra$Meeting == 'CCGP18' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_EU' & extra$Meeting == 'CCGP18' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_EU' & extra$Meeting == 'CCGP19' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_OIV' & extra$Meeting == 'CCGP19' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_OIV' & extra$Meeting == 'CCGP20' & extra$delegates == 0),
                 which(extra$deleg_type_short == 'IGO_OIV' & extra$Meeting == 'CCGP21' & extra$delegates == 0)
),]



names(extra) = c('event_number', 'year', 'meeting_start', 'meeting_end', 'Meeting_Number', 'actor_short', 'delegates', 'chair', 'observer', 'actor_type', 'actor_name', 'headquarters', 'region_group', 'income_group', 'interest_group', 'Meeting_Location', 'HQ_Meeting_Distance', 'event_short', 'deleg_type_short')
extra$delegations = ifelse(extra$delegates !=0, 1, 0)
extraParticip = extra
save(extraParticip, file =  paste0(pathMain, '/participation_development/extraParticip.rda')) 


extraPrep= dplyr:::select(extra, year, event_name_short,event_number, delegates, deleg_type_short) %>% tidyr:::spread(deleg_type_short, delegates) %>% data.frame()
extraPrep$delegates = rowSums(extraPrep[, -c(1:3)], na.rm = TRUE)
names(extraPrep)[2:3] = c('event_short', 'event_number')
extraPrep$NGO_AFI = extraPrep$NGO_NKM = extraPrep$state_ATF = extraPrep$state_IOT = extraPrep$state_MNE = extraPrep$state_MYT = extraPrep$state_SRB = extraPrep$state_UMI = 0
extraPrep = extraPrep[,-which(names(extraPrep) == 'NGO_NKMLInternational')]
extraParticipAgg = extraPrep
 
save(extraParticipAgg, file =  paste0(pathMain, '/participation_development/extraParticipAgg.rda'))
