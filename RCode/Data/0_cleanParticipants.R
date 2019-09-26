# clean Participant data

if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}
 
load(file = paste0(pathMain, '/participation_development/codex_event_dates_cac.rda'))
load(file = paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))
dates_cac = dates_cac[, c('event_short_cac', 'event_number_cac', 'year')]
names(dates_cac) = gsub('_cac', '', names(dates_cac))
dates_match = rbind(dates_all[, c('event_short', 'event_number', 'year')],
                    dates_cac)

# -------------------------
# Clean Participant Data
# --------------------------
# load and clean particip data
# particip = read.dta13(paste0(pathMain, '/participation_development/codex_participation_updated_chairs_observers.dta'))
# names(particip)[which(names(particip) == 'weight')] = 'delegates'
# 
# # ---- Clean data
# # make distinct actor name for when EU acts as a state
# particip[which(particip$actor_short == 'EU'), 'actor_name'] = c('European Union (IGO)')
# 
# particip$delegations = ifelse(particip$delegates !=0, 1, 0)
# 
# 
# # Code in missing information
# particip[which(particip$actor_short == 'RIOPPAH'), 'actor_type'] = 'IGO'
# 
# # add participants for ad hoc working group of GEQFF, which didn't have a 14th session but
# # an ad hoc working group was established to finish its work after it adjourned following its 13th session
# particip[which(particip$event_short == 'GEQFF' & particip$year == 1983 & particip$actor_name %in% c('Austria', 'Cuba', 'Germany', 'Japan', 'Mexico', 'Netherlands', 'Switzerland', 'United Kingdom', 'United States of America')), 'delegations'] = 1
# 
# 
# # we don't know how many delegates the US sent
# particip[which(particip$event_short == 'CCFH' & particip$year == 1964 & 
#                  particip$actor_name %in% c('United States')), 'delegates'] = 0 
# 
# particip[which(particip$event_short == 'CCFH' & particip$year == 1964 & 
#                  particip$actor_name %in% c('United Kingdom', 'Canada', 'Netherlands')), 'delegations'] = 1 
# 
# 
# 
# # add event number data
# particip$event_number = dates_match$event_number[match(paste0(particip$event_short, particip$year), paste0(dates_match$event_short, dates_match$year))] 
# particip = particip[-which(is.na(particip$event_number)),]
#  
#  
#  
# # add in extra data that was previously mistakenly collapsed into the same year
# load(file =  paste0(pathMain, '/participation_development/extraParticip.rda')) 
# particip = particip[-which(particip$event_number %in%  c('CCFICS10', 'CCFICS11',
#                                                         'CCFL13', 'CCFL14', 
#                                                         'CCGP18', 'CCGP19',
#                                                         'CCGP20', 'CCGP21',
#                                                         'CCM3', 'CCM4',
#                                                         paste0('CCEXEC', c(2:9, 11:14, 16, 17, 53:57, 59, 60, 71:76)))),]
# 
# 
# particip = particip[, -c(which(names(particip) %in% setdiff(names(particip), names(extraParticip))))] 
# 
# particip = rbind(particip, extraParticip[, names(particip)])
# save(particip, file= paste0(pathMain, '/participation_development/codex_participation_clean.rda'))


load(paste0(pathMain, '/participation_development/codex_participation_clean.rda'))

# add in data for wto 
load(file = paste0(pathMain, '/participation_development/mem-obs-list_wtoclean.rda'))
particip$wto_delegations = ifelse(particip$delegations == 1, wtoLong$wtoDum[match(paste0(particip$actor_name, particip$year), paste0(wtoLong$Members, wtoLong$year))], 0)
particip$wto_delegates =  particip$wto_delegations * particip$delegates

 


### create measure of number of delegates per region, incom and interest grouping and of chairs/observers from various regions or incomes
participDummy = dummy_cols(particip, select_columns = c('region_group', 'income_group', 'interest_group', "actor_type"))
colStart = which(names(participDummy) == 'region_group_Europe & Central Asia')
colEnd = which(names(participDummy) == 'actor_type_IGO')
 
# by delegates
disaggDelegates =   particip$delegates * as.matrix(participDummy[, colStart:colEnd])  %>% data.frame()
names(disaggDelegates) =paste0('delegates_', names(participDummy)[colStart:colEnd])

wtoDisaggDelegates =   particip$wto_delegates * as.matrix(participDummy[, colStart:which(names(participDummy)==  "income_group_Lower middle income" )])  %>% data.frame()
names(wtoDisaggDelegates) =paste0('delegates_', names(participDummy)[colStart:which(names(participDummy)==  "income_group_Lower middle income" )], '_wto')


# by chairs
colEndIncome = which(names(participDummy) == 'income_group_Lower middle income')
particip$chairDum = ifelse(particip$chair == 'chair', 1, 0)
disaggChair =   particip$chairDum * as.matrix(participDummy[, colStart:colEndIncome])   %>% data.frame()
names(disaggChair ) =paste0('chair_', names(participDummy)[colStart:colEndIncome])

# by observers
particip$observeDum = ifelse(particip$observer == 'observer', 1, 0)
disaggObserv =   particip$observeDum * as.matrix(participDummy[, colStart:colEndIncome])   %>% data.frame()
names(disaggObserv ) =paste0('obs_', names(participDummy)[colStart:colEndIncome])


# by delegations
disaggDelegations =   particip$delegations * as.matrix(participDummy[, colStart:colEnd])  %>% data.frame()
names(disaggDelegations) =paste0('delegations_', names(participDummy)[colStart:colEnd])


disaggDelegations_wto =   particip$wto_delegations * as.matrix(participDummy[, colStart:which(names(participDummy)==  "income_group_Lower middle income" )])  %>% data.frame()
names(disaggDelegations_wto ) =paste0('delegations_', names(participDummy)[colStart:which(names(participDummy)==  "income_group_Lower middle income" )], '_wto')



## add rows
disagg = cbind(disaggDelegates, disaggChair, disaggObserv, wtoDisaggDelegates, disaggDelegations, disaggDelegations_wto)  
disagg = disagg[, -grep('n/a|interest_group_$|region_group_$|income_group_$|actor_type_$|__wto', names(disagg))]

 
particip = cbind(particip, disagg) 


# create variable that meausures the square of number of delegates weighted by total number of delegates for that committee (delegateWt2) in order to later create herfindahl index of delegations (delegConc)
delegTotal = particip %>% dplyr:::group_by(event_number) %>% dplyr:::summarise(delegates = sum(delegates, na.rm = TRUE)) %>% data.frame()
particip$delegatesTotal = delegTotal$delegates[match(paste0(particip$event_number), paste0(delegTotal$event_number))]
particip$delegateWt2 = c(particip$delegates/particip$delegatesTotal)^2


# aggregate data by delegates
participAgg = particip %>% 
  group_by(year, event_number) %>% 
  summarise_at( vars(matches('deleg|chair_|obs_|wto')), sum, na.rm = TRUE) %>% data.frame()

 
names(participAgg)[which(names(participAgg) == 'delegateWt2')] = 'delegConc'				 
participAgg = participAgg[, - which(names(participAgg) == 'delegatesTotal')]
names(participAgg) = gsub('\\.', '', names(participAgg))


participAgg$wto_delegates_perc = participAgg$wto_delegates/participAgg$delegates_actor_type_state
participAgg$wto_delegations_perc = participAgg$wto_delegations/participAgg$delegations_actor_type_state



# add info for delegations by particular country actors
participCountry = particip[which(particip$actor_type == 'state'),]
participCountry$delegDummy_country = participCountry$delegations

participWideCountry= participCountry %>% 
  dplyr:::select( year, actor_name, event_number, 
                  delegDummy_country) %>% 
  spread(actor_name, delegDummy_country) 

names(participWideCountry)= gsub('\\.|\\/', '', names(participWideCountry))
names(participWideCountry)[-c(1:2)] = paste0('delegations_', names(participWideCountry)[-c(1:2)])
 
# add info for delegates by particular country actors 
participWideCountryDelegates= participCountry %>% 
  dplyr:::select( year, actor_name, event_number, 
                  delegates) %>% 
  spread(actor_name, delegates) 
names(participWideCountryDelegates)= gsub('\\.|\\/', '', names(participWideCountryDelegates))
names(participWideCountryDelegates)[-c(1:2)] = paste0('delegates_', names(participWideCountryDelegates)[-c(1:2)])
 
participAgg = merge(participAgg, participWideCountry , by = c('year', 'event_number'))
participAgg = merge(participAgg, participWideCountryDelegates , by = c('year', 'event_number'))
 
 
# fix typos
# we know that there were 9 delegations sent to this meeting, just not which ones exactly
participAgg[which(participAgg$event_short == 'CCFH' & participAgg$year == 1964), 'delegations'] = 9


# # add in dummies for meetings for which there is no info
meta_particip = read.csv(paste0(pathMain, '/participation_development/additional_coding_23_November_6_December_2018.csv'))
names(meta_particip) = c('event_short', 'event_short_meeting', 'coded', 'onlyChairDum', 'chairButNoDelegatesDum', 'Notes')

meta_particip$meetingCount = gsub('[A-Z]', '', meta_particip$event_short_meeting) %>% as.numeric()
meta_particip$infoDum = ifelse(meta_particip$coded == 0, 1, 0) 

 

participAgg$onlyChairDum = meta_particip$onlyChairDum[match(paste0(participAgg$event_number), meta_particip$event_short_meeting)]
participAgg$chairButNoDelegatesDum = meta_particip$chairButNoDelegatesDum[match(paste0(participAgg$event_number), meta_particip$event_short_meeting)]
participAgg$infoDum = meta_particip$infoDum[match(paste0(participAgg$event_number), meta_particip$event_short_meeting)]

 
participAgg$onlyChairDum[which(is.na(participAgg$onlyChairDum))] = 0
participAgg$chairButNoDelegatesDum[which(is.na(participAgg$chairButNoDelegatesDum))] = 0
participAgg$infoDum[which(is.na(participAgg$infoDum))] = 0
participAgg$missingDelegInfo = ifelse(participAgg$onlyChairDum ==1|participAgg$chairButNoDelegatesDum ==1 |participAgg$infoDum ==1 , 1, 0)

participAgg[which(participAgg$missingDelegInfo==1), which(names(participAgg) == 'delegates'):which(names(participAgg) == "delegates_Zimbabwe" )] = NA

 
save(participAgg, file = paste0(pathMain, '/participation_development/codex_participation_master.rda'))

