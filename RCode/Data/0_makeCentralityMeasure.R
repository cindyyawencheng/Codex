# create measure of eigenvector centrality
rm(list = ls())

if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

# load packages and data
library(gtools)
library(plyr)
library(igraph)
# returns a matrix that notes the average relative proportions of delegates between two committees
getAvgProportion = function(listOfMat){
  
  slice.mat = lapply(1:length(listOfMat), function(y){
    # select one column of delegates
    particip = listOfMat[[y]]
    
    # calculate the proportion of delegates any two committees share
    propMatrix = do.call(rbind,lapply(particip, function(x){
      prop = x/particip
      return(prop)
    }))
    propMatrix[is.nan(propMatrix)] = NA
    propMatrix[is.infinite(propMatrix)] = 0
    return(propMatrix)})
  
  
  # calculate the average proportion of delegates any two committees may share and use this as your measure of the strength of connection
  average_mat = aaply(laply(slice.mat, as.matrix), c(2, 3), mean, na.rm = TRUE)
  
  return(average_mat)
}


# returns network centrality from list of average proportions
getNetworkCentrality = function(average_mat_list){
  
  eigen = data.frame(committeeEigenCent = lapply(average_mat_list, function(x){
    
    x = x[rowSums(is.na(x)) != ncol(x),colSums(is.na(x)) != nrow(x)]
    diag(x) = 0
    graph = graph_from_adjacency_matrix(x, weighted=TRUE, mode = 'directed')
    eigen_centrality(graph, directed = TRUE)$vector
  }) %>% unlist())
  
  eigen = data.frame(cbind(do.call(rbind, str_split(rownames(eigen), '\\.') ), eigen))
  names(eigen) = c('event_number_cac', 'event_number', 'committeeEigenCent')
  rownames(eigen) = NULL
  return(eigen)
  
}

 


load(file = paste0(pathMain, '/participation_development/codex_event_dates_cac.rda'))
load(file = paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))
dates_cac = dates_cac[, c('event_short_cac', 'event_number_cac', 'year')]
names(dates_cac) = gsub('_cac', '', names(dates_cac))
dates_match = rbind(dates_all[, c('event_short', 'event_number', 'year')],
                    dates_cac)

 
# need to revise participant/delegate data so that you disaggregate by non-country groups as well
# for now, test the code with just country delegations
# -------------------------
# Clean Participant Data
# --------------------------
list.files(paste0(pathMain, '/participation_development'))  
 
 
particip = read.dta13(paste0(pathMain, '/participation_development/codex_participation_updated_chairs_observers.dta'))
names(particip)[which(names(particip) == 'weight')] = 'delegates'
# ---- Clean data
# make distinct actor name for when EU acts as a state
particip[which(particip$Delegation == 'EU'), 'Delegation_Name'] = c('European Union (IGO)')
 
 
# Code in missing information
# we don't know how many delegates the US sent
particip[which(particip$Committee == 'CCFH' & particip$year == 1964 & 
                 particip$Delegation_Name %in% c("United States of America" )), 'delegates'] = 0 
particip[which(particip$actor_short == 'RIOPPAH'), 'actor_type'] = 'IGO'

 
particip$deleg_type_short = paste0(particip$actor_type, '_', particip$actor_short)
particip$event_number = dates_match$event_number[match(paste0(particip$event_short, particip$year), paste0(dates_match$event_short, dates_match$year))] 

 
participPrep= dplyr:::select(particip, year, event_short, event_number,  delegates, deleg_type_short) %>% tidyr:::spread(deleg_type_short, delegates) %>% data.frame()
participPrep$delegates = rowSums(participPrep[, -c(1:3)], na.rm = TRUE)
 
 
# remove committees for which we have no participant info
participPrep = participPrep[-which(participPrep$delegates %in%c(0, 1)),] 


## the following committess met multiple times in the same year and are collapsed in the particip dataset
# uncollapse/replace them with the 'extra' dataset
# CCFICS11"  "CCFL14"    "CCGP19"    "CCGP21"    "CCM4" "CCEXEC" (multiple)

participPrep = participPrep[-which(participPrep$event_number %in% c('CCFICS10', 'CCFICS11',
                                                                    'CCFL13', 'CCFL14', 
                                                                    'CCGP18', 'CCGP19',
                                                                    'CCGP20', 'CCGP21',
                                                                    'CCM3', 'CCM4',
                                                                    paste0('CCEXEC', c(2:9, 11:14, 16, 17, 53:57, 59, 60, 71:76))
)), ]  

load( paste0(pathMain, '/participation_development/extraParticipAgg.rda'))


participPrep = rbind(participPrep, extraParticipAgg[, names(participPrep)])
 

# prep data so that you weight by the total number of delegates for each delegation
participPrep = participPrep[which(participPrep$year>1962), ]
participPrep = data.frame(event_number = participPrep[,3], year = participPrep[,1], participPrep[, 4:610]/participPrep$delegates)
participPrep = participPrep[order(participPrep$year),]

 
# note that some committees don't meet in the same year; figure out backward/forward linkages when necessary
# make these backward and forward linkages based on cac meetings
participPrep$cac = dates_all$event_number_cac[match(participPrep$event_number, dates_all$event_number)]
participPrep$cac[intersect(which(is.na(participPrep$cac)), grep('CAC', participPrep$event_number))] = participPrep$event_number[intersect(which(is.na(participPrep$cac) ), grep('CAC', participPrep$event_number))] %>% as.character()
#participPrep$cac[which(is.na(participPrep$cac))] = dates_exec$event_number_cac[match(participPrep$event_number[which(is.na(participPrep$cac))], dates_exec$event_number)]
participPrep = participPrep[-which(is.na(participPrep$cac)),]

# reorder columns and rows
participPrep = participPrep[, c('event_number', 'cac', names(participPrep)[3:609])]
participPrep = participPrep[mixedorder(participPrep$cac),]
 
 
#    rowSums(slice[, -c(1:2)], na.rm = TRUE) check this after incorporating non-country data
table(participPrep$cac)
## calculate similarity of delegate composition between two committees

# -------------------------
# calculate centrality measure
# --------------------------

average_mat_state_list = list()
average_mat_all_list = list()

average_mat_state_list = list()
average_mat_all_list = list()


for(i in 1:length(unique(participPrep$cac))){
  print(unique(participPrep$cac)[i] )
  
  # for each year
  slice = participPrep[which(participPrep$cac ==unique(participPrep$cac)[i] ),]
  
  # get all state participants
  slice_list_state = as.list(slice[, names(slice)[grep('^state_', names(slice))]])
  
  # get all participants
  slice_list_all = as.list(slice[, -c(1, 2)])
  
  average_mat_state = getAvgProportion(slice_list_state)
  average_mat_all = getAvgProportion(slice_list_all)
  colnames(average_mat_state) = rownames(average_mat_state) =  slice[,1]
  colnames(average_mat_all) = rownames(average_mat_all) =  slice[,1]
 
  # save results
   average_mat_state_list[[as.character(unique(participPrep$cac)[i])]] = average_mat_state
   average_mat_all_list[[as.character(unique(participPrep$cac)[i])]] = average_mat_all
}

  
stateCent = getNetworkCentrality(average_mat_state_list) 
names(stateCent)[3] = c('committeeEigenCentState')
allCent = getNetworkCentrality(average_mat_all_list)
names(allCent)[3] = c('committeeEigenCentAll')


centralityMeeting = merge(stateCent, allCent, by = c('event_number_cac', 'event_number'))

save(centralityMeeting, file = paste0(pathMain, '/participation_development/centralityMeetingMeasure.rda'))
