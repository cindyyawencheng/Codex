# codex meetings masterlist

## match committee to committee number
# first make masterlist for dates
dates = read.dta13(paste0(pathMain, '/participation_development/codex_event_dates.dta'))
dates$event_start  = as.Date(str_trim(dates$event_start), '%m/%d/%Y')
dates$event_end  = as.Date(str_trim(dates$event_end), '%m/%d/%Y')

# note codex made a mistake on their website for this date
dates[which(dates$event_number == 'CCLAC7'), c('event_start', 'event_end')] = c('1991-02-25', '1991-03-01')
 

# select only CAC meetings
dates_cac = dates[which(dates$event_short == 'CAC'),]
names(dates_cac)[1:4] = paste0(names(dates_cac)[1:4], '_cac')
dates_cac = dates_cac[-which(dates_cac$event_number_cac == 'CAC25'),] # no proposals passed on CAC25
dates_cac = dates_cac[order(dates_cac$event_start_cac),]

save(dates_cac, file = paste0(pathMain, '/participation_development/codex_event_dates_cac.rda'))


dates_all = dates[-which(dates$event_short == 'CAC'),]
#dates_all = dates_all[-grep('CCEXEC', dates_all$event_short),] 
dates_all = dates_all[order(dates_all$event_short, dates_all$event_start),]

dates_all$event_number_cac = NA
dates_all$event_start_cac = as.Date(NA)
dates_all$event_end_cac = as.Date(NA)

for ( i in 1: dim(dates_all)[1]){
  print(i)
  dates_all[i, c('event_number_cac', 'event_start_cac', 'event_end_cac') ] = dates_cac[which(dates_all$event_end[i] < dates_cac$event_start_cac)[1],2:4]
}
dates_all[which(dates_all$event_number %in% c('CCFH50', 'CCPR50', 'CCFICS24')), c('event_number_cac', 'event_start_cac', 'event_end_cac')]  = matrix(rep(c('CAC42', '2019-07-08', '2019-07-12'), each = 3), ncol = 3)


# add meeting data for ad hoc working group of GEQFF, which didn't have a 14th session but
# an ad hoc working group was established to finish its work after it adjourned following its 13th session

addRow = c('GEQFF', 'GEQFF14', '1983-07-05', NA, 1983, 'CAC15', '1983-07-04', '1983-07-15')

dates_all = rbind(dates_all, addRow)
 

# ----------------------------------
# add meeting type
# ----------------------------------
dates_all$meeting_type = NA
dates_all$meeting_type[which(dates_all$event_short %in% c('CCCF',
                                                  'CCFA',
                                                  'CCFAC',
                                                  'CCFH',
                                                  'CCFICS',
                                                  'CCFL',
                                                  'CCGP',
                                                  'CCMAS',
                                                  'CCNFSDU',
                                                  'CCPR',
                                                  'CCRVDF'))] = 'General Subject Committees'


dates_all$meeting_type[which(dates_all$event_short %in% c('CCCPC',
                                                  'CCCPL',
                                                  'CCFFP',
                                                  'CCFFV',
                                                  'CCFO',
                                                  'CCIE',
                                                  'CCM',
                                                  'CCMMP',
                                                  'CCMPH',
                                                  'CCNMW',
                                                  'CCPFV',
                                                  'CCPMPP',
                                                  'CCS',
                                                  'CCSB',
                                                  'CCSCH',
                                                  'CCVP',
                                                  'CXTO'))] = 'Commodity Committees'

dates_all$meeting_type[which(dates_all$event_short %in% c('CGECPMMP',
                                                  'GEFJ',
                                                  'GEQFF',
                                                  'TFAF',
                                                  'TFAMR',
                                                  'TFFBT',
                                                  'TFFJ',
                                                  'TFPHQFF'))] = 'ad hoc Intergovernmental Task Forces'

dates_all$meeting_type[which(dates_all$event_short %in% c('CCAFRICA',
                                                  'CCASIA',
                                                  'CCEURO',
                                                  'CCLAC', 
                                                  'CCNASWP',
                                                  'CCNEA'))] = 'FAO/WHO Coordinating Committees'

# ----------------------------------
# add meeting status
# ----------------------------------
dates_all$meeting_status = NA

dates_all$meeting_status[which(dates_all$event_short %in% c('CCCF',
                                                            'CCFA',
                                                            'CCFH',
                                                            'CCFICS',
                                                            'CCFL',
                                                            'CCGP',
                                                            'CCMAS',
                                                            'CCNFSDU',
                                                            'CCPR',
                                                            'CCRVDF',
                                                            'CCCPL',
                                                            'CCFFV',
                                                            'CCFO',
                                                            'CCPFV',
                                                            'CCS',
                                                            'CCSCH',
                                                            'TFAMR',
                                                            'CCAFRICA',
                                                            'CCASIA',
                                                            'CCEURO',
                                                            'CCLAC', 
                                                            'CCNASWP',
                                                            'CCNEA'))] = 'Active'


dates_all$meeting_status[which(dates_all$event_short %in% c('CCFAC',
                                                            'CGECPMMP'))] = 'Renamed and restablished'

dates_all$meeting_status[which(dates_all$event_short %in% c('CCCPC',
                                                            'CCFFP',
                                                            'CCMMP',
                                                            'CCMPH',
                                                            'CCNMW',
                                                            'CCVP'))] = 'Adjourned sine die'

dates_all$meeting_status[which(dates_all$event_short %in% c('CCIE',
                                                            'CCPMPP',
                                                            'CCSB',
                                                            'CXTO',
                                                            'GEFJ',
                                                            'GEQFF', 
                                                            'CCM'))] = 'Abolished'

dates_all$meeting_status[which(dates_all$event_short %in% c('TFAF',
                                                            'TFFBT',
                                                            'TFFJ',
                                                            'TFPHQFF'))] = 'Dissolved'

 
save(dates_all, file = paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))
 



