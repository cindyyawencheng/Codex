if(Sys.info()['user'] == 'cindycheng'){
	pathData = '/Users/cindycheng/Dropbox/Documents/Papers/Codex'
	pathCode = '/Users/cindycheng/Documents/Papers/Codex/RCode'
	pathResults = '/Users/cindycheng/Documents/Papers/Codex/Results'
}



loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}

# load packages
packages = c('readstata13', 'sampleSelection', 'dplyr', 'magrittr', 'texreg', 'fastDummies')
loadPkg(packages)

# --------------------
# create log of variables
# --------------------
log0 = function(variable){
	ifelse(variable == 0, 0, log(variable))}

# --------------------
# create share of total delegates variables
# --------------------
varShare = function(x, cac = FALSE){
	if(cac == FALSE){
		share = x/codex$delegates }
	if(cac == TRUE){
		share = x/codex$delegates_cac }
		return(share)
}


# --------------------
# extract tables
# --------------------
extractTables = function(dataset, committeeTypes, dv){
	if(committeeTypes == 'individualCommittees'){
		mods = results[[dataset]][-grep('cac', names(results[[dataset]]))]
		mods = mods[grep(dv, names(mods))]
	}
	

	if(committeeTypes == 'cac'){
		mods = results[[dataset]][grep('cac', names(results[[dataset]]))]
		mods = mods[grep(dv, names(mods))]
	}
	return(mods)
}

 


coef_map_list = list('S: ldelegates' = 'Log Delegates (s)' , 
				'S: ldelegations' = 'Log Delegations (s)' ,
				'S: delegConc' = 'Delegate Concentration (s)' ,
				'S: delegationSize' = 'Avg Delegates per Delegation (s)' ,
				'S: delegates_developedShare' = 'Share of Developed Country Delegates (s)' ,
				'S: ldelegates_region_group_EuropeCentralAsia' = 'Log Delegates, Europe and Central Asia (s)' ,
				'S: ldelegates_region_group_NorthAmerica' = 'Log Delegates, North America (s)' , 
				'S: ldelegates_region_group_LatinAmericaCaribbean' = 'Log Delegates, Latin American and Caribbean (s)' ,
				'S: ldelegates_region_group_MiddleEastNorthAfrica' = 'Log Delegates, MENA (s)' , 
				'S: ldelegates_region_group_SouthAsia' = 'Log Delegates South Asia (s)' ,
				'S: ldelegates_region_group_SubSaharanAfrica' = 'Log Delegates Sub Saharan Africa (s)' ,
				'S: ldelegates_region_group_EastAsiaPacific' = 'Log Delegates East Asia and Pacific (s)' ,
				'S: ldelegates_income_group_Highincome' = 'Log Delegates, High Income (s)' ,
				'S: ldelegates_income_group_Uppermiddleincome' = 'Log Delegates, Upper middle Income (s)' ,
				'S: ldelegates_income_group_Lowermiddleincome' = 'Log Delegates, Low middle income (s)' ,
				'S: ldelegates_income_group_Lowincome' = 'Log Delegates, Low income (s)' ,
				'S: ldelegates_interest_group_Industry' = 'Log Delegates, Industry (s)' ,
				'S: ldelegates_interest_group_Mixed' = 'Log Delegates, Mixed (s)' ,
				'S: ldelegates_interest_group_Consumer' = 'Log Delegates, Consumer (s)' , 
			#	'S: ldelegates_interest_group_Standardization' = 'Log Delegates, Standardization (s)' ,
			#	'S: ldelegates_interest_group_IGO' = 'Log Delegates, IGO (s)' , 
			#	'S: ldelegates_interest_group_Professional' = 'Log Delegates, Professional (s)' ,
			#	'S: ldelegates_interest_group_Environment' = 'Log Delegates, Environment (s)' , 
				'S: ldelegates_interest_group_Research' = 'Log Delegates, Research (s)' ,
			#	'S: ldelegates_interest_group_Media' = 'Log Delegates, Media (s)' , 


				'S: delegates_region_group_EuropeCentralAsia_share' = 'Delegates, Europe and Central Asia (% Total) (s)' ,
				'S: delegates_region_group_NorthAmerica_share' = 'Delegates, North America (% Total) (s)' ,
				'S: delegates_region_group_LatinAmericaCaribbean_share' = 'Delegates, Latin American and Caribbean (% Total) (s)' ,
				'S: delegates_region_group_MiddleEastNorthAfrica_share' = 'Delegates, MENA (% Total) (s)' ,
				'S: delegates_region_group_SouthAsia_share' = 'Delegates South Asia (% Total) (s)' ,
				'S: delegates_region_group_SubSaharanAfrica_share' = 'Delegates Sub Saharan Africa (% Total) (s)' ,
				'S: delegates_region_group_EastAsiaPacific_share' = 'Delegates East Asia and Pacific (% Total) (s)' , 
				'S: delegates_income_group_Highincome_share' = 'Delegates, High Income (% Total) (s)' ,
				'S: delegates_income_group_Uppermiddleincome_share' = 'Delegates, Upper middle Income (% Total) (s)' ,
				'S: delegates_income_group_Lowermiddleincome_share' = 'Delegates, Low middle income (% Total) (s)' , 
				'S: delegates_income_group_Lowincome_share' = 'Delegates, Low income (% Total) (s)' ,
				'S: delegates_interest_group_Industry_share' = 'Delegates, Industry (% Total) (s)' ,
				'S: delegates_interest_group_Mixed_share' = 'Delegates, Mixed (% Total) (s)' ,
				'S: delegates_interest_group_Consumer_share' = 'Delegates, Consumer (% Total) (s)' ,
				'S: delegates_interest_group_Standardization_share' = 'Delegates, Standardization (% Total) (s)' ,
				'S: delegates_interest_group_IGO_share' = 'Delegates, IGO (% Total) (s)' ,
				'S: delegates_interest_group_Professional_share' = 'Delegates, Professional (% Total) (s)' ,
				'S: delegates_interest_group_Environment_share' = 'Delegates, Environment (% Total) (s)' ,
				'S: delegates_interest_group_Research_share' = 'Delegates, Research (% Total) (s)' ,
				'S: delegates_interest_group_Media_share' = 'Delegates, Media (% Total) (s)' ,

				'S: wtoDummy' = 'WTO Dummy (s)' , 
				'S: meetingCount' = 'Meeting Count (s)',
				'S: meetingCount' = 'Meeting Count (s)' ,
				# 'S: event_shortCCASIA' = 'CCASIA (s)' ,
				# 'S: event_shortCCCF' = 'CCCF (s)' ,
				# 'S: event_shortCCCPC' = 'CCCPC (s)' ,
				# 'S: event_shortCCCPL' = 'CCCPL (s)' ,
				# 'S: event_shortCCEURO' = 'CCEURO (s)' ,
				# 'S: event_shortCCFA' = 'CCFA (s)' ,
				# 'S: event_shortCCFAC' = 'CCFAC (s)' ,
				# 'S: event_shortCCFFP' = 'CCFFP (s)' ,
				# 'S: event_shortCCFFV' = 'CCFFV (s)' ,
				# 'S: event_shortCCFH' = 'CCFH (s)' ,
				# 'S: event_shortCCFICS' = 'CCFICS (s)' ,
				# 'S: event_shortCCFL' = 'CCFL (s)' ,
				# 'S: event_shortCCFO' = 'CCFO (s)' ,
				# 'S: event_shortCCGP' = 'CCGP (s)' ,
				# 'S: event_shortCCIE' = 'CCIE (s)' ,
				# 'S: event_shortCCLAC' = 'CCLAC (s)' ,
				# 'S: event_shortCCM' = 'CCM (s)' ,
				# 'S: event_shortCCMAS' = 'CCMAS (s)' ,
				# 'S: event_shortCCMMP' = 'CCMMP (s)' ,
				# 'S: event_shortCCMPH' = 'CCMPH (s)' ,
				# 'S: event_shortCCNASWP' = 'CCNASWP (s)' ,
				# 'S: event_shortCCNEA' = 'CCNEA (s)' ,
				# 'S: event_shortCCNFSDU' = 'CCNFSDU (s)' ,
				# 'S: event_shortCCNMW' = 'CCNMW (s)' ,
				# 'S: event_shortCCPFV' = 'CCPFV (s)' ,
				# 'S: event_shortCCPMPP' = 'CCPMPP (s)' ,
				# 'S: event_shortCCPR' = 'CCPR (s)' ,
				# 'S: event_shortCCRVDF' = 'CCRVDF (s)' ,
				# 'S: event_shortCCS' = 'CCS (s)' ,
				# 'S: event_shortCCSB' = 'CCSB (s)' ,
				# 'S: event_shortCCSCH' = 'CCSCH (s)' ,
				# 'S: event_shortCCVP' = 'CCVP (s)' ,
				# 'S: event_shortCGECPMMP' = 'CGECPMMP (s)' ,
				# 'S: event_shortGEFJ' = 'GEFJ (s)' ,
				# 'S: event_shortGEQFF' = 'GEQFF (s)' ,
				# 'S: event_shortTFAF' = 'TFAF (s)' , 
				# 'S: event_shortTFAMR' = 'TFAMR (s)' ,
				# 'S: event_shortTFFBT' = 'TFFBT (s)' ,
				# 'S: event_shortTFFJ' = 'TFFJ (s)' ,
				# 'S: event_shortTFPHQFF' = 'TFPHQFF (s)' ,
				'O: ldelegates' = 'Log Delegates (o)' ,
				'O: ldelegations' = 'Log Delegations (o)' ,
				'O: delegConc' = 'Delegate Concentration (o)' ,
				'O: delegationSize' = 'Avg Delegates per Delegation (o)' ,
				'O: delegates_developedShare' = 'Share of Developed Country Delegates (o)' ,
				'O: ldelegates_region_group_EuropeCentralAsia' = 'Log Delegates, Europe and Central Asia (o)' ,
				'O: ldelegates_region_group_NorthAmerica' = 'Log Delegates, North America (o)' ,
				'O: ldelegates_region_group_LatinAmericaCaribbean' = 'Log Delegates, Latin American and Caribbean (o)' ,
				'O: ldelegates_region_group_MiddleEastNorthAfrica' = 'Log Delegates, MENA (o)' ,
				'O: ldelegates_region_group_SouthAsia' = 'Log Delegates South Asia (o)' ,
				'O: ldelegates_region_group_SubSaharanAfrica' = 'Log Delegates Sub Saharan Africa (o)' ,
				'O: ldelegates_region_group_EastAsiaPacific' = 'Log Delegates East Asia and Pacific (o)' , 
				'O: ldelegates_income_group_Highincome' = 'Log Delegates, High Income (o)' ,
				'O: ldelegates_income_group_Uppermiddleincome' = 'Log Delegates, Upper middle Income (o)' ,
				'O: ldelegates_income_group_Lowermiddleincome' = 'Log Delegates, Low middle income (o)' , 
				'O: ldelegates_income_group_Lowincome' = 'Log Delegates, Low income (o)' ,
				'O: ldelegates_interest_group_Industry' = 'Log Delegates, Industry (o)' ,
				'O: ldelegates_interest_group_Mixed' = 'Log Delegates, Mixed (o)' ,
				'O: ldelegates_interest_group_Consumer' = 'Log Delegates, Consumer (o)' ,
				'O: ldelegates_interest_group_Standardization' = 'Log Delegates, Standardization (o)' ,
				'O: ldelegates_interest_group_IGO' = 'Log Delegates, IGO (o)' ,
				'O: ldelegates_interest_group_Professional' = 'Log Delegates, Professional (o)' ,
				'O: ldelegates_interest_group_Environment' = 'Log Delegates, Environment (o)' ,
				'O: ldelegates_interest_group_Research' = 'Log Delegates, Research (o)' ,
				'O: ldelegates_interest_group_Media' = 'Log Delegates, Media (o)' ,


				'O: delegates_region_group_EuropeCentralAsia_share' = 'Delegates, Europe and Central Asia (% Total) (o)' ,
				'O: delegates_region_group_NorthAmerica_share' = 'Delegates, North America (% Total) (o)' ,
				'O: delegates_region_group_LatinAmericaCaribbean_share' = 'Delegates, Latin American and Caribbean (% Total) (o)' ,
				'O: delegates_region_group_MiddleEastNorthAfrica_share' = 'Delegates, MENA (% Total) (o)' ,
				'O: delegates_region_group_SouthAsia_share' = 'Delegates South Asia (% Total) (o)' ,
				'O: delegates_region_group_SubSaharanAfrica_share' = 'Delegates Sub Saharan Africa (% Total) (o)' ,
				'O: delegates_region_group_EastAsiaPacific_share' = 'Delegates East Asia and Pacific (% Total) (o)' , 
				'O: delegates_income_group_Highincome_share' = 'Delegates, High Income (% Total) (o)' ,
				'O: delegates_income_group_Uppermiddleincome_share' = 'Delegates, Upper middle Income (% Total) (o)' ,
				'O: delegates_income_group_Lowermiddleincome_share' = 'Delegates, Low middle income (% Total) (o)' , 
				'O: delegates_income_group_Lowincome_share' = 'Delegates, Low income (% Total) (o)' ,
				'O: delegates_interest_group_Industry_share' = 'Delegates, Industry (% Total) (o)' ,
				'O: delegates_interest_group_Mixed_share' = 'Delegates, Mixed (% Total) (o)' ,
				'O: delegates_interest_group_Consumer_share' = 'Delegates, Consumer (% Total) (o)' ,
				'O: delegates_interest_group_Standardization_share' = 'Delegates, Standardization (% Total) (o)' ,
				'O: delegates_interest_group_IGO_share' = 'Delegates, IGO (% Total) (o)' ,
				'O: delegates_interest_group_Professional_share' = 'Delegates, Professional (% Total) (o)' ,
				'O: delegates_interest_group_Environment_share' = 'Delegates, Environment (% Total) (o)' ,
				'O: delegates_interest_group_Research_share' = 'Delegates, Research (% Total) (o)' ,
				'O: delegates_interest_group_Media_share' = 'Delegates, Media (% Total) (o)' ,

				'O: wtoDummy' = 'WTO Dummy (o)' ,
				'O: meetingCount' = 'Meeting Count (o)')


coef_map_list_cac = list('S: ldelegates_cac' = 'Log Delegates (s)' , 
				'S: ldelegations_cac' = 'Log Delegations (s)' ,
				'S: delegConc_cac' = 'Delegate Concentration (s)' ,
				'S: delegationSize_cac' = 'Avg Delegates per Delegation (s)' ,
				'S: delegates_developedShare_cac' = 'Share of Developed Country Delegates (s)' ,
				'S: ldelegates_region_group_EuropeCentralAsia_cac' = 'Log Delegates, Europe and Central Asia (s)' ,
				'S: ldelegates_region_group_NorthAmerica_cac' = 'Log Delegates, North America (s)' , 
				'S: ldelegates_region_group_LatinAmericaCaribbean_cac' = 'Log Delegates, Latin American and Caribbean (s)' ,
				'S: ldelegates_region_group_MiddleEastNorthAfrica_cac' = 'Log Delegates, MENA (s)' , 
				'S: ldelegates_region_group_SouthAsia_cac' = 'Log Delegates South Asia (s)' ,
				'S: ldelegates_region_group_SubSaharanAfrica_cac' = 'Log Delegates Sub Saharan Africa (s)' ,
				'S: ldelegates_region_group_EastAsiaPacific_cac' = 'Log Delegates East Asia and Pacific (s)' ,
				'S: ldelegates_income_group_Highincome_cac' = 'Log Delegates, High Income (s)' ,
				'S: ldelegates_income_group_Uppermiddleincome_cac' = 'Log Delegates, Upper middle Income (s)' ,
				'S: ldelegates_income_group_Lowermiddleincome_cac' = 'Log Delegates, Low middle income (s)' ,
				'S: ldelegates_income_group_Lowincome_cac' = 'Log Delegates, Low income (s)' ,
				'S: ldelegates_interest_group_Industry_cac' = 'Log Delegates, Industry (s)' ,
				'S: ldelegates_interest_group_Mixed_cac' = 'Log Delegates, Mixed (s)' ,
				'S: ldelegates_interest_group_Consumer_cac' = 'Log Delegates, Consumer (s)' , 
				'S: ldelegates_interest_group_Standardization_cac' = 'Log Delegates, Standardization (s)' ,
				'S: ldelegates_interest_group_IGO_cac' = 'Log Delegates, IGO (s)' , 
				'S: ldelegates_interest_group_Professional_cac' = 'Log Delegates, Professional (s)' ,
				'S: ldelegates_interest_group_Environment_cac' = 'Log Delegates, Environment (s)' , 
				'S: ldelegates_interest_group_Research_cac' = 'Log Delegates, Research (s)' ,
				'S: ldelegates_interest_group_Media_cac' = 'Log Delegates, Media (s)' , 


				'S: delegates_region_group_EuropeCentralAsia_cac_share' = 'Delegates, Europe and Central Asia (% Total) (s)' ,
				'S: delegates_region_group_NorthAmerica_cac_share' = 'Delegates, North America (% Total) (s)' ,
				'S: delegates_region_group_LatinAmericaCaribbean_cac_share' = 'Delegates, Latin American and Caribbean (% Total) (s)' ,
				'S: delegates_region_group_MiddleEastNorthAfrica_cac_share' = 'Delegates, MENA (% Total) (s)' ,
				'S: delegates_region_group_SouthAsia_cac_share' = 'Delegates South Asia (% Total) (s)' ,
				'S: delegates_region_group_SubSaharanAfrica_cac_share' = 'Delegates Sub Saharan Africa (% Total) (s)' ,
				'S: delegates_region_group_EastAsiaPacific_cac_share' = 'Delegates East Asia and Pacific (% Total) (s)' , 
				'S: delegates_income_group_Highincome_cac_share' = 'Delegates, High Income (% Total) (s)' ,
				'S: delegates_income_group_Uppermiddleincome_cac_share' = 'Delegates, Upper middle Income (% Total) (s)' ,
				'S: delegates_income_group_Lowermiddleincome_cac_share' = 'Delegates, Low middle income (% Total) (s)' , 
				'S: delegates_income_group_Lowincome_cac_share' = 'Delegates, Low income (% Total) (s)' ,
				'S: delegates_interest_group_Industry_cac_share' = 'Delegates, Industry (% Total) (s)' ,
				'S: delegates_interest_group_Mixed_cac_share' = 'Delegates, Mixed (% Total) (s)' ,
				'S: delegates_interest_group_Consumer_cac_share' = 'Delegates, Consumer (% Total) (s)' ,
				'S: delegates_interest_group_Standardization_cac_share' = 'Delegates, Standardization (% Total) (s)' ,
				'S: delegates_interest_group_IGO_cac_share' = 'Delegates, IGO (% Total) (s)' ,
				'S: delegates_interest_group_Professional_cac_share' = 'Delegates, Professional (% Total) (s)' ,
				'S: delegates_interest_group_Environment_cac_share' = 'Delegates, Environment (% Total) (s)' ,
				'S: delegates_interest_group_Research_cac_share' = 'Delegates, Research (% Total) (s)' ,
				'S: delegates_interest_group_Media_cac_share' = 'Delegates, Media (% Total) (s)' ,

				'S: wtoDummy' = 'WTO Dummy (s)' , 
				 'S: meetingCount' = 'Meeting Count (s)',
				'O: ldelegates_cac' = 'Log Delegates (o)' ,
				'O: ldelegations_cac' = 'Log Delegations (o)' ,
				'O: delegConc_cac' = 'Delegate Concentration (o)' ,
				'O: delegationSize_cac' = 'Avg Delegates per Delegation (o)' ,
				'O: delegates_developedShare_cac' = 'Share of Developed Country Delegates (o)' ,
				'O: ldelegates_region_group_EuropeCentralAsia_cac' = 'Log Delegates, Europe and Central Asia (o)' ,
				'O: ldelegates_region_group_NorthAmerica_cac' = 'Log Delegates, North America (o)' ,
				'O: ldelegates_region_group_LatinAmericaCaribbean_cac' = 'Log Delegates, Latin American and Caribbean (o)' ,
				'O: ldelegates_region_group_MiddleEastNorthAfrica_cac' = 'Log Delegates, MENA (o)' ,
				'O: ldelegates_region_group_SouthAsia_cac' = 'Log Delegates South Asia (o)' ,
				'O: ldelegates_region_group_SubSaharanAfrica_cac' = 'Log Delegates Sub Saharan Africa (o)' ,
				'O: ldelegates_region_group_EastAsiaPacific_cac' = 'Log Delegates East Asia and Pacific (o)' , 
				'O: ldelegates_income_group_Highincome_cac' = 'Log Delegates, High Income (o)' ,
				'O: ldelegates_income_group_Uppermiddleincome_cac' = 'Log Delegates, Upper middle Income (o)' ,
				'O: ldelegates_income_group_Lowermiddleincome_cac' = 'Log Delegates, Low middle income (o)' , 
				'O: ldelegates_income_group_Lowincome_cac' = 'Log Delegates, Low income (o)' ,
				'O: ldelegates_interest_group_Industry_cac' = 'Log Delegates, Industry (o)' ,
				'O: ldelegates_interest_group_Mixed_cac' = 'Log Delegates, Mixed (o)' ,
				'O: ldelegates_interest_group_Consumer_cac' = 'Log Delegates, Consumer (o)' ,
				'O: ldelegates_interest_group_Standardization_cac' = 'Log Delegates, Standardization (o)' ,
				'O: ldelegates_interest_group_IGO_cac' = 'Log Delegates, IGO (o)' ,
				'O: ldelegates_interest_group_Professional_cac' = 'Log Delegates, Professional (o)' ,
				'O: ldelegates_interest_group_Environment_cac' = 'Log Delegates, Environment (o)' ,
				'O: ldelegates_interest_group_Research_cac' = 'Log Delegates, Research (o)' ,
				'O: ldelegates_interest_group_Media_cac' = 'Log Delegates, Media (o)' ,
				'O: delegates_region_group_EuropeCentralAsia_cac_share' = 'Delegates, Europe and Central Asia (% Total) (o)' ,
				'O: delegates_region_group_NorthAmerica_cac_share' = 'Delegates, North America (% Total) (o)' ,
				'O: delegates_region_group_LatinAmericaCaribbean_cac_share' = 'Delegates, Latin American and Caribbean (% Total) (o)' ,
				'O: delegates_region_group_MiddleEastNorthAfrica_cac_share' = 'Delegates, MENA (% Total) (o)' ,
				'O: delegates_region_group_SouthAsia_cac_share' = 'Delegates South Asia (% Total) (o)' ,
				'O: delegates_region_group_SubSaharanAfrica_cac_share' = 'Delegates Sub Saharan Africa (% Total) (o)' ,
				'O: delegates_region_group_EastAsiaPacific_cac_share' = 'Delegates East Asia and Pacific (% Total) (o)' , 
				'O: delegates_income_group_Highincome_cac_share' = 'Delegates, High Income (% Total) (o)' ,
				'O: delegates_income_group_Uppermiddleincome_cac_share' = 'Delegates, Upper middle Income (% Total) (o)' ,
				'O: delegates_income_group_Lowermiddleincome_cac_share' = 'Delegates, Low middle income (% Total) (o)' , 
				'O: delegates_income_group_Lowincome_cac_share' = 'Delegates, Low income (% Total) (o)' ,
				'O: delegates_interest_group_Industry_cac_share' = 'Delegates, Industry (% Total) (o)' ,
				'O: delegates_interest_group_Mixed_cac_share' = 'Delegates, Mixed (% Total) (o)' ,
				'O: delegates_interest_group_Consumer_cac_share' = 'Delegates, Consumer (% Total) (o)' ,
				'O: delegates_interest_group_Standardization_cac_share' = 'Delegates, Standardization (% Total) (o)' ,
				'O: delegates_interest_group_IGO_cac_share' = 'Delegates, IGO (% Total) (o)' ,
				'O: delegates_interest_group_Professional_cac_share' = 'Delegates, Professional (% Total) (o)' ,
				'O: delegates_interest_group_Environment_cac_share' = 'Delegates, Environment (% Total) (o)' ,
				'O: delegates_interest_group_Research_cac_share' = 'Delegates, Research (% Total) (o)' ,
				'O: delegates_interest_group_Media_cac_share' = 'Delegates, Media (% Total) (o)' ,

				'O: wtoDummy' = 'WTO Dummy (o)' ,
				'O: meetingCount' = 'Meeting Count (o)')