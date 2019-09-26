# clean superseding standards
if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}


superStds = read.csv(paste0(pathMain, '/participation_development/codex_superseding_standards.csv'), stringsAsFactors = FALSE, na.strings = '.')
superStds$Reference_original_standard[47] = "CAC/RCP 13-1976, Rev. 1 1985"
superStds$Reference_original_standard[49] = "CAC/RCP 32-1983"
superStds$Title_original_standard = str_trim(superStds$Title_original_standard ) 
superStds[which(superStds$Reference_original_standard == "CODEX STAN 139-1983"), 'Title_original_standard'] = "Concentrated pineapple juice preserved exclusively by physical means"
superStds[which(superStds$Reference_original_standard == "CODEX STAN 138-1983"), 'Title_original_standard'] = "Concentrated pineapple juice with preservatives for manufacturing"

# manually add row
addRow = c('CAC/RCP 66-2008', 'CAC/RCP 21-1979', 'Recommended International Code of Hygienic Practice for Foods for Infants and Children', 'CCFH10')
superStds = rbind(superStds, addRow)

save(superStds, file = paste0(pathMain, '/participation_development/codex_superseding_standards_clean.rda'))
