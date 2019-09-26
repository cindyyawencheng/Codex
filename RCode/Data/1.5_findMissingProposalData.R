if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

# -------------------------------------------------------
## Load packages and functions
# -------------------------------------------------------
library(tidytext)
library(tm)

# Function to create multiple tabs
make.tabs <- function(metaData, content){
  pages <-NULL
  for ( i in seq_along(metaData)){
    metaSlice = metaData[[i]]
    metaContent = content[[i]]
    
    meetingDoc <- NULL
    for (s in 1:dim(metaSlice)[1]){
      print(s)

      meetingDoc <- c(meetingDoc,  '#### ', paste(paste(metaSlice[s, c('Meeting', 'Folder')], collapse = ', Year '), metaSlice[s, c('id')], collapse = ' , Filename '), '\n\n',
                      '**', metaSlice$Title %>% unique(), '**', '\n\n',   
                      '**', metaSlice$Reference %>% unique(), '**', '\n\n',
                      '**', metaSlice$id[s], '**', '\n\n',
                      metaContent[s], '\n\n')
      }

    meetingDocs = meetingDoc
    pages <- c(pages, '\n' , '### ', i, '{.tabset}', '\n', meetingDocs)
    
  }
  
  return(pages)
}


# function to fill in year manually
fillYear = function(data, Title, id){
  data$year[which(data$Title == Title & data$id == id)] = data$Folder[which(data$Title == Title & data$id == id)]  
  return(data)
}

# function to fill in step of proposal stage manually
fillStep = function(data, Title, id, step){
  data$step[which(data$Title == Title & data$id == id)] = step
  return(data)
}

options(mc.cores=1) # this option is necssary for the verbgramTokenizer to run

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}


cleanProposalData = function(proposalData) {
  proposalData = apply(proposalData, 2, function(x)  gsub('\\-', ' ', x)) 
  proposalData = apply(proposalData, 2, function(x)  gsub('[[:punct:]]', ' ', x)) 
  proposalData = apply(proposalData, 2, function(x)  tolower(x)) 
  proposalData = apply(proposalData, 2, function(x)  str_trim(x))  
  proposalData = apply(proposalData, 2, function(x)  gsub("  |  |   ", ' ', x)) %>% data.frame(., stringsAsFactors = FALSE)
  return(proposalData) 
}



# remove meeting reports that happen after the standard was adopted
removeReports = function(proposalList){
  proposalList = lapply(proposalList, function(x){
    stringSize = x$Reference %>% unique() %>% nchar()
    x$year = str_sub(x$Reference, stringSize -3, stringSize)
    x = x[which(x$year>=x$Folder),]
    return(x)
  })

  # remove empty items from list as a result of above subsetting
  proposalList  = proposalList [sapply( proposalList, nrow)>0]
  
}

between1And10Matches = function(proposalList){
  proposalList = lapply(proposalList, function(x){
    if ( dim(x)[1]<11 & dim(x)[1]>0){
      return(x[order(x$Folder),])
    } else{
        x[order(x$Folder),][1:10,]
    }
  })
  
proposalList = Filter(function(a) any(!is.na(a)),proposalList)

} 


# newest version of tm (07-01) doesn't work with ngrams for whatever reason, so reinstalling old version
# install.packages("https://cran.r-project.org/src/contrib/Archive/tm/tm_0.6-1.tar.gz", repos = NULL, type = 'source')

# -------------------------------------------------------
## Load texts
# -------------------------------------------------------
setwd(pathMain)
# get list of directories inside the currect working directory
directories <- paste(paste(pathMain , c(list.files()[1:37]), sep= '/') , '/txt', sep = '')
FOLDERS = list.files()[1:37]

 
docList = list()
stdsMatch<- list()

# load in all texts into a list
ptm <- proc.time() # start time of code
for (i in 1:length(directories)){ 
  #for (s in 1:dim(ids)[1]){
  print(FOLDERS[i])
  
  setwd(pathData)
  files = list.files(directories[i])[grep('.txt',list.files(directories[i]))]
  docs <- VCorpus(DirSource(directories[i], encoding = "UTF-8"))
  
  # Remove punctuation
  docs <- tm_map(docs,  toSpace, '[[:punct:]]', ' ')
  docs <- tm_map(docs, toSpace, '\\/', ' ')
  docs <- tm_map(docs, content_transformer(tolower)) 
  
  #Remove common word endings
  #docs <- tm_map(docs, stemDocument)   
  
  #Strip whitespace
  docs <- tm_map(docs, stripWhitespace)   
  
  #Tell R to treat preprocessed documents as text documents.
  # docs <- tm_map(docs, PlainTextDocument)
  
  
  docList[[i]] = docs
  #check
  # print(colnames(verbs[[i]]))
}


# combine list of texts into a single corpus
docsAll  = do.call(function(...) c(..., recursive = TRUE), docList)
docsAll_tidy = tidy(docsAll)
docsAll_tidy$id = gsub('.txt', '', docsAll_tidy$id)
# match texts with meta data

# add in info about the document name and folder name
meta = read.csv(paste0(pathData, '/metadata.csv'), stringsAsFactors = F)
meta$reportID = gsub('.pdf', '', meta$reportID)
docsAll_tidy = merge(docsAll_tidy, meta[, c('Meeting', 'Folder', 'reportID', 'CodexError')], by.x = 'id', by.y = 'reportID', all.x = TRUE)

# ---------------------------------------------------------------

# ---------------------------------------------------------------
# Round 1
load(paste0(pathMain, '/participation_development/findMissingProposals.rda'))
findMissingProposals = findMissingProposals[is.na(findMissingProposals$standards_extracted_clean), -3]
findMissingProposals = cleanProposalData((findMissingProposals))
 
find_proposalList = list()
for ( i in 1:dim(findMissingProposals)[1]){
  print(i)
  find_proposalList[[i]] <-docsAll_tidy[str_detect(docsAll_tidy$text, unlist(findMissingProposals[i,])),]
  if(dim(find_proposalList[[i]])[1] > 0){
    find_proposalList[[i]]$Title = findMissingProposals[i, 'Title']
    find_proposalList[[i]]$Reference = findMissingProposals[i, 'Reference']
  }
}



# save(find_proposalList, file = paste0(pathMain, '/participation_development/findMissingProposalsList.rda'))
load(paste0(pathData, '/participation_development/findMissingProposalsList.rda'))

# remove matches for which the meeting year is later than when the standard was adopted
find_proposalList = removeReports(find_proposalList)
 
 
# select standards for which there are between 1 and 10 matches in the corpus
between1And10Matches = lapply(find_proposalList, function(x){
  if ( dim(x)[1]<11 & dim(x)[1]>0){
    return(x[order(x$Folder),])
  } else{
    NA
  }
})


between1And10Matches = Filter(function(a) any(!is.na(a)), between1And10Matches)

# highlight the text for easier identification in the html file
highLightedText = lapply(between1And10Matches, function(x){
  xTitle <- paste0("<font color='blue'>",x$Title %>% unique(),"</font>")
  xReference <- paste0("<font color='blue'>",x$Reference %>% unique(),"</font>")
  
  textHighlight = str_replace (x$text, coll(x$Title %>% unique()), xTitle)
  textHighlight = str_replace (textHighlight, coll(x$Reference %>% unique()), xReference)
}
)
 

# -------------------------------------------------------  
# write an .rmd file that will write an .html file that allows you to visually search for matches easier
# -------------------------------------------------------
setwd('/Users/cindycheng/Documents/Papers/Codex/RCode')


# Create the Rmd to knit
cat(
'---
title: "Look for Missing Proposals"
author: "Cindy Cheng"
date: "2019-02-21"
output: html_document
---
  
  
## Proposals {.tabset} \n

  ',
  # make.tabs(title = 1:length(between1And10Matches) ,
  #           codex_text = rep('test', length(between1And10Matches))),
  make.tabs(between1And10Matches, highLightedText),
  
  sep = "",
  file = "1.5_findMissingProposalData.Rmd")


# Render the Rmd created into html here
rmarkdown::render("1.5_findMissingProposalData.Rmd")

# -------------------------------------------------------
# Manually add in year
# -------------------------------------------------------


fillProposal = do.call(rbind, lapply(find_proposalList, function(x){
  x = dplyr:::select(x, id, Meeting, Folder, Title, Reference)
  x$year = NA
  x$step = NA
  return(x)
})) %>% data.frame()




# fill in year to be folder year
fillProposal = fillYear(fillProposal, 'code of hygienic practice for aseptically processed and packaged low acid foods', 'al93_13Ae')
fillProposal = fillYear(fillProposal, 'code of hygienic practice for canned fruit and vegetable products', 'Fal68_13e')
fillProposal = fillYear(fillProposal,'code of hygienic practice for dehydrated fruits and vegetables including edible fungi', '03%252Fal66_13e')
fillProposal = fillYear(fillProposal,'code of hygienic practice for desiccated coconut', 'Fal68_13e')
fillProposal = fillYear(fillProposal,'code of hygienic practice for dried fruits', '03%252Fal66_13e')
fillProposal = fillYear(fillProposal,'code of hygienic practice for low moisture foods', '5%252FREP14_FHe')
fillProposal = fillYear(fillProposal,'code of hygienic practice for the processing of frog legs', 'Fal81_39e')
fillProposal = fillYear(fillProposal,'code of hygienic practice for tree nuts', 'Fal71_13e')
fillProposal = fillYear(fillProposal,'code of practice for the prevention and reduction of aflatoxin contamination in dried figs', '01%252Fal30_41e')
fillProposal = fillYear(fillProposal,'code of practice for the prevention and reduction of arsenic contamination in rice', '8%252FREP14_CFe')
fillProposal = fillYear(fillProposal,'code of practice for the prevention and reduction of ochratoxin a contamination in coffee', '02%252Fal31_41e')
fillProposal = fillYear(fillProposal, 'code of practice for the reduction of contamination of food with polycyclic aromatic hydrocarbons pah from smoking and direct drying processes', '%252FREP12_CACe')
fillProposal = fillYear(fillProposal, 'code of practice for weed control to prevent and reduce pyrrolizidine alkaloid contamination in food and feed', '01%252Fal30_41e')
fillProposal = fillYear(fillProposal, 'general guidelines for use of the term halal', 'Fal97_37e')
fillProposal = fillYear(fillProposal, 'general methods for the detection of irradiated foods', 'FAl0112ae')
fillProposal = fillYear(fillProposal, 'general methods of analysis for contaminants', 'Fal93_23e')
fillProposal = fillYear(fillProposal, 'general standard for the use of dairy terms', 'FAl99_37e')
fillProposal = fillYear(fillProposal, 'group standard for unripened cheese including fresh cheese', 'Fal01_11e')
fillProposal = fillYear(fillProposal, 'guideline for the validation of food safety control measures', '31%252Fal31REPe')
fillProposal = fillYear(fillProposal, 'guideline procedures for the visual inspection of lots of canned foods for unacceptable defects', 'Fal93_13e')
fillProposal = fillYear(fillProposal, 'guidelines for food import control systems', 'Fal0130ae')
fillProposal = fillYear(fillProposal, 'guidelines for the control of taenia saginata in meat of domestic cattle', '5%252FREP14_FHe')
fillProposal = fillYear(fillProposal, 'guidelines for the control of trichinella spp in meat of suidae', '%252FREP14_CACe')
fillProposal = fillYear(fillProposal, 'guidelines for the development of equivalence agreements regarding food imports and export inspection and certification systems', 'FAl99_37e')
fillProposal = fillYear(fillProposal, 'guidelines for the exchange of information between countries on rejections of imported foods', 'al97_30Ae')
fillProposal = fillYear(fillProposal, 'guidelines on the application of general principles of food hygiene to the control of foodborne parasites', '5%252FREP14_FHe')
fillProposal = fillYear(fillProposal, 'guidelines on the application of general principles of food hygiene to the control of pathogenic vibrio species in seafood', 'Fal33_13e')
fillProposal = fillYear(fillProposal, 'harmonized iupac guidelines for single laboratory validation of methods of analysis', '24%252Fal03_23e')
fillProposal = fillYear(fillProposal, 'harmonized iupac guidelines for the use of recovery information in analytical measurement', 'Fal01_24e')
fillProposal = fillYear(fillProposal, 'principles and guidance on the selection of representative commodities for the extrapolation of maximum residue limits for pesticides to commodity groups', '4%252FREP12_PRe')
fillProposal = fillYear(fillProposal, 'principles and guidelines for the conduct of microbiological risk assessment', '4al97_13Ae')
fillProposal = fillYear(fillProposal, 'principles for the use of sampling and testing in international food trade', '%252FREP11_CACe')
fillProposal = fillYear(fillProposal, 'regional standard for canned foul medames', 'Fal28_41e')
fillProposal = fillYear(fillProposal, 'regional standard for canned humus with tehena', 'Fal28_41e')
fillProposal = fillYear(fillProposal, 'regional standard for date paste near east', '7%252FREP13_NEe')
fillProposal = fillYear(fillProposal, 'standard for aqueous coconut products coconut milk and coconut cream', '21%252Fal03_27e')
fillProposal = fillYear(fillProposal, 'standard for boiled dried salted anchovies', '24%252Fal03_23e')
fillProposal = fillYear(fillProposal, 'standard for butter', '-08%252Fcx65_8e')
fillProposal = fillYear(fillProposal, 'standard for canned applesauce', 'Fal69_67e')
fillProposal = fillYear(fillProposal, 'standard for canned fruit cocktail', 'Fal70_12e')
fillProposal = fillYear(fillProposal, 'standard for canned pineapple', 'Fal70_22e')
fillProposal = fillYear(fillProposal, 'standard for canned raspberries', 'Fal71_20e')
fillProposal = fillYear(fillProposal, 'standard for canned salmon', 'Fal81_13e')
fillProposal = fillYear(fillProposal, 'standard for canned stone fruits', 'Fal99_27e')
fillProposal = fillYear(fillProposal, 'standard for canned strawberries', 'Fal71_20e')
fillProposal = fillYear(fillProposal, 'standard for certain canned fruits', '%252FREP11_CACe')
fillProposal = fillYear(fillProposal, 'standard for chayotes', '2Fal97_3e')
fillProposal = fillYear(fillProposal, 'standard for cooked cured chopped meat', '10%252Fal78_23e')
fillProposal = fillYear(fillProposal, 'standard for cooked cured ham', '721-07%252Fal74_16e')
fillProposal = fillYear(fillProposal, 'standard for cooked cured pork shoulder', '721-07%252Fal74_16e')
fillProposal = fillYear(fillProposal, 'standard for corned beef', 'Fal70_16e')
fillProposal = fillYear(fillProposal, 'standard for dairy fat spreads', '07%252Fal29_11e')
fillProposal = fillYear(fillProposal, 'standard for degermed maize corn meal and maize corn grits', 'Fal83_43e')
fillProposal = fillYear(fillProposal, 'standard for durum wheat semolina and durum wheat flour', '06%252Fal89_29e')
fillProposal = fillYear(fillProposal, 'standard for edible fats and oils not covered by individual standards', 'Fal69_67e')
fillProposal = fillYear(fillProposal, 'standard for extra hard grating cheese', '18%252Fcx76_18e')
fillProposal = fillYear(fillProposal, 'standard for grapefruits', 'Fal99_35e')
fillProposal = fillYear(fillProposal, 'standard for jams jellies and marmalades', 'Fal99_35e')
fillProposal = fillYear(fillProposal, 'standard for kimchi', 'FAL97_15E')
fillProposal = fillYear(fillProposal, 'standard for live abalone and for raw fresh chilled or frozen abalone for direct consumption or for further processing', '%252FREP13_FFPe')
fillProposal = fillYear(fillProposal, 'standard for longans', 'Fal99_35e')
fillProposal = fillYear(fillProposal, 'standard for luncheon meat', 'Fal70_16e')
fillProposal = fillYear(fillProposal, 'standard for mangosteens', 'Fal97_35e')
fillProposal = fillYear(fillProposal, 'standard for named animal fats', 'Fal95_17e')
fillProposal = fillYear(fillProposal, 'standard for pickled fruits and vegetables', 'Fal28_27e')
fillProposal = fillYear(fillProposal, 'standard for pomegranate', 'Fal33_35e')
fillProposal = fillYear(fillProposal, 'standard for processed tomato concentrates', 'Fal71_20e')
fillProposal = fillYear(fillProposal, 'standard for pummelos', '2Fal97_3e')
fillProposal = fillYear(fillProposal, 'standard for quick frozen blocks of fish fillets minced fish flesh and mixtures of fillets and minced fish flesh', '8e_Part-I')
fillProposal = fillYear(fillProposal, 'standard for quick frozen blueberries', 'Fal76_25e')
fillProposal = fillYear(fillProposal, 'standard for quick frozen broccoli', '11%252Fal78_25e')
fillProposal = fillYear(fillProposal, 'standard for quick frozen cauliflower', '02%252Fal66_25e')
fillProposal = fillYear(fillProposal, 'standard for quick frozen peas', '02%252Fal66_25e')
fillProposal = fillYear(fillProposal, 'standard for quick frozen rasberries', '02%252Fal66_25e')
fillProposal = fillYear(fillProposal, 'standard for quick frozen spinach', '02%252Fal66_25e')
fillProposal = fillYear(fillProposal, 'standard for raisins', '713-10%252Fal74_20e')
fillProposal = fillYear(fillProposal, 'standard for salted atlantic herring and salted sprat', 'Fal01_19e')
fillProposal = fillYear(fillProposal, 'standard for smoked fish smoke flavoured fish and smoke dried fish', 'Fal33_13e')
fillProposal = fillYear(fillProposal, 'standard for special dietary foods with low sodium content including salt substitutes', 'Fal70_26e')
fillProposal = fillYear(fillProposal, 'standard for tannia', 'Fal01_35e')
fillProposal = fillYear(fillProposal, 'standard for tree tomatoes', '') # CCFFV14
fillProposal = fillYear(fillProposal, 'standard for whey powders', 'Fal95_11e')
fillProposal = fillYear(fillProposal, 'standard for whole and decorticated pearl millet grains', '08%252Fal89_28e')

# fill in step of propsals
# Coding rule: code step that the committee decides to advance/hold/return to

fillProposal = fillStep(fillProposal, 'code of hygienic practice for aseptically processed and packaged low acid foods', 'al93_13Ae', 8)
fillProposal = fillStep(fillProposal,'code of hygienic practice for canned fruit and vegetable products', 'Fal68_13e', 6)
fillProposal = fillStep(fillProposal,'code of hygienic practice for dehydrated fruits and vegetables including edible fungi', '03%252Fal66_13e', 1)
fillProposal = fillStep(fillProposal,'code of hygienic practice for dried fruits', '03%252Fal66_13e', 5)
fillProposal = fillStep(fillProposal,'code of hygienic practice for low moisture foods', '5%252FREP14_FHe', 2)
fillProposal = fillStep(fillProposal,'code of hygienic practice for the processing of frog legs', 'Fal81_39e', 5)
fillProposal = fillStep(fillProposal,'code of hygienic practice for tree nuts', 'Fal72_13e', 7)
fillProposal = fillStep(fillProposal,'code of practice for the prevention and reduction of aflatoxin contamination in dried figs', '01%252Fal30_41e', 2)
fillProposal = fillStep(fillProposal,'code of practice for the prevention and reduction of ochratoxin a contamination in coffee', '02%252Fal31_41e', 2)
fillProposal = fillStep(fillProposal, 'general methods for the detection of irradiated foods', 'FAl0112ae', 5)
fillProposal = fillStep(fillProposal, 'group standard for unripened cheese including fresh cheese', 'Fal01_11e', 8)
fillProposal = fillStep(fillProposal, 'guideline procedures for the visual inspection of lots of canned foods for unacceptable defects', 'Fal93_13e', 5)
fillProposal = fillStep(fillProposal, 'guidelines for food import control systems', 'Fal0130ae', 5)
fillProposal = fillStep(fillProposal, 'guidelines for the control of trichinella spp in meat of suidae', '%252FREP14_CACe', 5)
fillProposal = fillStep(fillProposal, 'guidelines on the application of general principles of food hygiene to the control of foodborne parasites', '5%252FREP14_FHe', 1)
fillProposal = fillStep(fillProposal, 'principles and guidance on the selection of representative commodities for the extrapolation of maximum residue limits for pesticides to commodity groups', '4%252FREP12_PRe', 8)
fillProposal = fillStep(fillProposal, 'principles and guidelines for the conduct of microbiological risk assessment', '4al97_13Ae', 3)
fillProposal = fillStep(fillProposal, 'regional standard for canned foul medames', 'Fal28_41e', 5)
fillProposal = fillStep(fillProposal, 'regional standard for canned humus with tehena', 'Fal28_41e', 5)
fillProposal = fillStep(fillProposal, 'regional standard for date paste near east', '7%252FREP13_NEe', 5)
fillProposal = fillStep(fillProposal, 'standard for aqueous coconut products coconut milk and coconut cream', '21%252Fal03_27e', 8)
fillProposal = fillStep(fillProposal, 'standard for boiled dried salted anchovies', '24%252Fal03_23e', 8)
fillProposal = fillStep(fillProposal, 'standard for canned stone fruits', 'Fal99_27e', 3)
fillProposal = fillStep(fillProposal, 'standard for canned strawberries', 'Fal71_20e', 7)
fillProposal = fillStep(fillProposal, 'standard for certain canned fruits', '%252FREP11_CACe', 1)
fillProposal = fillStep(fillProposal, 'standard for chayotes', '2Fal97_3e', 5)
fillProposal = fillStep(fillProposal, 'standard for cooked cured chopped meat', '10%252Fal78_23e', 8)
fillProposal = fillStep(fillProposal, 'standard for cooked cured ham', '721-07%252Fal74_16e', 6)
fillProposal = fillStep(fillProposal, 'standard for cooked cured pork shoulder', '721-07%252Fal74_16e', 6)
fillProposal = fillStep(fillProposal, 'standard for corned beef', 'Fal70_16e', 5)
fillProposal = fillStep(fillProposal, 'standard for dairy fat spreads', '07%252Fal29_11e', '5/8') #?
fillProposal = fillStep(fillProposal, 'standard for degermed maize corn meal and maize corn grits', 'Fal83_43e', 6)
fillProposal = fillStep(fillProposal, 'standard for durum wheat semolina and durum wheat flour', '06%252Fal89_29e', 5)
fillProposal = fillStep(fillProposal, 'standard for edible fats and oils not covered by individual standards', 'Fal69_67e', 8)
fillProposal = fillStep(fillProposal, 'standard for extra hard grating cheese', '18%252Fcx76_18e', 6)
fillProposal = fillStep(fillProposal, 'standard for grapefruits', 'Fal99_35e', 5)
fillProposal = fillStep(fillProposal, 'standard for jams jellies and marmalades', 'Fal99_35e', '2/3')
fillProposal = fillStep(fillProposal, 'standard for kimchi', 'FAL97_15E', 1)
fillProposal = fillStep(fillProposal, 'standard for live abalone and for raw fresh chilled or frozen abalone for direct consumption or for further processing', '%252FREP13_FFPe', 8)
fillProposal = fillStep(fillProposal, 'standard for longans', 'Fal99_35e', 5)
fillProposal = fillStep(fillProposal, 'standard for luncheon meat', 'Fal70_16e', 4)
fillProposal = fillStep(fillProposal, 'standard for mangosteens', 'Fal97_35e', 8)
fillProposal = fillStep(fillProposal, 'standard for named animal fats', 'Fal95_17e', 5)
fillProposal = fillStep(fillProposal, 'standard for pickled fruits and vegetables', 'Fal28_27e', 6) 
fillProposal = fillStep(fillProposal, 'standard for pomegranate', 'Fal33_35e', 1)
fillProposal = fillStep(fillProposal, 'standard for processed tomato concentrates', 'Fal71_20e', 6)
fillProposal = fillStep(fillProposal, 'standard for pummelos', '2Fal97_3e', 6)
fillProposal = fillStep(fillProposal, 'standard for quick frozen blocks of fish fillets minced fish flesh and mixtures of fillets and minced fish flesh', '8e_Part-I', 7)
fillProposal = fillStep(fillProposal, 'standard for quick frozen blueberries', 'Fal76_25e', 5)
fillProposal = fillStep(fillProposal, 'standard for quick frozen broccoli', '11%252Fal78_25e', 6)
fillProposal = fillStep(fillProposal, 'standard for quick frozen cauliflower', '02%252Fal66_25e', 1)
fillProposal = fillStep(fillProposal, 'standard for quick frozen peas', '02%252Fal66_25e', 1)
fillProposal = fillStep(fillProposal, 'standard for quick frozen rasberries', '02%252Fal66_25e', 1)
fillProposal = fillStep(fillProposal, 'standard for quick frozen spinach', '02%252Fal66_25e', 1)
fillProposal = fillStep(fillProposal, 'standard for raisins', '713-10%252Fal74_20e', 8)
fillProposal = fillStep(fillProposal, 'standard for smoked fish smoke flavoured fish and smoke dried fish', 'Fal33_13e', 5)
fillProposal = fillStep(fillProposal, 'standard for special dietary foods with low sodium content including salt substitutes', 'Fal70_26e', 5)
fillProposal = fillStep(fillProposal, 'standard for tannia', 'Fal01_35e', 8)
fillProposal = fillStep(fillProposal, 'standard for tree tomatoes', '', 1) # CCFFV14
fillProposal = fillStep(fillProposal, 'standard for whey powders', 'Fal95_11e', 8)
fillProposal = fillStep(fillProposal, 'standard for whole and decorticated pearl millet grains', '08%252Fal89_28e', 8)

 
 
# --------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Round 2

findMissingProposalsReit = fillProposal[-which(fillProposal$step %in% c('1')), - c(1:3)]

findMissingProposalsReit  = do.call(rbind, lapply(split(findMissingProposalsReit, findMissingProposalsReit$Reference), function(x){
  if(all(is.na(x$year))){
    x = x[1,]
  }
  else{
    x = x[-which(is.na(x$year)),]
  }
}))

 
row.names(findMissingProposalsReit)  = NULL

findMissingProposalsReit$keywords = findMissingProposalsReit$Title
findMissingProposalsReit$keywords = gsub('guidelines on the|guidelines for|guidelines for the|code of practice for the|general guidelines|principles and guidelines for the|harmonized guidelines for|principles for the use of|standard for| groupstandard for|generalmethods of|regional standard for|code of hygienic practice for |list of codex specifications for|guidelines on the applications of general principles of food hygiene to the|guidelines for the control of|group standard',
                                         '',findMissingProposalsReit$keywords )
findMissingProposalsReit$keywords = str_trim(findMissingProposalsReit$keywords)
findMissingProposalsReit$keywords[1] = findMissingProposalsReit$Title[1]

findMissingProposalsReit$standards_extracted = NA

 
# get names of standards that have proposal data in the same year as the standard adopted data
load(paste0(pathMain, '/participation_development/codexWide.rda'))


findMissingProposalsSameYear= codexWide[which(codexWide$Reference !=0 & codexWide$startYear ==  codexWide$endYear), c('Reference', 'Title', 'standards_extracted')]
findMissingProposalsSameYear = apply(findMissingProposalsSameYear, 2, function(x) gsub('\\-', ' ', x))
findMissingProposalsSameYear = apply(findMissingProposalsSameYear, 2, function(x) gsub('[[:punct:]]', ' ', x))
findMissingProposalsSameYear = apply(findMissingProposalsSameYear, 2, function(x) gsub('st andard', 'standard', x))
findMissingProposalsSameYear = apply(findMissingProposalsSameYear, 2, function(x) tolower(x))
findMissingProposalsSameYear = apply(findMissingProposalsSameYear, 2, function(x)  str_trim(x))
findMissingProposalsSameYear = apply(findMissingProposalsSameYear, 2, function(x) gsub("  |  |   ", ' ', x)) %>% data.frame(., stringsAsFactors = FALSE)


findMissingProposalsSameYear$keywords = findMissingProposalsSameYear$Title 
findMissingProposalsSameYear$keywords = gsub('guidance for|code of practice on|code of practice for|regional guidelines for|guide for the|guideline for the|guideline on|code of practice to|risk analysis of |assessment of the |performance criteria and validation of methods for|a blend of|general standard for|guidelines on|maximum residue limits mrls and risk management recommendations rmrs for|working principles for|guidelines on the|guidelines for|guidelines for the|code of practice for the|general guidelines|principles and guidelines for the|harmonized guidelines for|principles for the use of|standard for| groupstandard for|generalmethods of|regional standard for|code of hygienic practice for |list of codex specifications for|guidelines on the applications of general principles of food hygiene to the|guidelines for the control of|group standard',
                                             '',findMissingProposalsSameYear$keywords )
findMissingProposalsSameYear$keywords = str_trim(findMissingProposalsSameYear$keywords)
 

findMissingProposalsSameYear$keywords[which(findMissingProposalsSameYear$keywords == findMissingProposalsSameYear$Title)] = NA
findMissingProposalsSameYear$year = NA
findMissingProposalsSameYear$step = NA

findMissingProposals2 = rbind(findMissingProposalsReit[, names(findMissingProposalsSameYear)], findMissingProposalsSameYear)


find_proposalList2 = list()
for ( i in 1:dim(findMissingProposals2)[1]){
  print(i)
  if (i <77){
    find_proposalList2[[i]] <-docsAll_tidy[str_detect(docsAll_tidy$text, unlist(findMissingProposals2[i,c(1, 2, 4)])), ] }
  if (i >= 77){
  find_proposalList2[[i]] <-docsAll_tidy[str_detect(docsAll_tidy$text, unlist(findMissingProposals2[i,1:4])), ] }
  if(dim(find_proposalList2[[i]])[1] > 0){
    find_proposalList2[[i]]$Title = findMissingProposals2[i, 'Title']
    find_proposalList2[[i]]$Reference = findMissingProposals2[i, 'Reference']
    find_proposalList2[[i]]$keywords = findMissingProposals2[i, 'keywords']
    find_proposalList2[[i]]$standards_extracted = findMissingProposals2[i, 'standards_extracted']
    find_proposalList2[[i]]$year = findMissingProposals2[i, 'year']
    find_proposalList2[[i]]$step = findMissingProposals2[i, 'step']
  }
}

# save(find_proposalList2, file = paste0(pathData, '/participation_development/findMissingProposalsList2.rda'))
load( file = paste0(pathMain, '/participation_development/findMissingProposalsList2.rda'))

# remove matches for which the meeting year is later than when the standard was adopted
find_proposalList2= lapply(find_proposalList2, function(x){
  stringSize = x$Reference %>% unique() %>% nchar()
  x$year = ifelse(is.na(x$year), str_sub(x$Reference, stringSize -3, stringSize), x$year)
  x = x[which(x$year>x$Folder),]
  return(x)
})

# remove empty items from list as a result of above subsetting
find_proposalList2 =find_proposalList2[sapply(find_proposalList2, nrow)>0]
 
# select maximum 10 standards for each 
tenMatches = lapply(find_proposalList2, function(x){
        x = x[order(x$Folder),]
        if (dim(x)[1] > 10 ){
          x = x[1:10,]
        }
       return(x)
})


 
# highlight the text for easier identification in the html file
highLightedText2 = lapply(tenMatches, function(x){
  xTitle <- paste0("<font color='blue'>",x$Title %>% unique(),"</font>")
  xReference <- paste0("<font color='blue'>",x$Reference %>% unique(),"</font>")
  xstandards_extracted <- paste0("<font color='blue'>",x$standards_extracted %>% unique(),"</font>")
  xKeywords <- paste0("<font color='blue'>",x$standards_keywords %>% unique(),"</font>")
  
  textHighlight = str_replace (x$text, coll(x$Title %>% unique()), xTitle)
  textHighlight = str_replace (textHighlight, coll(x$Reference %>% unique()), xReference)
  textHighlight = str_replace (textHighlight, coll(x$Title %>% unique()), xstandards_extracted)
  textHighlight = str_replace (textHighlight, coll(x$Reference %>% unique()), xKeywords)
}
)


 
setwd('/Users/cindycheng/Documents/Papers/Codex/RCode')


# Create the Rmd to knit
cat(
  '---
title: "Look for Missing Proposals v2"
author: "Cindy Cheng"
date: "2019-02-21"
output: html_document
---
  
  
## Proposals {.tabset} \n

  ',
  make.tabs(tenMatches, highLightedText2),
  
  sep = "",
  file = "1.5_findMissingProposalData2.Rmd")


# Render the Rmd created into html here
rmarkdown::render("1.5_findMissingProposalData2.Rmd")


# -----------------------------------------------------

# -----------------------------------------------------
# manually fill in 

fillProposal2 = do.call(rbind, lapply(find_proposalList2, function(x){
  x = dplyr:::select(x, id, Meeting, Folder, Title, Reference, year, step)
  x$year = fillProposal$year[match(paste0(x$id, x$Title), paste0(fillProposal$id, fillProposal$Title))]
  x$step = fillProposal$step[match(paste0(x$id, x$Title), paste0(fillProposal$id, fillProposal$Title))]
  return(x)
})) %>% data.frame()

#save(fillProposal2, file = paste0(pathData, '/participation_development/fillProposal2.rda'))
load(file = paste0(pathMain, '/participation_development/fillProposal2.rda'))

 
fillProposal2 = fillYear(fillProposal2, 'general guidelines on claims', '12%252Fal78_22e')
fillProposal2 = fillYear(fillProposal2, 'principles and guidelines for the conduct of microbiological risk assessment', 'al97_13Ae')
fillProposal2 = fillYear(fillProposal2, 'guidelines for food import control systems', 'al97_30Ae')
fillProposal2 = fillYear(fillProposal2, 'guidelines on the application of general principles of food hygiene to the control of viruses in food', 'Fal33_13e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for the control of nontyphoidal salmonella spp in beef and pork meat', '5%252FREP14_FHe')
fillProposal2 = fillYear(fillProposal2, 'guidelines on the application of general principles of food hygiene to the control of foodborne parasites', '5%252FREP14_FHe')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for desiccated coconut', '02%252Fal65_13e')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for aseptically processed and packaged low acid foods', 'Fal91_13e')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for dehydrated fruits and vegetables including edible fungi', '02%252Fal65_13e')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for tree nuts', '02%252Fal65_13e')
fillProposal2 = fillYear(fillProposal2, 'code of practice for the prevention and reduction of ochratoxin a contamination in coffee', 'Fal04_12e')
fillProposal2 = fillYear(fillProposal2, 'code of practice for the prevention and reduction of ochratoxin a contamination in cocoa', '6%252FREP12_CFe')
fillProposal2 = fillYear(fillProposal2, 'code of practice for weed control to prevent and reduce pyrrolizidine alkaloid contamination in food and feed', '6%252FREP12_CFe')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for low moisture foods', '3%252FREP12_FHe')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for low moisture foods', '3%252FREP12_FHe')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen broccoli', 'Fal72_25e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen cauliflower', 'Fal72_25e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen blocks of fish fillets minced fish flesh and mixtures of fillets and minced fish flesh', 'Fal83_18e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen blocks of fish fillets minced fish flesh and mixtures of fillets and minced fish flesh', 'Fal83_18e')
fillProposal2 = fillYear(fillProposal2, 'group standard for unripened cheese including fresh cheese', 'Fal97_11e') # renamed from standard for unripened cheese
fillProposal2 = fillYear(fillProposal2, 'standard for kimchi', '2Fal97_3e')  
fillProposal2 = fillYear(fillProposal2, 'standard for grapefruits', '2Fal97_3e')  
fillProposal2 = fillYear(fillProposal2, 'standard for salted atlantic herring and salted sprat', '2Fal97_3e')  
fillProposal2 = fillYear(fillProposal2, 'standard for tree tomatoes', 'Fal28_35e')  
fillProposal2 = fillYear(fillProposal2, 'standard for pomegranate', 'Fal33_35e')
fillProposal2 = fillYear(fillProposal2, 'standard for live abalone and for raw fresh chilled or frozen abalone for direct consumption or for further processing', '%252FREP11_FFPe')
fillProposal2 = fillYear(fillProposal2, 'standard for okra', '6%252FREP11_FFe') # possibly step 1 at the meeting right after this one
fillProposal2 = fillYear(fillProposal2, 'standard for certain canned fruits', '%252FREP11_CACe')  
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen peas', 'Fal70_43e')  
fillProposal2 = fillYear(fillProposal2, 'standard for canned pineapple', 'Fal68_35e')  
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen strawberries', 'Fal71_25e')  
fillProposal2 = fillYear(fillProposal2, 'standard for canned strawberries', '02%252Fal65_20e') 
fillProposal2 = fillYear(fillProposal2, 'standard for raisins', '02%252Fal64_30e') 
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen raspberries', 'Fal70_43e') 
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen spinach', 'Fal70_43e') 
fillProposal2 = fillYear(fillProposal2, 'standard for canned fruit cocktail', 'Fal69_67e') 
fillProposal2 = fillYear(fillProposal2, 'standard for luncheon meat', 'Fal68_16e')
fillProposal2 = fillYear(fillProposal2, 'standard for cooked cured ham', 'Fal72_16e')
fillProposal2 = fillYear(fillProposal2, 'standard for cooked cured pork shoulder', 'Fal72_16e')
fillProposal2 = fillYear(fillProposal2, 'standard for cooked cured chopped meat', 'Fal72_16e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for the preservation of raw milk by use of the lactoperoxidase system', '18%252Fal89_40e')
fillProposal2 = fillYear(fillProposal2, 'standard for pearl millet flour', 'Fal85_47e')
fillProposal2 = fillYear(fillProposal2, 'standard for sorghum grains', '04%252Fal79_28e') #check
fillProposal2 = fillYear(fillProposal2, 'group standard for cheeses in brine', 'Fcx86_21e') 
fillProposal2 = fillYear(fillProposal2, 'standard for canned shrimps or prawns', 'Fal70_43e') 
fillProposal2 = fillYear(fillProposal2, 'general standard for the labelling of prepackaged foods', '2Fal69_6e') 
fillProposal2 = fillYear(fillProposal2, 'standard for fermented milks', 'Fal97_11e') 
fillProposal2 = fillYear(fillProposal2, 'classification of foods and animal feeds', 'al85_24Be')
fillProposal2 = fillYear(fillProposal2, 'standard for certain canned vegetables', '30%252Fal30REPe')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for groundnuts peanuts', '4%252Fal78_13Ae')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for the transport of food in bulk and semi packed food', 'FAl99_37e')
fillProposal2 = fillYear(fillProposal2, 'code of practice for the prevention and reduction of ochratoxin a contamination in wine', '38%252Fal29_12e')
fillProposal2 = fillYear(fillProposal2, 'code of practice for the reduction of acrylamide in foods', '31%252Fal31REPe')
fillProposal2 = fillYear(fillProposal2, 'code of practice on good animal feeding', 'al97_31Ae') # note that Fal99_13e says it was at step 1 because of the meeting in al97_31Ae
fillProposal2 = fillYear(fillProposal2, 'code of practice to minimize and contain antimicrobial resistance', '50%252Fal0303ae')  
fillProposal2 = fillYear(fillProposal2, 'standard for avocado', 'Fal91_35e')  
fillProposal2 = fillYear(fillProposal2, 'standard for cape gooseberry', 'FAl99_37e')  
fillProposal2 = fillYear(fillProposal2, 'standard for litchi', 'Fal91_35e')  
fillProposal2 = fillYear(fillProposal2, 'standard for rambutan', '10%252Fal03_35e')  
fillProposal2 = fillYear(fillProposal2, 'standard for table grapes', 'Fal9935ae')  
fillProposal2 = fillYear(fillProposal2, 'standard for carambola', 'Fal91_35e')  
fillProposal2 = fillYear(fillProposal2, 'standard for nopal', 'Fal91_35e')  
fillProposal2 = fillYear(fillProposal2, 'standard for prickly pear', 'Fal91_35e')  
fillProposal2 = fillYear(fillProposal2, 'general standard for the labelling of and claims for prepackaged foods for special dietary uses', '13%252Fal79_38e')
fillProposal2 = fillYear(fillProposal2, 'general standard for the labelling of food additives when sold as such', 'Fal72_12e')
fillProposal2 = fillYear(fillProposal2, 'general standard for vegetable protein products vpp', 'Fal81_30e')
fillProposal2 = fillYear(fillProposal2, 'guide for the microbiological quality of spices and herbs used in processed meat and poultry productsp', '18%252Fal89_40e')
fillProposal2 = fillYear(fillProposal2, 'guideline for the conduct of food safety assessment of foods derived from recombinant dna animals', '05%252Fal29_34e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for the control of campylobacter and salmonella in chicken meat', '38%252Fal30_13e')
fillProposal2 = fillYear(fillProposal2, 'code of practice for the prevention and reduction of lead contamination in foods', '35%252Fal0312Ae')
fillProposal2 = fillYear(fillProposal2, 'guideline on analytical terminology', '27%252Fal29_23e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for risk analysis of foodborne antimicrobial resistance', '02%252Fal32_42e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for the design and implementation of national regulatory food safety assurance programmes associated with the use of veterinary drugs in food producing animals', '29%252Fal29_41e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for the sensory evaluation of fish and shellfish in laboratories', 'Fal97_37e')
fillProposal2 = fillYear(fillProposal2, 'guidelines on the application of risk assessment for feed', '%252FREP12_CACe')
fillProposal2 = fillYear(fillProposal2, 'guidelines on measurement uncertainty', 'Fal99_23e')
fillProposal2 = fillYear(fillProposal2, 'model export certificate for milk and milk products', '29%252Fal29_41e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for the design operation assessment and accreditation of food import and export inspection and certification systems', 'al95_30Ae')
fillProposal2 = fillYear(fillProposal2, 'principles for the risk analysis of foods derived from modern biotechnology', 'Fal01_41e')
fillProposal2 = fillYear(fillProposal2, 'standard for dried edible fungi', '2Fal69_6e')
fillProposal2 = fillYear(fillProposal2, 'regional standard for lucuma', '16%252Fal32_36e')
fillProposal2 = fillYear(fillProposal2, 'standard for camembert', '-09%252Fcx66_9e')
fillProposal2 = fillYear(fillProposal2, 'standard for cocoa butter', '03%252Fal65_30e')
fillProposal2 = fillYear(fillProposal2, 'standard for cottage cheese', '-09%252Fcx66_9e')
fillProposal2 = fillYear(fillProposal2, 'standard for coulommiers', 'Fcx67_10e')
fillProposal2 = fillYear(fillProposal2, 'standard for brie', 'Fcx67_10e')
fillProposal2 = fillYear(fillProposal2, 'standard for emmental', '-09%252Fcx66_9e')
fillProposal2 = fillYear(fillProposal2, 'standard for provolone', '-09%252Fcx66_9e')
fillProposal2 = fillYear(fillProposal2, 'standard for saint paulin', '-09%252Fcx66_9e')
fillProposal2 = fillYear(fillProposal2, 'standard for whey cheeses', '-09%252Fcx66_9e')
fillProposal2 = fillYear(fillProposal2, 'standard for cream cheese', 'Fal72_12e')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for collecting processing and marketing of natural mineral waters', 'Fal83_13e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for packing media for canned fruits', 'Fal99_03e')
fillProposal2 = fillYear(fillProposal2, 'standard for a blend of evaporated skimmed milk and vegetable fat', 'Fal04_41e')
fillProposal2 = fillYear(fillProposal2, 'standard for a blend of skimmed milk and vegetable fat in powdered form', 'Fal04_41e')
fillProposal2 = fillYear(fillProposal2, 'standard for a blend of sweetened condensed skimmed milk and vegetable fat', 'Fal04_41e')
fillProposal2 = fillYear(fillProposal2, 'standard for canned baby foods', 'Fal70_26e')
fillProposal2 = fillYear(fillProposal2, 'standard for canned bamboo shoots', 'Fal95_15e')
fillProposal2 = fillYear(fillProposal2, 'standard for canned chestnuts and canned chestnut purée', 'Fal83_43e')
fillProposal2 = fillYear(fillProposal2, 'standard for canned tropical fruit salad', 'Fal72_20e')
fillProposal2 = fillYear(fillProposal2, 'standard for certain pulses', 'Fal85_29e')
fillProposal2 = fillYear(fillProposal2, 'standard for chilli peppers', '61%252Fal3103Ae')
fillProposal2 = fillYear(fillProposal2, 'standard for durian', '13%252Fal30_35e')
fillProposal2 = fillYear(fillProposal2, 'standard for fish sauce', '30%252Fal30REPe')
fillProposal2 = fillYear(fillProposal2, 'standard for food grade salt', 'Fal72_12e')
fillProposal2 = fillYear(fillProposal2, 'standard for formula foods for use in very low energy diets for weight reduction', 'Fal93_26e')
fillProposal2 = fillYear(fillProposal2, 'standard for instant noodles', 'Fal01_03e')
fillProposal2 = fillYear(fillProposal2, 'general standard for irradiated foods', '12%252Fal79_12e')
fillProposal2 = fillYear(fillProposal2, 'standard for mozzarella', 'Fal99_11e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen brussels sprouts', 'Fal71_25e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen french fried potatoes', '4%252Fal79_22Ae')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen green and wax beans', 'al72_20Ae')
fillProposal2 = fillYear(fillProposal2, 'standard for sturgeon caviar', '26%252Fal03_41e')
fillProposal2 = fillYear(fillProposal2, 'regional standard for tehena', 'FAL28_40e')
fillProposal2 = fillYear(fillProposal2, 'standard for dates', 'Fal72_20e')
fillProposal2 = fillYear(fillProposal2, 'standard for dried apricots', 'Fal72_20e')
fillProposal2 = fillYear(fillProposal2, 'standard for mango chutney', '14%252Fal79_20e')
fillProposal2 = fillYear(fillProposal2, 'standard for pickled cucumbers cucumber pickles', 'Fal71_20e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen shrimps or prawns', 'Fal71_18e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen bilberries', 'Fal72_25e')
fillProposal2 = fillYear(fillProposal2, 'standard for quick frozen peaches', 'Fal72_12e')
fillProposal2 = fillYear(fillProposal2, 'standard for soy protein products', 'Fal81_30e')
fillProposal2 = fillYear(fillProposal2, 'standard for unshelled pistachio nuts', '713-11%252Fal76_20e')
fillProposal2 = fillYear(fillProposal2, 'guidelines for the production processing labelling and marketing of organically produced foods', 'Fal93_22e')
fillProposal2 = fillYear(fillProposal2, 'standard for pitahayas', 'FAl99_37e') #standard for pitahayas used to be Proposed Draft Codex Standard for Yellow Pitahayas (FAl99_37e), check : they were, look at step 7 in al03_35e.pdf
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for dried fruits', '02%252Fal65_13e') # draft provisional hygiene standards for dried fruits turns into code for hygiene standards for dried fruit by 03%252Fal66_13e
fillProposal2 = fillYear(fillProposal2, 'standard for jams jellies and marmalades', '03%252Fal65_30e')
fillProposal2 = fillYear(fillProposal2, 'standard for apples', '02%252Fal64_30e')
fillProposal2 = fillYear(fillProposal2, 'standard for named vegetable oils', 'Fal93_40e')
fillProposal2 = fillYear(fillProposal2, 'code of hygienic practice for low and acidified low acid canned foods', 'al76_13Ae')
fillProposal2 = fillYear(fillProposal2, 'standard for edible casein products', 'al95_11e')
fillProposal2 = fillYear(fillProposal2, 'standard for follow up formula', 'Fal83_26e') # formerly known as draft standard for follow-up foods for older foods (see Fal87_26e)
fillProposal2 = fillYear(fillProposal2, 'standard for sorghum flour', 'Fal85_29e') # formerly known as draft standard for follow-up foods for older foods (see Fal87_26e)
fillProposal2 = fillYear(fillProposal2, 'guidelines on the application of general principles of food hygiene to the control of viruses in food', '40%252Fal32_13e') # according to Fal33_13e 'renamed the title of the proposed draft code of hygienic practice for control of viruses in food n072009 to the guidelines on the application of general principles of food hygiene to the control of viruses in food '



# Steps
fillProposal2 = fillStep(fillProposal2, 'principles and guidelines for the conduct of microbiological risk assessment', 'al97_13Ae', 3)
fillProposal2 = fillStep(fillProposal2, 'guidelines on the application of general principles of food hygiene to the control of viruses in food', 'Fal33_13e', 2) # renamed from Proposed Draft Code of Hygienic Practice for Control of Viruses in Food 
fillProposal2 = fillStep(fillProposal2, 'guidelines for the control of nontyphoidal salmonella spp in beef and pork meat', '5%252FREP14_FHe', 1)
fillProposal2 = fillStep(fillProposal2, 'guidelines on the application of general principles of food hygiene to the control of foodborne parasites', '5%252FREP14_FHe', 1)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for desiccated coconut', '02%252Fal64_30e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for aseptically processed and packaged low acid foods', 'Fal91_13e', 5)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for dehydrated fruits and vegetables including edible fungi', '02%252Fal65_13e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for tree nuts', '02%252Fal65_13e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of practice for the prevention and reduction of ochratoxin a contamination in coffee', 'Fal04_12e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of practice for the prevention and reduction of ochratoxin a contamination in cocoa', '6%252FREP12_CFe', 1)
fillProposal2 = fillStep(fillProposal2, 'code of practice for weed control to prevent and reduce pyrrolizidine alkaloid contamination in food and feed', '6%252FREP12_CFe', 1)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for low moisture foods', '3%252FREP12_FHe', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen broccoli', 'Fal72_25e', 2)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen cauliflower', 'Fal72_25e', 2)
fillProposal2 = fillStep(fillProposal2, 'group standard for unripened cheese including fresh cheese', 'Fal97_11e', 6) # renamed from standard for unripened cheese
fillProposal2 = fillStep(fillProposal2, 'standard for kimchi', '2Fal97_3e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for grapefruits', '2Fal97_3e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for salted atlantic herring and salted sprat', '2Fal97_3e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for tree tomatoes', 'Fal28_35e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for pomegranate', 'Fal33_35e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for live abalone and for raw fresh chilled or frozen abalone for direct consumption or for further processing', '%252FREP11_FFPe', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for okra', '6%252FREP11_FFe', 1) # possibly step 1 at the meeting right after this one
fillProposal2 = fillStep(fillProposal2, 'standard for certain canned fruits', '%252FREP11_CACe', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen vegetables', '5%252FREP11_PFe', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen peas', 'Fal70_43e', 6)  # might mean something diff for earlier years
fillProposal2 = fillStep(fillProposal2, 'standard for canned pineapple', 'Fal68_35e', 6)  # might mean something diff for earlier years
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen strawberries', 'Fal71_25e', 8) # might mean something diff for earlier years
fillProposal2 = fillStep(fillProposal2, 'standard for canned strawberries', '02%252Fal65_20e', 3)  
fillProposal2 = fillStep(fillProposal2, 'standard for raisins', '02%252Fal64_30e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen raspberries', 'Fal70_43e', 6) 
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen spinach', 'Fal70_43e', 6) 
fillProposal2 = fillStep(fillProposal2, 'standard for canned fruit cocktail', 'Fal69_67e', 6) 
fillProposal2 = fillStep(fillProposal2, 'standard for luncheon meat', 'Fal68_16e', 3) 
fillProposal2 = fillStep(fillProposal2, 'standard for cooked cured ham', 'Fal72_16e', 6)
fillProposal2 = fillStep(fillProposal2, 'standard for cooked cured pork shoulder', 'Fal72_16e', 6)
fillProposal2 = fillStep(fillProposal2, 'standard for cooked cured chopped meat', 'Fal72_16e', 6)
fillProposal2 = fillStep(fillProposal2, 'guidelines for the preservation of raw milk by use of the lactoperoxidase system', '18%252Fal89_40e', 3)
fillProposal2 = fillStep(fillProposal2, 'standard for pearl millet flour', 'Fal85_47e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for sorghum grains', '04%252Fal79_28e', 1)
fillProposal2 = fillStep(fillProposal2, 'group standard for cheeses in brine', 'Fcx86_21e', 1) 
fillProposal2 = fillStep(fillProposal2, 'standard for canned shrimps or prawns', 'Fal70_43e', 9) 
fillProposal2 = fillStep(fillProposal2, 'standard for fermented milks', 'Fal97_11e', 3) 
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for groundnuts peanuts', '4%252Fal78_13Ae', 4)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for the transport of food in bulk and semi packed food', 'FAl99_37e', 6)
fillProposal2 = fillStep(fillProposal2, 'code of practice for the prevention and reduction of ochratoxin a contamination in wine', '38%252Fal29_12e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of practice for the reduction of acrylamide in foods', '31%252Fal31REPe', 5)
fillProposal2 = fillStep(fillProposal2, 'code of practice on good animal feeding', 'al97_31Ae', 1) # note that Fal99_13e says it was at step 1 because of the meeting in al97_31Ae
fillProposal2 = fillStep(fillProposal2, 'code of practice to minimize and contain antimicrobial resistance', '50%252Fal0303ae', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for avocado', 'Fal91_35e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for cape gooseberry', 'FAl99_37e', 6)  
fillProposal2 = fillStep(fillProposal2, 'standard for litchi', 'Fal91_35e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for rambutan', '10%252Fal03_35e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for table grapes', 'Fal9935ae', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for carambola', 'Fal91_35e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for nopal', 'Fal91_35e', 1)  
fillProposal2 = fillStep(fillProposal2, 'standard for prickly pear', 'Fal91_35e', 1)  
fillProposal2 = fillStep(fillProposal2, 'general standard for the labelling of and claims for prepackaged foods for special dietary uses', '13%252Fal79_38e', 5)
fillProposal2 = fillStep(fillProposal2, 'general standard for vegetable protein products vpp', 'Fal81_30e', 3)
fillProposal2 = fillStep(fillProposal2, 'guide for the microbiological quality of spices and herbs used in processed meat and poultry productsp', '18%252Fal89_40e', 6)
fillProposal2 = fillStep(fillProposal2, 'guideline for the conduct of food safety assessment of foods derived from recombinant dna animals', '05%252Fal29_34e', 1)
fillProposal2 = fillStep(fillProposal2, 'guidelines for the control of campylobacter and salmonella in chicken meat', '38%252Fal30_13e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of practice for the prevention and reduction of lead contamination in foods', '35%252Fal0312Ae', 5)
fillProposal2 = fillStep(fillProposal2, 'guideline on analytical terminology', '27%252Fal29_23e', 1)
fillProposal2 = fillStep(fillProposal2, 'guidelines for risk analysis of foodborne antimicrobial resistance', '02%252Fal32_42e', 1)
fillProposal2 = fillStep(fillProposal2, 'guidelines for the design and implementation of national regulatory food safety assurance programmes associated with the use of veterinary drugs in food producing animals', '29%252Fal29_41e', 6)
fillProposal2 = fillStep(fillProposal2, 'guidelines for the sensory evaluation of fish and shellfish in laboratories', 'Fal97_37e', 6)
fillProposal2 = fillStep(fillProposal2, 'guidelines on the application of risk assessment for feed', '%252FREP12_CACe', 5)
fillProposal2 = fillStep(fillProposal2, 'guidelines on measurement uncertainty', 'Fal99_23e', 1)
fillProposal2 = fillStep(fillProposal2, 'model export certificate for milk and milk products', '29%252Fal29_41e', 6)
fillProposal2 = fillStep(fillProposal2, 'guidelines for the design operation assessment and accreditation of food import and export inspection and certification systems', 'al95_30Ae', 4)
fillProposal2 = fillStep(fillProposal2, 'principles for the risk analysis of foods derived from modern biotechnology', 'Fal01_41e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for dried edible fungi', '2Fal69_6e', 5)
fillProposal2 = fillStep(fillProposal2, 'regional standard for lucuma', '16%252Fal32_36e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for camembert', '-09%252Fcx66_9e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for cocoa butter', '03%252Fal65_30e', 2)
fillProposal2 = fillStep(fillProposal2, 'standard for cottage cheese', '-09%252Fcx66_9e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for coulommiers', 'Fcx67_10e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for brie', 'Fcx67_10e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for provolone', '-09%252Fcx66_9e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for saint paulin', '-09%252Fcx66_9e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for collecting processing and marketing of natural mineral waters', 'Fal83_13e', 5)
fillProposal2 = fillStep(fillProposal2, 'guidelines for packing media for canned fruits', 'Fal99_03e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for a blend of evaporated skimmed milk and vegetable fat', 'Fal04_41e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for a blend of skimmed milk and vegetable fat in powdered form', 'Fal04_41e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for a blend of sweetened condensed skimmed milk and vegetable fat', 'Fal04_41e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for canned baby foods', 'Fal70_26e', 3)
fillProposal2 = fillStep(fillProposal2, 'standard for canned bamboo shoots', 'Fal95_15e', 3)
fillProposal2 = fillStep(fillProposal2, 'standard for canned chestnuts and canned chestnut purée', 'Fal83_43e', 6)
fillProposal2 = fillStep(fillProposal2, 'standard for canned tropical fruit salad', 'Fal72_20e', 4)
fillProposal2 = fillStep(fillProposal2, 'standard for certain pulses', 'Fal85_29e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for chilli peppers', '61%252Fal3103Ae', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for durian', '13%252Fal30_35e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for fish sauce', '30%252Fal30REPe', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for food grade salt', 'Fal72_12e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for formula foods for use in very low energy diets for weight reduction', 'Fal93_26e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for instant noodles', 'Fal01_03e', 1)
fillProposal2 = fillStep(fillProposal2, 'general standard for irradiated foods', '12%252Fal79_12e', 8)
fillProposal2 = fillStep(fillProposal2, 'standard for mozzarella', 'Fal99_11e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen brussels sprouts', 'Fal71_25e', 3)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen french fried potatoes', '4%252Fal79_22Ae', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen green and wax beans', 'al72_20Ae', 2)
fillProposal2 = fillStep(fillProposal2, 'standard for sturgeon caviar', '26%252Fal03_41e', 1)
fillProposal2 = fillStep(fillProposal2, 'regional standard for tehena', 'FAL28_40e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for mango chutney', '14%252Fal79_20e', 3)
fillProposal2 = fillStep(fillProposal2, 'standard for pickled cucumbers cucumber pickles', 'Fal71_20e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen shrimps or prawns', 'Fal71_18e', 2)
fillProposal2 = fillStep(fillProposal2, 'standard for quick frozen bilberries', 'Fal72_25e', 7)
fillProposal2 = fillStep(fillProposal2, 'standard for unshelled pistachio nuts', '713-11%252Fal76_20e', 1)
fillProposal2 = fillStep(fillProposal2, 'guidelines for the production processing labelling and marketing of organically produced foods', 'Fal93_22e', 5)
fillProposal2 = fillStep(fillProposal2, 'standard for pitahayas', 'FAl99_37e', 5) #standard for pitahayas used to be Proposed Draft Codex Standard for Yellow Pitahayas (FAl99_37e), check : they were, look at step 7 in al03_35e.pdf
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for dried fruits', '02%252Fal65_13e', 3) # draft provisional hygiene standards for dried fruits turns into code for hygiene standards for dried fruit by 03%252Fal66_13e
fillProposal2 = fillStep(fillProposal2, 'standard for jams jellies and marmalades', '03%252Fal65_30e', 1)
fillProposal2 = fillStep(fillProposal2, 'standard for apples', '02%252Fal64_30e', 1)
fillProposal2 = fillStep(fillProposal2, 'code of hygienic practice for low and acidified low acid canned foods', 'al76_13Ae', 3)
fillProposal2 = fillStep(fillProposal2, 'standard for edible casein products', 'al95_11e', 8)
fillProposal2 = fillStep(fillProposal2, 'standard for follow up formula', 'Fal83_26e', 3) # formerly known as draft standard for follow-up foods for older foods (see Fal87_26e)
fillProposal2 = fillStep(fillProposal2, 'standard for sorghum flour', 'Fal85_29e', 1) # formerly known as draft standard for follow-up foods for older foods (see Fal87_26e)
fillProposal2 = fillStep(fillProposal2, 'guidelines on the application of general principles of food hygiene to the control of viruses in food', '40%252Fal32_13e', 1) # according to Fal33_13e 'renamed the title of the proposed draft code of hygienic practice for control of viruses in food n072009 to the guidelines on the application of general principles of food hygiene to the control of viruses in food '

 
# ------------------------------------------------------------

# ------------------------------------------------------------
# Round 3
library(stringr)
missingTexts = read.csv(paste0(pathMain, '/participation_development/standardsMissingProposalsApril2019_SK.csv'), stringsAsFactors = FALSE, na.strings = '.')
missingTexts = missingTexts[which(is.na(missingTexts$cac)),][1:35,]


cleanProposalData = function(proposalData) {
  proposalData = apply(proposalData, 2, function(x)  gsub('\\-', ' ', x)) 
  proposalData = apply(proposalData, 2, function(x)  gsub('[[:punct:]]', ' ', x)) 
  proposalData = apply(proposalData, 2, function(x)  tolower(x)) 
  proposalData = apply(proposalData, 2, function(x)  str_trim(x))  
  proposalData = apply(proposalData, 2, function(x)  gsub("  |  |   ", ' ', x)) %>% data.frame(., stringsAsFactors = FALSE)
  return(proposalData) 
}


missingTexts = cleanProposalData(missingTexts)


missingTexts$keywords = missingTexts$text 
missingTexts$keywords = gsub('glossary of|advisory lists of |code of ethics for|protocol for the|principles for|principles and guidance on the|guideline procedures for the|general methods of|general methods for the|code of practice|guidance for|code of practice on|code of practice for|regional guidelines for|guide for the|guideline for the|guideline on|code of practice to|risk analysis of |assessment of the |performance criteria and validation of methods for|a blend of|general standard for|guidelines on|maximum residue limits mrls and risk management recommendations rmrs for|working principles for|guidelines on the|guidelines for|guidelines for the|code of practice for the|general guidelines|principles and guidelines for the|harmonized guidelines for|principles for the use of|standard for| groupstandard for|generalmethods of|regional standard for|code of hygienic practice for |list of codex specifications for|guidelines on the applications of general principles of food hygiene to the|guidelines for the control of|group standard',
                             '',missingTexts$keywords )

missingTexts$keywords = gsub('^for|general  |concerning|regional|principles and  monitoring the|application of general principles of|^for|for use of the term|intented for infants and young children|for food additives|the processing of |prevention and reduction of|reduction of contamination of food with|reduction of|compilation of codex texts relevant to the',
                             '',missingTexts$keywords )
missingTexts$keywords = gsub('$s', '', missingTexts$keywords )
missingTexts$keywords = str_trim(missingTexts$keywords)

find_proposalList3 = list()
for ( i in 1:dim(missingTexts)[1]){
  print(i)
  find_proposalList3[[i]] <-docsAll_tidy[str_detect(docsAll_tidy$text, unlist(missingTexts[i,c(2, 3, 7)])), ] 
  if(dim(find_proposalList3[[i]])[1] > 0){
    find_proposalList3[[i]]$Title = missingTexts[i, 'text']
    find_proposalList3[[i]]$Reference = missingTexts[i, 'code']
    find_proposalList3[[i]]$keywords = missingTexts[i, 'keywords']
    find_proposalList3[[i]]$year = missingTexts[i, 'adoption_year']
  }
}


# remove matches for which the meeting year is later than when the standard was adopted
find_proposalList3 = removeReports(find_proposalList3)


between1And10FindProposal3 = between1And10Matches(find_proposalList3)

highLightedText3 = lapply(between1And10FindProposal3, function(x){
  xTitle <- paste0("<font color='blue'>",x$Title %>% unique(),"</font>")
  xReference <- paste0("<font color='blue'>",x$Reference %>% unique(),"</font>")
  xstandards_extracted <- paste0("<font color='blue'>",x$standards_extracted %>% unique(),"</font>")
  xKeywords <- paste0("<font color='blue'>",x$standards_keywords %>% unique(),"</font>")
  
  textHighlight = str_replace (x$text, coll(x$Title %>% unique()), xTitle)
  textHighlight = str_replace (textHighlight, coll(x$Reference %>% unique()), xReference)
  textHighlight = str_replace (textHighlight, coll(x$Title %>% unique()), xstandards_extracted)
  textHighlight = str_replace (textHighlight, coll(x$Reference %>% unique()), xKeywords)
}
)



setwd('/Users/cindycheng/Documents/Papers/Codex/RCode')

getwd()
# Create the Rmd to knit
cat(
  '---
  title: "Look for Missing Proposals v3"
  author: "Cindy Cheng"
  date: "2019-05-08"
  output: html_document
  ---
  
  
  ## Proposals {.tabset} \n
  
  ',
  make.tabs(between1And10FindProposal3, highLightedText3),
  
  sep = "",
  file = "1.5_findMissingProposalData3.Rmd")


# Render the Rmd created into html here
rmarkdown::render("1.5_findMissingProposalData3.Rmd")

missingTexts$committee = NA
missingTexts$committee_proposal_year = NA
missingTexts$step = NA
missingTexts[grep('stone', missingTexts$text),  c('committee', 'committee_proposal_year', 'step')] = c('CCCF3' , 2009, '1/2/3')
missingTexts[grep('mycotoxin contamination in cereals', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFAC33' , 2001, '1/2/3')
missingTexts[grep('mycotoxins in spices', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCCF9', 2015, '1/2/3')
missingTexts[grep('patulin', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFAC31', 1999, '3') # there was a position paper on pautlins in 1997, but nothing on patulins and apples
missingTexts[grep('polycyclic', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCCF1' , 2007, '2/3/4')  
missingTexts[grep('hydrocyanic', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCCF6' , 2012, '1/2/3')  
missingTexts[grep('hazard', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('TFAF6' , 2012, '2/3')  
missingTexts[grep('food safety control', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFH39' , 2007, '2/3') 
missingTexts[grep('canned', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFH24' , 1989, '1')  
missingTexts[grep('import and export control', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCMAS20' , 1995, '1') 
missingTexts[grep('saginata', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFH45' , 2013, '5/8') # meeting before talked about developing risk profiles for taenia saginata but no proposal
missingTexts[grep('specific dna sequences ', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCMAS30' , 2009, '1/2/3') # previously known as 'Proposed Draft Guidelines for Criteria for Methods for the Detection and Identification of Foods Derived from Biotechnology'
missingTexts[grep('vibrio', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFH39' , 2007, '1') # previously known as 'Proposed Draft Guidelines for Criteria for Methods for the Detection and Identification of Foods Derived from Biotechnology'
missingTexts[grep('fresh fruits', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFH32' , 1999, '3')
missingTexts[grep('source directed', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFAC29' , 1997, '2/3')
missingTexts[grep('advisory lists of nutrient compounds for use in foods for special dietary uses intented for infants and young', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCNFSDU7' , 1972, '')   # not really a standard that is proposed; used to be two advisory lists: adivsory list of mineral salts for use in foods for infants and chilrden and advisory list of vitamin compounds for use in foods for infants and children; changed in 2002 to be a single advisory list  
missingTexts[grep('class names and the international numbering system for food additives', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFAC13' , 1979, '')  # not really a standard that is proposed...
missingTexts[grep('code of practice for the prevention and reduction of dioxin and dioxin like', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFAC36' , 2004, '1/2/3')  # not really a standard that is proposed...
missingTexts[grep('code of practice for the storage and transport of edible fats and oils in bulk', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFO12' , 1982, '1')  
missingTexts[grep('general standard for bottled packaged drinking waters other than natural mineral', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCNMW5' , 1996, '1')  
missingTexts[grep('general standard for the use of dairy terms', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCMMP1' , 1994, '1')  # used to be called 'PROPOSED DRAFT REVISED CODE OF PRINCIPLES CONCERNING MILK AND MILK PRODUCTS'
missingTexts[grep('guidelines for design production issuance and use of generic official certificates', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFICS1' , 1992, '1')   
missingTexts[grep('guidelines for simple evaluation of dietary exposure to food additives', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFAC18' , 1985, '')
missingTexts[grep('guidelines for the development of equivalence agreements regarding food imports and export inspection and certification systems', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFICS1' , 1992, '1')
missingTexts[grep('guidelines on good laboratory practice in pesticide residue analysis', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCPR19' , 1987, '')
missingTexts[grep('general principles for the addition of essential nutrients to foods', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCNFSDU14' , 1985, '')
missingTexts[which(missingTexts$text == 'general principles of food hygiene'), c('committee', 'committee_proposal_year', 'step')] = c('CCFH3' , 1966, '')
missingTexts[grep('irrad', missingTexts$text), c('committee', 'committee_proposal_year', 'step')] = c('CCFH11', '1974', '1' ) # CCHFH14 makes reference to this standard having been proposed at CCFH11 
missingTexts[grep('glossary', missingTexts$text), c('committee', 'committee_proposal_year')] = c('CCRVDF1', '1986' ) 


text = read.csv(paste0(pathMain, '/participation_development/standardsMissingProposalsApril2019_SK.csv'), stringsAsFactors = FALSE, na.strings = '.')
text = text[which(is.na(text$cac)),][1:35,]

missingTexts$text_original = text$text
missingTexts$reference_original = text$code

  
write.csv(missingTexts, paste0(pathMain, '/participation_development/standardsMissingProposalsMay2019_CC.csv'))



missingSeb[which(is.na(missingSeb$committee)),] 
# maximum residue limits mrls and risk management recommendations rmrs for residues of veterinary drugs in foods: not a standard...?



# 
# load(paste0(pathData, '/participation_development/codexWide.rda'))
# 
# findMissingProposals3 = codexWide[c(which(codexWide$Reference!=0 & codexWide$yearsAfterProposal == 0), which(codexWide$Reference!=0 & codexWide$step >3)) %>% unique() %>% sort(), c('Reference', 'Title', 'proposal_clean')] 
# findMissingProposals3 = cleanProposalData((findMissingProposals3))
# 
# findMissingProposals3$Title = gsub('reccomendations',
#                                    'recommendations',findMissingProposals3$Title)
# findMissingProposals3$Title = gsub('  ',
#                                    ' ',findMissingProposals3$Title)
# findMissingProposals3$Title = gsub('samsø',
#                                    'samso',findMissingProposals3$Title)
# 
# 
# findMissingProposals3$keywords = findMissingProposals3$Title 
# findMissingProposals3$keywords = gsub('code of ethics for|protocol for the|principles for|principles and guidance on the|guideline procedures for the|general methods of|general methods for the|code of practice|guidance for|code of practice on|code of practice for|regional guidelines for|guide for the|guideline for the|guideline on|code of practice to|risk analysis of |assessment of the |performance criteria and validation of methods for|a blend of|general standard for|guidelines on|maximum residue limits mrls and risk management recommendations rmrs for|working principles for|guidelines on the|guidelines for|guidelines for the|code of practice for the|general guidelines|principles and guidelines for the|harmonized guidelines for|principles for the use of|standard for| groupstandard for|generalmethods of|regional standard for|code of hygienic practice for |list of codex specifications for|guidelines on the applications of general principles of food hygiene to the|guidelines for the control of|group standard',
#                                       '',findMissingProposals3$keywords )
# 
# findMissingProposals3$keywords = gsub('^for|general  |concerning|regional|principles and  monitoring the|application of general principles of|^for|for use of the term|intented for infants and young children|for food additives|the processing of |prevention and reduction of|reduction of contamination of food with|reduction of|compilation of codex texts relevant to the',
#                                       '',findMissingProposals3$keywords )
# findMissingProposals3$keywords = gsub('$s', '', findMissingProposals3$keywords )
# findMissingProposals3$keywords = str_trim(findMissingProposals3$keywords)
# 
#  
# 
# findMissingProposals3$keywords[which(findMissingProposals3$keywords == findMissingProposals3$Title)] = NA
# findMissingProposals3$year = NA
# findMissingProposals3$step = NA
# findMissingProposals3 = findMissingProposals3[order(findMissingProposals3$proposal_clean),]
# findMissingProposals3[40:50,]
# find_proposalList3 = list()
# for ( i in 1:dim(findMissingProposals3)[1]){
#   print(i)
#   if (i <47){   
#     find_proposalList3[[i]] <-docsAll_tidy[str_detect(docsAll_tidy$text, unlist(findMissingProposals3[i,1:4])), ] }
#     if (i >= 47){
#       find_proposalList3[[i]] <-docsAll_tidy[str_detect(docsAll_tidy$text, unlist(findMissingProposals3[i,c(1, 2, 4)])), ] }
#    if(dim(find_proposalList3[[i]])[1] > 0){
#     find_proposalList3[[i]]$Title = findMissingProposals3[i, 'Title']
#     find_proposalList3[[i]]$Reference = findMissingProposals3[i, 'Reference']
#     find_proposalList3[[i]]$keywords = findMissingProposals3[i, 'keywords']
#     find_proposalList3[[i]]$standards_extracted = findMissingProposals3[i, 'standards_extracted']
#     find_proposalList3[[i]]$year = findMissingProposals3[i, 'year']
#     find_proposalList3[[i]]$step = findMissingProposals3[i, 'step']
#   }
# }
# 
# # save(find_proposalList3, file = paste0(pathMain, '/participation_development/findMissingProposalsList3.rda'))
# load(file = paste0(pathMain, '/participation_development/findMissingProposalsList3.rda'))
# 
#  
# # remove matches for which the meeting year is later than when the standard was adopted
# find_proposalList3 = removeReports(find_proposalList3)
# 
#  
# between1And10FindProposal3 = between1And10Matches(find_proposalList3)
# 
#  
# highLightedText3 = lapply(between1And10FindProposal3, function(x){
#   xTitle <- paste0("<font color='blue'>",x$Title %>% unique(),"</font>")
#   xReference <- paste0("<font color='blue'>",x$Reference %>% unique(),"</font>")
#   xstandards_extracted <- paste0("<font color='blue'>",x$standards_extracted %>% unique(),"</font>")
#   xKeywords <- paste0("<font color='blue'>",x$standards_keywords %>% unique(),"</font>")
#   
#   textHighlight = str_replace (x$text, coll(x$Title %>% unique()), xTitle)
#   textHighlight = str_replace (textHighlight, coll(x$Reference %>% unique()), xReference)
#   textHighlight = str_replace (textHighlight, coll(x$Title %>% unique()), xstandards_extracted)
#   textHighlight = str_replace (textHighlight, coll(x$Reference %>% unique()), xKeywords)
# }
# )
# 
# 
# 
# setwd('/Users/cindycheng/Documents/Papers/Codex/RCode')
# 
# 
# # Create the Rmd to knit
# cat(
# '---
# title: "Look for Missing Proposals v3"
# author: "Cindy Cheng"
# date: "2019-02-21"
# output: html_document
# ---
#   
#   
# ## Proposals {.tabset} \n
#   
#   ',
#   make.tabs(between1And10FindProposal3, highLightedText3),
#   
#   sep = "",
#   file = "1.5_findMissingProposalData3.Rmd")
# 
# 
# # Render the Rmd created into html here
# rmarkdown::render("1.5_findMissingProposalData3.Rmd")
# 
#  
# 
# 
# findMissingProposals3[which(!is.na(findMissingProposals3$proposal_clean)),] %>% head()
# 
# 
# find_proposalList3 
# # ------------------------------------------------
# 
# # ------------------------------------------------
# fillProposal3 = do.call(rbind, lapply(find_proposalList3, function(x){
#   x = dplyr:::select(x, id, Meeting, Folder, Title, Reference, year, step)
#    return(x)
# })) %>% data.frame()
# 
#  
# fillProposal3 = fillYear(fillProposal3, 'standard for edible cassava flour', 'Fal87_29e') 
# fillProposal3 = fillYear(fillProposal3, 'code of practice for the prevention and reduction of dioxin and dioxin like pcb contamination in food and feeds', '34%252FAl03_12e') 
# 
# # Steps
# fillProposal3 = fillStep(fillProposal3, 'standard for edible cassava flour', 'Fal87_29e', 3)
# fillProposal3 = fillStep(fillProposal3, 'code of practice for the prevention and reduction of dioxin and dioxin like pcb contamination in food and feeds', '34%252FAl03_12e', '2/3')
# 



# edible cassava flour first adopted as regional standard in 1989, and then started to be adopted as a world-wide standard at step 3 in 1991






fillProposalClean = rbind(fillProposal2[-which(is.na(fillProposal2$year)),],
                          fillProposal[-which(is.na(fillProposal$year)),])

 
 
fillProposalClean$Reference = toupper(fillProposalClean$Reference)
fillProposalClean$Title = sapply(fillProposalClean$Title, simpleCap)
fillProposalClean$Title = gsub(' For ', ' for ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' On ', ' on ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' And ', ' and ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' Or ', ' or ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' Of ', ' of ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' The ', ' the ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' Including ', ' including ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' To ', ' to ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' In ', ' in ', fillProposalClean$Title)
fillProposalClean$Title = gsub(' With ', ' with ', fillProposalClean$Title)

fillProposalClean$Reference = sapply(fillProposalClean$Reference, function(x){
  n = nchar(x)
  paste0(substr(x, 1, n-5), "-", substr(x, n-3, n ))
} 
) 

fillProposalClean$Reference = gsub('CAC ', 'CAC/', fillProposalClean$Reference )
 
meeting_cac = data.frame(year = c(1962:1966, 1968:1972, 1974, 1976, 1978, 1979, 1981, 1983, 1985, 1987, 1989, 1991, 1993, 1995, 1997, 1999, 2001, 2003, 2003:2018),
                         meeting = 0:41)

fillProposalClean$meeting_cac = paste0('CAC', meeting_cac$meeting[match(fillProposalClean$year, meeting_cac$year)])
fillProposalClean$meetingCount_cac =  meeting_cac$meeting[match(fillProposalClean$year, meeting_cac$year)]

 
save(fillProposalClean, file = paste0(pathData, '/fillProposalClean.rda')) 




which(unlist(lapply(find_proposalList2, function(x){
  if(dim(x[grep('claims', x$Title),])[1]>0){
    TRUE
  } else{FALSE}
}))== TRUE )
 
 
# standard for quick frozen fish fillets
# standard for edible fats and oils not covered by individual standards has many reference codes, clean up



# gluten, step 1: Fal81_30e


# # how many matches between 1 and 10 documents
# sum(unlist(lapply(find_proposalList, function(x){
#   
# }))*1 )
# 
# # mean number of matches given match between 1 and 10 documents
# mean(unlist(lapply(find_proposalList, function(x){
#   if(dim(x)[1]<11 & dim(x)[1]>0){
#     return(dim(x)[1])
#   } else{return(NA)}
# })), na.rm = TRUE)
# 
# 
# # mean number of matches given that match more than 10 documents
# mean(unlist(lapply(find_proposalList, function(x){
#   if(dim(x)[1] > 10){
#     return(dim(x)[1])
#   } else{return(NA)}
# })), na.rm = TRUE )
# 


 
 


