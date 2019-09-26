# clean Proposal data
 
if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}


findDupes = function(dataset, var){
  
  dupes = dataset[which(duplicated(dataset[, var])|duplicated(dataset[,var], fromLast = TRUE)),]
  dupes = dupes[order(dupes[, c('year')]),]
  dupes[,var] = dupes[,var] %>% as.character()
  dupes = do.call(rbind, lapply(split(dupes, dupes[,var]), function(x){
    x[1,]
  }))
  row.names(dupes) = NULL
  dupesClean = rbind(dupes, dataset[-which(duplicated(dataset[, var])|duplicated(dataset[,var], fromLast = TRUE)),])
  return(dupesClean)
} 

# ---------------------------------------
# Clean Standard Proposal Data
# ---------------------------------------
# question: gsfsa should be CODEX STAN 192-1995, but i see references to CODEX STAN 192-XXXX?

# load proposed standard data
proposed = read.csv(paste0(pathMain, '/participation_development/CAC_standards_clean_31.01.2019.csv'), stringsAsFactors = FALSE, na.strings = '.')
proposed = proposed[, -which(names(proposed) == 'number')]
proposed$year = proposed$year - 1
proposed$handcodedDummy = 0



# load master committee dates data
load(paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))

# match CAC meeting to proposed committee data
names(proposed)[2] = 'meeting_cac'
proposed$committee[which(proposed$committee == 'CCFA' & proposed$year<2007)] = 'CCFAC'
proposed$meeting_num = dates_all$event_number[match(paste0(proposed$committee, proposed$meeting_cac), paste0(dates_all$event_short, dates_all$event_number_cac))]

proposed = proposed[-which(proposed$standards_extracted_clean == 'Proposed Draft Standard for Processed Cheese' & proposed$year>2009),]
proposed[which(proposed$standards_extracted == 'proposed draft Standard for Dairy Permeate Powders'), 'meeting_num' ] = 'Adjourned'
proposed[which(proposed$committee == 'CCMMP' & proposed$year == 2009),  c('meetingCount_cac', 'meeting_cac', 'meeting_num',  'committee', 'year', 'handcodedDummy')]  = c('24', 'CAC24', 'CCMMP6', 'CCMMP', 2004, 1)
proposed$meeting_num[which(proposed$committee == 'CCS' & proposed$year == 1970)]  = 'CCS5'
proposed$meeting_num[which(proposed$committee == 'CCS' & proposed$year %in% c(1990, 1992))]  = 'CCS6'
proposed$meeting_num[which(proposed$committee == 'CCMPH' & proposed$year %in% c(1977))]  = 'CCMPH3'
proposed$meeting_num[which(proposed$committee == 'CCFO' & proposed$year %in% c(1990))]  = 'CCFO13'
proposed[which(proposed$committee == 'TFAMR' & proposed$year %in% c(2016)), c('year', 'meeting_num')]  =c(2017, 'TFAMR6')
proposed$meeting_num[which(proposed$committee == 'CCS' & proposed$year> c(2000))]  = 'CCS7'
proposed[which(proposed$standards_extracted == 'Proposed draft maximum residue limits (MRLs) for zilpaterol hydrochloride (cattle fat, kidney, liver, muscle)'), c('meetingCount_cac', 'meeting_cac', 'meeting_num',  'committee', 'year', 'handcodedDummy')]  = c('40', 'CAC40', 'CCRVDF23','CCRVDF',  2016, 1)
proposed[which(proposed$committee == 'CCPR' & proposed$year %in% c(2017)),c('meeting_num', 'year')] = c('CCPR50', 2018)

 
## Some committees incorrectly coded; fix with corrected names
# write.csv(proposed[which(!is.na(proposed$committee) & is.na(proposed$meeting_num)), ], file = '~/Downloads/proposedWrongCommitteeName.csv')
correctedNames = read.csv(paste0(pathMain, '/participation_development/proposedWrongCommitteeName_corrected.csv'), stringsAsFactors = FALSE)
correctedNames$committee = correctedNames$corrected
correctedNames$handcodedDummy = 0
correctedNames$year = correctedNames$year -1
proposed[which(!is.na(proposed$committee) & is.na(proposed$meeting_num)), ] = correctedNames[,names(proposed)]
proposed[which(!is.na(proposed$committee) & is.na(proposed$meeting_num)),'meeting_num' ] = dates_all$event_number[match(paste0(proposed$committee[which(!is.na(proposed$committee) & is.na(proposed$meeting_num))], proposed$meeting_cac[which(!is.na(proposed$committee) & is.na(proposed$meeting_num))]), paste0(dates_all$event_short, dates_all$event_number_cac))]
proposed$meeting_num[which(proposed$committee == 'CCMPH' & proposed$year %in% c(1978))] = 'CCMPH3'
proposed[which(proposed$standards_extracted == 'Proposed Draft Code of Hygienic Practice for Game'), c('committee', 'meeting_num', 'handcodedDummy')] = c('CCPMPP', 'CCPMPP10', 1)

# remove duplicates
proposed = proposed[-which(duplicated(proposed[, c('Reference', 'year', 'Title')])& proposed$Reference!=0),] 
 
# ------------------------------
# clean standards_extracted names
# -------------------------------
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

proposed$standards_extracted_clean = proposed$standards_extracted
proposed$standards_extracted_clean= tolower(proposed$standards_extracted_clean)
proposed$standards_extracted_clean = apply(proposed$standards_extracted_clean %>% data.frame(),1, function(x) simpleCap(x))
proposed$standards_extracted_clean = gsub(' For ', ' for ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' To ', ' to ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' The ', ' the ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' On ', ' on ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' In ', ' in ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' Of ', ' of ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' With ', ' with ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' And ', ' and ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub("Dtaft'standdrd|Dtaft'Standdrds|Dtaft'Standdrds", 'Draft standard', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub("St Andard ", 'Standard', proposed$standards_extracted_clean)


## for identifying near duplicates
# proposedNearDupe = tidy_comb_all(unique(proposed$standards_extracted_clean)[-which(unique(proposed$standards_extracted_clean)=='0'|is.na(unique(proposed$standards_extracted_clean)))])
# proposedNearDupeDist = tidy_stringdist(proposedNearDupe) %>% data.frame()
# proposedNearDupeDist[which(proposedNearDupeDist$cosine <.03),c('V1', 'V2')][6:10,]
# which(unlist(lapply(split(proposed$standards_extracted_clean, proposed$Reference)[-1], function(x){
#   length(unique(x))==1
# }
# )) == FALSE)



proposed$standards_extracted_clean[grep('Gluten-free', proposed$standards_extracted_clean)] = 'Proposed Draft Standard for Gluten-Free Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Composite and Filled Chocolate') )] = 'Proposed Draft Standard for Composite and Filled Chocolate'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Codex Standard for Oats.') )] =   'Proposed Draft Codex Standard for Oats'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Ginseng Product',
                                                                                   'Proposed Draft Codex Standard for Ginseng') )] = 'Proposed Draft Standard for Ginseng Products'

proposed$standards_extracted_clean[grep('Gadidae Family', proposed$standards_extracted_clean) ]  =  'Proposed Draft Amendment to the Standard for Salted Fish and Dried Salted Fish of the Gadidae Family (Sampling and Analysis)'

proposed$standards_extracted_clean[grep('Judgement of Equivalence of Sanitary Measures', proposed$standards_extracted_clean) ] =   'Proposed Draft Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels for Doexynivalenol') )] = 'Proposed Draft Maximum Level for Deoxynivalenol'

proposed$standards_extracted_clean[grep('Processed Cheese', proposed$standards_extracted_clean) ] = "Proposed Draft Standard for Processed Cheese" 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment to the Standard for Canned Sardines and Sardine-type Products',
                                                                                   'Proposed Draft Amendment to the Standard for Sardine and Sardine Type Products') ) ]  ='Proposed Draft Amendment to the Standard for Canned Sardine and Sardine Type Products'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for “bitter” Cassava') ) ]  = 'Proposed Draft Standard for Bitter Cassava'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels Hydrocyanic Acid in Cassava and Cassava Products') )] = 'Proposed Draft Maximum Levels for Hydrocyanic Acid in Cassava and Cassava Products'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Mrls for Bovine Somatotropins (bsts)',
                                                                                   'Proposed Draft for Mrl for Bovine Somatotropins (bsts)') )] =  'Proposed Draft for Mrl for Bovine Somatotropins (BSTS)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendments to Codex Standards Fot Food for Infanta and Children',
                                                                                   'Proposed Draft Revision of the Recommended International Code of Practice for Foods for Infants and Children (CAC/RCP 21-1979 – amended 1981)',
                                                                                   'Proposed Draft Atendtents to Codex Standards for Foods for Infants and Children') )]  = 'Proposed Draft Amendments to Codex Standards for Foods for Infants and Children'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendments to the International Numbering System for Food Additives',
                                                                                   'Proposed Draft Amendments to the International Numbering (ins) System for Food Additives',
                                                                                   'Proposed Draft Amendments to the International Numbering System (ins) for Food Additives',
                                                                                   'Proposed Draft Revision of the Codex Class Names and the International Numbering System for Food Additives',
                                                                                   'Proposed Draft Revision of the “class Names and International Numbering System for Food Additives - Cac/gl 36-1989',
                                                                                   'Proposed Draft Amendments to the International Numbering System for Food Additives (cac/gl 36- 1989)',
                                                                                   'Proposed Draft List of Class Titles for Food Additives',
                                                                                   'Proposed Draft Revisions to the Codex International Numbering System for Food Additives',
                                                                                   'Class Names and the International Numbering System for Food Additives',
                                                                                   'Class Names and the International Numbering System for Food Additives') )]  =  'Proposed Draft Amendments/Revisions to the International Numbering (INS) System for Food Additives'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Food Additives Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft for Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Amendments to the Codex General Standard for Food Additives: Annex to Table 3 (food Categories Or Individual Food Items Excluded From the General Conditions of Table 3)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additives Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft (step 3) and Draft (step 6) Food Additive Provisions of the Codex General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the Gsfa',
                                                                                   'Proposed Draft Food-additive Provisions of the Gsfa',
                                                                                   'Proposed Draft Amendment to the General Standard for Food Additives: Preamble – Footnote',
                                                                                   'Proposed Draft Amendments to the Food Category System for the General Standard for Food Additives',
                                                                                   'Proposed Draft for Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives',
                                                                                   'Proposed Draft Codex General Standard for Food Additives',
                                                                                   'Proposed Draft General Standard on Food Additives',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa) (codex Stan 192-1995)',
                                                                                   'Draft and Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa) (codex Stan 192-1995)',
                                                                                   'Food Additive Provisions of the Gsfa (proposed Draft and Draft)',
                                                                                   'Proposed Draft Amendments to the Codex General Standard for Food Additives: Annex to Table 3 (food Categories Or Individual Food Items Excluded From the General Conditions of Table 3)',
                                                                                   'Food Additive Provisions of the Gsfa (draft and Proposed Draft)'))] = "Proposed Draft Food Additive Provisions of the General Standard for Food Additives (GSFA)"

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Maximum Residue Limits',
                                                                                    'Proposed Draft Maximum Residue Limits (mrls)',
                                                                                    "Proposed Draft Maximum Residue Limits for Pesticides",
                                                                                    'Proposed Draft Maximum Limits for Pesticide Residues',
                                                                                    'Proposed Draft Maximum Residue Limits for Pesticides (mrls)'))] = "Proposed Draft Maximum Residue Limits for Pesticides (MRLs)"


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft and Proposed Draft Residue Limits for Pesticides',
                                                                                    'Maximum Residue Limits for Pesticides (draft and Proposed Draft)'))] = "Proposed Draft Residue Limits for Pesticides"



proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Codex Standard for Tiquisque (white and Lilac)',
                                                                                    'Proposed Draft Standards for Tiquisque (white and Lilac)') )]  ='Proposed Draft Codex Standard for Tiquisque (White and Lilac)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Standard for Quick Frozen Scallop Adductor Muscle Meat') )] = 'Proposed Draft Standard for Quick Frozen Scallop Adductor Meat'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Guidelines for the Utilization of Vegetable Proteins in Foods' ,
                                                                                    'Proposed Draft General Guidelines for the Utilization of Vegetable Proteins in Foods') )] =    'Proposed Draft General Guidelines for the Utilization of Vegetable Proteins in Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Residue Limits for Veterinary Drugs', 
                                                                                   'Proposed Draft Maximum Residues Limits (mrls) for Veterinary Drugs',
                                                                                   'Proposed Draft Maximum Residue Limits and Proposed Draft Revised Maximum Residue Limits for Veterinary Drugs',
                                                                                   'Proposed Draft Maximum Residue Limits for Veterinary Drugs in Foods') )]  =  'Proposed Draft Maximum Residues Limits (MRLS) for Veterinary Drugs'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Canned Mangoes') ) ] = 'Proposed Draft Standards for Canned Mango Products' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Revised Code of Ethics for International Trade in Foods') ) ] = 'Proposed Draft Code of Ethics for International Trade in Food' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Regional Standard for Non-fermented Soybean Products' ) )] ='Proposed Draft Standard for Non-fermented Soybean Products'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes',
                                                                                   'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Previous Cargoes',
                                                                                   'Proposed Draft Amendment to Table 1 of the Recommended International Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk with Accelerated Procedure',
                                                                                   'Proposed Draft Amendments to the Code of Practice for the Transport of Edible Fats and Oils in Bulk (list of Acceptable Previous Cargoes and List of Immediate Previous Banned Cargoes)') )]= 'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Provisional Standard for Dried Edible Fungi') )] =   'Proposed Draft Provisional Standard for Edible Fungi and Fungus Products'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft General Standard for Vegetable Juices and Vegetable Nectars') ) ]  =   'Proposed Draft General Standard for Vegetable Juices'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Codex Standard for Durum Wheat') )]  =   'Proposed Draft Codex Standards for Wheat and Durum Wheat'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Appendices to the Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification') )]  =   'Proposed Draft Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft List of Acceptable Previous Cargoes') ) ] = 'Proposed Draft List and Draft Lists of Acceptable Previous Cargoes'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Principles for the Application of Traceability/product Tracing in the Context of Food Import and Export Inspection and Certification Systems') )] =        'Proposed Draft Principles for Traceability/product Tracing As A Tool Within A Food Import and Export Inspection and Certification System'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Recommended International Code of Practice for the Handling and Processing of Quick Frozen Foods',
                                                                                   'Proposed Draft Code of Practice for the Processing and Handling of Quick-frozen Foods',
                                                                                   'Proposed Draft Revised Code of Practice for the Processing and Handling of Quick Frozen Foods',
                                                                                   'Proposed Draft Standards for Quick Frozen Foods') ) ] = 'Proposed Draft Recommended International Code of Practice for the Processing and Handling of Quick Frozen Foods'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Previous Cargoes',
                                                                                   'Proposed Draft Code of Practice for the Storage and Transport of Edible Oils and Fats in Bulk') ) ]  =   'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Hygienic Practice for the Inspection of Game') )]  = 'Proposed Draft Code of Hygienic Practice for Game'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels for Aflatoxins in Almonds, Hazelnuts and Pistachios') )]  =  'Proposed Draft Maximum Level for Total Aflatoxins in Unprocessed Almonds, Hazelnuts and Pistachios'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Practice for the Prevention and Reduction of Tin Contamination in Foods') )] = 'Proposed Draft Code of Practice for the Prevention and Reduction of Inorganic Tin Contamination in Canned Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels for Cadmium in Various Commodities') )] =   'Proposed Draft Maximum Levels for Cadmium' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Model Certificate for Fish and Fishery Products') )] = 'Proposed Draft Model Certificate for Fish and Fishery Products (other Certificates)' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft St Andard for Chocolate' ) )]  ='Proposed Draft Standard for Chocolate'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft St Andard for Fructose' ) )]  ='Proposed Draft Standard for Fructose'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Code of Hygienic Practice for Foods for Inf Ants and Children' ) )]  ='Proposed Draft Code of Hygienic Practice for Foods for Infants and Children'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Processed Foods for Infants and Children Based on Cereals',
                                                                                   'Proposed Draft Amendments to the Codex Standard for Processed Cereal Based Foods for Infants and Children',
                                                                                   'Proposed Draft Revised Standard for Cereal-based Foods for Infants and Young Children',
                                                                                   'Proposed Draft Revised Standard for Processed Cerealbased Foods for Infants and Young Children',
                                                                                   'Proposed Draft Standard for Processed Cerealbased Foods for Infants and Young Childrens',
                                                                                   'Standard for Processed Cereal-based Foods for Infants and Young Children (codex Stan 74-1981) to Include A New Part B for Underweight Children (proposed Draft Amendment)') ) ]  =  'Proposed Draft Standard for Processed Cerealbased Foods for Infants and Young Children' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for the Labelling of and Claims for Pre-packaged Foods Claimed to Be Suitable for Incorporation in A Dietary Regimen for Diabetics (returned to Step 3), Low Energy and Reduced Energy Foods (returned to Step 3) and Draft Guidelines for [medical] Foods.',
                                                                                   'Proposed Draft Standard for the Labelling of and Claims for Prepackaged Foods Claimed to Be Suitable for Incorporation Into A Prescribed Dietary Regimen for Diabetes',
                                                                                   'Proposed Draft Standard for the Labelling of and Claims for Prepackaged Foods Claimed to Be Suitable for Incorporation Into A Prescribed Dietary Regimen for Diabetes,') )]  = 'Proposed Draft Standard for the Labelling of and Claims For, Pre-packaged Foods Claimed to Be Suitable for Diabetics'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Hygienic Practice for the Collecting, Processing and Marketing of Natural Mineral Waters') )]  = 'Proposed Draft Revision of the Recommended International Code of Hygienic Practice for Collecting, Processing and Marketing of Natural Mineral Waters (cac/rcp 33-1985)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Cocoa (cacao) Beans Cocoa (cacao) Nib, Cocoa Cacao Mass Cocoa Press Cake and Cocoa Dust Cocoa Fines for Use in the Manufacture of Cocoa An Chocolate Products') )]  =   'Proposed Draft Revised Standard for Cocoa (cacao) Mass (cocoa/chocolate Liquor) and Cocoa Cake, for the Use in the Manufacture of Cocoa and Chocolate Products'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Guidelines for the Use of Health and Nutrition Claims in Food Product Labelling',
                                                                                   'Proposed Draft Guidelines on Nutrition and Health Claims for Food Labelling.',
                                                                                   'Proposed Draft Guidelines for the Use of Health and Nutrition Claims, Including the Table of Conditions for Claims for Nutrient Contents',
                                                                                   'Proposed Draft Definition of Advertising in Relation to Health and Nutrition Claims',
                                                                                   'Proposed Draft Revision of the Guidelines for Use of Nutrition and Health Claims (cac/gl 23-1997) Concerning A New Definition for “non-addition Claim”, Conditions for Free of Salt Claims, Amendments to the Section on Comparative Claims and Conditions for Non-addition of Sugars Claims') )]  =  'Proposed Draft Guidelines on Nutrition and Health Claims for Food Labelling'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Bottled/packaged Waters Other Than Natural Mineral Water' ))   ]  =  'Proposed Draft General Standard for Bottled/packaged Drinking Waters (other Than Natural Mineral Waters)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment to the Codex General Standard for Cheese (description)',
                                                                                   'Proposed Draft Amendment to the Codex General Standard for Cheese: Appendix') )]  = 'Proposed Draft Amendment to the Codex General Standard for Cheese'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Annex to the Codex Guideline for the Conduct of Food Safety Assessment of Foods Derived From Recombinant-dna Plants (cac/gl 45-2003)') )]  = 'Proposed Draft Guideline for the Conduct of Food Safety Assessment of Foods Derived From Recombinant-dna Plants'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Codex Advisory Specifications for the Identity and Purity of Food Additives' ) )]  =     'Proposed Draft Specifications for the Identity and Purity of Food Additives (category I)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (% V/v)' ) )]  =         'Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (%v/v) - Orange, Lemon, Lime and Pineapple Juices/nectars'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment to the Guidelines for the Production, Processing, Marketing and Labelling of Organically Produced Foods - Annex 2 (permitted Substances for the Production of Organic Foods)',
                                                                                   'Proposed Draft Amendment to the Guidelines for Organically Produced Foods (Ethylene)',
                                                                                   'Proposed Draft Amendment to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods: Annex 2 – Permitted Substances : Table 1 (natural Sodium Nitrate)',
                                                                                   'Proposed Draft Amendment to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods (table 1:substances Used in Soil Fertilizing and Conditioning)',
                                                                                   'Proposed/draft Amendment to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods (cac/gl 32-1999): Use of Ethylene for Ripening of Fruit (step 8) and Inclusion of New Substances (step 5a)',
                                                                                   'Proposed Draft Guidelines on Organically Produced Food',
                                                                                   'Proposed Draft Amendment to the Guidelines for Organically Produced Foods (ethylene)'))  ]  = 'Proposed Draft to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revised Guideline Levels for Radionuclides in Foods Contaminated Following A Nuclear Or Radiological Emergency for Use in International Trade16')) ]  ='Proposed Draft Guideline Levels for Radionuclides in Food for Use in International Trade'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Appendix to the Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Tree Nuts to Address Additional Measures for the Prevention and Reduction of Aflatoxins in Brazil Nuts'))  ]  ='Proposed Draft Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Tree Nuts'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revision of the Code of Practice to Minimize and Contain Antimicrobial Resistance (cac/rcp 61-2005)')) ]  ='Proposed Draft Code of Practice to Minimize and Contain Antimicrobial Resistance'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean_clean %in% c('Proposed Draft Principles and Guidelines for National Food Control System') ) ]  ='Proposed Draft Principles and Guidelines for National Food Control Systems (introduction, Sections 1-3)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean_clean %in% c('Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn',
                                                                                         'Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn (discontinued) By Ccpr50') )]  ='Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Definition of Trans-fatty Acids (amendment to the General Standard for the Labelling of Prepackaged Foods and the The Guidelines on Nutrition Labelling) - Accelerated Procedure',
                                                                                   'Proposed Draft Amendment to the General Standard for the Labelling of Prepackaged Foods (quantitative Declaration of Ingredients)',
                                                                                   'Proposed Draft Amendment to the General Standard for the Labelling of Prepackaged Foods (recommendations for the Labelling of Foods That Can Cause Hypersensitivity)',
                                                                                   'General Standard for the Labelling of Prepackaged Foods: Proposed Draft Amendment Concerning the Labelling of Foods Obtained Through Biotechnology')) ]  ='Proposed Draft Amendment to the General Standard for the Labelling of Prepackaged Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment to the Guidelines on Nutrition Labelling',
                                                                                   'Proposed Draft Definition of Trans-fatty Acids (amendment to the Guidelines on Nutrition Labelling)',
                                                                                   'Proposed Draft Definition of Trans-fatty Acids (amendment to the Guidelines on Nutrition Labelling)',
                                                                                   'Proposed Draft Revision of the Guidelines on Nutrition Labelling (cac/gl 2-1985) Concerning the List of Nutrients That Are Always Declared on A Voluntary Or Mandatory Basis25',
                                                                                   'Proposed Draft Additional Or Revised Nutrient Reference Values for Labelling Purposes in the Codex Guidelines on Nutrition Labelling')) ]  ='Proposed Draft Guidelines on Nutrition Labelling'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Guidelines for the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems') )] =  'Proposed Draft Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft St Andard for Cocoa Butters'))  ]  = 'Proposed Draft Revised Standard for Cocoa Butters'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Practice - General Principles of Food Hygiene'))]= 'Proposed Draft (revised) International Code of Practice: General Principles of Food Hygiene'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Fruit Juices and Nectars',
                                                                                   'Proposed Draft Codex General Standard for Fruit Juices and Nectars') )] =    'Proposed Draft Codex General Standard for Fruit Juices and Nectars'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft European Regional Standard for Mayonnaise') ) ]  ='Proposed Draft Standard for Mayonnaise'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Residue Limits (MRLs) for Pesticides Proposed Draft Revised Maximum Residue Limits (MRLs) for Pesticides Proposed Draft Revised Extraneous Maximum Residue Limit (EMRL)') )] = 'Proposed Draft Maximum Residue Limits and Extraneous Maximum Residue Limits for Pesticides'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Labelling of and Claims for Low-energy and Reduced-energy Foods',
                                                                                   'Proposed Draft Standard for the Labelling of and Claims for "low Energy" and "reduced Energy" Foods') )]  = 'Proposed Draft Standard for Labelling of and Claims for Low-energy and Reduced-energy Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Standard for the Labelling of and Claims for "low Energy" and "reduced Energy" Foods') ) ]  ='Proposed Draft Standard for Labelling of and Claims for Low-energy and Reduced-energy Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Amendments to the Recommended International Code of Practice for Salted Fish') ) ]  ='Proposed Draft Code for Salted Fish'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Sampling Plans for Aflatoxins in Almonds, Brazil Nuts, Hazelnuts and Pistachios',
                                                                                    'Proposed Draft Maximum Level for Total Aflatoxins in Almonds, Hazelnuts and Pistachios for “ready to Eat”') ) ]  ='Proposed Draft Maximum Level for Total Aflatoxins in Unprocessed Almonds, Hazelnuts and Pistachios'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Standard for Hot Pepper Fermented Soybean Paste (gochujang)') ) ]  ='Proposed Draft Standard for Gochujang'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Provisional Standard for Fresh Fungus Chanterelle',
                                                                                    'Proposed Draft Revised Regional Standard for Chanterelles') ) ]  ='Proposed Draft Standard for Chanterelles'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Generic Model Official Certicate (annex to the Guidelines for Design, Production, Issuance and Use of Generic Official Certificates (cac/gl 38-2001))') ) ]  ='Proposed Draft Revision to the Guidelines for Generic Official Certificate Formats and Design, Production, Issuance and Use of Certificates'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn (discontinued) By Ccpr50'))] = 'Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revised Standard for Infant Formula [and Formulas for Special Medical Purposes Intended for Infants]',
                                                                                   'Proposed Draft Revised Standard for Infant Formula and Formulas for Special Medical Purposes Intended for Infants (section B)'))] = 'Proposed Draft Revised Standard for Infant Formula [and Formulas for Special Medical Purposes Intended for Infants'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Guidelines for Risk-based Inspection of Imported Foods'))] ='Proposed Draft Principles and Guidelines for Imported Food Inspection Based on Risk'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Fermented Soybean Paste (doenjang)'))] = 'Proposed Draft Regional Standard for Fermented Soybean Paste'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Specifications for the Identity and Purity of Food Additives (category I)'))] = 'Proposed Draft Codex Advisory Specifications for the Identity and Purity of Food Additives'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (%v/v) - Orange, Lemon, Lime and Pineapple Juices/nectars'))] = 'Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (% V/v)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revised Methods of Sampling for the Determination of Pesticide Residues for Compliance with Mrls',
                                                                                   'Recommended Methods of Analysis for Pesticide Residues: Proposed Draft Amendments to the Introduction Section'))] = 'Proposed Recommended Methods of Sampling for the Determination of Pesticide Residues for Compliance with MRLs'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Compilation of Codex Texts Relevant to Labelling of Foods Derived From Modern Biotechnology'))] = 'Proposed Draft Principles for the Risk Analysis of Foods Derived From Modern Biotechnology'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Guidelines for the Use of Flavourings (n03-2006) (with the Exception of Section 4 and Annexes A and B)',
                                                                                   'Proposed Draft Guidelines for the Use of Flavourings (n03-2006)'))] = 'Proposed Draft Guidelines for the Use of Flavourings'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard on Follow,up Food for Older Infants and Young Children',
                                                                                   'Proposed Draft Revision of the Guidelines on Formulated Supplementary Foods for Older Infants and Young Children (cac/gl 8-1991)'))] = 'Proposed Draft Standard for Foods for Older Infants and Children'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Principles and Guidelines for National Food Control Systems (introduction, Sections 1-3)'))] = 'Proposed Draft Principles and Guidelines for National Food Control System'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Hygienic Practice for Spices and Herbs'))] = 'Proposed Draft Code of Hygienic Practice for Spices and Condiments'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Practice for Fish and Fishery Products (sections on Aquaculture and on Quick Frozen Coated Fish Products)',
                                                                                   'Proposed Draft Code of Practice for Fish and Fishery Products (shrimps and Prawns; Cephalopods; Transport; Retail; and Relevant Definitions)',
                                                                                   'Proposed Draft Code of Practice for Fish and Fishery Products (section on Smoked Fish and Relevant Definitions)'))] = 'Proposed Draft Code of Practice for Fish and Fishery Products'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Hygienic Practice for Foods for Infants and Children'))] = 'Proposed Draft Code of Hygienic Practice for Powdered Formulae for Infants and Young Children'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft St Andard for Cocoa Powder (cocoa) and Sweetened Cocoa Powder (sweetened Cocoa)',
                                                                                   'Proposed Draft Revised Standard for Cocoa Powders (cocoas) and Dry Cocoa-sugar Mixture'))] = 'Proposed Draft Standard for Cocoa Powders'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Provisional Standard for Natural Mineral Waters',
                                                                                   'Proposed Draft Amendment to the Regional European Standard for Natural Mineral Waters',
                                                                                   'Proposed Draft Amendment to Sections 3.2 and 6.3.2 of the Standard for Natural Mineral Waters (codex Stan 108-1981) (n12-2007)'))] = 'Proposed Draft Standard for Natural Mineral Waters'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revised Standard for Food Grade Salt (salt Iodization)'))] = 'Proposed Draft Standard for Food Grade Salt'



proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment of the Codex Standard for Wheat Flour to Revise the Fat Acidity Provision and Related Method of Analysis',
                                                                                   'Roposed Draft Amendment to the Codex Standard for Wheat Flour'))] = 'Proposed Draft Standard for Wheat Flour'



proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft African Regional Standards for  Grated Desiccated Coconut',
                                                                                   'Proposed Draft Standard for Desiccated Coconut (revision of Codex Stan 177-1991)'))] = 'Proposed Draft Standard for Desiccated Coconut'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Annex on Mangoes'))] = 'Proposed Draft Codex Standard for Mangoes'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Revision of the Codex General Standard for Contaminants and Toxins in Foods'))] = 'Proposed Draft General Standard for Contaminants in Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Amendments to the Standards for Palm Oil and Palm Kernel Oil',
                                                                                    'Proposed Draft Amendments to the Stanard for Named Vegetable Oils',
                                                                                    'Proposed Draft Amendment to the Standard for Named Vegetable Oils; Amendment of Sesameseed Oil and Inclusion of Rice Bran Oil',
                                                                                    'Proposed Draft Amendment to the Codex Standard for Named Vegetable Oil; Low Linolenic Acid Soyabean Oil; Mid-oleic Acid Soyabean Oil'))] = 'Proposed Draft Amendments to the Standard for Named Vegetable Oils'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Amendment for the Standard for Sugars'))] = 'Proposed Draft Standard for Sugars'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Standard for Fat Spreads/spreadable Table Fats'))] = 'Proposed Draft Standard for Fat Spreads'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Revised Text of Codex Standard for Table Olive'))] = 'Proposed Draft Standard for Table Olives'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Revised Standards for Tuna and Bonito and Sardines and Sardine-like Products'))] = 'Proposed Draft Standard for Canned Tuna and Bonito in Water Or Oil'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Amendment to the Codex Standard for Canned Crab Meat with Respect to Food Additive Provisions'))] = 'Proposed Draft Standard for Canned Crab Meat'



proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c("Proposed Draft Maximum Residue Limits (mrls) for Pesticides Proposed Draft Revised Maximum Residue Limits (mrls) for Pesticides Proposed Draft Revised Extraneous Maximum Residue Limit (emrl)"))] = 'Proposed Draft Maximum Residue Limits and Extraneous Maximum Residue Limits for Pesticides'


# note in CCFICS7, title change from Proposed Draft Guidelines for the Utilisation and Promotion of Quality Assurance Systems to Proposed Draft Guidelines for the Utilization and Promotion of Quality Assurance Systems to meet Requirements in Relation to Food
proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c("Proposed Draft Guidelines for the Utilisation and Promotion of Quality Assurance Systems",
                                                                                   "Proposed Draft Guidelines for the Utilization and Promotion of Quality Assurance Systems to Meet Requirements in Relation to Food."))] = "Proposed Draft Guidelines for the Utilization and Promotion of Quality Assurance Systems to Meet Requirements in Relation to Food" 


# in CCFICS11, agreed to discontinue work on Proposed Draft Guidelines for the Utilization and Promotion of Quality Assurance Systems to meet Requirements in Relation to Food
proposed[which(proposed$standards_extracted_clean %in% c("Proposed Draft Guidelines for the Utilization and Promotion of Quality Assurance Systems to Meet Requirements in Relation to Food") & 
                 proposed$meetingCount_cac == 26), c('meeting_num')] = c('CCFICS11')


# # from CAC 14 and CAC15 looks like Proposed Draft Standard for Pulpy Mango Nectar preserved exclusively by physical means
# # can be linked to Proposed Draft standards for Pulpy Mango Nectar
proposed[which(proposed$standards_extracted_clean %in% c( "Proposed Draft standards for Pulpy Mango Nectar",
                                                          "Proposed Draft Standard for Pulpy Mango Nectar Preserved Exclusively By Physical Means" )), 'standards_extracted_clean'] = 'Proposed Draft Standard for Liquid Pulpy Mango Products Preserved Exclusively by Physical Means' 

# duplicate, remove
proposed = proposed[-which(proposed$standards_extracted_clean == 'Proposed Draft Code of Practice for Street-vended Foods' & proposed$Reference == '0'),]

# from CAC 30, started proposal for street vended foods in near east
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Regional Guidelines for Street-vended Foods' & proposed$Reference == '0'), c('Reference', 'Title', 'handcodedDummy')] = c('CAC/RCP 71R-2013', "Regional Code of Practice for Street-vended Foods (Near East)", 1)


# from CCFFP11, looks like it became a code of practice (that is no longer in use)
proposed[grep('Smoked Fish', proposed$standards_extracted_clean ),c('Reference', 'Title')] = c('Code of Practice for Smoked Fish', 'CX/FFP 77/6')


# update proposal data for Ayran
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Standard for Ayran'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'meeting_num')] = c(34, 'CAC34', 2010, 1, 'CCEURO27' )
proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Ayran', 'Regional Standard for Ayran (proposed Draft)'))] = 'Proposed Draft Standard for Ayran'


# add proposal data for Proposed Draft Guidelines on the Judgement of Equivalence of Technical Regulations Associated with Food Inspection and Certification Systems
# addRow = data.frame(meetingCount_cac = c(24, 27),
#                     meeting_cac = c('CAC24', 'CAC27'),
#                     standards_extracted = rep('Proposed Draft Guidelines on the Judgement of Equivalence of Technical Regulations Associated with Food Inspection and Certification Systems', 2),
#                     Reference = rep(0, 2),
#                     Title = rep('Proposed Draft Guidelines on the Judgement of Equivalence of Technical Regulations Associated with Food Inspection and Certification Systems', 2),
#                     year = c(2000, 2003),
#                     standards_extracted_clean = rep('Proposed Draft Guidelines on the Judgement of Equivalence of Technical Regulations Associated with Food Inspection and Certification Systems', 2),
#                     committee = rep('CCFICS', 2),
#                     handcodedDummy = rep(1, 2),
#                     meeting_num = c('CCFICS8', 'CCFICS12'))

addRow = data.frame(meetingCount_cac = c(24),
                    meeting_cac = c('CAC24'),
                    standards_extracted = 'Proposed Draft Guidelines on the Judgement of Equivalence of Technical Regulations Associated with Food Inspection and Certification Systems',
                    Reference = 0,
                    Title = 0,
                    year = 2000,
                    standards_extracted_clean = 'Proposed Draft Guidelines on the Judgement of Equivalence of Technical Regulations Associated with Food Inspection and Certification Systems',
                    committee = 'CCFICS',
                    handcodedDummy =1,
                    meeting_num = 'CCFICS8')
names(addRow) = names(proposed)
proposed = rbind(proposed, addRow)


# remove duplicate
proposed = proposed[-which(proposed$standards_extracted_clean == 'Proposed Draft Code of Hygienic Practice for Aseptically Processed and Packaged Foods'),]

# add proposal data for quick frozen lobster; appears that there is a standard for quick frozen lobster,
proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Amendment to the Standard for Quick Frozen Lobsters'))] = 'Proposed Draft Code for Lobsters'
proposed = proposed[-which(proposed$standards_extracted == 'Proposed Draft Standards for quick Frozen Lobsters, Rock Lobsters, Spiny Lobsters and Slipper Lobsters and Canned Sardines and Sardine-Type Products'),]
addRow = data.frame(9, 'CAC9', 'Proposed Draft Standard for Quick Frozen Lobsters', 'CODEX STAN 95-1981', 'Standard for Quick Frozen Lobsters', 1971, 'Proposed Draft Code for Lobsters', 'CCFFP', 1, 'CCFFP6')
names(addRow) = names(proposed)
proposed = rbind(proposed, addRow)

# revise proposal data for Proposed Draft Code of Practice for the Safe Use of Active Chlorine
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Code of Practice for the Safe Use of Active Chlorine' & proposed$meetingCount_cac == 26), c( 'meeting_num', 'year')] = c('CCFAC35', 2003)



# revise proposal data for Proposed Draft Standard for Olive Oil and Olive Pomace Oils
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Standard for Olive Oil and Olive Pomace Oils'), c('meetingCount_cac', 'meeting_cac', 'year',  'committee', 'handcodedDummy', 'meeting_num')] = c(3, 'CAC3', 1965, 'CCFO', 1, 'CCFO1')
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Code of Practice for the Safe Use of Active Chlorine' & proposed$meetingCount_cac == 27), 'year' ] = 2004


# ----------------- Discrepancies that Cindy resolved but should ask Seb
# # looked in CAC 24 and looks like entry for "Proposed Draft Guideline for the Conduct of Food Safety Assessment of Recombinant-dna Microorganisms' is not mentioned there, so removed it
# proposed = proposed[-which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Guideline for the Conduct of Food Safety Assessment of Recombinant-dna Microorganisms') ),]
# 
# looks like 'Proposed Draft Code of Practice for Refrigerated Packaged Foods with Extended Shelf- Life' is a part of the 'Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf Life' which was passed in 1999
# proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Practice for Refrigerated Packaged Foods with Extended Shelf- Life' ) ), c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf-life', 'Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf Life', 'CAC/RCP 46-1999')
# 
# # looks like 'Proposed Draft List of Class Titles for Food Additives' is a part of the 'Class Names and the International Numbering System for Food Additives' which was last revised in 2018
# proposed[which(proposed$standards_extracted %in% c('Proposed Draft List of Class Titles for Food Additives') ), c('Title', 'Reference')] = c('Class Names and the International Numbering System for Food Additives', 'CAC/GL 36-1989')
# proposed[which(proposed$standards_extracted %in% c('Revised Proposed Draft List of Class Titles for Food Additives') ), 'standards_extracted_clean'] = 'Proposed Draft Amendments/Revisions to the International Numbering (INS) System for Food Additives'
# 
# # looks like 'Proposed Draft Revised Standard for Gluten-Free Foods' is linked to the 'Standard for Foods for Special Dietary Use for Persons Intolerant to Gluten' and 'incorrectly' duplicated in 22, so remove the incorrect duplicate
# # proposed = proposed[-which(proposed$standards_extracted %in% c( 'Proposed Draft Revised Standard for Gluten-Free Foods') ),]
#  
# # according to CAC 19, Proposed Draft Codex Codes of Practice for the Packaging. Transport, Control and Inspection of Tropical Fresh Fruits and Vegetables
# # was subsequently split into one code which dealt with packaging and transport and another which deals with control and inspection
# # it seems to me that it would be sensible to link Proposed Draft Codex Codes of Practice for the Packaging. Transport, Control and Inspection of Tropical Fresh Fruits and Vegetables
# # to both standards (actually doesn't look like we have data on control and insepction standard) instead of considering it a failed proposal
# proposed[proposed$standards_extracted_clean == 'Proposed Draft Codex Codes of Practice for the Packaging. Transport, Control and Inspection of Tropical Fresh Fruits and Vegetables', c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Code of Practice for the Packaging and Transport of Tropical Fresh Fruits and Vegetables', 'Code of Practice for the Packaging and Transport of Fresh Fruit and Vegetables', 'CAC/RCP 44-1995')
# 
# # from CAC 24, looks like 'Proposed Draft List of Acceptable Previous Cargoes and Banned Immediate Previous Cargoes for inclusion in the Code of Practice for the Storage and Transport of Fats and Oils in Bulk'
# # is associated with the codex code : 'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk'
# proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft List of Acceptable Previous Cargoes and Banned Immediate Previous Cargoes for Inclusion in the Code of Practice for the Storage and Transport of Fats and Oils in Bulk') ), c('standards_extracted_clean', 'Title', 'Reference')]   = c('Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes', 'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk', 'CAC/RCP 36-1987')
# 
# # from CAC 28, looks like 'Proposed Draft Revision of the Codex Guidelines for Generic Official Certificate Formats and the Production and Issuance of Certificates'
# # is associated with the codex 'Guidelines for Design, Production, Issuance and Use of Generic Official Certificates'
# proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revision of the Codex Guidelines for Generic Official Certificate Formats and the Production and Issuance of Certificates') ), c('standards_extracted_clean', 'Title', 'Reference')]  = c('Proposed Draft Revision to the Guidelines for Generic Official Certificate Formats and Design, Production, Issuance and Use of Certificates', 'Guidelines for Design, Production, Issuance and Use of Generic Official Certificates', 'CAC/GL 38-2001')
# 


# # from CAC 15, draft for mango juice indeed looks like its discontinued, have chosen to make the standard_extracted_clean variable 'Discontinuation of work...' to note that explictely
# proposed$standards_extracted_clean[which(proposed$standards_extracted_clean == 'Proposed Draft standards for Mango Juice')] = 'Discontinuation of Work on Proposed Draft Standard for Mango Juice'
# 
# 
# # this extracted standard refers to CODEX STAN 292-2008 in its name so adjusted 'Title' and 'Reference' variables accordingly
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Performance Criteria for Reference and Confirmatory Methods for Marine Biotoxins (section I-8.6 Determination of Marine Biotoxins) in the Standard for Live and Raw Bivalve Molluscs (codex Stan 292-2008)'),
#          c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Standard for Live and Raw Bivalve Molluscs','Standard for Live and Raw Bivalve Molluscs',  'CODEX STAN 292-2008')
# 
# # from CAC 17, looks like draft standard for dried shark fins and quick frozen squid are separate
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Standards for Quick Frozen Squid and Dried Shark Fins'),c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Standard for Quick Frozen Squid','Standard for Quick Frozen Raw Squid',  'CODEX STAN 191-1995')
# 
# proposed = rbind(proposed, data.frame(meetingCount_cac = 18, meeting = 'CAC18', standards_extracted = 'Proposed Draft Standards for Dried Shark Fins', Reference = 'CODEX STAN 189-1993', Title = 'Standard for Dried Shark Fin', year = 1989, standards_extracted_clean = 'Proposed Draft Standard for Shark Fins'))
# 
# # from CAC 14, looks like draft standard 'Proposed Draft Code of Practice for the Handling of Quick Frozen Foods During Transport' is connected to code 'Code of Practice for the Processing and Handling of Quick Frozen Foods'
# 
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Code of Practice for the Handling of Quick Frozen Foods During Transport'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft Recommended International Code of Practice for the Processing and Handling of Quick Frozen Foods', 'Code of Practice for the Processing and Handling of Quick Frozen Foods', 'CAC/RCP 8-1976')
# 
# # from CAC 36, looks like Proposed Draft Maximum Level for Deoxynevalenol (don) in Cereal-based Foods for Infants and Young Children linked to Standard for Processed Cereal-Based Foods for Infants and Young Children
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Maximum Level for Deoxynevalenol (don) in Cereal-based Foods for Infants and Young Children'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft Standard for Processed Cerealbased Foods for Infants and Young Childrens', 'Standard for Processed Cereal-Based Foods for Infants and Young Children', 'CODEX STAN 74-1981')
# 
# # needed change evident from standard_extracted title
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Microbiological Criteria for Powdered Follow-up Formulae and Formulae for Special Medical Purposes for Young Children (annex Ii to the Code of Hygienic Practice for Powdered Formulae for Infants and Young Children (cac/rcp 66-2008))'), c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Code of Hygienic Practice for Powdered Formulae for Infants and Young Children', 'Code of Hygienic Practice for Powdered Formulae for Infants and Young Children', 'CAC/RCP 66-2008')
# 
# 
# # no clear link from CAC 26 or CAC 30 but seems sensible
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Standard for Live and Processed Bivalve Molluscs'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft Standard for Live and Raw Bivalve Molluscs', 'Standard for Live and Raw Bivalve Molluscs', 'CODEX STAN 292-2008')
# 
# # from CAC 35 and CAC 36, seems like both
# # 'Proposed Draft General Principles for Establishing Nutrient Reference Values (nrvs-ncd) for the General Population; and Consolidated Version of the General Principles for Establishing Nutrient Reference Values',
# # 'Proposed Draft General Principles for Establishing Nutrient Reference Values for Nutrients Associated with the Risk of Diet-related Non-communicable Diseases for General Population
# # belong to
# 
# proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft General Principles for Establishing Nutrient Reference Values (nrvs-ncd) for the General Population; and Consolidated Version of the General Principles for Establishing Nutrient Reference Values',
#                                                          'Proposed Draft General Principles for Establishing Nutrient Reference Values for Nutrients Associated with the Risk of Diet-related Non-communicable Diseases for General Population,')), c('standards_extracted_clean', 'Title', 'Reference') ] =matrix(rep(c('Proposed Draft Guidelines on Nutrition Labelling', 'Guidelines on Nutrition Labelling', 'CAC/GL 2-1985') , each = 2), nrow = 2)
# 
# # not 100 certain, but from CAC 19 and CAC36 and CAC 31 Proposed Draft Nutrient Reference Values for Food Labelling Purposes seems to be related to Guidelines on Nutrition Labelling
# proposed[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Nutrient Reference Values for Food Labelling Purposes',
#                                                           'Proposed Draft Nutrient Reference Values (nrvs)'))
#          , c('standards_extracted_clean', 'Title', 'Reference') ] = matrix(rep(c('Proposed Draft Guidelines on Nutrition Labelling', 'Guidelines on Nutrition Labelling', 'CAC/GL 2-1985'), each =2), nrow = 2)
# 
# 
# # from CAC 20 and CAC 21 it looks like Proposed Draft Code of Practice for Street Foods in Africa is attached to CAC/GL 22R-1997
# 
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Code of Practice for Street Foods in Africa'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft (regional) Code of Hygienic Practice for Street Foods - Africa', 'Regional Guidelines for the Design of Control Measures for Street-Vended Foods (Africa)', 'CAC/GL 22R-1997')
# 
# 
# # CAC 18 suggests that Proposed Draft Code of Practice for Street-vended Foods was iniiated in CAC18 with the goal of creating a common core standard that could later be adapted into regional standards
# # As such, it seems reasonable that the proposal in CAC 18 could be seen as the starting point for all the different subsequent regional standards
# 
# 
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Code of Practice for Street-Vended Foods'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft (regional) Code of Hygienic Practice for Street Foods - Africa', 'Regional Guidelines for the Design of Control Measures for Street-Vended Foods (Africa)', 'CAC/GL 22R-1997')
# 
# proposed = rbind(proposed, data.frame(meetingCount_cac = rep(18, 2),
#                                       meeting = rep('CAC18', 2),
#                                       standards_extracted = 'Proposed Draft Code of Practice for Street-Vended Foods',
#                                       Reference = c('CAC/RCP 43R-1995', 'CAC/RCP 71R-2013'),
#                                       Title = c('Regional Code of Hygienic Practice for the Preparation and Sale of Street Foods (Latin America and the Caribbean)', 'Regional Code of Practice for Street-vended Foods (Near East)'),
#                                       year = rep(1989, 2),
#                                       standards_extracted_clean = c('Proposed Draft Revised Regional Code of Practice for the Preparation and Sale of Street-vended Foods (latin America and the Caribbean)', 'Proposed Draft Regional Code of Practice for Street Vended Foods (near East)' )))
# 

# # from CAC 33
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Principles and Guidelines for the Conduct of Assessment of Foreign Official Inspection and Certification Systems (annex to the Guidelines for the Design, Operation, Assessment and Accreditation of Food Import and Export Inspection and Certification Systems (cac/gl 26-1997))'), c('Reference', 'Title')] = c('CAC/GL 26-1997', 'Guidelines for the Design, Operation, Assessment and Accreditation of Food Import and Export Inspection and Certification Systems')

# # from Codex website
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Microbiological Criteria for Listeria Monocytogenes in Ready-to-eat Foods (annex Ii to the Guidelines on the Application of General Principles of Food Hygiene to the Control of Listeria Monocytogenes in Ready-to-eat Foods (cac/gl 61-2007)11'), c('Title', 'Reference')] = c('Guidelines on the Application of General Principles of Food Hygiene to the Control of Listeria Monocytogenes in Foods', 'CAC/GL 61-2007')
# 
# # based on CAC 36, and CAC 38, looks like these ayran proposals are the same
# proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Standard for Ayran'), c('standards_extracted_clean')] = c('Regional Standard for Ayran (proposed Draft)')
# 
# # note that the years for CODEX STAN 192 should be fixed to 1995
# proposed[grep('CODEX STAN 192', proposed$Reference), 'Reference'] = 'CODEX STAN 192-1995'
# 
# # Note that from CAC 36,  'Proposed Draft Revision of the Procedure for the Inclusion of Additional Species in Standards for Fish and Fishery Products' belongs in the Procedurual Manual, Section II: Elaboration of Codex Standards and Related Texts: Guidelines for the Inclusion of Specific Provisions in Codex Standards and Related Texts
# 
# 

# # remove duplicates
# proposed = proposed[-which(duplicated(proposed)),]


# write.csv(proposed, file = paste0(pathMain, '/participation_development/CAC_standards_clean_May2019.csv'), row.names = FALSE)
# proposed = read.csv( paste0(pathMain, '/participation_development/CAC_standards_clean_May2019.csv'))


#-------------------------------------------
# add in proposal data that was found through atomizing codex texts
# ------------------------------------------
names(proposed)[c(2:3,7, 10)] = c('meeting_cac', 'proposal', 'proposal_clean', 'Meeting')
proposed$step = NA
proposed = proposed[, -which(names(proposed) == 'committee')]


# some reference names have the wrong years; change them
proposed$Reference = as.character(proposed$Reference)
proposed[grep('CODEX STAN 192', proposed$Reference), 'Reference'] = 'CODEX STAN 192-1995'


## add in proposals that Cindy found from automizing search through codex texts
# findMissingProposals = merge(unique(proposed[, c('Title', 'Reference', 'standards_extracted_clean')]), unique(std[, c('Reference', 'Title')]), by = c('Title', 'Reference'), all = TRUE )
# save(findMissingProposals, file = paste0(pathMain, '/participation_development/findMissingProposals.rda'))
load(file = paste0(pathData, '/fillProposalClean.rda'))  
fillProposalClean$proposal = paste0('Proposed Draft ', fillProposalClean$Title )
fillProposalClean$proposal_clean = fillProposalClean$proposal
fillProposalClean$Meeting = gsub(' [A-z ]*', '' ,  fillProposalClean$Meeting)
fillProposalClean$Meeting = gsub('/Codex:|/WHO', '' ,  fillProposalClean$Meeting)
fillProposalClean$handcodedDummy = 1
fillProposalClean = fillProposalClean[, names(proposed)]


# fix Reference data that has wrong years
fillProposalClean[grep('CODEX STAN 19-', fillProposalClean$Reference),'Reference'] = 'CODEX STAN 19-1981'
fillProposalClean[which(fillProposalClean$Title == 'Standard for Quick Frozen Bilberries'), c('meetingCount_cac', 'year', 'meeting_cac', 'Meeting', 'step', 'handcodedDummy')]  = c('5',  '1966', 'CAC5', 'GEQFF2', '1', 1)

proposed = rbind(proposed, fillProposalClean)

# get rid of duplicates
proposalsThatLedToStandards = proposed[which(proposed$Reference !=0),]
deDupedProposalsThatLedToStandards = findDupes(proposalsThatLedToStandards, 'Reference')

proposalsUnadopted = proposed[which(proposed$Reference ==0),]
#deDupedUnadoptedProposals =  findDupes(proposalsUnadopted , 'proposal_clean')

proposed = rbind(deDupedProposalsThatLedToStandards,
                 proposalsUnadopted)

proposed$Title = as.character(proposed$Title)
proposed$Title[which(proposed$Title == "Code of Hygienic Practice for Aseptically Processed and Packaged Low Acid Foods")] = 'Code of Hygienic Practice for Aseptically Processed and Packaged Low-Acid Foods'
proposed$Title[which(proposed$Title == "Guidelines for the Control of Nontyphoidal Salmonella Spp in Beef and Pork Meat")] = 'Guidelines for the Control of Nontyphoidal Salmonella spp. in Beef and Pork Meat'
proposed$Title[which(proposed$Title == "Code of Hygienic Practice for Low Moisture Foods")] = 'Code of Hygienic Practice for Low-Moisture Foods'
proposed$Title[which(proposed$Title == "Standard for Quick Frozen Blocks of Fish Fillets Minced Fish Flesh and Mixtures of Fillets and Minced Fish Flesh")] = 'Standard for Quick Frozen Blocks of Fish Fillets, Minced Fish Flesh and Mixtures of Fillets and Minced Fish Flesh'
proposed$Title[which(proposed$Title == "Standard for Jams Jellies and Marmalades")] = 'Standard for Jams, Jellies and Marmalades'
proposed$Title[which(proposed$Title == "Standard for Live Abalone and for Raw Fresh Chilled or Frozen Abalone for Direct Consumption or for Further Processing")] = 'Standard for Live Abalone and for Raw Fresh Chilled or Frozen Abalone for Direct Consumption or for further Processing'
proposed$Title[which(proposed$Title == "MISSING")] = 'Code of Hygienic Practice for Eggs and Egg Products'
proposed$Title[which(proposed$Title == "Guidelines for the Design Operation Assessment and Accreditation of Food Import and Export Inspection and Certification Systems")] = 'Guidelines for the Design, Operation, Assessment and Accreditation of Food Import and Export Inspection and Certification Systems'
proposed$Title[which(proposed$Title == "General Standard for Vegetable Protein Products Vpp")] = 'General Standard for Vegetable Protein Products (VPP)'
proposed$Title[which(proposed$Title == "Standard for Dried Shark Fin")] = 'Standard for Dried Shark Fins'
proposed$Title[which(proposed$Title == "Standard for Saint Paulin")] = 'Standard for Saint-Paulin'
proposed$Title[which(proposed$Title == "General Guidelines for Use of the Term Halal")] = 'General Guidelines for Use of the Term "Halal"'
proposed$Title[which(proposed$Title == "Guidelines for the Control of Trichinella Spp in Meat of Suidae")] = 'Guidelines for the Control of Trichinella Spp. in Meat of Suidae'
proposed$Title[which(proposed$Title == "Guidelines for the Exchange of Information Between Countries on Rejections of Imported Foods")] = 'Guidelines for the Exchange of Information between Countries on Rejections of Imported Foods'
proposed$Title[which(proposed$Title == "Harmonized Iupac Guidelines for Single Laboratory Validation of Methods of Analysis")] = 'Harmonized IUPAC Guidelines for Single-Laboratory Validation of Methods of Analysis'
proposed$Title[which(proposed$Title == "Harmonized Iupac Guidelines for the Use of Recovery Information in Analytical Measurement")] = 'Harmonized IUPAC Guidelines for the Use of Recovery Information in Analytical Measurement'
proposed$Title[which(proposed$Title == "Regional Standard for Date Paste Near East")] = 'Regional Standard for Date Paste (Near East)'
proposed$Title[which(proposed$Title == "Standard for Aqueous Coconut Products Coconut Milk and Coconut Cream")] = 'Standard for Aqueous Coconut Products - Coconut Milk and Coconut Cream'
proposed$Title[which(proposed$Title == "Standard for Degermed Maize Corn Meal and Maize Corn Grits")] = 'Standard for Degermed Maize (Corn) Meal and Maize (Corn) Grits'
proposed$Title[which(proposed$Title == "Standard for Edible Fats and Oils Not Covered By Individual Standards")] = 'Standard for Edible Fats and Oils not Covered by Individual Standards'
proposed$Title[which(proposed$Title == "Standard for Smoked Fish Smoke Flavoured Fish and Smoke Dried Fish")] = 'Standard for Smoked Fish, Smoke-Flavoured Fish and Smoke-Dried Fish'
proposed$Title[which(proposed$Title == "Standard for Special Dietary Foods with Low Sodium Content including Salt Substitutes")] = 'Standard for Special Dietary Foods with Low-Sodium Content (Including Salt Substitutes)'
proposed$Title[which(proposed$Title == "Standard for Samsø")] = "Standard for Samso"

proposed$proposal_clean = as.character(proposed$proposal_clean)
proposed$proposal_clean[which(proposed$proposal_clean %in% c("Proposed Draft standards for Mango Juice",
                                                             'Discontinuation of Work on Proposed Draft Standard for Mango Juice'))] = "Proposed Draft Standard for Mango Juice"

proposed$Meeting = as.character(proposed$Meeting)

#-------------------------------------------
# add in proposal data that was found manually 
# ------------------------------------------
# fill in missing committee data manually
proposed[which(proposed$Title == 'Standard for Follow-up formula'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('11', 'CAC11', '1975', 'CCNFSDU9', '3', 1)
proposed[which(proposed$Title == 'Standard for Cocoa (Cacao) Mass (Cocoa/Chocolate Liquor) and Cocoa Cake'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy' )] = c('3', 'CAC3', '1965', 'CCCPC3', '3', 1)
proposed[which(proposed$Title == 'General Standard for the Labelling of and Claims for Prepackaged Foods for Special Dietary Uses'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('13', 'CAC13', '1979', 'CCFL13', '5', 1)
proposed[which(proposed$Title == 'Standard for Canned Crab Meat'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('7', 'CAC7', '1969', 'CCFFP4', '3', 1)
proposed[which(proposed$Title == 'Standard for Edible Fungi and Fungus Products'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('8', 'CAC8', '1971', 'CCPFV8', '', 1)
proposed[which(proposed$Title == 'General Standard for the Labelling of Food Additives when sold as such'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('9', 'CAC9', '1972', 'CCFAC8', '1', 1)
proposed[which(proposed$Title == 'Regional Code of Practice for Street-vended Foods (Near East)'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('24', 'CAC24', '2001', 'CCNEA1', '1', 1)
proposed[which(proposed$Title == 'Code of Hygienic Practice for Eggs and Egg Products'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('4', 'CAC4', '1966', 'CCFH3', '1', 1)
proposed[which(proposed$Title == 'Code of Hygienic Practice for Groundnuts (Peanuts)'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('9', 'CAC9', '1972', 'CCFH9', '1', 1)
proposed[which(proposed$Title == 'Code of Practice for Radiation Processing of Food'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c('10', 'CAC10', '1974', 'CCFH11', '', 1) # in CCFH14 says that they do this in CCFH11


# get final missing proposal data manually coded from Seb and Cindy
missingAll = read.csv(paste0(pathMain, '/participation_development/standardsMissingProposalsMay2019_SK.csv'), stringsAsFactors = FALSE, na.strings = '.')
missingAll = missingAll[, -which(names(missingAll) == 'X')]
names(missingAll)[grep('committe_year', names(missingAll))] = "committee_year"
missingAll$step = NA
missingAll$handcodedDummy = 1
missingAll[grep('Code of Ethics', missingAll$text), c("committee", "committee_year")] = c('CCGP5', '1976')
missingAll[grep('Fish and Fishery', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCFFP23', '1998', '3') # note This Code of Practice for Fish and Fishery Products has been developed by the Codex Committee on Fish and Fishery Products from the merging of current individual codes of practice plus sections on aquaculture and frozen surimi. 
missingAll[grep('Guide for the Microbiological Quality of Spices and Herbs', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCPMPP13', '1984', '1') 
missingAll[grep('Recombiant-DNA Microorganisms', missingAll$text), c('committee', 'committee_year', 'step')] = c('TFFBT2', '2001', '1') 
missingAll[grep('Campylobacter and Salmonella in Chicken Meat', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCFH40', '2008', '1') 
missingAll[grep('Countries on Rejections of Imported Foods', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCFICS1', '1992', '1') 
missingAll[grep('Performance Criteria for Methods of Analysis', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCPR', '2013', '1/2/3') 
missingAll[grep('Guidelines on Substances used as Processing Aids', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCFA40', '2008', '1/2/3') 
missingAll[grep('General Principles of Food Hygiene to the Control of Listeria Monocytogenes in Foods', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCFH35', '2003', '3') #"it was also proposed to split the guidelines into two new documents, namely, one document that contained general guidance for managing Listeria monocytogenes in foods and another document on the specific microbiological criteria on Listeria monocytogenes for foods in international trade."
missingAll[grep('Compilation of Codex texts relevant to the labelling of foods derived from modern biotechnolog', missingAll$text), c('committee', 'committee_year', 'step')] = c('CCFL39', '2011', '5/8') # couldn't find anything earlier
missingAll[grep('General Standard for Cheese', missingAll$text), c('committee', 'committee_year')] = c('CGECPMMP1', '1958' ) 
missingAll[grep('List of Codex Specifications for Food Additives', missingAll$text), c('committee', 'committee_year')] = c('CCFAC', '1989' ) 


filledSeb = missingAll[c(51:116),]
filledSeb[grep('Emergency', filledSeb$text), c('committee', 'committee_year')] = c('CCFICS1', '1992')
filledSeb[grep('trade', filledSeb$text), c('committee', 'committee_year')]# = c('CCFICS1', '1992')
filledCindy = read.csv(paste0(pathMain, '/participation_development/standardsMissingProposalsMay2019_CC.csv'), stringsAsFactors = FALSE)
filledCindy  = filledCindy[, -which(names(filledCindy) %in% c('text', 'code'))]
filledCindy$handcodedDummy = 1
filledRest = missingAll[which(missingAll$text %in% setdiff(missingAll$text[1:50], filledCindy$text_original)),]
filledRest[grep('General Methods of Analysis for Food Additives', filledRest$text), c('committee', 'committee_year', 'step', 'cac')] = c('CCMAS23', 2001, 1, 'CAC24')
filledRest[grep('Portion of Commodities to which Maximum Residues Limits Apply and which is Analyzed', filledRest$text), c('committee', 'committee_year')] = c('CCPR11', 1979)
names(filledCindy)[which(names(filledCindy) == 'committee_proposal_year')] = 'committee_year'
names(filledCindy)[which(names(filledCindy) == 'text_original')] = 'text'
names(filledCindy)[which(names(filledCindy) == 'reference_original')] = 'code'
filled = rbind(filledSeb, filledRest, filledCindy[, names(filledSeb)] )
filled$text[which(filled$text == "Standard for SamsÃ¸")] = "Standard for Samso"
 
proposedFill = proposed[which(proposed$Title %in% filled$text),]
proposedFill = proposedFill[order(proposedFill$Title),]
proposedFill$tmpMeeting = proposedFill$Meeting 
proposedFill$tmpStep = proposedFill$step
proposedFill$tmpYear = proposedFill$year

# add in updated missing proposal data manually coded from Seb and Cindy
filled = filled[order(filled$text),]
filledSub = filled[which(filled$text %in% proposedFill$Title),]
 
  
proposedFill$Meeting = filledSub$committee
proposedFill$Meeting[which(is.na(proposedFill$Meeting))] = proposedFill$tmpMeeting[which(is.na(proposedFill$Meeting))]
proposedFill$step = filledSub$step
proposedFill$step[which(is.na(proposedFill$step))] =  proposedFill$tmpStep[which(is.na(proposedFill$step))]
proposedFill$year = filledSub$committee_year
proposedFill$year[which(is.na(proposedFill$year))] = proposedFill$tmpYear[which(is.na(proposedFill$year))]
proposedFill$handcodedDummy = 1
proposedFill = proposedFill[,- grep('tmp', names(proposedFill)),]


# rbind in missing proposal data for which there was no previous proposal data at all
filledRest = filled[-which(filled$text %in% proposed$Title),] 
names(filledRest)[c(2, 3, 5, 7, 8)] = c('Title', 'Reference', 'meeting_cac', 'Meeting', 'year')
filledRest$meetingCount_cac = gsub('[A-Z]', '', filledRest$meeting_cac)
filledRest$proposal = paste0('Proposed ', filledRest$Title)
filledRest$proposal_clean = filledRest$proposal

# note there are still 7 standards that don't have info for, remove for now
filledRest = filledRest[-which(is.na(filledRest$year)),]

 
# add in cac data for standards that previously we had no data for
dates = read.dta13(paste0(pathMain, '/participation_development/codex_event_dates.dta'))
cac = dates[grep('CAC', dates$event_short),]
filledRest[which(is.na(filledRest$meeting_cac)),'meeting_cac'] = cac$event_number[match(filledRest$year[which(is.na(filledRest$meetingCount_cac))], cac$year)]
filledRest[which(is.na(filledRest$meeting_cac)),'meeting_cac'] = c('CAC22', 'CAC17', 'CAC21')
filledRest[which(is.na(filledRest$meetingCount_cac)),'meetingCount_cac'] = gsub('[A-Z]', '', filledRest[which(is.na(filledRest$meetingCount_cac)),'meeting_cac'] )


# --------------------------------------
# create final cleaned proposal data
# ---------------------------------------
proposedAll = rbind(proposedFill, proposed[-which(proposed$Title %in% filled$text),], filledRest[, names(proposed)] )  
proposedAll$year = as.numeric(proposedAll$year)
proposedAll$meetingCount_cac = as.numeric(proposedAll$meetingCount_cac)

proposedAll[which(proposedAll$Reference == 'CAC/RCP 76R-2017'), 'Reference'] = "CAC/RCP 76-2017"
proposedAll[which(proposedAll$Reference == 'CAC/MISC 6-2018'), 'Reference'] = "CAC/MISC 6-2015"
proposedAll[which(proposedAll$Title == 'Advisory Lists of Nutrient Compounds for Use in Foods for Special Dietary Uses intented for Infants and Young Children'), 'Reference'] = 'CAC/GL 10-1979'



# match up proposals that are i) missing committees and ii) are attached to standards with the committees listed in the standards
load(file = paste0(pathMain, '/participation_development/codex_standard_development_master.rda'))

proposedAll$Meeting = gsub(' [A-z ]*', '' , proposedAll$Meeting)
proposedAll$Meeting = gsub('\\/Codex:|\\/WHO', '', proposedAll$Meeting )
proposedAll$tmpMeeting = NA
proposedAll[which(proposedAll$Meeting %>% is.na()),'tmpMeeting'] = std$event_short_raw[match(proposedAll[which(proposedAll$Meeting %>% is.na()),'Reference'], std$Reference)]
proposedAll$tmpMeeting[which(proposedAll$tmpMeeting == 'CCCF' & proposedAll$year<2007 )] = 'CCFAC'
proposedAll$tmpMeeting[which(proposedAll$tmpMeeting == 'CCFA' & proposedAll$year<2007 )] = 'CCFAC'
proposedAll$tmpMeeting[which(proposedAll$tmpMeeting == 'CCMMP' & proposedAll$year<1994 )] = 'CGECPMMP'

proposedAll$tmpMeetingNum = dates$event_number[match(paste0(proposedAll$tmpMeeting, proposedAll$year), paste0(dates$event_short, dates$year))]
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CCPFV' & proposedAll$year<1998 &  proposedAll$year > 1986)] = 'CCPFV18'
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CCCPC' & proposedAll$year<1996 &  proposedAll$year > 1982)] = 'CCCPC15'
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CCCPL' &   proposedAll$year > 1994)] = 'CCCPL9'
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CCVP' &   proposedAll$year > 1989)] = 'CCVP5'
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CGECPMMP' & proposedAll$year>1986 &   proposedAll$year < 1990 )] = 'CGECPMMP21'
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CCFO' & proposedAll$year>1987 &   proposedAll$year < 1993 )] = 'CCFO13'
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CCS' & proposedAll$year>1977 &   proposedAll$year < 2000 )] = 'CCS6'
proposedAll$tmpMeetingNum[which(proposedAll$tmpMeeting == 'CCGP' & proposedAll$year>1968 &   proposedAll$year < 1974 )] = 'CCGP3'
proposedAll$tmpMeetingNum[which(is.na(proposedAll$tmpMeetingNum))] = dates$event_number[match(paste0(proposedAll$tmpMeeting[which(is.na(proposedAll$tmpMeetingNum))], proposedAll$year[which(is.na(proposedAll$tmpMeetingNum))]-1), paste0(dates$event_short, dates$year))]

proposedAll$tmpMeetingNum[-which(is.na(proposedAll$Meeting))] = proposedAll$Meeting[-which(is.na(proposedAll$Meeting))]
proposedAll$Meeting = proposedAll$tmpMeetingNum

proposedAll = proposedAll[, -grep('tmp', names(proposedAll))]
proposedAll[proposedAll$Meeting %in% c('CCPR', 'CCFAC'),'Meeting'] = c('CCPR45', 'CCFAC21')
proposedAll$Meeting = gsub('\\,' ,'', proposedAll$Meeting)

 

# note don't need to code handcodedDummy == 1 because this is already true for most the following:
proposedAll[which(proposedAll$Title == 'Code of Practice to Minimize and Contain Antimicrobial Resistance'), c('year', 'Meeting')] = c(2001, 'CCRVDF13')
proposedAll[which(proposedAll$Title == 'Guidelines for Packing Media for Canned Fruits'), c('year', 'Meeting', 'step')] = c(1998, 'CCPFV19', '1/2/3')
proposedAll[which(proposedAll$Title == 'Standard for Chayotes'), c('year', 'Meeting', 'step')] = c(1994, 'CCFFV5', '1')

proposedAll[which(proposedAll$Title == 'Standard for Pummelos'), c('year', 'Meeting', 'step', 'meetingCount_cac', 'meeting_cac')] = c(1993, 'CCFFV4', '1/2/3', 20, 'CAC20')
proposedAll[which(proposedAll$Title == 'Standard for Kimchi'), c('year', 'Meeting', 'step')] = c(1998, 'CCPFV19', '1')
proposedAll[which(proposedAll$Title == 'Standard for Chilli Peppers'), c('Meeting')] = c( 'CCFFV14')
proposedAll[which(proposedAll$Title == 'Standard for Instant Noodles'), c('year', 'Meeting', 'step')] = c(1999, 'CCASIA12', '1/2')
proposedAll[which(proposedAll$Meeting == 'Adjourned'), c('Meeting' )] = c('CCMMP')
proposedAll[which(proposedAll$proposal == 'Proposed draft maximum residue limits (MRLs) for zilpaterol hydrochloride (cattle fat, kidney, liver, muscle)'), c('step')] = 4
proposedAll[which(proposedAll$Title == 'Standard for Canned Sardines and Sardine-Type Products'), c('year', 'Meeting', 'step', 'meetingCount_cac', 'meeting_cac', 'handcodedDummy')] = 
  c(1973, 'CCFFP8', '2' , 10, 'CAC10', 1)
proposedAll[which(proposedAll$Title == 'Standard for Ginseng Products' & proposedAll$year == 2002), c('meetingCount_cac','meeting_cac', 'handcodedDummy', 'step', 'Meeting')] = c(26, 'CAC26', 1, 1, 'CCPFV21')

proposedAll[which(proposedAll$Title == 'Standard for Guavas'), -which(names(proposedAll) %in% c('Reference', 'Title'))] = c(21, 'CAC21', 'Proposed Draft Standard for Guavas', 1994, 'Proposed Draft Standard for Guavas', 1, 'CCFFV5', 1)

proposedAll$proposal_clean[which(proposedAll$proposal_clean %in% c('Proposed Draft Guidelines for Use of the Term "natural" in Food Product Labelling',
                                                                   'Proposed Draft Guidelines for the Use of the Term "natural"'))] = 'Proposed Draft Guidelines for Use of the Term "natural" in Food Product Labelling'

proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Blocks of Fish Fillets, Minced Fish Flesh and Mixtures of Fillets and Minced Fish Flesh'),c('meetingCount_cac', 'meeting_cac', 'Meeting', 'year' )] = c('13', 'CAC13', 'CCFFP13', 1979)
proposedAll[which(proposedAll$Title == 'Principles and Guidelines for the Conduct of Microbiological Risk Management (MRM)'),c('proposal_clean', 'step' )] = c('Proposed Draft Guidance on Microbiological Risk Management Metrics to the Principles and Guidelines for the Conduct of Microbiological Risk Management', 1) # used to be called 'Recommendations for the management of microbiological hazards for foods in international trade'
proposedAll[which(proposedAll$Title == 'General Guidelines for the Utilization of Vegetable Protein Products (VPP) in Foods'),
                  c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(14, 'CAC14', 1980, 1, 'CCVP1', 1)


proposedAll[which(proposedAll$Title == 'Guidelines on the Application of Risk Assessment for Feed'),
            c('year', 'handcodedDummy', 'Meeting', 'step')] = c(2012, 1, 'TFAF6', 5)

proposedAll[which(proposedAll$Title == 'Code of Practice for the Processing and Handling of Quick Frozen Foods'),
            c( 'handcodedDummy',   'step')] = c( 1,   2)

proposedAll[which(proposedAll$Title == 'Standard for Whole Maize (Corn) Meal'),
            c( 'handcodedDummy',   'step')] = c( 1,  3)

proposedAll[which(proposedAll$Title == 'Standard for Gari'),
            c('year', 'handcodedDummy', 'Meeting',  'step')] = c(1979, 1, 'CCAFRICA4', 2)


proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Blueberries'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(5, 'CAC5', 1967, 1, 'GEQFF3', 1)
proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Broccoli'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(5, 'CAC5', 1967, 1, 'GEQFF3', 1)
proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Peaches'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(5, 'CAC5', 1967, 1, 'GEQFF3', 3)
proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Raspberries'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(5, 'CAC5', 1967, 1, 'GEQFF3', 3)
proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Brussels Sprouts'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(5, 'CAC5', 1967, 1, 'GEQFF3', 3)
proposedAll[which(proposedAll$proposal == 'Proposed Draft Standards for Fruit Juices and Nectars'),c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(24, 'CAC24', 2000, 1, 'TFFJ1', 3)

 
proposedAll[which(proposedAll$proposal == 'Proposed Draft Standard for the Labelling of and Claims for Foods for Special Medical Purposes'),] = c(17, 'CAC17', 'Proposed Standard for Labelling of and Claims for Foods for Special Medical Purposes', 'CODEX STAN 180-1991', 'Standard for Labelling of and Claims for Foods for Special Medical Purposes', '1987', 'Proposed Standard for Labelling of and Claims for Foods for Special Medical Purposes', '1', 'CCNFSDU15', 3)
proposedAll = proposedAll[-which(proposedAll$Title == 'Standard for Labelling of and Claims for Foods for Special Medical Purposes' & proposedAll$meetingCount_cac == 18),]

proposedAll$proposal_clean = gsub('Standardfor', 'Standard for',  proposedAll$proposal_clean)
proposedAll$proposal_clean = gsub('Draftregional', 'Draft Regional',  proposedAll$proposal_clean)
proposedAll$proposal_clean[which(proposedAll$proposal_clean == 'Proposed Draft Annex on Berries to the Code of Hygienic Practice for Fresh Fruits and Vegetables (cac/rcp 53-2003)')] = c('Proposed Draft Code of Hygienic Practice for Fresh Fruits and Vegetables (cac/rcp 53-2003)')
proposedAll[grep('Proposed Draft Code of Practice for Street Foods in Africa', proposedAll$proposal_clean), 'step'] = 3

proposedAll[which(proposedAll$Title == 'Standard for Edible Fats and Oils not Covered by Individual Standards'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(5, 'CAC5', 1967, 1, 'CCFO4', 5)
proposedAll[which(proposedAll$Title == 'Standard for Certain Canned Vegetables'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(28, 'CAC28', 2004, 1, 'CCPFV22', 3)
proposedAll[which(proposedAll$Title == 'Standard for Fish Sauce'), c('year', 'Meeting', 'step')] = c(2006,  'CCFFP28', 1)
proposedAll[which(proposedAll$Title == 'Standard for Certain Canned Fruits'), c( 'Meeting', 'step')] = c('CCPFV25', 1)
proposedAll[which(proposedAll$Title == 'Standard for Canned Shrimps or Prawns'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(4, 'CAC4', 1966, 'CCFFP1', 1)
proposedAll[which(proposedAll$Title == 'Standard for Canned Pineapple'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(2, 'CAC2', 1964, 'CCPFV1', 1)
proposedAll[which(proposedAll$Title == 'Standard for Raisins'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(2, 'CAC2', 1964, 'CCPFV1', 1)
proposedAll[which(proposedAll$Title == 'Standard for Canned Fruit Cocktail'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(2, 'CAC2', 1964, 'CCPFV1', 1)
proposedAll[which(proposedAll$Title == 'Standard for Cocoa Butter'), c('Meeting')] = c(  'CCCPC3')
proposedAll[which(proposedAll$Title == 'Code of Hygienic Practice for the Processing of Frog Legs'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(10, 'CAC10', 1973, 'CCFH10', 1)
proposedAll[which(proposedAll$Title == 'Guidelines for the Control of Trichinella Spp. in Meat of Suidae'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c( 34, 'CAC34', 2011, 'CCFH42', 1)
proposedAll[which(proposedAll$Title == 'Principles for the Use of Sampling and Testing in International Food Trade'), c( 'Meeting', 'step')] = c(  'CCMAS32', 1)
proposedAll[which(proposedAll$Title == 'Regional Standard for Canned Foul Medames'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(24, 'CAC24', 2001,   'CCNEA1', 1)
proposedAll[which(proposedAll$Title == 'Regional Standard for Canned Humus with Tehena'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(24, 'CAC24', 2001,   'CCNEA1', 1)
proposedAll[which(proposedAll$Title == 'Standard for Canned Applesauce'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(2, 'CAC2', 1964, 'CCPFV1', 1)
proposedAll[which(proposedAll$Title == 'Standard for Degermed Maize (Corn) Meal and Maize (Corn) Grits'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(14, 'CAC14', 1980, 'CCCPL1', 1)

proposedAll[which(proposedAll$Title == 'Standard for Whole and Decorticated Pearl Millet Grains'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step')] = c(14, 'CAC14', 1981, 'CCAFRICA5', 1)

proposedAll[which(proposedAll$Title =='Code of Hygienic Practice for Powdered Formulae for Infants and Young Children'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy') ] = c(27, 'CAC27', 2004, 'CCFH36', '1/2/3', 1)

proposedAll[which(proposedAll$Title == "Standard for Certain Canned Citrus Fruits"),c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(24, 'CAC24', 2000, 'CCPFV20', 1, 1)

proposedAll[which(proposedAll$Title == "Standard for Canned Strawberries"),c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(2, 'CAC2', 1964, 'CCPFV1', 1, 1)



proposedAll[which(proposedAll$Title == 'Code of Hygienic Practice for Canned Fruit and Vegetable Products'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(3, 'CAC3', 1965, 'CCFH2', 3, 1)
proposedAll[which(proposedAll$Title == 'Risk Analysis Principles and Procedures Applied By the Codex Committee on Food Hygiene'), c('step')] = 1

proposedAll[which(proposedAll$Title == 'Standard for Cooked Cured Ham'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(5, 'CAC5', 1967, 'CCPMPP2', 1, 1)

proposedAll[which(proposedAll$Title == 'Standard for Dairy Fat Spreads'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(22, 'CAC22', 1996, 'CCMMP2', '1/2/3', 1) # previously known as Standard for Dairy Spreads
 
proposedAll[which(proposedAll$Title == 'Standard for Edible Casein Products'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(9, 'CAC9', 1971, 'CGECPMMP14', '1', 1) # switched committees
 
proposedAll[which(proposedAll$Title == 'Standard for Mangosteens'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(20, 'CAC20', 1991, 'CCFFV3', '1', 1) 


proposedAll[which(proposedAll$Title == 'Standard for Processed Tomato Concentrates'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(4, 'CAC4', 1966, 'CCPFV3', '1', 1) 
proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Strawberries'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(4, 'CAC4', 1966, 'GEQFF2', '3', 1) 

proposedAll[which(proposedAll$Title == 'Standard for Whey Powders'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(17, 'CAC17', 1986, 'CGECPMMP21', '1', 1) 

 
proposedAll[which(proposedAll$Title == 'Guidelines for the Control of Taenia Saginata in Meat of Domestic Cattle'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(34, 'CAC34', 2010, 'CCFH42', '1', 1)  # used to be called Guidelines for Control of Specific Zoonotic Parasites in Meat: Cysticercus bovis

proposedAll[which(proposedAll$Title == 'Regional Standard for Chilli Sauce'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(30, 'CAC30', 2006, 'CCASIA15', '1', 1)   

proposedAll[which(proposedAll$Title == 'Standard for Ginger'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(22, 'CAC22', 1996, 'CCFFV6', '1', 1)   


proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen French Fried Potatoes'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(11, 'CAC11', 1974, 'GEQFF9', '2', 1)   

proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Green and Wax Beans'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(10, 'CAC10', 1973, 'GEQFF8', '1', 1)   

proposedAll[which(proposedAll$Title == 'Standard for Cooked Cured Chopped Meat'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(9, 'CAC9', 1972, 'CCPMPP6', '1', 1)   

proposedAll[which(proposedAll$Title == 'Standard for Smoked Fish, Smoke-Flavoured Fish and Smoke-Dried Fish'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(23, 'CAC23', 1998, 'CCFFP23', '1', 1)   


proposedAll[which(proposedAll$Title == 'Standard for Pearl Millet Flour'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(14, 'CAC14', 1981, 'CCAFRICA5', 1, 1)


proposedAll[which(proposedAll$Title == 'Standard for Edible Cassava Flour'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(16, 'CAC16', 3, 1985, 'CCAFRICA7')

# --------------------------
# Remove irrelevant data
# --------------------------
# remove duplicates
proposedAll = proposedAll[-which(proposedAll$proposal_clean == "Proposed Draft African Regional Standard for Processed Couscous" & proposedAll$year == 1992),]

proposedAll = proposedAll[-which(proposedAll$proposal_clean %in%  c('Discontinuation of Work on Draft and Proposed Draft Food Additive Provisions of the Gsfa',
                                                                    'Proposed Draft Schedules of Food Additives Specifically Permitted in Foods',
                                                                    "Proposed Draft St Andards for Blackcurrant Juice, Concentrated Blackcurrant Juice and Pulpy Nectars of Certain Small Fruits",
                                                                    'Proposed Draft Group Standard for Unripened Cheese including Fresh Cheese', #  duplicated by Proposed Draft Code of Hygienic Practice for Uncured/unripened Cheese and Ripened Soft Cheese   
                                                                    'Proposed Draft Standard for Tannia')),] # duplicated by Proposed Draft Codex Standard for Tiquisque (White and Lilac)

# remove Proposed Draft Guidelines for Testing Safety and Nutritional Quality of Vegetable Protein Products because this is under broader standard for  'Draft Guidliens for the Utilization of Vegetable Protein Products"# this became part of Standard for Vegetable Protein Products (VPP), see CCPMPP12
proposedAll = proposedAll[-which(proposedAll$proposal_clean == 'Proposed Draft Guidelines for Testing Safety and Nutritional Quality of Vegetable Protein Products'),]

 
# remove proposed amendments
proposedAll = proposedAll[-which(proposedAll$proposal_clean %in% c('Proposed Draft Amendment to the Standard for Infant Formula (vitamin B 12)',
                                                                   'Proposed Draft Amendments to Codex Standards for Foods for Infants and Children',  
                                                                   'Proposed Draft Advanced Amendment (foods That Can Cause Hypersensitivity)',
                                                                   'Proposed Draft Amendment to Sesameseed Oil',
                                                                   'Proposed Draft Amendments to Table 3 (additives with An Acceptable Daily Intake of “not Specified”)',
                                                                   'Proposed Draft Amendments to the Rules of Procedure: Duration of the Term of Office of the Members of the Executive Committee',
                                                                   'Proposed Draft Amendment to the Codex MRL Elaboration Procedure (in Relation to the Establishment of Interim MRLs)',
                                                                   'Proposed Draft Amendment on Microbiological Requirements',
                                                                   'Proposed Draft Risk Analysis Principles and Procedures Applied By the Codex Committee on Food Hygiene', # amendment according to cac 33
                                                                   'Proposed Draft List and Draft Lists of Acceptable Previous Cargoes')),] # # Proposed Draft List and Draft Lists of Acceptable Previous Cargoes at CCFO21 is an amendment is an amendment to Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk




# remove proposed revisions
proposedAll = proposedAll[-which(proposedAll$proposal_clean %in% c('Proposed Draft Revised Code of Principles Concerning Milk and Milk Products',
                                                                   'Proposed Draft Revision of the Recommended International Code of Practice for Foods for Infants and Children (cac/rcp 21-1979 – Amended 1981)',
                                                                   'Proposed Draft Revision to the Preamble of the Gsctf',
                                                                   'Proposed Draft Revision of Guideline Levels for Radionuclides in the General Standard for Contaminants and Toxins in Food and Feed Including Development of Guidance to Facilitate the Application and Implementation of the Gls',
                                                                   'Proposed Draft Revision of the Procedure for the Inclusion of Additional Species in Standards for Fish and Fishery Products',
                                                                   'Proposed Draft Revised Guideline for the Development of Maximum Levels of Use for Food Additives with Numerical Acceptable Daily Intakes',
                                                                   'Proposed Draft Revised Standard for Infant Formula [and Formulas for Special Medical Purposes Intended for Infants')),]



# remove maximum levels
proposedAll = proposedAll[-setdiff( grep('Maximum Levels|MRL|Mrl|mrl|Maximum Level for|Maximum Residue Limits|Mls|Maximum Residue Levels In/on', proposedAll$proposal_clean), grep('Recommended Methods|Code of Practice|Extrapolation', proposedAll$Title) ), ]

proposedAll = proposedAll[-which(proposedAll$proposal_clean %in% c(
                          'Proposed Draft Codex Advisory Specifications for the Identity and Purity of Food Additives',
                          'Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (% V/v)')),]
 

# remove proposed annexes
proposedAll = proposedAll[-grep('Annex', proposedAll$proposal_clean), ]

proposedAll = proposedAll[-which(proposedAll$proposal_clean %in% c(
  'Proposed Draft Principles and Guidelines for Imported Food Inspection Based on Risk', # Proposed Draft Principles and Guidelines for Imported Food Inspection Based on Risk is an annex to Guidelines for Food Import Control Systems (CAC/GL 47-2003)
  'Proposed Draft Principles for Electronic Certification' # Proposed Draft Principles for Electronic Certification is appnedix to Codex Guidelines for Generic Official Certificate Formats and the Production and Issuance of Certificate
  )),]


 

# -----------------------------------
# integrate in data for standards that were subsequently superseded by other standards
# -----------------------------------
load(file = paste0(pathMain, '/participation_development/codex_superseding_standards_clean.rda'))

# according to CAC20, Standard for Raw Cane sugar adopted at step 5 at CAC19
# but subsequently superseded by standard for sugars
superStds = rbind(superStds, data.frame(Reference_superseding_standard = 'CODEX STAN 212-1999',
                                        Reference_original_standard = 'Missing', 
                                        Title_original_standard = 'Standard for Raw Cane Sugar',
                                        Proposal_meeting_original_standard = 'CAC17'))


# extract proposal year
load(paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))
superStds$year =  dates_all$year[match(superStds$Proposal_meeting_original_standard, dates_all$event_number)]
superStds$meeting_cac = dates_all$event_number_cac[match(superStds$Proposal_meeting_original_standard, dates_all$event_number)]
superStds$meetingCount_cac = gsub('CAC', '', superStds$meeting_cac) %>% as.numeric()
superStds[grep('Raw Cane', superStds$Title_original_standard), c('year', 'meeting_cac', 'meetingCount_cac')] = c(1987, 'CAC17', 17)
superStds[which(superStds$Title_original_standard == 'Canned asparagus'),c('Proposal_meeting_original_standard', 'year', 'meeting_cac', 'meetingCount_cac')] = c('CCPFV1', 1964, 'CAC2', 2)
superStds$step = NA  

 
# identify observations for which we have unmatched proposal data to standards that were superseded
proposedAll$Reference_superseding_standard = NA
proposedAll$supersededDummy = 0
superStds$supersededDummy = 1

proposalVariables =  c('Reference', 'Title', 'Meeting', 'year', 'meetingCount_cac', 'meeting_cac', 'Reference_superseding_standard', 'supersededDummy')
superStdsVariables =  c('Reference_original_standard', 'Title_original_standard', 'Proposal_meeting_original_standard', 'year', 'meetingCount_cac', 'meeting_cac', 'Reference_superseding_standard', 'supersededDummy')


proposedAll[which(proposedAll$proposal_clean == "Proposed Draft Standard for Non-pulpy Blackcurrant Nectar"),proposalVariables] = c('CODEX STAN 101-1981', 'Non-pulpy blackcurrant nectar', 'GEFJ9', 1972, 9, 'CAC9', 'CODEX STAN 247-2005', 1)
proposedAll[which(proposedAll$proposal_clean =="Proposed Draft Standard for Liquid Pulpy Mango Products Preserved Exclusively by Physical Means"  & proposedAll$year == 1980),proposalVariables] = c(superStds[which(superStds$Title_original_standard == "Liquid pulpy mango products" ), superStdsVariables ][1:2], 'GEFJ13', 1978, 13, 'CAC13', 'CODEX STAN 247-2005', 1)

proposedAll =proposedAll[-which(proposedAll$proposal_clean =="Proposed Draft Standard for Liquid Pulpy Mango Products Preserved Exclusively by Physical Means"  & proposedAll$year == 1982),] 
proposedAll[which(proposedAll$proposal_clean =="Proposed Draft Code of Ante-mortem and Post-mortem Inspection of Slaughter Animals" ),proposalVariables]  = c('CAC/RCP 41-1993', 'Recommended International Code for Ante-Mortem and Post-Mortem Inspection of Slaughter Animals and for Ante-Mortem and Post-Mortem Judgement of Slaughter Animals and Meat', 'CCMPH1', 1972, 9, 'CAC9', 'CAC/RCP 58-2005', 1)
proposedAll[which(proposedAll$proposal_clean =="Proposed Draft Code of Practice for the Production, Storage and Composition of Mechanically Separated Meat and Poultry Intended for Further Processing" ), c('Reference', 'Title')]  = c('CAC/RCP 32-1983', 'Recommended International Code of Practice for the Production, Storage and Composition of Mechanically Separated Meat Intended for Further Processing')
proposedAll[which(proposedAll$proposal =="Proposed Draft Code of Hygienic Practice for Processed Meat Products"), proposalVariables]  = c('CAC/RCP 13-1976, Rev. 1 1985', 'Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products', 'CCPMPP4', 1969, 7, 'CAC7', 'CAC/RCP 58-2005', 1)
proposedAll[which(proposedAll$proposal_clean == "Proposed Draft Standard for Pineapple Juice"),c(proposalVariables, 'handcodedDummy', 'step')]  = data.frame(superStds[which(superStds$Title_original_standard == 'Pineapple juice'),superStdsVariables], 1, 5)
proposedAll[which(proposedAll$proposal_clean == "Proposed Draft Standards for Canned Carrots"),proposalVariables] = superStds[which(superStds$Title_original_standard == 'Canned carrots'), superStdsVariables ]

proposedAll[which(proposedAll$proposal_clean =="Proposed Draft Standard for Quick Frozen Corn-on-the-cob" ),c(proposalVariables, 'step')]  = c(superStds[which(superStds$Title_original_standard == "Quick Frozen Corn-on-the-Cob"), superStdsVariables[1:2]], 'GEQFF9', 1974, 11, 'CAC11', superStds[which(superStds$Title_original_standard == "Quick Frozen Corn-on-the-Cob"), superStdsVariables[7:8]], 2 )
proposedAll[which(proposedAll$proposal_clean =="Proposed Draft Standards for Canned Palmito" ),proposalVariables] = superStds[which(superStds$Title_original_standard == "Canned palmito" ), superStdsVariables]
proposedAll[which(proposedAll$proposal_clean =="Proposed Draft Standard for Concentrated Pineapple Juice Preserved Exclusively By Physical Means" ),proposalVariables] = superStds[grep('Concentrated pineapple juice preserved exclusively by physical means', superStds$Title_original_standard),superStdsVariables ]
proposedAll[which(proposedAll$proposal =="Proposed Draft Standards for Canned Mangoes" & proposedAll$year == 1980 ), proposalVariables] = superStds[which(superStds$Title_original_standard == 'Canned Mangoes'), superStdsVariables]
proposedAll = proposedAll[-which(proposedAll$proposal_clean =="Proposed Draft Standards for Canned Mango Products" & proposedAll$year == 1984 ),]
proposedAll[which(proposedAll$proposal =="Proposed Draft Code of Hygienic Practice for Fresh Meat"), proposalVariables] = superStds[which(superStds$Title_original_standard == 'Recommended International Code of Hygienic Practice for Fresh Meat'), superStdsVariables]
proposedAll[which(proposedAll$proposal =="Proposed Draft Code of Hygienic Practice for Game"), proposalVariables] = superStds[which(superStds$Title_original_standard ==  "Recommended International Code of Hygienic Practice for Game"  ), superStdsVariables]
proposedAll[which(proposedAll$proposal =="Proposed Draft Standard for Jams Jellies and Marmalades"), proposalVariables] = superStds[which(superStds$Title_original_standard ==  "Jams (fruit preserves) and jellies"  ), superStdsVariables]

# note that draft standard for raw sugar and raw cane sugar are the same; see CAC17 through CAC19
proposedAll = proposedAll[-grep('Proposed Draft Standard for Raw Cane Sugar', proposedAll$proposal_clean),]
proposedAll[grep('Proposed Draft Standard for Raw Sugar', proposedAll$proposal_clean),proposalVariables] = superStds[which(superStds$Title_original_standard ==  "Standard for Raw Cane Sugar"  ), superStdsVariables]

 
# likely but not 100 percent
proposedAll[which(proposedAll$proposal_clean =="Proposed Draft Code of Hygienic Practice for Poultry and Poultry Parts" ), proposalVariables[1:2]  ]  = superStds[which(superStds$Title_original_standard== "Recommended Code of Hygienic Practice for Poultry Processing"),superStdsVariables[1:2] ]


# remove duplicates
proposedAll = proposedAll[-c(which(proposedAll$proposal_clean  %in% c("Proposed Draft Standards for Quick Frozen Whole Kernel Corn and Quick Frozen Carrots")), # this should have been split up into two standards; remove  
                             which(proposedAll$proposal %in% c('Proposed Draft Code of Hygienic Practice for the Inspection of Game' )))   ,]  # already found in Proposed Draft Code of Hygienic Practice for the Inspection of Game
                                   

proposedAll[grep('sparagus', proposedAll$Title),]

# remove superseding standards that are already matched with proposal data
superStds = superStds[-which(superStds$Title_original_standard %in% 
                              c("Non-pulpy blackcurrant nectar",
                                "Liquid pulpy mango products",
                                "Pineapple juice" ,
                                "Canned carrots" ,
                                "Jams (fruit preserves) and jellies",
                                 "Quick Frozen Corn-on-the-Cob",
                                "Canned palmito"  ,
                                "Canned Mangoes",
                                'Quick Frozen Whole Kernel Corn',
                                'Quick Frozen Carrots', 
                               "Concentrated pineapple juice preserved exclusively by physical means",
                               "Recommended International Code for Ante-Mortem and Post-Mortem Inspection of Slaughter Animals and for Ante-Mortem and Post-Mortem Judgement of Slaughter Animals and Meat",
                               "Recommended International Code of Practice for the Production, Storage and Composition of Mechanically Separated Meat Intended for Further Processing",
                               "Recommended Code of Hygienic Practice for Poultry Processing", 
                               "Recommended International Code of Hygienic Practice for Fresh Meat",
                               "Recommended International Code of Hygienic Practice for Game",
                               'Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products',
                               "Standard for Raw Cane Sugar")),]

 
 

superStds$proposal = superStds$proposal_clean = paste0('Proposal for ', superStds$Title_original_standard)
names(superStds)[which(names(superStds) %in% c('Reference_original_standard', 'Title_original_standard', 'Proposal_meeting_original_standard'))] = c('Reference', 'Title', 'Meeting')
superStds$handcodedDummy = 0


superStds[which(superStds$Title =='Recommended International Code of Hygienic Practice for Foods for Infants and Children' ), 'step'] = 1

proposedAll = rbind(proposedAll, superStds[, names(proposedAll)])
 
proposedAll[which(proposedAll$Title == 'Quick Frozen Leeks'), c('meetingCount_cac', 'meeting_cac', 'year', 'handcodedDummy', 'Meeting', 'step')] = c(5, 'CAC5', 1967, 1, 'GEQFF3', 1)



# -----------------------------------
# integrate in data for standards that were subsequently dropped
# -----------------------------------
droppedStds = read.csv(paste0(pathMain, '/participation_development/dropped_standards/standards_dropped.csv'), stringsAsFactors = FALSE)
droppedStds$Title = str_trim(droppedStds$Title)


# keep only data on dropped stds that were not previously incorporated in 'std' dataset
droppedStds = droppedStds[which(droppedStds$Reference %in% c(setdiff(droppedStds$Reference, proposedAll$Reference))),]

# add in dummy info for if proposal/std was dropped
proposedAll$droppedDummy = droppedStds$Dropped[match(proposedAll$Reference, droppedStds$Reference)]
proposedAll$droppedDummy[which(is.na(proposedAll$droppedDummy))] = 0
 

## split up proposals that were previously incorrectly combined
# vinylchloride/acryl and methyl for fish
addRows = data.frame(matrix(rep(proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guideline Levels for Vinylchloride Monomer and Acrylonitrile in Foods and Food Packaging Material and for Methyl Mercury in Fish'),], each = 1) %>% unlist(), nrow = 1))
names(addRows) = names(proposedAll)
proposedAll = rbind(proposedAll, addRows)

proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guideline Levels for Vinylchloride Monomer and Acrylonitrile in Foods and Food Packaging Material and for Methyl Mercury in Fish'), c('proposal', 'proposal_clean')][1,] = rep("Proposed Draft Guideline Levels for Vinylchloride Monomer and Acrylonitrile in Foods and Food Packaging Material", 2)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guideline Levels for Vinylchloride Monomer and Acrylonitrile in Foods and Food Packaging Material and for Methyl Mercury in Fish'), c('proposal', 'proposal_clean')] = rep("Proposed Draft Guideline Levels for for Methyl Mercury in Fish", 2)

# palm oil and palm stearin
addRows = data.frame(matrix(rep(proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standards for Palm Olein and Palm Stearin'),], each = 1) %>% unlist(), nrow = 1))
names(addRows) = names(proposedAll)
proposedAll = rbind(proposedAll, addRows)

proposedAll[grep('Palm Olein', proposedAll$proposal),c('proposal_clean', 'proposal')][1,] = rep("Proposed Draft Standard for Palm Olein", 2)
proposedAll[grep('Palm Stearin', proposedAll$proposal),c('proposal_clean', 'proposal')]= rep("Proposed Draft Standard for Palm Stearin", 2)


# Proposed Draft Standards for Coconut Oil, Palm Oil, Palm Kernel Oil, Grapeseed Oil and Babassu Oil
addRows = data.frame(matrix(rep(proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standards for Coconut Oil, Palm Oil, Palm Kernel Oil, Grapeseed Oil and Babassu Oil'),], each = 4) %>% unlist(), nrow = 4))
names(addRows) = names(proposedAll)
proposedAll = rbind(proposedAll, addRows)

proposedAll[grep('Coconut Oil', proposedAll$proposal_clean), c('proposal', 'proposal_clean')][1,] = rep("Proposed Draft Standard for Coconut Oil", 2)
proposedAll[grep('Palm Oil', proposedAll$proposal_clean), c('proposal', 'proposal_clean')][1,] = rep("Proposed Draft Standard for Palm Oil", 2)
proposedAll[grep('Palm Kernel Oil', proposedAll$proposal_clean), c('proposal', 'proposal_clean')][1,] = rep("Proposed Draft Standard for Palm Kernel Oil", 2)
proposedAll[grep('Grapeseed Oil', proposedAll$proposal_clean), c('proposal', 'proposal_clean')][1,] = rep("Proposed Draft Standard for Grapeseed Oil", 2)
proposedAll[grep('Babassu Oil', proposedAll$proposal_clean), c('proposal', 'proposal_clean')] = rep("Proposed Draft Standard for Babassu Oil", 2)



# match proposals to previously dropped stds
proposedVariables = c('Reference', 'Title', 'droppedDummy')
droppedVariables = c('Reference', 'Title', 'Dropped')
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft System for the Description of Carcasses of Bovine and Porcine Species'), proposedVariables] = droppedStds[grep('Porcine', droppedStds$Title), droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Fructose'), proposedVariables] = droppedStds[grep('Fructose', droppedStds$Title), droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standards for Canned Apricots'), proposedVariables] = droppedStds[which(droppedStds$Title == 'Canned apricots'),droppedVariables]
proposedAll[which(proposedAll$proposal == 'Proposed Draft European Regional Standard for Mayonnaise'), proposedVariables] = droppedStds[which(droppedStds$Title == 'Mayonnaise'),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Powdered Dextrose (icing Dextrose)'), proposedVariables]  = droppedStds[which(droppedStds$Title == 'Powdered dextrose (icing dextrose)'),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guidelines for the Application of the Hazard Analysis Critical Control Point (haccp) System'), proposedVariables] = droppedStds[which(droppedStds$Title == 'Hazard Analysis and Critical Control Point (HACCP) System and Guidelines for its Application'),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Code of Practice for Shrimps and Prawns'), proposedVariables] = droppedStds[which(droppedStds$Title == 'Code of Practice for Shrimps or Prawns'),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Low Erucic Acid Rapeseed Oil'), proposedVariables] =droppedStds[which(droppedStds$Title == "Edible low erucic acid rapeseed oil"),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Code of Practice for Smoked Fish'), proposedVariables] =droppedStds[which(droppedStds$Title == 'Recommended International Code of Hygienic Practice for Smoked Fish'),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Code of Practice for Frozen Fish'), proposedVariables] =droppedStds[which(droppedStds$Title == 'Recommended International Code of Practice for Frozen Fish'),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Code of Practice for the Control of the Use of Veterinary Drugs'), proposedVariables] = droppedStds[which(droppedStds$Title == 'Recommended International Code of Practice for the Control of the Use of Veterinary Drugs'),droppedVariables]


proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guideline Levels for Radionuclides in Food for Use in International Trade'), proposedVariables] =matrix(rep(droppedStds[which(droppedStds$Title == 'Guideline Levels for Radionuclides in Foods following accidental Nuclear Contamination for use in International Trade'),droppedVariables], each = 2)%>% unlist(), nrow = 2)

proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Composite and Filled Chocolate'), proposedVariables] = matrix(rep(droppedStds[which(droppedStds$Title == 'Composite and filled chocolate'),droppedVariables], each = 2) %>% unlist(), nrow = 2)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft General Standard for Vegetable Juices'), proposedVariables] = matrix(rep(droppedStds[which(droppedStds$Title == 'Codex General Standard for Vegetable Juices'),droppedVariables], each = 2)%>% unlist(), nrow = 2)  
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Code of Hygienic Practice for Molluscan Shellfish'), proposedVariables] = matrix(rep(droppedStds[which(droppedStds$Title == 'Codex of Hygienic Practice for Mulluscan Shellfish'),droppedVariables], each = 2)%>% unlist(), nrow = 2) 
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Code for Salted Fish'), proposedVariables] = matrix(rep(droppedStds[which(droppedStds$Title == 'Recommended International Code of Practice for Salted Fish'),droppedVariables], each = 2)%>% unlist(), nrow = 2) 

 

proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guidelines for the Establishment of A Regulatory Programme for Control of Veterinary Drug Residues in Foods'), proposedVariables] =droppedStds[which(droppedStds$Title == "Guidelines for the Establishment of a Regulatory Programme for Control of Veterinary Drug Residues in Foods"),droppedVariables]
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guideline Levels for Vinylchloride Monomer and Acrylonitrile in Foods and Food Packaging Material'), c(proposedVariables, 'step')] = c(droppedStds[which(droppedStds$Title == 'Guideline Levels for Vinyl Chloride Monomer and Acrylonitrile in Food and Packaging Material'), droppedVariables], 3)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guideline Levels for for Methyl Mercury in Fish'), c(proposedVariables, 'step')] = c(droppedStds[which(droppedStds$Title == 'Guideline Levels for Methylmercury in Fish'), droppedVariables], 3)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Palm Olein'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year') ] = c(droppedStds[which(droppedStds$Title == 'General Standard for Edible Palm Olein'), droppedVariables], 18, 'CAC18', 1987)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Palm Stearin'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year') ] = c(droppedStds[which(droppedStds$Title == 'Codex Standard for Edible Palm Stearin'), droppedVariables], 18, 'CAC18', 1987)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Coconut Oil'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year', 'step', 'Meeting') ] = c(droppedStds[which(droppedStds$Title == 'Edible coconut oil'), droppedVariables], 10, 'CAC10', 1974, 1, 'CCFO7')
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Palm Oil'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year', 'step', 'Meeting') ] = c(droppedStds[which(droppedStds$Title == 'Edible palm oil'), droppedVariables], 10, 'CAC10', 1974, 1, 'CCFO7')
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Palm Kernel Oil'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year', 'step', 'Meeting') ] = c(droppedStds[which(droppedStds$Title == 'Edible palm kernel oil'), droppedVariables], 10, 'CAC10', 1974, 1, 'CCFO7')
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Grapeseed Oil'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year', 'step', 'Meeting') ] = c(droppedStds[which(droppedStds$Title == 'Edible grape seed oil'), droppedVariables], 11, 'CAC11', 1975, 3, 'CCFO8')
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Babassu Oil'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year', 'step', 'Meeting') ] = c(droppedStds[which(droppedStds$Title == 'Edible babassu oil'), droppedVariables], 11, 'CAC11', 1975, 3, 'CCFO8')
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standards for Quick Frozen Fillets of Hake'), c(proposedVariables, 'step') ] = c(droppedStds[which(droppedStds$Title == 'Quick-Frozen Fillets of Hake Canned Sardines and Sardine-Type Products'), droppedVariables], 2)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standards for Quick Frozen Fillets of Hake'), c('Title') ] =  "Quick-Frozen Fillets of Hake"
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Code of Hygienic Practice for Dried Milk'), c(proposedVariables, 'step') ] = c(droppedStds[which(droppedStds$Title == 'Codex of Hygienic Practice for Dried Milk'), droppedVariables], 4)
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft European Regional Standard for Vinegar'), c(proposedVariables, 'meetingCount_cac', 'meeting_cac', 'year',  'Meeting') ] =c(droppedStds[which(droppedStds$Title == 'Vinegar'), droppedVariables], 12, 'CAC12', 1977, 'CCEURO10')
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Guideline Levels for Radionuclides in Food for Use in International Trade' & proposedAll$meetingCount_cac == 25), c('meetingCount_cac', 'meeting_cac')] = c(27, 'CAC27')

proposedAll[which(proposedAll$Title == 'Code of Practice for Shrimps or Prawns'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(10, 'CAC10', 1973, 'CCFFP8', '2', 1) 

proposedAll[which(proposedAll$Title == 'Guidelines for Mixed Fruit Nectars'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(16, 'CAC16', 1984, 'GEFJ16', '1', 1)  
proposedAll[which(proposedAll$Title == 'Hazard Analysis and Critical Control Point (HACCP) System and Guidelines for its Application'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(19, 'CAC19', 1989, 'CCFH24', '3', 1)  

proposedAll[which(proposedAll$Title == 'Recommended International Code of Practice for the Control of the Use of Veterinary Drugs'),  c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(18, 'CAC18', 1987, 'CCRVDF2', '1', 1)  


# add row for milk and milk products
  # previously 'dried milk' had been incorrectly attached to 'milk and milk products'
proposedAll = rbind(proposedAll, data.frame(meetingCount_cac = 22, meeting_cac = 'CAC22', 
  proposal = 'Proposed Code of Hygienic Practice for Milk and Milk Products',
  Reference = 'CAC/RCP 57-2004', 
  Title = 'Code of Hygienic Practice for Milk and Milk Products',
  year = 1996,
  proposal_clean = 'Proposed Code of Hygienic Practice for Milk and Milk Products',
  handcodedDummy = 1,
  Meeting = 'CCFH29',
  step = 1,
  Reference_superseding_standard= NA, supersededDummy = 0,  droppedDummy = 0))

# note when proposal is attached to standard that was subsequnetly dropped
proposedAll[c(which(proposedAll$proposal_clean %in% c('Proposal for Guidelines for Mixed Fruit Nectars',
                                                    'Proposed Draft Standard for Extra Hard Grating Cheese',
                                                    'Recommended International Code of Hygienic Practice for Game',
                                                    'Proposed Draft Code of Hygienic Practice for Fresh Meat')),
              which(proposedAll$Title %in% c('Standard for Dried Edible Fungi'))), 'droppedDummy'] = 1



# remove maximum levels of residues
droppedStds = droppedStds[-grep('Maximum|MAXIMUM', droppedStds$Title),]



# delete dropped stds for which matches are found
droppedStds = droppedStds[-which(droppedStds$Title %in% c(
  'System for the Description of Carcasses of Bovine and Porcine Species',
  'Fructose', 
  'Composite and filled chocolate', 
  "Canned apricots",
  'Mayonnaise', 
  "GUIDELINES FOR MIXED FRUIT NECTARS" ,
  'Powdered Dextrose (icing Dextrose)',
  'Hazard Analysis and Critical Control Point (HACCP) System and Guidelines for its Application',
  "Code of Practice for Shrimps or Prawns",
  'Codex General Standard for Vegetable Juices',
  'Codex International Standard for Extra Hard Grating Cheese',
  'Codex of Hygienic Practice for Game',
  'Codex of Hygienic Practice for Mulluscan Shellfish',
  'Edible low erucic acid rapeseed oil',
  "Guideline Levels for Radionuclides in Foods following accidental Nuclear Contamination for use in International Trade" ,
  "Guidelines for the Establishment of a Regulatory Programme for Control of Veterinary Drug Residues in Foods",
  'Recommended International Code of Hygienic Practice for Fresh Meat',
  'Recommended International Code of Hygienic Practice for Smoked Fish',
  'Recommended International Code of Practice for Frozen Fish',
  'Recommended International Code of Practice for Salted Fish',
  'Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products',
  'Recommended International Code of Practice for the Control of the Use of Veterinary Drugs',
  'Standard for dried edible fungi',
  'Guideline Levels for Vinyl Chloride Monomer and Acrylonitrile in Food and Packaging Material',
  'Guideline Levels for Methylmercury in Fish',
  'General Standard for Edible Palm Olein',
  'Codex Standard for Edible Palm Stearin',
  'Edible palm kernel oil',
  'Edible palm oil',
  'Edible coconut oil',
  "Edible grape seed oil",
  "Edible babassu oil" ,
  "Quick-Frozen Fillets of Hake Canned Sardines and Sardine-Type Products",
  'Codex of Hygienic Practice for Dried Milk'
)),]


# -----------------------------------
# add in proposal data for dropped standards that previously had no proposal data for
# and then subsequently handcoded
# -----------------------------------
missingProposalDropped = read.csv(paste0(pathMain, '/participation_development/missingAll_SK.csv'), stringsAsFactors = FALSE, na.string = '.')
missingProposalDropped = missingProposalDropped[-c(1:55, 121:177), -which(names(missingProposalDropped) == 'X')]

# this standard is superseded, not dropped
missingProposalDropped = missingProposalDropped[-which(missingProposalDropped$Title =='Recommended International Code of Hygienic Practice for Foods for\nInfants and Children'),]

missingProposalDropped$proposal = missingProposalDropped$proposal_clean = paste0('Proposed ', missingProposalDropped$Title)
missingProposalDropped$meetingCount = gsub('[A-Z]', '', missingProposalDropped$Meeting) %>% as.numeric()
missingProposalDropped$handcodedDummy = 1

missingProposalDropped$year =  dates_all$year[match(missingProposalDropped$Meeting, dates_all$event_number)]

# fix committee and cac 
missingProposalDropped[grep('Perch', missingProposalDropped$Title), c('year', 'meetingCount_cac', 'meeting_cac', 'Meeting', 'step')] = c('1966', 4, 'CAC4', 'CCFFP1', 3)
missingProposalDropped[grep('Cod and Haddock', missingProposalDropped$Title), c('meetingCount_cac', 'meeting_cac', 'step')] = c( 4, 'CAC4', 3)


# manually add in info on missingUnadopted
missingProposalDropped[which(missingProposalDropped$Title == 'GENERAL PRINCIPLES OF MEAT HYGIENE'), c('year', 'Meeting', 'step')] = c('2002', 'CCMPH8', 5)
missingProposalDropped[which(missingProposalDropped$Title == 'Codex Standard for Specified Vegetable Fat Products'), c('meeting_cac', 'meetingCount_cac', 'year', 'Meeting', 'step')] = c(14, 'CAC14', 1980, 'CCFO11', 1) # used to be named standard for vegetable ghee then draft standard for vanaspati 
missingProposalDropped[which(missingProposalDropped$Title == 'Specified Animal or Mixed Animal and vegetable Fat Products'), c('meeting_cac', 'meetingCount_cac', 'year', 'Meeting', 'step')] = c(14, 'CAC14', 1980, 'CCFO11', 1) # used to be named standard for mixed vegetable ghee then draft standard for vegetable fat mixture
missingProposalDropped[which(missingProposalDropped$Title == 'Sampling Plans for Prepackaged Foods'), c('meeting_cac', 'meetingCount_cac', 'year', 'Meeting', 'step')] = c(4, 'CAC4', 1966, 'CCPFV3', 3) # used to be named standard for mixed vegetable ghee then draft standard for vegetable fat mixture

# these standards are duplicated in proposedAll
missingProposalDropped = missingProposalDropped[-which(missingProposalDropped$Title %in% c(
  'Quick Frozen Leeks',
  'Guideline Levels for Radionuclides in Foods following accidental Nuclear Contamination for use in International Trade'
)),]

 
# see CCS1, all of the following were proposed in CCS1
missingProposalDropped[grep('ugar|extrose|lucose', missingProposalDropped$Title), c('year', 'meetingCount_cac', 'meeting_cac', 'Meeting', 'step', 'meetingCount')] = matrix(rep(c(1964, 2, 'CAC2', 'CCS1', 1, 1), each = 7), nrow = 7)

# check/fix cac meetings
# missingProposalDropped$tmpMeeting = dates_all$event_number_cac[match(missingProposalDropped$Meeting, dates_all$event_number)]
# missingProposalDropped$tmpyear = dates_all$year[match(missingProposalDropped$tmpMeeting, dates_all$event_number_cac)]
# missingProposalDropped$tmpyear %>%as.numeric() - missingProposalDropped$year %>%as.numeric() 
missingProposalDropped$meeting_cac = dates_all$event_number_cac[match(missingProposalDropped$Meeting, dates_all$event_number)]
missingProposalDropped$meetingCount_cac = gsub('[A-Z]', '', missingProposalDropped$meeting_cac) 


proposedAll = rbind(proposedAll, missingProposalDropped[, names(proposedAll)])


# -----------------------------------
# add proposal data for proposals that previously could not find information for
# and then subsequently handcoded
# -----------------------------------
missingUnadopted = read.csv(paste0(pathMain, '/participation_development/codexStandardsUnadoptedMissing.csv'), stringsAsFactors = FALSE)
proposedAll$status = 0

# remove proposals that are duplicates or amendments/revisions
delete =  missingUnadopted[c(grep('Delete', missingUnadopted$Reference),
                             grep('Delete', missingUnadopted$status)), ]


 
proposedAll= proposedAll[-which(proposedAll$proposal_clean %in% delete$proposal_clean),]
missingUnadopted =  missingUnadopted[-c(grep('Delete', missingUnadopted$Reference),
                                        grep('Delete', missingUnadopted$status)), ]



# incorporate proposals that were discontinued
discontinued = missingUnadopted[grep('Discontinue|suspend|discussed but never|seems to disappear', missingUnadopted$status), ]
discontinued$discontinuedDummy = 1

proposedAll = proposedAll[-which(proposedAll$proposal_clean %in% discontinued$proposal_clean),]
proposedAll$discontinuedDummy = 0

proposedAll[which(proposedAll$proposal_clean %in% c(
                                                  "Proposed Draft Standard for Mango Juice" ,
                                                  'Proposed Draft Codification of Carcases of the Species Ovis',
                                                  'Proposed Draft Standard for the Labelling of and Claims For, Pre-packaged Foods Claimed to Be Suitable for Diabetics',
                                                  'Proposed Draft Standard for Labelling of and Claims for Low-energy and Reduced-energy Foods',
                                                  'Proposed Draft Guidelines for Use of the Term "natural" in Food Product Labelling',
                                                  'Proposed Draft Guidelines for the Utilization and Promotion of Quality Assurance Systems to Meet Requirements in Relation to Food',
                                                  'Proposed Draft Guidelines on the Judgement of Equivalence of Technical Regulations Associated with Food Inspection and Certification Systems',
                                                  'Proposed Draft Standard for Ayran',
                                                  'Proposed Draft Code of Practice for the Safe Use of Active Chlorine'
)), 'discontinuedDummy'] = 1


proposedAll = rbind(proposedAll, discontinued[, names(proposedAll)])
missingUnadopted =  missingUnadopted[-grep('Discontinue|suspend|discussed but never|seems to disappear', missingUnadopted$status), ]


# incorporate data that found evidence that it was adopted into a standard or 
adopted = missingUnadopted[grep('Adopted|Changed name|Used to be known as', missingUnadopted$status), ]
adopted$Title = ifelse(adopted$Title == 0, gsub('Proposed Draft |Proposed ', '' , adopted$proposal_clean), adopted$Title)
adopted$Reference = ifelse(adopted$Reference == 0, 'Missing', adopted$Reference)
adopted$discontinuedDummy = 0

proposedAll = proposedAll[-which(proposedAll$proposal_clean %in% adopted$proposal_clean),] 
proposedAll = rbind(proposedAll, adopted[, names(proposedAll)])
missingUnadopted =  missingUnadopted[-grep('Adopted|Changed name|Used to be known as', missingUnadopted$status), ]

proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Non-fermented Soybean Products'), c('Reference', 'Title')] = as.matrix(data.frame(Reference =rep('CODEX STAN 322R-2015', 2),Title = rep('Regional Standard for Non-Fermented Soybean Products')))

proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Standard for Non-fermented Soybean Products' & proposedAll$meetingCount_cac == 36), c('step')] = c(  5)
 
# incoporate data that is either i) its still ongoing work or ii) status unknown
proposedAll = proposedAll[-which(proposedAll$proposal_clean %in% missingUnadopted$proposal_clean),] 
missingUnadopted$discontinuedDummy = 0
proposedAll = rbind(proposedAll, missingUnadopted[, names(proposedAll)])

proposedAll$ongoingDummy = ifelse(grepl('Next meeting not yet held',proposedAll$status), 1, 0)
proposedAll$missingDummy = ifelse(grepl('Committee abolished',proposedAll$status)|is.na(proposedAll$status), 1, 0)


proposedAll = proposedAll[-which(proposedAll$proposal_clean == 'Proposed Draft Guidelines for the Use of Vegetable Protein Products and Milk Protein Products in Processed Meat and Poultry Products'),] # this became a part of standard for  Vegetable Protein Products (VPP) see CCPMPP12

# -----------------------------------------
# clean up committee-year-CAC concordance
# -----------------------------------------

# clean up step variable
proposedAll$step[which(proposedAll$step == '1/2/03')] = '1/2/3'
proposedAll$step[which(proposedAll$step == '3-Feb')] = '2/3'
proposedAll$step[which(proposedAll$step == '8-May')] = '5/8'
proposedAll$step[which(proposedAll$step %in% c('', 'NA'))] = NA

 
# check to make sure committees are correctly specified relative to years
proposedAll[which(proposedAll$Title == 'Standard for Desiccated Coconut'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(16, 'CAC16', 1985, 'CCAFRICA7', 3, 1)
proposedAll[which(proposedAll$Title == 'Code of Hygienic Practice for Aseptically Processed and Packaged Low-Acid Foods'), c('meetingCount_cac', 'meeting_cac', 'year', 'Meeting', 'step', 'handcodedDummy')] = c(17, 'CAC17', 1985, 'CCFH21', 4, 1)
proposedAll[which(proposedAll$Title == 'Standard for Named Animal Fats'), c('year')] = 1993
proposedAll[which(proposedAll$Title == 'Powdered dextrose (icing dextrose)'), c('meetingCount_cac', 'meeting_cac', 'year', 'step', 'handcodedDummy')] = c(6, 'CAC6', 1968, 1, 1)



checkYear = dates_all$year[match(proposedAll$Meeting, dates_all$event_number)] %>% as.numeric()
gapYear = abs(checkYear - proposedAll$year %>% as.numeric())
proposedAll[which(gapYear == 1), 'year'] = checkYear[which(gapYear == 1)] # when the year is off by one, just assume that the committee is correct and change year
proposedAll[which(gapYear == 2), 'year'] = checkYear[which(gapYear == 2)] # manually checked (Guidelines for the Preservation of Raw Milk by Use of the Lactoperoxidase System)


# check to make sure CAC meetings are correctly specificed relative to years
proposedAll[which(proposedAll$Title == 'Standard for Canned Salmon'), c('meetingCount_cac', 'meeting_cac', 'step')] = c( 4, 'CAC4', 3)
proposedAll[which(proposedAll$Title == 'Regional Standard for Tempe'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 34, 'CAC34', 1, 2010, 'CCASIA17')
proposedAll[which(proposedAll$Title == 'Standard for Quick Frozen Fish Sticks (Fish Fingers), Fish Portions and Fish Fillets - Breaded or in Batter'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 11, 'CAC11', 2, 1974, 'CCFFP9')
proposedAll[which(proposedAll$Title == 'Working Principles for Risk Analysis for Food Safety for Application by Governments'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 23, 'CAC23', 2, 1998, 'CCGP13')
proposedAll[which(proposedAll$Title == 'Regional Standard for Culantro Coyote (LAC)'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 32, 'CAC32', '1/2/3', 2008, 'CCLAC16')
proposedAll[which(proposedAll$Title == 'Standard for Milk Powders and Cream Powder'), c('step')] = c(5)
proposedAll[which(proposedAll$Title == 'Standard for Extra Hard Grating Cheese'), c('step')] = c(1)
proposedAll[which(proposedAll$Title == 'Regional Standard for Harissa (Red Hot Pepper Paste)'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 32, 'CAC32', '1/2/3', 2009, 'CCNEA5')
proposedAll[which(proposedAll$Title == 'Regional Standard for Date Paste (Near East)'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 34, 'CAC34', '1/2/3', 2011, 'CCNEA6')
proposedAll[which(proposedAll$Title == 'General Methods for the Detection of Irradiated Foods'), c( 'year', 'Meeting')] = c( 2001, 'CCMAS23')
proposedAll[which(proposedAll$Title == 'Principles and Guidelines for the Conduct of Microbiological Risk Management (MRM)'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 23, 'CAC23', '3', 1998, 'CCFH30')


proposedAll[which(proposedAll$Title == 'Code of Hygienic Practice for Fresh Fruits and Vegetables'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 23, 'CAC23', 1, 1997, 'CCFH30')
proposedAll[which(proposedAll$Title == 'Compilation of Codex texts relevant to the labelling of foods derived from modern biotechnology'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 24, 'CAC24', 3, 2000, 'CCFL28') # previously known as # Proposed draft Recommendations for the labelling of foods and food ingredients obtained through certain techniques of genetic modification/genetic engineering (
proposedAll[which(proposedAll$Title == 'Standard for Sweet Cassava'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 23, 'CAC23', 1, 1997, 'CCFFV7') # previously known as # used to be called a proposed standard for yucca, then proposed standard for cassava
proposedAll[which(proposedAll$Title == 'Model Export Certificate for Milk and Milk Products'), c('step')] = 2
proposedAll[which(proposedAll$proposal_clean == 'Proposed Draft Codex Standard for Parmesan' & proposedAll$meetingCount_cac == 25),c('step')] = 1
proposedAll[which(proposedAll$Title == 'Regional Standard for Tehena'), c('step', 'year', 'Meeting', 'handcodedDummy')] = c( 1, 2003, 'CCNEA2', 1) 
proposedAll[which(proposedAll$Title == 'Regional Standard for Halwa Tehenia'), c('step')] = '1/2/3'
proposedAll[which(proposedAll$Title == 'Principles and Guidance on the Selection of Representative Commodities for the Extrapolation of Maximum Residue Limits for Pesticides to Commodity Groups'), c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(32, 'CAC32', 2, 2009, 'CCPR41')

# some proposals originated in other regional committees, add this information
# note this includes  though this is already properly coded

proposedAll[which(proposedAll$Title == 'Standard for Maize (Corn)'),c('meetingCount_cac', 'meeting_cac', 'step')] = c(14, 'CAC14', 5)
addRows_0 = proposedAll[which(proposedAll$Title == 'Standard for Maize (Corn)'), ]
addRows_0[, c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')]= c( 12, 'CAC12', 1, 1977, 'CCAFRICA3')

proposedAll[which(proposedAll$Title == 'Standard for Boiled Dried Salted Anchovies'), c('meetingCount_cac', 'meeting_cac', 'step')] = c( 22, 'CAC22', 7)  
addRows_1 =  proposedAll[which(proposedAll$Title == 'Standard for Boiled Dried Salted Anchovies'),] 
addRows_1[, c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 20, 'CAC20', '1/2/3', 1992, 'CCASIA8')  

proposedAll[which(proposedAll$Title == 'Standard for Crackers from Marine and Freshwater Fish, Crustaceans and Molluscan Shellfish'),c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(22, 'CAC22', 5, 1996, 'CCFFP22')
addRows_2 = proposedAll[which(proposedAll$Title == 'Standard for Crackers from Marine and Freshwater Fish, Crustaceans and Molluscan Shellfish'), ]
addRows_2[, c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c( 20, 'CAC20', '1/2/3', 1992, 'CCASIA8')  

proposedAll[which(proposedAll$Title == 'General Guidelines for Use of the Term "Halal"'),c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(21, 'CAC21', 5, 1994, 'CCFL23')
addRows_3 =proposedAll[which(proposedAll$Title == 'General Guidelines for Use of the Term "Halal"'),]
addRows_3[,c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(20, 'CAC20', 3, 1992, 'CCASIA8')

addRows_4 = c(15, 'CAC15', 'Proposed European Regional Standard for Mayonnaise', 'CODEX STAN 168-1989', 'Mayonnaise', 1982, 'Proposed European Regional Standard for Mayonnaise', 1, 'CCEURO13', 3, NA, 0, 1, 0, 0, 0, 0, 13)

addRows_5 =proposedAll[which(proposedAll$Title == 'Standard for Aqueous Coconut Products - Coconut Milk and Coconut Cream'),]
addRows_5[,c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(22, 'CAC22', 1, 1996, 'CCASIA10')


proposedAll[which(proposedAll$Title == 'Standard for Pickled Fruits and Vegetables'),c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(23, 'CAC23', 3, 1998, 'CCPFV19')
addRows_6 =proposedAll[which(proposedAll$Title == 'Standard for Pickled Fruits and Vegetables'),] # used to be called standard for pickles
addRows_6[,c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(20, 'CAC20', 1, 1992, 'CCASIA8')


addRows_7 =proposedAll[which(proposedAll$Title == 'Standard for Sorghum Grains'),] 
addRows_7[,c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(16, 'CAC16', 4, 1985, 'CCAFRICA7')


proposedAll[which(proposedAll$Title == 'Standard for Sorghum Flour'),c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(16, 'CAC16', 1, 1983, 'CCAFRICA6')
addRows_8 =proposedAll[which(proposedAll$Title == 'Standard for Sorghum Flour'),] 
addRows_8[,c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(16, 'CAC16', 4, 1985, 'CCAFRICA7')

proposedAll[which(proposedAll$Title == 'Standard for Canned Bamboo Shoots'),c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')] = c(20, 'CAC20', 1, 1992, 'CCASIA8')
addRows_9 =proposedAll[which(proposedAll$Title == 'Standard for Canned Bamboo Shoots'),] 
addRows_9[,c('meetingCount_cac', 'meeting_cac', 'step', 'year', 'Meeting')]= c(23, 'CAC23', 7, 1998, 'CCPFV19')

 
 
addRows = rbind(addRows_0, addRows_1, addRows_2, addRows_3, addRows_4, addRows_5, addRows_6, addRows_7, addRows_8, addRows_9)
proposedAll = rbind(proposedAll, addRows) 

# used to be called Proposed Draft Recommendations for the Labelling of Food and Food Ingredients Obtained through Certain Techniques of Genetic Modification/ Genetic Engineering
checkCAC = dates_all$event_number_cac[match(proposedAll$Meeting, dates_all$event_number)]
checkCAC = gsub('CAC', '', checkCAC) %>% as.numeric()
gap = abs(checkCAC - proposedAll$meetingCount_cac %>% as.numeric())


# checked all gaps greater than 1 CAC meeting manually 
proposedAll[intersect(grep('CGECPMMP', proposedAll$Meeting), which(gap>2)  ), c('meetingCount_cac', 'meeting_cac')] = cbind(checkCAC[intersect(grep('CGECPMMP', proposedAll$Meeting), which(gap>2)  )], paste0('CAC', checkCAC[intersect(grep('CGECPMMP', proposedAll$Meeting), which(gap>2)  )]))
proposedAll[setdiff( which(gap>1),grep('CCS|CGECPMMP', proposedAll$Meeting)), c('meetingCount_cac', 'meeting_cac')] = cbind(checkCAC[setdiff( which(gap>1),grep('CCS|CGECPMMP', proposedAll$Meeting))], paste0('CAC', checkCAC[setdiff( which(gap>1),grep('CCS|CGECPMMP', proposedAll$Meeting))]))

# assume that all gaps that are 1 CAC meeting are measurement errors, and match to meeting year as reference
proposedAll[setdiff(which(gap==1),grep('CCS|CGECPMMP', proposedAll$Meeting)), c('meetingCount_cac', 'meeting_cac')] = cbind(checkCAC[setdiff( which(gap==1),grep('CCS|CGECPMMP', proposedAll$Meeting))], paste0('CAC', checkCAC[setdiff( which(gap==1),grep('CCS|CGECPMMP', proposedAll$Meeting))]))

# two meetings in 2003, only the second (26th) meeting were there things that got proposed
proposedAll$meetingCount_cac[which(proposedAll$meetingCount_cac == 25)]  = 26 
proposedAll$meeting_cac[which(proposedAll$meeting_cac == c('CAC25'))] = 'CAC26' 
 
# get rid of empty rows
proposedAll =proposedAll[-which(proposedAll$proposal == 0),]


# fix standard for apples data
proposedAll[which(proposedAll$Title == 'Standard for Apples'), c( 'Meeting')] = NA
 
 
save(proposedAll, file = paste0(pathMain, '/participation_development/proposals_master.rda'))
#load(file = paste0(pathMain, '/participation_development/proposals_master.rda'))



# checkCAC correct
# Regional Code of Hygienic Practice for Street-Vended Foods in Asia
# Class Names and the International Numbering System for Food Additives ?
# List of Codex Specifications for Food Additives ?
# Standard for Salted Fish and Dried Salted Fish of the Gadidae Family of Fishes
# Code of Practice for Fish and Fishery Products
# Guidelines on the Application of General Principles of Food Hygiene to the Control of Listeria Monocytogenes in Foods
# Guidelines for Design, Production, Issuance and Use of Generic Official Certificates
# Guidelines for the Development of Equivalence Agreements Regarding Food Imports and Export Inspection and Certification Systems
# Principles and Guidelines for the Exchange of Information in Food Safety Emergency Situations
# Code of Practice for the Storage and Transport of Edible Fats and Oils
# Code of Ethics for International Trade in Food including Concessional and Food Aid Transactions.
# Standard for Edible Casein Products
# Standard for Milk Powders and Cream Powder
# General Principles for the Addition of Essential Nutrients to Foods
# Guide for the Microbiological Quality of Spices and Herbs Used in Processed Meat and Poultry Products
# Guidelines on Performance Criteria for Methods of Analysis for the Determination of Pesticide Residues in Food and Feed
# Guideline for the Conduct of Food Safety Assessment of Foods Produced Using Recombiant-DNA Microorganisms
# General Principles of Food Hygiene
# Principles and Guidelines for the Conduct of Microbiological Risk Management (MRM)
# Guidelines on the Application of General Principles of Food Hygiene to the Control of Pathogenic Vibrio Species in Seafood
# Code of Practice for the Prevention and Reduction of Dioxin and Dioxin-like PCB Contamination in Food and Feeds
# Guidelines for the Control of Campylobacter and Salmonella in Chicken Meat
# Proposed Draft Guidelines for the Exchange of Information Between Countries on Rejections of Imported Foods
# Proposed Draft Guidelines for Evaluating Acceptable Methods of Analysis
# General Standard for the Use of Dairy Terms
# 'Model Export Certificate for Milk and Milk Products'
# Regional Guidelines for Codex Contact Points and National Codex Committees (Near East)
# Regional Standard for Halwa Tehenia
# Proposed Draft Standard for Emmental




# TFAMR6, 10/12/2018 to 14/12/2018
# CCFFV didnt exist before 1988
# CCASIA didnt exist before 1977
# CCNEA didnt exist before 2001
# CCFFV didnt exist before 1988
# CCFICS didnt exist before 1991
# CCLAC didnt exist before 1976
# TFAF didnt exist before 1978




# all under code of practice for fish and fishery products
# PROPOSED DRAFT CODE OF PRACTICE FOR SHRIMPS AND PRAWNS (Agenda Item 7) 
# PROPOSED DRAFT CODE OF PRACTICE FOR MOLLUSCAN SHELLFISH (Agenda Item 8) 
# PROPOSED DRAFT CODE OF PRACTICE FOR THE PRODUCTS OF AQUACULTURE

 


 # mortem...... has a lot of different references:
# from superseded : CAC/RCP 41-1993
# from dropped: CAC/RCP 12-1976   AND    CAC/RCP 34-1985

 
 
 
