if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}
 

# TO DO
# check which countries have most concentration: DONE
# do ols model with logged dv : DONE
# make wto member variables: DONE
# do survival analysis


# note should run models that corrects for wto membership as a robustness check for a subsample of the data after the wto comes into effect
# because by definition, all of the delegates will not be wto members before 1995


# -----------------------------------------
# load function
# -----------------------------------------

# helper function to insepct model results
analyzeResults = function(modList){
  lapply(modList, function(z){ 
    lapply(z, function(y){
      lapply(y,function(x){
        
        results = summary(x)$coefficients
      
        if(any(grepl('^committee', rownames(results)))){
          results = results[-grep('^committee', rownames(results)),]
        }
 
       return(results)
      })
    })
  })
}

# function that logs variables  
logVar <- function(df, var) {
  varname <- paste("l", var , sep="")
  mutate(df, !!varname := ifelse(df[,var] == 0, 0, log(df[,var])))
}
 

# -----------------------------------------
# load function
# -----------------------------------------
# load file
load(file = paste0(pathMain, '/participation_development/codexWideParticip.rda'))

names(codexWideAll)[1:100] 
# -----------------------------------------
# create variables
# -----------------------------------------
varsToLog = c('yearsAfterProposal',
              'cacAfterProposal',
              'delegates', 
              'delegations', 
              'delegates_cac', 
              'delegations_cac',
              'delegates_cac', 
              'delegations_cac',
              names(codexWideAll)[grep('_region_|_income_|_interest_|_actor_type_', names(codexWideAll))],
              'wto_delegates',
              'wto_delegates_cac',
              'wto_delegations',
              'wto_delegations_cac',
              "numTotProposals"
              )

# log variables
for ( i in seq_along(varsToLog)){
 codexWideAll = logVar(codexWideAll, varsToLog[i])
}


# create average number of delegates
codexWideAll$delegAvg = codexWideAll$delegates/codexWideAll$delegations
codexWideAll$delegAvg_cac = codexWideAll$delegates_cac/codexWideAll$delegations_cac

 
# -----------------------------------------
## select variables
# -----------------------------------------
 
deleg_regions = names(codexWideAll)[setdiff(grep('ldelegates_region', names(codexWideAll)), grep('cac|wto', names(codexWideAll)))]
deleg_regions_cac = names(codexWideAll)[intersect(grep('ldelegates_region', names(codexWideAll)), grep('a_cac$|c_cac|n_cac', names(codexWideAll)))]
delegations_regions = names(codexWideAll)[setdiff(grep('ldelegations_region', names(codexWideAll)), grep('cac|wto', names(codexWideAll)))]
delegations_regions_cac = names(codexWideAll)[intersect(grep('ldelegations_region', names(codexWideAll)), grep('a_cac$|c_cac|n_cac', names(codexWideAll)))]

deleg_regions_wto = names(codexWideAll)[intersect(grep('ldelegates_region', names(codexWideAll)), grep('_wto$', names(codexWideAll)))]
deleg_regions_cac_wto = names(codexWideAll)[intersect(grep('ldelegates_region', names(codexWideAll)), grep('_wto_cac$', names(codexWideAll)))]
delegations_regions_wto = names(codexWideAll)[intersect(grep('ldelegations_region', names(codexWideAll)), grep('_wto$', names(codexWideAll)))]
delegations_regions_cac_wto = names(codexWideAll)[intersect(grep('ldelegations_region', names(codexWideAll)), grep('_wto_cac$', names(codexWideAll)))]

deleg_incomes = names(codexWideAll)[setdiff(grep('ldelegates_income', names(codexWideAll)), grep('cac|wto', names(codexWideAll)))]
deleg_incomes_cac = names(codexWideAll)[intersect(grep('ldelegates_income', names(codexWideAll)), grep('e_cac', names(codexWideAll)))]
delegations_incomes = names(codexWideAll)[setdiff(grep('ldelegations_income', names(codexWideAll)), grep('cac|wto', names(codexWideAll)))]
delegations_incomes_cac = names(codexWideAll)[intersect(grep('ldelegations_income', names(codexWideAll)), grep('e_cac$', names(codexWideAll)))]

deleg_incomes_wto = names(codexWideAll)[intersect(grep('ldelegates_income', names(codexWideAll)), grep('_wto$', names(codexWideAll)))]
deleg_incomes_cac_wto = names(codexWideAll)[intersect(grep('ldelegates_income', names(codexWideAll)), grep('_wto_cac$', names(codexWideAll)))]
delegations_incomes_wto = names(codexWideAll)[intersect(grep('ldelegations_income', names(codexWideAll)), grep('_wto$', names(codexWideAll)))]
delegations_incomes_cac_wto = names(codexWideAll)[intersect(grep('ldelegations_income', names(codexWideAll)), grep('_wto_cac$', names(codexWideAll)))]

deleg_interests = names(codexWideAll)[setdiff(grep('ldelegates_interest', names(codexWideAll)), grep('cac', names(codexWideAll)))]
deleg_interests_cac = names(codexWideAll)[intersect(grep('ldelegates_interest', names(codexWideAll)), grep('cac', names(codexWideAll)))]
delegations_interests = names(codexWideAll)[setdiff(grep('ldelegations_interest', names(codexWideAll)), grep('cac', names(codexWideAll)))]
delegations_interests_cac = names(codexWideAll)[intersect(grep('ldelegations_interest', names(codexWideAll)), grep('cac', names(codexWideAll)))]

deleg_actors = names(codexWideAll)[setdiff(grep('ldelegates_actor', names(codexWideAll)), grep('cac', names(codexWideAll)))]
deleg_actors_cac = names(codexWideAll)[intersect(grep('ldelegates_actor', names(codexWideAll)), grep('cac', names(codexWideAll)))]
delegations_actors = names(codexWideAll)[setdiff(grep('ldelegations_actor', names(codexWideAll)), grep('cac', names(codexWideAll)))]
delegations_actors_cac = names(codexWideAll)[intersect(grep('ldelegations_actor', names(codexWideAll)), grep('cac', names(codexWideAll)))]



dvs = c('yearsAfterProposal',
        'cacAfterProposal',
        'lyearsAfterProposal',
        'lcacAfterProposal')

ivList = list(
              # 'delegates',
              # 'delegations',
              # 'ldelegates',
              # 'ldelegations',
              # 'delegConc',
              # 'delegAvg',
              # 'delegates_cac',
              # 'delegations_cac',
              # 'ldelegates_cac',
              # 'ldelegations_cac',
              # 'delegConc_cac',
              # 'delegAvg_cac',
              # c('delegates', 'delegates_cac'),
              # c('delegations', 'delegations_cac'),
              # c('ldelegates', 'ldelegates_cac'),
              # c('ldelegations', 'ldelegations_cac'),
              # c('delegConc', 'delegConc_cac'),
              # c('delegAvg', 'delegAvg_cac'),
              c('delegates', 'delegates_cac', 'delegConc', 'delegConc_cac'),
              c('delegations', 'delegations_cac', 'delegConc', 'delegConc_cac'),
              c('ldelegates', 'ldelegates_cac', 'delegConc', 'delegConc_cac'),
              c('ldelegations', 'ldelegations_cac', 'delegConc', 'delegConc_cac')#,
              
             # c(deleg_actors, deleg_actors_cac, 'delegConc', 'delegConc_cac' ),
            #  c(delegations_actors, delegations_actors_cac, 'delegConc', 'delegConc_cac' )
              # c(deleg_regions, deleg_regions_cac, 'delegConc', 'delegConc_cac'),
              # c(delegations_regions, delegations_regions_cac, 'delegConc', 'delegConc_cac'),
              # c(deleg_interests, deleg_interests_cac, 'delegConc', 'delegConc_cac'),
              # c(delegations_interests,delegations_interests_cac, 'delegConc', 'delegConc_cac'),
              # c(deleg_incomes, deleg_incomes_cac,'delegConc', 'delegConc_cac'),
              # c(delegations_incomes, delegations_incomes_cac, 'delegConc', 'delegConc_cac')
         
)

 
 
ivListAll = list(ivList, lapply(ivList, function(x){
    sapply(x, function(y){
      paste0(y, '*wtoDummy')
    })}) )

 

 
controlVars = list(c(NULL),
                  # c( 'committee'),
                 #  c( 'cac_start_num', 'cac_end_num', 'committee'), 
                  # c( 'adopted', 'wtoDummy', 'cac_start_num', 'cac_end_num', 'committee'),
                 c('handcodedDummy','adopted', 'wtoDummy', 'cac_start_num', 'cac_end_num', 'committee', 'supersededDummy', 'droppedDummy', "lnumTotProposals"),
                   c('handcodedDummy','adopted', 'wtoDummy', 'cac_start_num', 'cac_end_num', 'meeting_type', 'meeting_status', 'supersededDummy', 'droppedDummy', "lnumTotProposals"))

subData = codexWideAll[which(codexWideAll$yearsAfterProposal<18),]
standardData = codexWideAll[which(codexWideAll$adopted == 1),]
dataSets = list(codexWideAll, subData)

# -----------------------------------------
# run models
# -----------------------------------------

# save results without interaction term with wto
results_poisson = list() 
results_quasipoisson = list()
results_ols = list() 

# save results with interaction term with wto
results_int_poisson = list() 
results_int_quasipoisson = list()
results_int_ols = list() 

# save results without interaction term with wto for subdata
results_poisson_sub = list() 
results_quasipoisson_sub = list()
results_ols_sub = list() 

# save results with interaction term with wto for subdata
results_int_poisson_sub = list() 
results_int_quasipoisson_sub = list()
results_int_ols_sub = list() 
 
for (data in 1:length(dataSets)){
  for (d in dvs){
    for (IL in 1:length(ivListAll)){
      ivs = ivListAll[[IL]]  
      for ( i in 1:length(ivs)){
        for (cv in 1:length(controlVars)){

      if (IL == 1 & data == 1){
        if(grepl('^l', d)==FALSE){
        results_poisson[[d]][[ paste0(ivs[[i]], collapse = '_') ]][[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = poisson(link = 'log'))
        results_quasipoisson[[d]][[ paste0(ivs[[i]], collapse = '_')]][[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = quasipoisson(link = "log"))}
        
        if(grepl('^l', d)){
        results_ols[[d]][[ paste0(ivs[[i]], collapse = '_') ]][[cv]] = lm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]])}
        
      } else if (IL == 2 & data == 1){
        if(grepl('^l', d)==FALSE){
        results_int_poisson[[d]][[paste0(ivs[[i]], collapse = '_')  ]][[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = poisson(link = 'log'))
        results_int_quasipoisson[[d]][[paste0(ivs[[i]], collapse = '_')  ]][[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = quasipoisson(link = "log"))}

        if(grepl('^l', d)){
          results_int_ols[[d]][[ paste0(ivs[[i]], collapse = '_') ]][[cv]] = lm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]] )}
        
      } else if (IL == 1 & data == 2){
        if(grepl('^l', d)==FALSE){
        results_poisson_sub[[d]][[paste0(ivs[[i]], collapse = '_')  ]][[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = poisson(link = 'log'))
        results_quasipoisson_sub[[d]][[paste0(ivs[[i]], collapse = '_')  ]][[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = quasipoisson(link = "log"))}
        if(grepl('^l', d)){
          results_ols_sub[[d]][[ paste0(ivs[[i]], collapse = '_') ]][[cv]] = lm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]])}
        
      } else if (IL == 2 & data == 2){
        
        if(grepl('^l', d)==FALSE){
        results_int_poisson_sub[[d]][[paste0(ivs[[i]], collapse = '_') ]][[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = poisson(link = 'log'))
        results_int_quasipoisson_sub[[d]][[paste0(ivs[[i]], collapse = '_') ]] [[cv]] = glm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]], family = quasipoisson(link = "log"))}
        
        if(grepl('^l', d)){
          results_int_ols_sub[[d]][[ paste0(ivs[[i]], collapse = '_') ]][[cv]] = lm(as.formula(paste0(d, '~', paste(c(ivs[[i]], controlVars[[cv]]), collapse = '+' ))),data = dataSets[[data]])}}

       }
    }
  }
  }
}
 

# -----------------------------------------
## take a first look at the model results
# -----------------------------------------
options(digits=4)
# for models without interaction term with wto
analyzeResults(results_poisson)
analyzeResults(results_quasipoisson)$yearsAfterProposal

# for models with interaction term with wto
analyzeResults(results_int_poisson)
analyzeResults(results_int_quasipoisson)$yearsAfterProposal

# for models without interaction term with wto for subdata
analyzeResults(results_poisson_sub)
analyzeResults(results_quasipoisson_sub)

analyzeResults(results_quasipoisson_sub)$yearsAfterProposal

# for models with interaction term with wto for subdata
analyzeResults(results_int_poisson_sub)
analyzeResults(results_int_quasipoisson_sub)$yearsAfterProposal %>% names()

# -----------------------------------------
## plot interaction effects...
# -----------------------------------------

test2 = results_int_ols_sub$lyearsAfterProposal$`ldelegates*wtoDummy_ldelegates_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[2]]
test3 = results_int_ols_sub$lyearsAfterProposal$`ldelegates*wtoDummy_ldelegates_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[3]]

test2d = results_int_ols_sub$lyearsAfterProposal$`ldelegations*wtoDummy_ldelegations_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[2]]
test3d = results_int_ols_sub$lyearsAfterProposal$`ldelegations*wtoDummy_ldelegations_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[3]]


testc2d = results_int_ols_sub$lcacAfterProposal$`ldelegations*wtoDummy_ldelegations_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[2]]
testc3d = results_int_ols_sub$lcacAfterProposal$`ldelegations*wtoDummy_ldelegations_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[3]]

testc2 = results_int_ols_sub$lcacAfterProposal$`ldelegates*wtoDummy_ldelegates_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[2]]
testc3 = results_int_ols_sub$lcacAfterProposal$`ldelegates*wtoDummy_ldelegates_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[3]]

test2p = results_int_quasipoisson_sub$yearsAfterProposal$`ldelegates*wtoDummy_ldelegates_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[2]]
test3p = results_int_quasipoisson_sub$yearsAfterProposal$`ldelegates*wtoDummy_ldelegates_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[3]]

test2p = results_int_quasipoisson_sub$yearsAfterProposal$`ldelegates_actor_type_NGO*wtoDummy_ldelegates_actor_type_state*wtoDummy_ldelegates_actor_type_IGO*wtoDummy_ldelegates_actor_type_NGO_cac*wtoDummy_ldelegates_actor_type_state_cac*wtoDummy_ldelegates_actor_type_IGO_cac*wtoDummy_delegConc*wtoDummy_delegConc_cac*wtoDummy`[[2]]


test2p %>% summary()
plot_model(test, type = 'pred', terms = c( 'delegations_actor_type_state',  'wto_delegations_perc', 'wtoDummy'))



 
plot_model(test2p, type = 'pred', terms = c( 'delegConc', 'wtoDummy'))
plot_model(test2p, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'))
plot_model(test2p, type = 'pred', terms = c( 'ldelegates_actor_type_state','wtoDummy'))
plot_model(test2p, type = 'pred', terms = c( 'ldelegates_actor_type_state_cac','wtoDummy'))

plot_model(test2p, type = 'pred', terms = c( 'ldelegates_actor_type_NGO','wtoDummy'))
plot_model(test2p, type = 'pred', terms = c( 'ldelegates_actor_type_NGO_cac','wtoDummy'), ci.lvl = c(0.95))

?plot_model

plot_model(test3p, type = 'pred', terms = c( 'delegConc', 'wtoDummy'))
plot_model(test3p, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'), show.values = TRUE)
plot_model(test3p, type = 'pred', terms = c( 'ldelegates_cac','wtoDummy'))
plot_model(test3p, type = 'pred', terms = c( 'ldelegates','wtoDummy'))

?plot_model

plot_model(testc2, type = 'pred', terms = c( 'delegConc', 'wtoDummy'))
plot_model(testc2, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'))
plot_model(testc2, type = 'pred', terms = c( 'ldelegates_cac','wtoDummy'))
plot_model(testc2, type = 'pred', terms = c( 'ldelegates','wtoDummy'))


plot_model(testc3, type = 'pred', terms = c( 'delegConc', 'wtoDummy'), transform = 'exp')
plot_model(testc3, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'), transform = 'exp')
plot_model(testc3, type = 'pred', terms = c( 'ldelegations_cac','wtoDummy'), transform = 'exp')
plot_model(testc3, type = 'pred', terms = c( 'ldelegations','wtoDummy'))


plot_model(test2, type = 'pred', terms = c( 'delegConc', 'wtoDummy'))
plot_model(test2, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'))
plot_model(test2, type = 'pred', terms = c( 'ldelegates_cac','wtoDummy'))
plot_model(test2, type = 'pred', terms = c( 'ldelegates','wtoDummy'))

plot_model(test3, type = 'pred', terms = c( 'delegConc', 'wtoDummy'))
plot_model(test3, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'))
plot_model(test3, type = 'pred', terms = c( 'ldelegates_cac','wtoDummy'))
plot_model(test3, type = 'pred', terms = c( 'ldelegates','wtoDummy'))

plot_model(test2d, type = 'pred', terms = c( 'delegConc', 'wtoDummy'))
plot_model(test2d, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'))
plot_model(test2d, type = 'pred', terms = c( 'ldelegations_cac','wtoDummy'))
plot_model(test2d, type = 'pred', terms = c( 'ldelegations','wtoDummy'))


plot_model(test3d, type = 'pred', terms = c( 'delegConc', 'wtoDummy'))
plot_model(test3d, type = 'pred', terms = c( 'delegConc_cac', 'wtoDummy'))
plot_model(test3d, type = 'pred', terms = c( 'ldelegations_cac','wtoDummy'))
plot_model(test3d, type = 'pred', terms = c( 'ldelegations','wtoDummy'))


# add in data for percent wto membership 

?plot_model
exp(1.5)
lapply(results_int_ols_sub$lyearsAfterProposal, length)
analyzeResults(results_int_ols_sub)$lyearsAfterProposal 
analyzeResults(results_ols_sub)


?plot_model

analyzeResults(results_int_quasipoisson_sub)$yearsAfterProposal
summary(results_int_quasipoisson_sub$yearsAfterProposal[[1]][[3]])$coefficients %>% rownames()


model1 = glm( yearsAfterProposal ~ log(delegates) , data = codexWideAll, family = poisson(link = 'log'))

# model2 = glm( yearsAfterProposal ~ log(delegates) + committee + cac_start_num + cac_end_num , data = codexWideAll, family = poisson(link = 'log'))
# model3 = glm( yearsAfterProposal ~ log(delegates)  + wtoDummy + cac_start_num + cac_end_num + as.factor(committee), data = codexWideAll, family = poisson(link = 'log'))
# model4 = glm( yearsAfterProposal ~ log(delegates)*wtoDummy + cac_start_num + cac_end_num + as.factor(committee) , data = codexWideAll, family = poisson(link = 'log'))
# model5 = glm( yearsAfterProposal ~ delegates*wtoDummy + cac_start_num + cac_end_num + as.factor(committee) , data = codexWideAll, family = poisson(link = 'log'))
# model6 = glm( yearsAfterProposal ~ delegConc + wtoDummy+ cac_start_num + cac_end_num + as.factor(committee) , data = codexWideAll, family = poisson(link = 'log'))
# model7 = glm( yearsAfterProposal ~ delegConc*wtoDummy + cac_start_num + cac_end_num + as.factor(committee) , data = codexWideAll, family = poisson(link = 'log'))


