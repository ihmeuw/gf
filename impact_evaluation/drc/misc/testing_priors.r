# random scratchy code for testing priors
# all of the below are variants on the prior for should-be-negative coefficients...

# dnorm(-1,.01)
set.seed(1)
source('./impact_evaluation/models/drc_malaria_impact3.r')
name = 'norm_n1_01'
assign(paste0('semFit_', name), bsem(model, subData, adapt=500, burnin=100, sample=100, bcontrol=list(thin=3)))
parTable(get(paste0('semFit_', name)))
standardizedSolution(get(paste0('semFit_', name)))

# dnorm(-10,.01)
set.seed(1)
source('./impact_evaluation/models/drc_malaria_impact3.r')
name = 'norm_n10_01'
assign(paste0('semFit_', name), bsem(model, subData, adapt=500, burnin=100, sample=100, bcontrol=list(thin=3)))
parTable(get(paste0('semFit_', name)))
standardizedSolution(get(paste0('semFit_', name)))

# dnorm(-10,.001)
set.seed(1)
source('./impact_evaluation/models/drc_malaria_impact3.r')
name = 'norm_n10_001'
assign(paste0('semFit_', name), bsem(model, subData, adapt=500, burnin=100, sample=100, bcontrol=list(thin=3)))
parTable(get(paste0('semFit_', name)))
standardizedSolution(get(paste0('semFit_', name)))

# dnorm(-1000,.001)
set.seed(1)
source('./impact_evaluation/models/drc_malaria_impact3.r')
name = 'norm_n1000_001'
assign(paste0('semFit_', name), bsem(model, subData, adapt=500, burnin=100, sample=100, bcontrol=list(thin=3)))
parTable(get(paste0('semFit_', name)))
standardizedSolution(get(paste0('semFit_', name)))

# default
set.seed(1)
source('./impact_evaluation/models/drc_malaria_impact3.r')
name = 'default'
assign(paste0('semFit_', name), bsem(model, subData, adapt=500, burnin=100, sample=100, bcontrol=list(thin=3)))
parTable(get(paste0('semFit_', name)))
standardizedSolution(get(paste0('semFit_', name)))

# dunif(-1,0)
set.seed(1)
source('./impact_evaluation/models/drc_malaria_impact3.r')
name = 'unif_n1_0'
assign(paste0('semFit_', name), bsem(model, subData, adapt=500, burnin=100, sample=100, bcontrol=list(thin=3)))
parTable(get(paste0('semFit_', name)))
standardizedSolution(get(paste0('semFit_', name)))


# indices to check
idx = which(parTable(semFit_norm_n1_1)$prior=='dnorm(-1,1)')

i=14
parTable(semFit_norm_n1_01)[idx,]
parTable(semFit_norm_n10_01)[idx,]
parTable(semFit_norm_n10_001)[idx,]
parTable(semFit_norm_n1000_001)[idx,]
parTable(semFit_default)[idx,]
parTable(semFit_unif_n1_0)[idx,]



