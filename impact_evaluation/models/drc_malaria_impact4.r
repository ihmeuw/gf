# model: drc_malaria_impact4.r
# purpose: drc_malaria_impact3 with controls for completeness

# to do

model = '

	# linkage 1 regressions
	ITN_rate_cumul ~ prior("dgamma(1,1)")*ITN + completeness_ITN_consumed
	mildMalariaTreated_rate ~ prior("dgamma(1,1)")*mildMalariaTreated + RDT_rate + completeness_totalPatientsTreated
	severeMalariaTreated_rate ~ prior("dgamma(1,1)")*severeMalariaTreated + RDT_rate + completeness_severeMalariaTreated
	ACTs_CHWs_rate ~ prior("dgamma(1,1)")*SSCACT + completeness_ACTs_SSC
	SP_rate ~ prior("dgamma(1,1)")*SP + completeness_SP
	RDT_rate ~ prior("dgamma(1,1)")*RDT + completeness_RDT_completed
	
	# linkage 2 regressions
	lead_newCasesMalariaMild_rate ~ prior("dnorm(-1,1)")*ITN_rate_cumul + prior("dnorm(-1,1)")*mildMalariaTreated_rate + prior("dnorm(-1,1)")*ACTs_CHWs_rate + prior("dnorm(-1,1)")*SP_rate + completeness_ITN_consumed + completeness_totalPatientsTreated + completeness_ACTs_SSC + completeness_SP
	
	lead_newCasesMalariaSevere_rate ~ prior("dnorm(-1,1)")*ITN_rate_cumul + prior("dnorm(-1,1)")*severeMalariaTreated_rate + prior("dnorm(-1,1)")*ACTs_CHWs_rate + prior("dnorm(-1,1)")*SP_rate + completeness_ITN_consumed + completeness_severeMalariaTreated + completeness_ACTs_SSC + completeness_SP
	
	lead_case_fatality ~ prior("dnorm(-1,1)")*mildMalariaTreated_rate + prior("dnorm(-1,1)")*severeMalariaTreated_rate + prior("dnorm(-1,1)")*ACTs_CHWs_rate + completeness_totalPatientsTreated + completeness_severeMalariaTreated + completeness_ACTs_SSC
	
	lead_malariaDeaths_rate ~ prior("dnorm(-1,1)")*lead_newCasesMalariaMild_rate + prior("dnorm(-1,1)")*lead_newCasesMalariaSevere_rate + prior("dgamma(1,1)")*lead_case_fatality
	
	# latent variables
	
	# fixed variances
	
	# covariances
	
	# fixed covariances
	ITN ~~ 0*mildMalariaTreated
	ITN ~~ 0*severeMalariaTreated
	ITN ~~ 0*SSCACT
	ITN ~~ 0*SP
	ITN ~~ 0*RDT
	mildMalariaTreated ~~ 0*RDT
	mildMalariaTreated ~~ 0*severeMalariaTreated
	mildMalariaTreated ~~ 0*SSCACT
	mildMalariaTreated ~~ 0*SP
	severeMalariaTreated ~~ 0*RDT
	severeMalariaTreated ~~ 0*SSCACT
	severeMalariaTreated ~~ 0*SP
	RDT ~~ 0*SSCACT
	RDT ~~ 0*SP
	RDT ~~ 0*SP
	
'
