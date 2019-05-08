# model: drc_malaria_impact4.r
# purpose: drc_malaria_impact3 with controls for completeness

# to do

model = '

	# linkage 1 regressions
	ITN_rate_cumul ~ prior("dgamma(1,1)")*ITN + completeness_ITN_consumed
	mildMalariaTreated_under5_rate ~ prior("dgamma(1,1)")*mildMalariaTreated_under5 + RDT_rate + completeness_totalPatientsTreated
	severeMalariaTreated_under5_rate ~ prior("dgamma(1,1)")*severeMalariaTreated_under5 + RDT_rate + completeness_severeMalariaTreated
	ACTs_CHWs_under5_rate ~ prior("dgamma(1,1)")*SSCACT_under5 + completeness_ACTs_SSC
	SP_rate ~ prior("dgamma(1,1)")*SP + completeness_SP
	RDT_rate ~ prior("dgamma(1,1)")*RDT + completeness_RDT_completed
	
	# linkage 2 regressions
	lead_newCasesMalariaMild_under5_rate ~ prior("dnorm(-1,1)")*ITN_rate_cumul + prior("dnorm(-1,1)")*mildMalariaTreated_under5_rate + prior("dnorm(-1,1)")*ACTs_CHWs_under5_rate + prior("dnorm(-1,1)")*SP_rate + completeness_ITN_consumed + completeness_totalPatientsTreated + completeness_ACTs_SSC + completeness_SP
	
	lead_newCasesMalariaSevere_under5_rate ~ prior("dnorm(-1,1)")*ITN_rate_cumul + prior("dnorm(-1,1)")*severeMalariaTreated_under5_rate + prior("dnorm(-1,1)")*ACTs_CHWs_under5_rate + prior("dnorm(-1,1)")*SP_rate + completeness_ITN_consumed + completeness_severeMalariaTreated + completeness_ACTs_SSC + completeness_SP
	
	lead_case_fatality_under5 ~ prior("dnorm(-1,1)")*mildMalariaTreated_under5_rate + prior("dnorm(-1,1)")*severeMalariaTreated_under5_rate + prior("dnorm(-1,1)")*ACTs_CHWs_under5_rate + completeness_totalPatientsTreated + completeness_severeMalariaTreated + completeness_ACTs_SSC
	
	lead_malariaDeaths_under5_rate ~ prior("dnorm(-1,1)")*lead_newCasesMalariaMild_under5_rate + prior("dnorm(-1,1)")*lead_newCasesMalariaSevere_under5_rate + prior("dgamma(1,1)")*lead_case_fatality_under5
	
	# latent variables
	
	# fixed variances
	
	# covariances
	lead_newCasesMalariaSevere_under5_rate ~~ lead_newCasesMalariaMild_under5_rate
	severeMalariaTreated_under5_rate ~~ mildMalariaTreated_under5_rate
	ACTs_CHWs_under5_rate ~~ mildMalariaTreated_under5_rate
	severeMalariaTreated_under5_rate ~~ ACTs_CHWs_under5_rate
	lead_newCasesMalariaSevere_under5_rate ~~ lead_case_fatality_under5 
	lead_newCasesMalariaMild_under5_rate ~~ lead_case_fatality_under5 
	
	# fixed covariances
	ITN ~~ 0*mildMalariaTreated_under5
	ITN ~~ 0*severeMalariaTreated_under5
	ITN ~~ 0*SSCACT_under5
	ITN ~~ 0*SP
	ITN ~~ 0*RDT
	mildMalariaTreated_under5 ~~ 0*RDT
	mildMalariaTreated_under5 ~~ 0*severeMalariaTreated_under5
	mildMalariaTreated_under5 ~~ 0*SSCACT_under5
	mildMalariaTreated_under5 ~~ 0*SP
	severeMalariaTreated_under5 ~~ 0*RDT
	severeMalariaTreated_under5 ~~ 0*SSCACT_under5
	severeMalariaTreated_under5 ~~ 0*SP
	RDT ~~ 0*SSCACT_under5
	RDT ~~ 0*SP
	RDT ~~ 0*SP
	
'
