Synthesis 2020

This folder contains code which has been written and used to analyze the PCE Synthesis 2020-2021 report.

Code:

synthesis_budget_prep.R: This code was written by Matt Schneider, pulling in budgets from the 4 IHME/PATH PCE countries during NFM2 and NFM3 (source detailed budgets). 
	It created the file draft_synthesis_budget_quant.xlsx within the Global Fund Box folder "synthesisi/data".

synthesis_absorption_prep.R: This code was written by Matt Schneider, pulling in absorption from the 4 IHME/PATH PCE countries during NFM2 (source PUDRs). 
	It created the file draft_synthesis_absorption_quant.xlsx within the Global Fund Box folder "synthesisi/data".

synthesis_indicator_prep.R: This code was written by Matt Schneider, pulling in budgets from the 4 IHME/PATH PCE countries during NFM2 (source PUDRs). 
	It created the file draft_synthesis_indicators_quant.xlsx within the Global Fund Box folder "synthesisi/data".

synthesis_absorption_all_hrg_rssh_fig.R: Uses above prepped absorption data, in addition to the absorption data shared by EHG (other consortium, which has 4 additional countries)
	to create the line graph of average absorption for all NFM modules, HRG-Equity related modules, and RSSH modules by country and year. This figure was used within the 
	synthesis report and slides. This code was written by Matt Schneider, adpated from Francisco Rios Casas.

synthesis_nfm2_nfm3_crossconsortia_figures_nfm3gmincluded.R: Creates budget figures for synthesis 2020 report and slides, including horizontal bar charts for HRG-Equity and
	RSSH investment changes during NFM2 implementation and changing trajectory between NFM2 and NFM3. This code was written by Matt Schneider, adpated from Francisco Rios Casas.