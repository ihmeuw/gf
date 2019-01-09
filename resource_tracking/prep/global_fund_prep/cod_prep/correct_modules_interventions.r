# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Correct unmapped modules for DRC resource tracking database. 
#         Add initial, date, and filepath.
# DATE: Last updated December 2018
# --------------------------------------------------------------------

correct_modules_interventions = function(resource_database){

#Irena Chen, before October 2018 
resource_database[module == "systcmesdesanteresiliantsetperennesstrategiesnationalesdesante"  & intervention == "strategiessanitairesnationalesalignementaveclesplansmaladiespecifiquesgouvernancedusecteurdelasanteetfinancement", module := 'ssrsestrategiasnacionalesdesalud']
resource_database[module == 'ssrsestrategiasnacionalesdesalud' & intervention == "strategiessanitairesnationalesalignementaveclesplansmaladiespecifiquesgouvernancedusecteurdelasanteetfinancement", intervention := 'estrategiasnacionalesensaludalineamientoconplanesespecificosdeenfermedadesgobernanzayfinanciamientoenelsectorsalud']

#EKL 10/25/18, official_budgets/1c.COD-M-SANRU_Budget_IL1_20.08.2018.xlsx
resource_database$intervention = ifelse(resource_database$module == "lutteantivectorielle" & resource_database$intervention == "ieccccpriseencharge", "iecccc", resource_database$intervention)

#EKL 11/2/18, pudrs/Copy of LFA_Review_COD-T-MOH_Progress Report_30Jun2018_Sent_02102018-Brk....xlsx
resource_database$module = ifelse(resource_database$module == "humanresourcesforhealthhrhincludingcommunityhealthworkers" & resource_database$intervention == "retentionandscaleupofhealthworkersincludingforcommunityhealthworkers",
                        "humanresourcesforhealthincludingcommunityhealthworkers", resource_database$module)

#EKL 11/2/18, pudrs/Copy of LFA Review_COD-H-MOH_Progress  Report_30Jun2018_07092018 ok_Sent....xlsb.xlsx
resource_database$module = ifelse(resource_database$module == "comprehensivepreventionprogramsfortgs", "comprehensivepreventionprogramsfortransgenderpeople", resource_database$module)
resource_database$intervention = ifelse(resource_database$module == "comprehensivepreventionprogramsfortransgenderpeople" & resource_database$intervention == "hivtestingservicesfortgs", 
                              "hivtestingservicesfortransgenderpeople", resource_database$intervention)
resource_database$intervention = ifelse(resource_database$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners" & resource_database$intervention == "diagnosisandtreatmentofstisandothersexualhealthservicesforpwid", 
                              "diagnosisandtreatmentofsexuallytransmittedinfectionsandothersexualhealthservicesforpeoplewhoinjectdrugs", resource_database$intervention)
resource_database$intervention = ifelse(resource_database$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners" & resource_database$intervention == "behavioralinterventionsforpwid", 
                              "behavioralinterventionsforpeoplewhoinjectdrugs", resource_database$intervention)
resource_database$intervention = ifelse(resource_database$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners" & resource_database$intervention == "hivtestingservicesforpwid", 
                              "hivtestingservicesforpeoplewhoinjectdrugs", resource_database$intervention)
resource_database$module = ifelse(resource_database$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners", "comprehensivepreventionprogramsforpeoplewhoinjectdrugsandtheirpartners", 
                        resource_database$module)

#EKL 11/14/18, COD_C_CORDAID_catalytic_budget_27Feb2018.xlsx, LFA_Review_COD-C-CORDAID_PU 30 June 2018_Sent_27092018_OK.xlsx, NFMBudget_COD-H-CORDAID_Finance.xlsx, 
#   Revised LFA_COD-H-MOH_PUDR_S2 2016 17.04.17 for denominator change.xlsx, COD-H-SANRU_Budget.xlsx, initial_gf_budgets_2018_2020.csv
resource_database$module = ifelse(resource_database$module == "programmesdepreventiondestinesylapopulationgenerale", "programmesdepreventiondestinesslapopulationgenerale", resource_database$module)
resource_database$intervention = ifelse(resource_database$intervention == 'diagnosticettraitementdesistetautresservicesliesylasantesexuellepourleshsh', "diagnosticettraitementdesistetautresservicesliesslasantesexuellepourleshsh", 
                              resource_database$intervention)
resource_database$module = ifelse(resource_database$module == "programmesdepreventioncompletsdestinesauxprofessionnelsdusexeetyleursclients","programmesdepreventioncompletsdestinesauxprofessionnelsdusexeetsleursclients", 
                        resource_database$module)
resource_database$intervention = ifelse(resource_database$module == "programmesdepreventiondestinesslapopulationgenerale" & resource_database$intervention == "changementdecomportementdanslecadredesprogrammesdestinesylapopulationgenerale", 
                              "changementdecomportementdanslecadredesprogrammesdestinesslapopulationgenerale", resource_database$intervention)
resource_database$module = ifelse(resource_database$module == "programmesdepreventioncompletsdestinesauxusagersdedroguesinjectablesetyleurspartenaires", "programmesdepreventioncompletsdestinesauxusagersdedroguesinjectablesetsleurspartenaires"
                        , resource_database$module)
resource_database$intervention = ifelse(resource_database$intervention == "diagnosticettraitementdesinfectionssexuellementtransmissiblesdanslecadredesprogrammesdestinesauxconsommateursdedroguesinjectablesetyleurspartenaires", 
                              "diagnosticettraitementdesinfectionssexuellementtransmissiblesdanslecadredesprogrammesdestinesauxconsommateursdedroguesinjectablesetsleurspartenaires", resource_database$intervention )
resource_database$intervention = ifelse(resource_database$intervention == "interventionsliesauxaiguillesetauxseringuesdestinesauxusagersdedroguesinjectablesetyleurspartenaires",
                              "interventionsliesauxaiguillesetauxseringuesdestinesauxusagersdedroguesinjectablesetsleurspartenaires", resource_database$intervention)
resource_database$module = ifelse(resource_database$module == "preventiondelatransmissiondelamcreylenfantptme", "preventiondelatransmissiondelamcreslenfantptme", resource_database$module )
resource_database$module = ifelse(resource_database$module == "programmesvisantyreduirelesobstaclesliesauxdroitshumainsquientraventlacccsauxservicesvih", 
                        "programmesvisantsreduirelesobstaclesliesauxdroitshumainsquientraventlacccsauxservicesvih", resource_database$module )
resource_database$intervention = ifelse(resource_database$intervention == "servicesjuridiquesliesauvihetylacoinfectionvihtuberculose", "servicesjuridiquesliesauvihetslacoinfectionvihtuberculose", resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "ameliorationdesloisdesreglementationsetdespolitiquesrelativesauvihetylacoinfectionvihtuberculose", 
                              "ameliorationdesloisdesreglementationsetdespolitiquesrelativesauvihetslacoinfectionvihtuberculose", resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "preservatifsdanslecadredesprogrammesdestinesylapopulationgenerale", "preservatifsdanslecadredesprogrammesdestinesslapopulationgenerale", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "formationdesprofessionnelsdesanteenmaticrededroitshumainsetdethiquemedicaleliesylaluttecontrelevihetylalutteconjointecontrelevihlatuberculose", 
                              "formationdesprofessionnelsdesanteenmaticrededroitshumainsetdethiquemedicaleliesslaluttecontrelevihetslalutteconjointecontrelevihlatuberculose", resource_database$intervention)

resource_database$module = ifelse(resource_database$module == "preventiondelatransmissiondelamcreylenfant", "preventiondelatransmissiondelamcreslenfant", resource_database$module)
resource_database$intervention = ifelse(resource_database$module == "tbhiv" & resource_database$intervention == "communitytbcaredelivery" & resource_database$disease == "tb/hiv", "communitytbhivcaredelivery", resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "changementdecomportementdanslecadredesprogrammesdestinesylapopulationgenerale","changementdecomportementdanslecadredesprogrammesdestinesslapopulationgenerale", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "changementdecomportementdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetyleursclients", "changementdecomportementdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetsleursclients", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "depistageduvihetconseildanslecadredesprogrammesdestinesauxprofessionnelsdusexeetyleursclients","depistageduvihetconseildanslecadredesprogrammesdestinesauxprofessionnelsdusexeetsleursclients", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "volet1preventionprimairedelinfectionyvihchezlesfemmesenagedeprocreer", "volet1preventionprimairedelinfectionsvihchezlesfemmesenagedeprocreer", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "depistageduvihetconseildanslecadredesprogrammesdestinesylapopulationgenerale", "depistageduvihetconseildanslecadredesprogrammesdestinesslapopulationgenerale", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "autresinterventionsrealiseespourprevenirlatransmissiondelamcreylenfantveuillezpreciser","autresinterventionsrealiseespourprevenirlatransmissiondelamcreslenfantveuillezpreciser", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "preservatifsdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetyleursclients", "preservatifsdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetsleursclients", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$intervention == "diagnosticettraitementdesinfectionssexuellementtransmissiblesdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetyleursclients", 
                              "diagnosticettraitementdesinfectionssexuellementtransmissiblesdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetsleursclients", resource_database$intervention)
resource_database$module = ifelse(resource_database$module == "depistageduvihetconseildanslecadredesprogrammesdestinesauxprofessionnelsdusexeetyleursclients", "depistageduvihetconseildanslecadredesprogrammesdestinesauxprofessionnelsdusexeetsleursclients", 
                        resource_database$module)
resource_database$intervention = ifelse(resource_database$intervention == "diagnosticettraitementdesinfectionssexuellementtransmissiblesdanslecadredesprogrammesdestinesylapopulationgenerale", 
                              "diagnosticettraitementdesinfectionssexuellementtransmissiblesdanslecadredesprogrammesdestinesslapopulationgenerale", resource_database$intervention )
resource_database$intervention = ifelse(resource_database$intervention == "reductiondesrisquesdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetyleursclients", "reductiondesrisquesdanslecadredesprogrammesdestinesauxprofessionnelsdusexeetsleursclients", 
                              resource_database$intervention)
resource_database$intervention = ifelse(resource_database$module == "mdrtb" & resource_database$intervention == "otherservicedeliveryinterventions", "othermdrtbinterventions", resource_database$intervention)
resource_database$module = ifelse(resource_database$intervention == "otherinterventionsforadolescentandyouth", "preventionprogramsforadolescentsandyouthinandoutofschool", resource_database$module)

#EKL 11/14/18 LFA_Review_COD-C-CORDAID_PU 30 June 2018_Sent_27092018_OK.xlsx - needed to reclassify combined tb/hiv grants as either one disease or the other. 
resource_database$disease = ifelse((resource_database$module == "tbcareandprevention" | resource_database$module == "mdrtb") & resource_database$disease == "tb/hiv", "tb", resource_database$disease)
resource_database$disease = ifelse((resource_database$module == "comprehensivepreventionprogramsfortransgenderpeople" | resource_database$module == "comprehensivepreventionprogramsforsexworkersandtheirclients" | resource_database$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugsandtheirpartners" 
                          | resource_database$module == "comprehensivepreventionprogramsformenwhohavesexwithmen" | resource_database$module == "preventionprogramsforgeneralpopulation" | resource_database$module == "programstoreducehumanrightsrelatedbarrierstohivservices" | 
                            resource_database$module == "preventionprogramsforadolescentsandyouthinandoutofschool")
                         & resource_database$disease == "tb/hiv", "hiv", resource_database$disease)
resource_database$disease = ifelse(resource_database$module == "treatmentcareandsupport" & resource_database$disease == "tb/hiv", "hiv", resource_database$disease)
resource_database$disease = ifelse(resource_database$module == "pmtct" & resource_database$disease == "tb/hiv", "hiv", resource_database$disease)

#EKL 12/17/18 
resource_database[module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspeoplewhoinjectdrugsandtheirpartners", 
                  module:= "comprehensivepreventionprogramsforpeoplewhoinjectdrugsandtheirpartners"]
resource_database[module == "rsshhumanresourcesforhealthhrhincludingcommunityhealthworkers", module:= "rsshhumanresourcesforhealthincludingcommunityhealthworkers"]
resource_database[module == "4", module:= "all"]
resource_database[intervention == "6", intervention:= "all"]
resource_database[sda_activity == "9", sda_activity:= "all"]

#EKL 1/2/19 initial_gf_budgets_2018_2020.csv
resource_database[fileName == 'initial_gf_budgets_2018_2020.csv' & module == 'tuberculosemultiresistante' & intervention == 'all', disease:='tb']

#EKL 12/21/18 - relabeling all "tb/hiv" as "hiv/tb"
resource_database[disease == "tb/hiv", disease:= "hiv/tb"]

return(resource_database)
}
