
#...................................................................................................#
#### ABCD4kids study: Extract Analysis Dataset from 4C database                                  ####
# author: Jonas Hofstetter                                                                          #
# last update: 12.05.2023                                                                           #
#...................................................................................................#

## ............................................ ##
## install relevant packages             
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("haven")
#install.packages("openxlsx")
## ............................................ ##

## ............................................ ##
## load relevant packages             
library(plyr)
library(dplyr)
library(haven)
library(openxlsx)
## ............................................ ##

## ............................................ ##
## load derived 4C datasets
investigation <- read_sas("~/investigation.sas7bdat", NULL)
medication <- read_sas("~/medication.sas7bdat", NULL)
abpm <- read_sas("~/abpm.sas7bdat", NULL)
patients <- read_sas("~/patients.sas7bdat", NULL)
med_names <- read.xlsx("~/Medication_names_dictionary.xlsx")

## load cardiovascular sds measures computed with lms-method in SAS
v_sds <- read_sas("~/cv_sds.sas7bdat", NULL)
pwv_sds_h_3h <- read_sas("~/pwv_sds_h_3h.sas7bdat", NULL)
## ............................................ ##

#### Derived Investigation Dataset: Visit Inclusion Around KRT Onset & Variable Selection #### 

## reorder columns
investigation_reordered_var <- investigation %>% select(patid, datadate, month, vdate, rrtdate, tpldate, diadate, height, center, everything())
#View(investigation_reordered_var)


### Problem: Date variable rrtdate does not consistently indicate the start of renal replacement therapy
### Solution: Redefine variable rrtdate so that it indicates the earlier date of either dialysis or pre-emptive transplantation 

## reset all original date values of rrtdate variable to NA
for (row in 1:nrow(investigation_reordered_var)) {
  if (!is.na(investigation_reordered_var$rrtdate[row])) {
    investigation_reordered_var$rrtdate[row] <- NA
  }
}

## write date of either dialysis (diadate) or transplantation (tpldate) to rrtdate variable at the same visit date 
## when either diadate or tpldate were documented
for (row in 1:nrow(investigation_reordered_var)) {
  if (!is.na(investigation_reordered_var$diadate[row])) {
    investigation_reordered_var$rrtdate[row] <- investigation_reordered_var$diadate[row]
  } else if (!is.na(investigation_reordered_var$tpldate[row])) {
    investigation_reordered_var$rrtdate[row] <- investigation_reordered_var$tpldate[row]
  }
}

## replace all but the first date entry in rrtdate per patient with NA
investigation_reordered_var_rrtdate_new <- investigation_reordered_var %>% 
                                                        group_by(patid) %>% 
                                                        dplyr::mutate(rrtdate = replace(rrtdate, rrtdate > dplyr::first(na.omit(rrtdate)), NA))


## create variable "KRT_onset_by" that indicates if KRT start was due to dialysis or due to pre-emptive transplantation
investigation_reordered_var_rrtdate_new$KRT_onset_by <- NA

for (row in 1:nrow(investigation_reordered_var_rrtdate_new)) {
  if (!is.na(investigation_reordered_var_rrtdate_new$rrtdate[row]) & !is.na(investigation_reordered_var_rrtdate_new$diadate[row])) {
    investigation_reordered_var_rrtdate_new$KRT_onset_by[row] <- 1 
  } else if (!is.na(investigation_reordered_var_rrtdate_new$rrtdate[row]) & !is.na(investigation_reordered_var_rrtdate_new$tpldate[row])) {
    investigation_reordered_var_rrtdate_new$KRT_onset_by[row] <- 0
  } else {
    investigation_reordered_var_rrtdate_new$KRT_onset_by[row] <- NA
  }
}

## reorder variables
investigation_reordered_var_rrtdate_new <- investigation_reordered_var_rrtdate_new %>% select(patid, datadate, month, vdate, rrtdate, KRT_onset_by, tpldate, diadate, height, center, everything())

## create two separate data sets: 
# - one for patients with a documented rrtdate (patients with either KRT start by dialysis, or by pre-emptive transplantation)
# - one for patients without a documented rrtdate (patients without KRT start)
only_pat_without_rrtdate <- investigation_reordered_var_rrtdate_new %>% group_by(patid) %>% filter(all(is.na(rrtdate)))
only_pat_with_rrtdate <- investigation_reordered_var_rrtdate_new %>% group_by(patid) %>% filter(!all(is.na(rrtdate)))

## check number of patients in both subsets
n1 <- length(unique(only_pat_without_rrtdate$patid))
n2 <- length(unique(only_pat_with_rrtdate$patid))

n1
n2
n1+n2 # sums up to 704 patients

## expand rrtdate across all rows per patient
only_pat_with_rrtdate_all_rows <- only_pat_with_rrtdate %>% 
  group_by(patid) %>% 
  dplyr::mutate(rrtdate = first(na.omit(rrtdate)))

## expand KRT_onset_by entry across all rows per patient
only_pat_with_rrtdate_all_rows <- only_pat_with_rrtdate_all_rows %>% 
  group_by(patid) %>% 
  dplyr::mutate(KRT_onset_by = first(na.omit(KRT_onset_by)))

## expand diadate entry across all rows per patient
only_pat_with_rrtdate_all_rows <- only_pat_with_rrtdate_all_rows %>% 
  group_by(patid) %>% 
  dplyr::mutate(diadate = first(na.omit(diadate)))

## expand tpldate entry across all rows per patient
only_pat_with_rrtdate_all_rows <- only_pat_with_rrtdate_all_rows %>% 
  group_by(patid) %>% 
  dplyr::mutate(tpldate = first(na.omit(tpldate)))



###.............................................................................................###
#### Visit Inclusion for 4C Study Group - - From Derived 4C Investigation Dataset:             ####
## select only visits:                                                                            #
# 1) within window size of 200 days pre start date of KRT and 20 days post start date of KRT:     #
#     - start date: subtract 200 days from onset date of KRT (rrtdate)                            #
#     - stop date: add 20 days to onset date of KRT (rrtdate)                                     #
# 2) only select visits at yearly intervals (i.e. month 0, 12, 24, 36, ...)                       #
#    where cardiovascular measurements were performed                                             #
# 3) select the earliest visit in case that several visits are selected per patient               #
###.............................................................................................###

pre_days <- 200
post_days <- 20

study_group_4C <- only_pat_with_rrtdate_all_rows %>% filter(month %in% c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108) & 
                                                                           vdate >= rrtdate-pre_days & vdate <= rrtdate+post_days) %>%
  select(patid, subid, gender, region, datadate, month, vdate, rrtdate, KRT_onset_by, diadate, tpldate, changerrtdate, rrt, 
         diaheight, diaweight, diachangedate, 
         hdmode, hdnrsess, hddursess,
         urineout, uf, rgfr,
         pdmode, pdmodesp,
         tpldia, tpldnonfunc, tplneph,
         entnut, physact, cigsmoke, cigday,
         heightsds, weight, bmisds,
         hip, waist, whratio, bpsys, bpdia,
         extrawater, intrawater, totwater, exwater, pcexwater,
         imtdate, imtl, imtr, imtsds,
         dc_old, dcsds, beta_old, betasds, imtcalcl, imtcalcr,
         pwvdate, pwvsys, pwvdia, pwvsyssds, pwvdiasds, 
         pwv1, pwv2, pwv3, pwv, 
         pwadate,
         pai1, pai2, pai3, pai,
         ai1, ai2, ai3, ai,
         pwatpr1, pwatpr2, pwatpr3, pwatpr, echodate,
         echoladia, echoaodia, echoivsdia, echolvmiddia, echopwdia, echoivssys, echolvsys, echopwsys, echolvmidsys, 
         echoearatio, echoisovolcont, echoisovolrel,
         lvmi,
         cctdate, cctagat, cctvalve, cctvalvesp, cctaortic, cctaorticsp,
         hb, hc,
         pot, bicar, fer, pthlocal, Eingangsdatum, cal, chol, tri, hdl, ldl, 
         uacid, na, phos, crea, salb, crp, 
         age, agegr, 
         physactgr, pubgr, egfr, ckd, ckd1, 
         syssds_raw, syssds, diasds_raw, diasds,
         pwvsyssds_raw, pwvsyssds, pwvdiasds_raw, pwvdiasds, 
         lvh, 
         dc, beta, einc, 
         hvitd,
         dvitd,
         hvitdgr,
         feta,
         sost,
         cfgf23,
         iPTH,
         Indoxylsulfat__mg_l_,
         p_Cresylsulfat__mg_l_,
         wert_12,
         mg,
         tfsat, 
         wert_16, 
         wert_17, 
         wert_21,
         wert_22,
         wert_23,
         wert_24,
         wert_25,
         wert_29,
         wert_31,
         bnp,
         b_gfr,
         gfr_slope
         ) %>%
            mutate(RWT = ((echopwdia + echoivsdia)/echolvmiddia),
                   aRWT = (RWT - 0.005*(age-10)),
                   transplanted = ifelse(vdate >= tpldate, 1, 0)) %>%
            group_by(patid) %>% 
               filter(vdate == min(vdate)) %>% 
                   mutate(ABCD4kids_group = "study_group")

# test how many patients remain after window was applied for visit selection
length(unique(study_group_4C$patid))
#View(study_group_4C$patid)

### add cardiovascular sds measures computed with lms-method relative to patient age and patient height:
study_group_4C <- left_join(study_group_4C,  
                            select(cv_sds, patid, vdate, imt_sds_a, imt_sds_h, pwv_sds_a, pwv_sds_h, beta_sds_a, beta_sds_h, dc_sds_a, dc_sds_h, einc_sds_a, einc_sds_h), 
                            by = c("patid" = "patid", "vdate" = "vdate"))

##..................................................##




###..............................................................................................###
#### Visit Inclusion for 4C: COMPARATOR GROUP 1 (CKD4) - From Derived 4C Investigation Dataset: ####
## select only visits:                                                                            #
# 1) before start of window around KRT onset for study group (vdate < rrtdate-pre_days)           #
# 2) only select visits at yearly intervals (i.e. month 0, 12, 24, 36, ...)                       #
#    where cardiovascular measurements were performed                                             #
# 3) select the earliest visit in case that several visits are selected per patient               #
# 4) visits from patients who are in study group can NOT be included                              #
###.............................................................................................###

comparator_group1_4C <- only_pat_with_rrtdate_all_rows %>% 
                          group_by(patid) %>% 
                          filter(vdate < (rrtdate-pre_days) & egfr >= 15 & egfr <= 20 & 
                                   month %in% c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108) &
                                   !(patid %in% study_group_4C$patid)) %>%
                          select(patid, subid, gender, region, datadate, month, vdate, rrtdate, KRT_onset_by, diadate, tpldate, changerrtdate, rrt, 
                                 diaheight, diaweight, diachangedate, 
                                 hdmode, hdnrsess, hddursess,
                                 urineout, uf, rgfr,
                                 pdmode, pdmodesp,
                                 tpldia, tpldnonfunc, tplneph,
                                 entnut, physact, cigsmoke, cigday,
                                 heightsds, weight, bmisds,
                                 hip, waist, whratio, bpsys, bpdia,
                                 extrawater, intrawater, totwater, exwater, pcexwater,
                                 imtdate, imtl, imtr, imtsds,
                                 dc_old, dcsds, beta_old, betasds, imtcalcl, imtcalcr,
                                 pwvdate, pwvsys, pwvdia, pwvsyssds, pwvdiasds, 
                                 pwv1, pwv2, pwv3, pwv, 
                                 pwadate,
                                 pai1, pai2, pai3, pai,
                                 ai1, ai2, ai3, ai,
                                 pwatpr1, pwatpr2, pwatpr3, pwatpr, echodate,
                                 echoladia, echoaodia, echoivsdia, echolvmiddia, echopwdia, echoivssys, echolvsys, echopwsys, echolvmidsys, 
                                 echoearatio, echoisovolcont, echoisovolrel,
                                 lvmi,
                                 cctdate, cctagat, cctvalve, cctvalvesp, cctaortic, cctaorticsp,
                                 hb, hc,
                                 pot, bicar, fer, pthlocal, Eingangsdatum, cal, chol, tri, hdl, ldl, 
                                 uacid, na, phos, crea, salb, crp, 
                                 age, agegr, 
                                 physactgr, pubgr, egfr, ckd, ckd1, 
                                 syssds_raw, syssds, diasds_raw, diasds,
                                 pwvsyssds_raw, pwvsyssds, pwvdiasds_raw, pwvdiasds, 
                                 lvh, 
                                 dc, beta, einc, 
                                 hvitd,
                                 dvitd,
                                 hvitdgr,
                                 feta,
                                 sost,
                                 cfgf23,
                                 iPTH,
                                 Indoxylsulfat__mg_l_,
                                 p_Cresylsulfat__mg_l_,
                                 wert_12,
                                 mg,
                                 tfsat, 
                                 wert_16, 
                                 wert_17, 
                                 wert_21,
                                 wert_22,
                                 wert_23,
                                 wert_24,
                                 wert_25,
                                 wert_29,
                                 wert_31,
                                 bnp,
                                 b_gfr,
                                 gfr_slope
                          ) %>%
                          mutate(RWT = ((echopwdia + echoivsdia)/echolvmiddia),
                                 aRWT = (RWT - 0.005*(age-10)),
                                 transplanted = ifelse(vdate >= tpldate, 1, 0)) %>%
                          group_by(patid) %>% 
                            filter(vdate == min(vdate)) %>% 
                              mutate(ABCD4kids_group = "comparator_group_1")

# number of patients in comparator group 1
length(unique(comparator_group1_4C$patid))
#View(comparator_group1_4C)

### add cardiovascular sds measures computed with lms-method relative to patient age and patient height:
comparator_group1_4C <- left_join(comparator_group1_4C,  
                                  select(cv_sds, patid, vdate, imt_sds_a, imt_sds_h, pwv_sds_a, pwv_sds_h, beta_sds_a, beta_sds_h, dc_sds_a, dc_sds_h, einc_sds_a, einc_sds_h), 
                                  by = c("patid" = "patid", "vdate" = "vdate"))

##..................................................##



###..............................................................................................###
#### Visit Inclusion for 4C COMPARATOR GROUP 2 ('at least 1 year on KRT')                       ####                
#    - From Derived 4C Investigation Dataset:                                                     #
## select only visits:                                                                            #
# 1) after at least 1 year past KRT onset (vdate >= rrtdate+365)                                  #
# 2) only select visits at yearly intervals (i.e. month 0, 12, 24, 36, ...)                       #
#    where cardiovascular measurements were performed                                             #
# 3) select the earliest visit in case that several visits are selected per patient               #
# 4) visits from patients who are in study group can NOT be included                              #
###.............................................................................................###

comparator_group2_4C <- only_pat_with_rrtdate_all_rows %>% 
            group_by(patid) %>% 
            filter(vdate >= (rrtdate+365) & 
                     month %in% c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108) &
                     !(patid %in% study_group_4C$patid)) %>%
            select(patid, subid, gender, region, datadate, month, vdate, rrtdate, KRT_onset_by, diadate, tpldate, changerrtdate, rrt, 
                   diaheight, diaweight, diachangedate, 
                   hdmode, hdnrsess, hddursess,
                   urineout, uf, rgfr,
                   pdmode, pdmodesp,
                   tpldia, tpldnonfunc, tplneph,
                   entnut, physact, cigsmoke, cigday,
                   heightsds, weight, bmisds,
                   hip, waist, whratio, bpsys, bpdia,
                   extrawater, intrawater, totwater, exwater, pcexwater,
                   imtdate, imtl, imtr, imtsds,
                   dc_old, dcsds, beta_old, betasds, imtcalcl, imtcalcr,
                   pwvdate, pwvsys, pwvdia, pwvsyssds, pwvdiasds, 
                   pwv1, pwv2, pwv3, pwv, 
                   pwadate,
                   pai1, pai2, pai3, pai,
                   ai1, ai2, ai3, ai,
                   pwatpr1, pwatpr2, pwatpr3, pwatpr, echodate,
                   echoladia, echoaodia, echoivsdia, echolvmiddia, echopwdia, echoivssys, echolvsys, echopwsys, echolvmidsys, 
                   echoearatio, echoisovolcont, echoisovolrel,
                   lvmi,
                   cctdate, cctagat, cctvalve, cctvalvesp, cctaortic, cctaorticsp,
                   hb, hc,
                   pot, bicar, fer, pthlocal, Eingangsdatum, cal, chol, tri, hdl, ldl, 
                   uacid, na, phos, crea, salb, crp, 
                   age, agegr, 
                   physactgr, pubgr, egfr, ckd, ckd1, 
                   syssds_raw, syssds, diasds_raw, diasds,
                   pwvsyssds_raw, pwvsyssds, pwvdiasds_raw, pwvdiasds, 
                   lvh, 
                   dc, beta, einc, 
                   hvitd,
                   dvitd,
                   hvitdgr,
                   feta,
                   sost,
                   cfgf23,
                   iPTH,
                   Indoxylsulfat__mg_l_,
                   p_Cresylsulfat__mg_l_,
                   wert_12,
                   mg,
                   tfsat, 
                   wert_16, 
                   wert_17, 
                   wert_21,
                   wert_22,
                   wert_23,
                   wert_24,
                   wert_25,
                   wert_29,
                   wert_31,
                   bnp,
                   b_gfr,
                   gfr_slope
            ) %>%
            mutate(RWT = ((echopwdia + echoivsdia)/echolvmiddia),
                   aRWT = (RWT - 0.005*(age-10)),
                   transplanted = ifelse(vdate >= tpldate, 1, 0)) %>%
            group_by(patid) %>% 
            filter(vdate == min(vdate)) %>% 
            mutate(ABCD4kids_group = "comparator_group_2")

# number of patients in comparator group 2
length(unique(comparator_group2_4C$patid))
#View(comparator_group2_4C)

### add cardiovascular sds measures computed with lms-method relative to patient age and patient height:
comparator_group2_4C <- left_join(comparator_group2_4C,  
                                  select(cv_sds, patid, vdate, imt_sds_a, imt_sds_h, pwv_sds_a, pwv_sds_h, beta_sds_a, beta_sds_h, dc_sds_a, dc_sds_h, einc_sds_a, einc_sds_h), 
                                  by = c("patid" = "patid", "vdate" = "vdate"))


##..................................................##




###........................................................................###
#### Derived Medication Dataset: Variable Selection & Exclude Medications #### 
# Remark: exclude all medications from medication groups:                    #
# - Immunosuppressives                                                       #
# - Cardioprotective Agent                                                   #
# - Insulin                                                                  #
# - Non Statin Lipid Lowering Agent                                          #
# - Uric Acid Lowering Agent                                                 #
###........................................................................###

# select variables from medications table and exclude medications that are not of interest 
# REMARK: selected variables are needed for calculation of medication group columns
medication_var_selected <- medication %>% select(patid, 
                                                 editdate, 
                                                 medid,
                                                 medigr,
                                                 startdate, 
                                                 stopdate, 
                                                 vdate) %>%
                                                 filter(!medid %in% c(49, 50, 51, 52, 53, 54, 55, 57, 142, 150, 188,
                                                                      145, 187,
                                                                      111, 112, 113, 123, 186,
                                                                      152, 133,
                                                                      67, 72, 105, 109, 135, 140, 141, 147, 158, 160, 170, 180))

# prefilter medication_var_selected for current medications
medication_var_selected_current <- medication_var_selected %>% filter((vdate >= startdate & vdate < stopdate) | (vdate >= startdate & is.na(stopdate)))

# prefilter medication_var_selected for medications that are terminated
medication_var_selected_terminated <- medication_var_selected %>% filter((vdate > startdate & vdate > stopdate))

# prefilter medication_var_selected for medications that are started after a patient´s visit date
medication_var_selected_not_yet_started <- medication_var_selected %>% filter((vdate < startdate))

# ...for medical database entries (rows) with current medications
## initialize empty binary medication group indicator variables with 0.
# medication group indicator variables have the following level coding:
# --> level coding = 1: at least one current medication per group present; 
# --> level coding = 0: no current medication per group present
medication_var_selected_current$ACE_inhibitors <- 0
medication_var_selected_current$AT1_blockers <- 0
medication_var_selected_current$direct_renin_inhibitors <- 0
medication_var_selected_current$ca_blockers <- 0
medication_var_selected_current$beta_blockers <- 0
medication_var_selected_current$peripheral_alpha_blockers <- 0
medication_var_selected_current$central_alpha_blockers <- 0
medication_var_selected_current$loop_diuretics <- 0
medication_var_selected_current$thiazide_diuretics <- 0
medication_var_selected_current$potassium_sparing_diuretics <- 0
medication_var_selected_current$vasodilatators <- 0
medication_var_selected_current$vitamin_D <- 0
medication_var_selected_current$active_vitamin_D <- 0
medication_var_selected_current$GH <- 0
medication_var_selected_current$iron <- 0
medication_var_selected_current$ESA <- 0
medication_var_selected_current$acidosis_treatment <- 0
medication_var_selected_current$statins <- 0
medication_var_selected_current$CaRec_Agonist <- 0
medication_var_selected_current$Non_CA_P_Bind <- 0
medication_var_selected_current$CA_P_Bind <- 0
medication_var_selected_current$calcium_sources <- 0
medication_var_selected_current$magnesium <- 0
medication_var_selected_current$uric_acid_lowering_agent <- 0

# ...for medical database entries (rows) with terminated medications
## fill in level coding = 0 (no current medication per group present) for binary medication group indicator variables
medication_var_selected_terminated$ACE_inhibitors <- 0
medication_var_selected_terminated$AT1_blockers <- 0
medication_var_selected_terminated$direct_renin_inhibitors <- 0
medication_var_selected_terminated$ca_blockers <- 0
medication_var_selected_terminated$beta_blockers <- 0
medication_var_selected_terminated$peripheral_alpha_blockers <- 0
medication_var_selected_terminated$central_alpha_blockers <- 0
medication_var_selected_terminated$loop_diuretics <- 0
medication_var_selected_terminated$thiazide_diuretics <- 0
medication_var_selected_terminated$potassium_sparing_diuretics <- 0
medication_var_selected_terminated$vasodilatators <- 0
medication_var_selected_terminated$vitamin_D <- 0
medication_var_selected_terminated$active_vitamin_D <- 0
medication_var_selected_terminated$GH <- 0
medication_var_selected_terminated$iron <- 0
medication_var_selected_terminated$ESA <- 0
medication_var_selected_terminated$acidosis_treatment <- 0
medication_var_selected_terminated$statins <- 0
medication_var_selected_terminated$CaRec_Agonist <- 0
medication_var_selected_terminated$Non_CA_P_Bind <- 0
medication_var_selected_terminated$CA_P_Bind <- 0
medication_var_selected_terminated$calcium_sources <- 0
medication_var_selected_terminated$magnesium <- 0
medication_var_selected_terminated$uric_acid_lowering_agent <- 0

# ...for medical database entries (rows) with medications not yet started at visit date
## fill in level coding = 0 (no current medication per group present) for binary medication group indicator variables
medication_var_selected_not_yet_started$ACE_inhibitors <- 0
medication_var_selected_not_yet_started$AT1_blockers <- 0
medication_var_selected_not_yet_started$direct_renin_inhibitors <- 0
medication_var_selected_not_yet_started$ca_blockers <- 0
medication_var_selected_not_yet_started$beta_blockers <- 0
medication_var_selected_not_yet_started$peripheral_alpha_blockers <- 0
medication_var_selected_not_yet_started$central_alpha_blockers <- 0
medication_var_selected_not_yet_started$loop_diuretics <- 0
medication_var_selected_not_yet_started$thiazide_diuretics <- 0
medication_var_selected_not_yet_started$potassium_sparing_diuretics <- 0
medication_var_selected_not_yet_started$vasodilatators <- 0
medication_var_selected_not_yet_started$vitamin_D <- 0
medication_var_selected_not_yet_started$active_vitamin_D <- 0
medication_var_selected_not_yet_started$GH <- 0
medication_var_selected_not_yet_started$iron <- 0
medication_var_selected_not_yet_started$ESA <- 0
medication_var_selected_not_yet_started$acidosis_treatment <- 0
medication_var_selected_not_yet_started$statins <- 0
medication_var_selected_not_yet_started$CaRec_Agonist <- 0
medication_var_selected_not_yet_started$Non_CA_P_Bind <- 0
medication_var_selected_not_yet_started$CA_P_Bind <- 0
medication_var_selected_not_yet_started$calcium_sources <- 0
medication_var_selected_not_yet_started$magnesium <- 0
medication_var_selected_not_yet_started$uric_acid_lowering_agent <- 0

##...for medical database entries (rows) with current medications:
# fill in binary medication group indicator variables with the following level coding:
# --> level coding = 1: at least one current medication per group present; 
# --> level coding = 0: no current medication per group present
# 3-step Implementation code block below:
# In step 1) Group by patient-ID and visit date and based on variable "medigr" fill in medication group variables representing if a patient 
#            receives at least one medication per medication group at a visit.
# In step 2) --> retain only one row per patient AND visit date 
# In step 3) Remove context variables which have now become uninformative: medid, medigr, startdate, stopdate

medication_var_selected_current <- medication_var_selected_current %>% 
                                          group_by(patid, vdate) %>% 
                                                mutate(ACE_inhibitors = case_when(any(medigr == 1) ~ 1,
                                                                                  TRUE ~ 0),
                                                       AT1_blockers = case_when(any(medigr == 2) ~ 1,
                                                                                TRUE ~ 0),
                                                       direct_renin_inhibitors = case_when(any(medigr == 3) ~ 1,
                                                                                           TRUE ~ 0),
                                                       ca_blockers = case_when(any(medigr == 4) ~ 1,
                                                                               TRUE ~ 0),
                                                       beta_blockers = case_when(any(medigr == 5) ~ 1,
                                                                                 TRUE ~ 0),
                                                       peripheral_alpha_blockers = case_when(any(medigr == 6) ~ 1,
                                                                                             TRUE ~ 0),
                                                       central_alpha_blockers = case_when(any(medigr == 7) ~ 1,
                                                                                          TRUE ~ 0),
                                                       loop_diuretics = case_when(any(medigr == 8) ~ 1,
                                                                                  TRUE ~ 0),
                                                       thiazide_diuretics = case_when(any(medigr == 9) ~ 1,
                                                                                  TRUE ~ 0),
                                                       potassium_sparing_diuretics = case_when(any(medigr == 10) ~ 1,
                                                                                      TRUE ~ 0),
                                                       vasodilatators = case_when(any(medigr == 11) ~ 1,
                                                                                      TRUE ~ 0),
                                                       vitamin_D = case_when(any(medigr == 12) ~ 1,
                                                                                  TRUE ~ 0),
                                                       active_vitamin_D = case_when(any(medigr == 13) ~ 1,
                                                                                    TRUE ~ 0),
                                                       GH = case_when(any(medigr == 14) ~ 1,
                                                                      TRUE ~ 0),
                                                       iron = case_when(any(medigr == 15) ~ 1,
                                                                        TRUE ~ 0),
                                                       ESA = case_when(any(medigr == 16) ~ 1,
                                                                       TRUE ~ 0),
                                                       acidosis_treatment = case_when(any(medigr == 17) ~ 1,
                                                                                      TRUE ~ 0),
                                                       statins = case_when(any(medigr == 18) ~ 1,
                                                                           TRUE ~ 0),
                                                       CaRec_Agonist = case_when(any(medigr == 19) ~ 1,
                                                                                 TRUE ~ 0),
                                                       Non_CA_P_Bind = case_when(any(medigr == 20) ~ 1,
                                                                                 TRUE ~ 0),
                                                       CA_P_Bind = case_when(any(medigr == 21) ~ 1,
                                                                                 TRUE ~ 0),
                                                       calcium_sources = case_when(any(medigr == 22) ~ 1,
                                                                                   TRUE ~ 0),
                                                       magnesium = case_when(any(medigr == 23) ~ 1,
                                                                             TRUE ~ 0),
                                                       uric_acid_lowering_agent = case_when(any(medigr == 28) ~ 1,
                                                                             TRUE ~ 0)
                                                       ) %>% filter(row_number()==1) %>% 
                                                       select(-c(medid, medigr, startdate, stopdate))


##...for medical database entries (rows) with terminated medications:
# all binary medication group indicator variables have level coding = 0 (no current medication per group present)
# - retain only one row per patient AND visit date 
# - Remove context variables which have now become uninformative: medid, medigr, startdate, stopdate
medication_var_selected_terminated <- medication_var_selected_terminated %>% group_by(patid, vdate) %>% filter(row_number()==1) %>% 
                                                                                                        select(-c(medid, medigr, startdate, stopdate))

##...for medical database entries (rows) with medications not yet started at visit date:
# all binary medication group indicator variables have level coding = 0 (no current medication per group present)
# - retain only one row per patient AND visit date 
# - Remove context variables which have now become uninformative: medid, medigr, startdate, stopdate
medication_var_selected_not_yet_started <- medication_var_selected_not_yet_started %>% group_by(patid, vdate) %>% filter(row_number()==1) %>% 
                                                                                                                  select(-c(medid, medigr, startdate, stopdate))

##...After creating medication group variables:
# 1) concatenate medical database entries (rows) with 
#    - current medications 
#    - with terminated medications 
#    - with not yet started medications
# 2) in case that a patient was both on current medications and had terminated medications and/or not yet started medications at the same visit:
#     --> expand level coding = 1 (at least one current medication per group present) across rows for the same visit
# 3) retain only one row per patient and visit
df_med_groups <- rbind(medication_var_selected_current, medication_var_selected_terminated, medication_var_selected_not_yet_started) %>% 
                   arrange(patid, vdate) %>%
                      group_by(patid, vdate) %>%
                          mutate(ACE_inhibitors = case_when(any(ACE_inhibitors == 1) ~ 1,
                                                            TRUE ~ 0),
                                 AT1_blockers = case_when(any(AT1_blockers == 1) ~ 1,
                                                          TRUE ~ 0),
                                 direct_renin_inhibitors = case_when(any(direct_renin_inhibitors == 1) ~ 1,
                                                                     TRUE ~ 0),
                                 ca_blockers = case_when(any(ca_blockers == 1) ~ 1,
                                                         TRUE ~ 0),
                                 beta_blockers = case_when(any(beta_blockers == 1) ~ 1,
                                                           TRUE ~ 0),
                                 peripheral_alpha_blockers = case_when(any(peripheral_alpha_blockers == 1) ~ 1,
                                                                       TRUE ~ 0),
                                 central_alpha_blockers = case_when(any(central_alpha_blockers == 1) ~ 1,
                                                                    TRUE ~ 0),
                                 loop_diuretics = case_when(any(loop_diuretics == 1) ~ 1,
                                                            TRUE ~ 0),
                                 thiazide_diuretics = case_when(any(thiazide_diuretics == 1) ~ 1,
                                                                TRUE ~ 0),
                                 potassium_sparing_diuretics = case_when(any(potassium_sparing_diuretics == 1) ~ 1,
                                                                         TRUE ~ 0),
                                 vasodilatators = case_when(any(vasodilatators == 1) ~ 1,
                                                            TRUE ~ 0),
                                 vitamin_D = case_when(any(vitamin_D == 1) ~ 1,
                                                       TRUE ~ 0),
                                 active_vitamin_D = case_when(any(active_vitamin_D == 1) ~ 1,
                                                              TRUE ~ 0),
                                 GH = case_when(any(GH == 1) ~ 1,
                                                TRUE ~ 0),
                                 iron = case_when(any(iron == 1) ~ 1,
                                                  TRUE ~ 0),
                                 ESA = case_when(any(ESA == 1) ~ 1,
                                                 TRUE ~ 0),
                                 acidosis_treatment = case_when(any(acidosis_treatment == 1) ~ 1,
                                                                TRUE ~ 0),
                                 statins = case_when(any(statins == 1) ~ 1,
                                                     TRUE ~ 0),
                                 CaRec_Agonist = case_when(any(CaRec_Agonist == 1) ~ 1,
                                                           TRUE ~ 0),
                                 Non_CA_P_Bind = case_when(any(Non_CA_P_Bind == 1) ~ 1,
                                                           TRUE ~ 0),
                                 CA_P_Bind = case_when(any(CA_P_Bind == 1) ~ 1,
                                                       TRUE ~ 0),
                                 calcium_sources = case_when(any(calcium_sources == 1) ~ 1,
                                                             TRUE ~ 0),
                                 magnesium = case_when(any(magnesium == 1) ~ 1,
                                                       TRUE ~ 0),
                                 uric_acid_lowering_agent = case_when(any(uric_acid_lowering_agent == 1) ~ 1,
                                                                      TRUE ~ 0)
                          ) %>% filter(row_number()==1)




#### Derived Patient Dataset: Variable Selection #### 

patients_var_selected <- patients %>% select(patid,
                                             diagbase, 
                                             dob, 
                                             ckd2date,
                                             cardiac,
                                             pulmo,
                                             diab1,
                                             diab2,
                                             dyslipid,
                                             hyper,
                                             hyperdate,
                                             lefthyper,
                                             gestage,
                                             bweight,
                                             blength,
                                             consang,
                                             consangsp,
                                             fyob,
                                             fcig,
                                             fhyper,
                                             fstroke,
                                             fheart, 
                                             fdiab,
                                             flip,
                                             frenal,
                                             myob,
                                             msmoke,
                                             mcig,
                                             mhyper,
                                             mstroke,
                                             mheart,
                                             mdiab,
                                             mlip,
                                             mrenal,
                                             finaldate,
                                             doreason,
                                             deathdate,
                                             doreasonsp,
                                             country,
                                             ethnic,
                                             cakut,
                                             glom, 
                                             nephloss,
                                             tubint,
                                             otherunknown,
                                             diagnose2,
                                             bwsds,
                                             sga
                                             )


# Convert the variable "country" into a factor (categorical) variable and assign legible character labels to numeric factor levels.
# The 4C category label dictionary "format.sasbdt" is used for the level-label conversion.
patients_var_selected$country <- factor(patients_var_selected$country, 
                                        levels = c(seq(1,13,1)),
                                        labels = c("Turkey",
                                                   "Serbia",
                                                   "Germany",
                                                   "Italy",
                                                   "France",
                                                   "Switzerland",
                                                   "Poland",
                                                   "Austria",
                                                   "UK",
                                                   "Portugal",
                                                   "Czech Republic",
                                                   "Lithuania",
                                                   "Other"))

# Convert the variable "ethnic" into a factor (categorical) variable and assign legible character labels to numeric factor levels.
# The 4C category label dictionary "format.sasbdt" is used for the level-label conversion.
patients_var_selected$ethnic <- factor(patients_var_selected$ethnic ,
                                       levels = c(seq(1,7,1)),
                                       labels = c("Caucasian",
                                                  "East Asian",
                                                  "Hispanic",
                                                  "Indian",
                                                  "Arabic",
                                                  "African / African American",
                                                  "Other"))


#.........................................................................................................................................#
# REMARK on merging with ABPM variable selection:                                                                                         #
# 1) PROBLEM: Extra abpm visits were recorded for patients (see extra dates under "abpmdate" compared to documented dates under "vdate")  #
# IN OTHER WORDS: Patients´ abpmdates in ABPM dataset are not necessarily vdates in INVESTIGATION datset and vice versa.                  #
# -> This means one cannot simply use one join step to merge both variable selections by "patid" = "patid" & "vdate" = "abpmdate".        #
# 2) SOLUTION: Also apply window to derived ABPM dataset before merging only by patid                                                     #
#    For Study Group - select only abpm measurements:                                                                                     #                  #
#       i) within window size of 200 days pre start date of KRT and 20 days post start date of KRT:                                       #
#       ii) select only abpm measurements of the earliest abpmdate in case that several abpmdates are selected per patient                #  
#    For Comparator Group 1 - select only abpm measurements:                                                                              #                  #
#       i) before start of window around KRT onset & egfr >= 15 & egfr <= 20:                                                                                       #
#       ii) select only abpm measurements of the earliest abpmdate in case that several abpmdates are selected per patient                #
#       iii) select only abpm measurements from patients who were not included in study group                                             #
#    For Comparator Group 2 - select only abpm measurements:                                                                              #                  #
#       i) after at least 1 year past KRT onset:                                                                                          #
#       ii) select only abpm measurements of the earliest abpmdate in case that several abpmdates are selected per patient                #
#       iii) select only abpm measurements from patients who were not included in study group                                             #
#.........................................................................................................................................#

#### Derived ABPM Dataset: Variable Selection #### 

abpm_var_selected <- abpm %>% select(patid,
                                     abpmdate, 
                                     abpmfile, 
                                     abpmheight,
                                     daydia,
                                     dayhr,
                                     daymap,
                                     daysys,
                                     dia24h,
                                     dissleep,
                                     hr24h,
                                     map24h,
                                     map24hsds,
                                     nightdias,
                                     nighthr,
                                     nightmap,
                                     nightsys,
                                     sys24h)

##..............................##
## abpm dataset for study group ##

# create df only with info about KRT onset date per patient
df_KRT_onset_per_patient <- study_group_4C %>% select(patid, rrtdate)

# join (by patid) abpm variable selection with info about KRT onset date (remove patients without KRT)
abpm_var_selected_with_rrt <- left_join(abpm_var_selected, df_KRT_onset_per_patient, by = c("patid" = "patid")) %>% filter(!is.na(rrtdate))

# apply window per patient, and retain earliest abpm measurement within window per patient
abpm_var_selected_study_group <- abpm_var_selected_with_rrt %>% 
                                        filter(abpmdate >= rrtdate-pre_days & abpmdate <= rrtdate+post_days) %>%
                                          select(-rrtdate) %>%
                                            group_by(patid) %>%
                                              filter(abpmdate == min(abpmdate))

##..............................##



##..............................##
## abpm dataset for comparator group 1 ##

# create df only with info about KRT onset date per patient
df_KRT_onset_per_patient <- comparator_group1_4C %>% select(patid, vdate, rrtdate, egfr)

# join (by patid) abpm variable selection with info about KRT onset date (remove patients without KRT)
abpm_var_selected_with_rrt <- left_join(abpm_var_selected, df_KRT_onset_per_patient, by = c("patid" = "patid", "abpmdate" = "vdate")) %>% filter(!is.na(rrtdate))

# apply window per patient, and retain earliest abpm measurement within window per patient
abpm_var_selected_comparator1_group <- abpm_var_selected_with_rrt %>% 
                                          filter(abpmdate < (rrtdate-pre_days) & 
                                                 egfr >= 15 & egfr <= 20 & 
                                                 !(patid %in% study_group_4C$patid)) %>%
                                             select(-c(rrtdate, egfr)) %>%
                                               group_by(patid) %>%
                                                 filter(abpmdate == min(abpmdate))

##..............................##



##..............................##
## abpm dataset for comparator group 2 ##

# create df only with info about KRT onset date per patient
df_KRT_onset_per_patient <- comparator_group2_4C %>% select(patid, rrtdate)

# join (by patid) abpm variable selection with info about KRT onset date (remove patients without KRT)
abpm_var_selected_with_rrt <- left_join(abpm_var_selected, df_KRT_onset_per_patient, by = c("patid" = "patid")) %>% filter(!is.na(rrtdate))

# apply window per patient, and retain earliest abpm measurement within window per patient
abpm_var_selected_comparator2_group <- abpm_var_selected_with_rrt %>% 
                                          filter(abpmdate > (rrtdate+365) &
                                                   !(patid %in% study_group_4C$patid)) %>%
                                          select(-c(rrtdate)) %>%
                                            group_by(patid) %>%
                                              filter(abpmdate == min(abpmdate))

##..............................##


##............................................................................................##
#### Final Join operation 4C - For study group, comparator group 1, and comparator group 2: ####
# -> merge all custom datasets (visit data, patient data, medication data, abpm data)          #
##............................................................................................##

### study group:

# join preselected (windowed) variables from study group with selected variables from derived medication dataset
# merge by: patid & vdate 
# use left join (keeps all patient entries in study group)
joined_study_med <- left_join(study_group_4C, df_med_groups, by = c("patid" = "patid", "vdate" = "vdate"))
#View(head(joined_study_med, 100))

# join (by patid) "joined_invest_med" with selected variables from patient dataset
joined_study_med_pat <- left_join(joined_study_med, patients_var_selected, by = c("patid" = "patid"))
#View(head(joined_study_med_pat))

# join (by patid) "joined_invest_med_pat" with selected abpm measures  
study_group_DF <- left_join(joined_study_med_pat, abpm_var_selected_study_group, by = c("patid" = "patid")) %>% select(patid, ABCD4kids_group, everything())
#View(head(study_group_DF, 100))



### comparator group 1:

# join preselected (windowed) variables from comparator group 1 with selected variables from derived medication dataset
# merge by: patid & vdate 
# use left join (keeps all patient entries in comparator group 1)
joined_comp1_med <- left_join(comparator_group1_4C, df_med_groups, by = c("patid" = "patid", "vdate" = "vdate"))
#View(head(joined_comp1_med, 100))

# join (by patid) "joined_invest_med" with selected variables from patient dataset
joined_comp1_med_pat <- left_join(joined_comp1_med, patients_var_selected, by = c("patid" = "patid"))
#View(head(joined_comp1_med_pat))

# join (by patid) "joined_comp1_med_pat" with selected abpm measures  
comparator_group1_DF <- left_join(joined_comp1_med_pat, abpm_var_selected_comparator1_group, by = c("patid" = "patid")) %>% select(patid, ABCD4kids_group, everything())
#View(head(comparator_group1_DF, 100))




### comparator group 2:

# join preselected (windowed) variables from comparator group 2 with selected variables from derived medication dataset
# merge by: patid & vdate 
# use left join (keeps all patient entries in comparator group 2)
joined_comp2_med <- left_join(comparator_group2_4C, df_med_groups, by = c("patid" = "patid", "vdate" = "vdate"))
#View(head(joined_comp2_med, 100))

# join (by patid) "joined_invest_med" with selected variables from patient dataset
joined_comp2_med_pat <- left_join(joined_comp2_med, patients_var_selected, by = c("patid" = "patid"))
#View(head(joined_comp2_med_pat))

# join (by patid) "joined_comp2_med_pat" with selected abpm measures  
comparator_group2_DF <- left_join(joined_comp2_med_pat, abpm_var_selected_comparator2_group, by = c("patid" = "patid")) %>% select(patid, ABCD4kids_group, everything())
#View(head(comparator_group2_DF, 100))


##.........................................................................................................##
#### Obtain 4C ABCD4kids Analysis Datasets: Join 4C study group, comparator group 2, comparator group 2  ####
##.........................................................................................................##

ABCD4kids_4C_analysis_DF <- rbind(study_group_DF, comparator_group1_DF, comparator_group2_DF) %>%
                              mutate(database = "4C") %>% select(patid, database,  ABCD4kids_group, subid, gender, region, country, everything())

# adjust variable types to enable join operation with 3H
ABCD4kids_4C_analysis_DF$patid <- as.character(ABCD4kids_4C_analysis_DF$patid)
ABCD4kids_4C_analysis_DF$Non_CA_P_Bind <- as.character(ABCD4kids_4C_analysis_DF$Non_CA_P_Bind)
ABCD4kids_4C_analysis_DF$CA_P_Bind <- as.character(ABCD4kids_4C_analysis_DF$CA_P_Bind)
ABCD4kids_4C_analysis_DF$diagnose2 <- as.character(ABCD4kids_4C_analysis_DF$diagnose2)

##.........................................................................................................##
####                              Rbind 3H data with 4C ABCD4kids Analysis Dataset                       ####
##.........................................................................................................##

## load 3H datasets (as prepared by Priyanka)
study_group_dia_3H <- read_excel("~/3H_for_merging_with_4C.xlsx")
comparator_1_year_past_KRT_3H <- read_excel("~/3H_for_ABCD4Kids_1_year_after_dialysis_group.xlsx") 

## add variable indicating a patient´s assigned cross-sectional analysis group 
study_group_dia_3H$ABCD4kids_group <- "study_group"
comparator_1_year_past_KRT_3H$ABCD4kids_group <- "comparator_group_2"

# rbind both 3H subsets of data
data_3H <- rbind(study_group_dia_3H, comparator_1_year_past_KRT_3H)

## add variable indicating the database
data_3H$database <- "3H"

# add variables to 3H dataset
data_3H$KRT_onset_by <- 1

# convert diagnosis category labels of variable "under_renal" to corresponding numeric category levels
# of 4C variable diagnosis2
data_3H$under_renal <- factor(data_3H$under_renal, 
                              levels = c("cystic kidney disease", "dysplasia", "glomerulonephritis", "other", "unknown"),
                              labels = c(1, 1, 2, 5, 5)
)

# Convert the variable "ethnicity" into a factor (categorical) variable and assign legible character labels to numeric factor levels.
# The 4C category label dictionary "format.sasbdt" is used for the level-label conversion.
data_3H$ethnicity <- factor(data_3H$ethnicity ,
                            levels = c(seq(1,7,1)),
                            labels = c("Caucasian",
                                       "East Asian",
                                       "Hispanic",
                                       "Indian",
                                       "Arabic",
                                       "African / African American",
                                       "Other")) 

### recode category lables of variable "calcgp" in 3H to binary category labels 
### indicating presence of Ca-P-Binders (1: Present; 0: Not Present)
data_3H$CA_P_Bind = as.character(factor(data_3H$calcgp,
                                 levels = c("calcium", "calcium plus lan/seve", "lanthanum or sevelamer", "none"),
                                 labels = c(0,1,1,0)))

### recode category lables of variable "calcgp" in 3H to binary category labels 
### indicating presence of Non-Ca-P-Binders (1: Present; 0: Not Present)
data_3H$Non_CA_P_Bind = as.character(factor(data_3H$calcgp,
                                     levels = c("calcium", "calcium plus lan/seve", "lanthanum or sevelamer", "none"),
                                     labels = c(1,0,0,0)))


## rename variable names in 3H to names used in 4C
data_3H <- data_3H %>% select(-c(lvmi, pwv_sds_age)) %>% mutate(dt_basevis_copy = dt_basevis,
                                                   basedate_copy = basedate,
                                                   mth0_ace_arb_copy1 = mth0_ace_arb,
                                                   mth0_ace_arb_copy2 = mth0_ace_arb,
                                                   mth0_ahd_oth_copy1 = mth0_ahd_oth,
                                                   mth0_ahd_oth_copy2 = mth0_ahd_oth,
                                                   mth0_diuretic_copy1 = mth0_diuretic,
                                                   mth0_diuretic_copy2 = mth0_diuretic) %>%
                                                   rename(patid = h3_id,
                                                           ethnic = ethnicity,
                                                           dob = date_of_birth,
                                                           age = baseage,
                                                           datadate = basedate_copy,
                                                           vdate = dt_basevis_copy,
                                                           rrtdate = dt_basevis,
                                                           diagnose2 = under_renal,
                                                           cardiac = cardiac_abnormalities,
                                                           pulmo = pulmonary_abnormalities,
                                                           heightsds = base_height_sds,
                                                           bmisds = base_bmi_sds,
                                                           syssds = base_sbp_sds,
                                                           diasds = base_dbp_sds,
                                                           imtl = base_cimt,
                                                           imtsds = base_imt_sds,
                                                           pwv = base_pwv,
                                                           pwv_sds_age = base_pwv_sds,
                                                           echodate = basedate,
                                                           lvmi = base_lvmi,
                                                           abpmdate = map_date,
                                                           daymap = map_daytime_mean,
                                                           map24h = map_24h_mean,
                                                           map24hsds = basemapsds,
                                                           nightmap = map_night_mean,
                                                           hb = mth0_hb,
                                                           hc = mth0_hematocrit,
                                                           pot = BASE_K,
                                                           bicar = mth0_hco3,
                                                           fer = mth0_ferritin,
                                                           pthlocal = mth0_pth,
                                                           cal = BASE_cA,
                                                           chol = mth0_chol,
                                                           tri = mth0_trigly,
                                                           hdl = mth0_hdl,
                                                           ldl = mth0_ldl,
                                                           uacid = mth0_uric_acid,
                                                           na = BASE_NA,
                                                           phos = BASE_PHOS,
                                                           crea = BASE_cREA,
                                                           salb = BASE_ALB,
                                                           crp = BASE_HSCRP,
                                                           hvitd = BASE_TVITD,
                                                           mg = BASE_MG,
                                                           tfsat = mth0_transferrin,
                                                           hdmode = dialysis_mode,
                                                           uf = dialysis_av_ultrafiltration,
                                                           ACE_inhibitors = mth0_ace_arb,
                                                           AT1_blockers = mth0_ace_arb_copy1,
                                                           direct_renin_inhibitors = mth0_ace_arb_copy2,
                                                           ca_blockers = mth0_cacechannel,
                                                           beta_blockers = mth0_betablocker,
                                                           peripheral_alpha_blockers = mth0_ahd_oth, 
                                                           central_alpha_blockers = mth0_ahd_oth_copy1,
                                                           loop_diuretics = mth0_diuretic,
                                                           thiazide_diuretics = mth0_diuretic_copy1,
                                                           potassium_sparing_diuretics = mth0_diuretic_copy2,
                                                           vasodilatators = mth0_ahd_oth_copy2,
                                                           GH = mth0_growth_hormone,
                                                           iron = mth0_Fesupp,
                                                           ESA = mth0_epo,
                                                           acidosis_treatment = mth0_bicarb)  %>%
                                                      select(patid,
                                                             KRT_onset_by,
                                                             database,
                                                             ABCD4kids_group,
                                                             ethnic,
                                                             gender,
                                                             dob,
                                                             age,
                                                             height,
                                                             datadate,
                                                             vdate,
                                                             rrtdate,
                                                             diagnose2,
                                                             cardiac,
                                                             pulmo,
                                                             heightsds,
                                                             bmisds,
                                                             syssds,
                                                             diasds,
                                                             imtl,
                                                             imtsds,
                                                             pwv,
                                                             pwv_sds_age,
                                                             echodate,
                                                             lvmi,
                                                             abpmdate,
                                                             daymap,
                                                             map24h,
                                                             map24hsds,
                                                             nightmap,
                                                             hb,
                                                             hc,
                                                             pot,
                                                             bicar,
                                                             fer,
                                                             pthlocal,
                                                             cal,
                                                             chol,
                                                             tri,
                                                             hdl,
                                                             ldl,
                                                             uacid,
                                                             na,
                                                             phos,
                                                             crea,
                                                             salb,
                                                             crp,
                                                             hvitd,
                                                             mg,
                                                             tfsat,
                                                             hdmode,
                                                             uf,
                                                             CA_P_Bind,
                                                             Non_CA_P_Bind,
                                                             ACE_inhibitors,
                                                             AT1_blockers,
                                                             direct_renin_inhibitors,
                                                             ca_blockers,
                                                             beta_blockers,
                                                             peripheral_alpha_blockers,
                                                             central_alpha_blockers,
                                                             loop_diuretics,
                                                             thiazide_diuretics,
                                                             potassium_sparing_diuretics,
                                                             vasodilatators,
                                                             GH,
                                                             iron,
                                                             ESA,
                                                             acidosis_treatment)

### add pwv_sds_height computed with lms-method relative to patient height:
data_3H <- left_join(data_3H,  
                     select(pwv_sds_h_3h, patid, vdate, pwv_sds_h), 
                     by = c("patid" = "patid", "vdate" = "vdate")) %>% rename(pwv_sds_height = pwv_sds_h)

# join data_3H with ABCD4kids_4C_analysis_DF
ABCD4kids_final_analysis_DF <- bind_rows(ABCD4kids_4C_analysis_DF, data_3H)
#View(ABCD4kids_final_analysis_DF)

# category level correction for variable gender
ABCD4kids_final_analysis_DF$gender <- factor(ABCD4kids_final_analysis_DF$gender, 
                                             levels = c("m", "f", "M", "F"),
                                             labels = c("M", "F", "M" , "F"))

#.......................................................................#
####      Write .csv and .xlsx files for ABCD4Kids_4C_Analysis_DF    ####

write.csv(ABCD4kids_final_analysis_DF, "~/ABCD4kids_final_analysis_DF.csv")
write.xlsx(ABCD4kids_final_analysis_DF, "~/ABCD4kids_final_analysis_DF.xlsx")
