install.packages("party")
#load packages
library(tidyverse)
library(dplyr)
library(haven)
library(psych)
library(epiDisplay)

# Import the datasets
K18 = haven::read_spss("lsacgrk18.sav")
K16 = haven::read_spss("lsacgrk16.sav")
K14 = haven::read_spss("lsacgrk14.sav")
K12 = haven::read_spss("lsacgrk12.sav")
K10 = haven::read_spss("lsacgrk10.sav")
K8 = haven::read_spss("lsacgrk8.sav")
K6 = haven::read_spss("lsacgrk6.sav")
K4 = haven::read_spss("lsacgrk4.sav")

# Read in merged file
lsacwide = haven::read_spss("lsacgrk_all_wide.sav")

# Check this has worked and the linked file matches separate datafiles
# Full checks saved in LSAC_Checking.R

#### Socio-demographics ####
#### Gender (all coded 0 no; 1 yes) ####
tab1(lsacwide$jid51c1a, cum.percent = TRUE) # male 
tab1(lsacwide$jid51c1b, cum.percent = TRUE) # female
tab1(lsacwide$jid51c, cum.percent = TRUE) # 1 No conflict with Gender;2 Genderqueer/Transgender/Other/Conflicting Gender
tab1(lsacwide$zf02m1, cum.percent = TRUE) # Study child sex, 1 male; 2 female
Cisgender <- subset(lsacwide, jid51c ==1) # subset of those with 1 (no conflict with gender)
tab1(Cisgender$zf02m1, cum.percent = TRUE)

lsacwide$trans <- ifelse(lsacwide$jid51c ==2, 1,
                    ifelse(lsacwide$jid51c == -9, NA, 0)) # those with conflicting gender

tab1(K18$jid51c, cum.percent = TRUE)
tab1(lsacwide$trans, cum.percent = TRUE)

lsacwide$cisfemale <- ifelse(lsacwide$zf02m1 ==2 & lsacwide$jid51c ==1, 1,
                          ifelse(lsacwide$jid51c == -9, NA, 0)) # those with sex female and no conflict with gender
tab1(lsacwide$cisfemale, cum.percent = TRUE)

lsacwide$cismale <- ifelse(lsacwide$zf02m1 ==1 & lsacwide$jid51c ==1, 1,
                             ifelse(lsacwide$jid51c == -9, NA, 0)) # those with sex male and no conflict with gender
tab1(lsacwide$cismale, cum.percent = TRUE)

# Create single gender variable with 3 levels
lsacwide$gender <- ifelse(lsacwide$trans ==1,2,
                           ifelse(lsacwide$cisfemale == 1,1,
                                  ifelse(lsacwide$cismale ==1,0,NA)))

tab1(lsacwide$gender, cum.percent = TRUE)

#### SES ####
mean(lsacwide$csep, na.rm=TRUE)
tab1(lsacwide$ccnfsad, cum.percent = TRUE)


##### Parental education ####
# Compute highest qualification of household
# Year 12 or lower
# Diploma/ cert/ other
# University degree
# Parent 1
# fd08a1 - highest level of school completed
# fd08a2a - completed any further education, 1 yes, 2 no
# fd08a3b
# fd08a3a - highest qualification completed
    # 1 Postgraduate degree; 2 Graduate diploma/certificate; 3 Bachelor degree; 
    # 4 Advanced diploma/diploma; 5 Certificate; 6 Other

lsacwide$parent1_ed <- ifelse(lsacwide$cfd08a2a == 2, 1,
                    ifelse(lsacwide$cfd08a2a == -4, -4,
                    ifelse(lsacwide$cfd08a2a == -2, -2,
                    ifelse(lsacwide$cfd08a3a %in% 4:6, 2, 
                    ifelse(lsacwide$cfd08a3a %in% 1:3, 3, NA)))))

tab1(lsacwide$parent1_ed, cum.percent = TRUE)

# Those with -4 and -2 are "section refused" and "don't know", so coding these as missing
lsacwide$parent1_ed <- ifelse(lsacwide$cfd08a2a == 2, 1,
                                           ifelse(lsacwide$cfd08a3a %in% 4:6, 2, 
                                                  ifelse(lsacwide$cfd08a3a %in% 1:3, 3, NA)))
# new var "parent_ed " coded as 1 for Yr 12 or lower; 2 for diploma/trade degree/other; 3 for uni degree

# Parent 2
# cfd08b2a - completed post school qualification, 1 yes, 2 no
cfd08b1
cfd08b2b
cfd08b2a
# cfd08b3a - highest qualification completed
    # 1 Postgraduate degree; 2 Graduate diploma/certificate; 3 Bachelor degree; 
    # 4 Advanced diploma/diploma; 5 Certificate; 6 Other
cfd09b1
cfd09b2

tab1(lsacwide$cfd08b2a)
tab1(lsacwide$cfd08b3a)

lsacwide$parent2_ed <- ifelse(lsacwide$cfd08b2a == 2, 1,
                              ifelse(lsacwide$cfd08b3a %in% 4:6, 2, 
                                     ifelse(lsacwide$cfd08b3a %in% 1:3, 3, NA)))
# new var "parent2_ed " coded as 1 for Yr 12 or lower; 2 for diploma/trade degree/other; 3 for uni degree
tab1(lsacwide$parent2_ed)

lsacwide$household_ed <- pmax(lsacwide$parent1_ed, lsacwide$parent2_ed) # give each person their highest parental qual
tab1(lsacwide$household_ed)
tab1(lsacwide$parent1_ed)
lsacwide$university <- ifelse(lsacwide$household_ed == 3, 1, 0)
tab1(lsacwide$university)
lsacwide$diploma <- ifelse(lsacwide$household_ed == 2, 1, 0)
tab1(lsacwide$diploma)

#### Housing ####
#### How many homes since birth - by age 14 ####
tab1(lsacwide$hho03a2a)
lsacwide$homes_since_birth <- ifelse(lsacwide$hho03a2a == -9, NA, lsacwide$hho03a2a)
tab1(lsacwide$homes_since_birth)

##### Neighbourhood liveability nliveb1 4-10 and again ta 14 ####
tab1(lsacwide$cnliveb1)
tab1(lsacwide$dnliveb1)
tab1(lsacwide$enliveb1)
tab1(lsacwide$fnliveb1)
tab1(lsacwide$hnliveb1)

#### Housing insecurity ####
tab1(lsacwide$gho11a1)
tab1(lsacwide$gho11a1a)
# Last two years always had a permanent place to live
lsacwide$g_housing_security <- ifelse(lsacwide$gho11a1a < 0, NA, lsacwide$gho11a1a)
tab1(lsacwide$g_housing_security)
tab1(lsacwide$hho11a1a)
tab1(lsacwide$iho11a1a)
lsacwide$h_housing_security <- ifelse(lsacwide$hho11a1a < 0, NA, lsacwide$hho11a1a)
lsacwide$i_housing_security <- ifelse(lsacwide$iho11a1a < 0, NA, lsacwide$iho11a1a)
tab1(lsacwide$h_housing_security)
tab1(lsacwide$i_housing_security)

lsacwide$any_housing_insecruity <- ifelse(lsacwide$g_housing_security ==0 | lsacwide$h_housing_security ==0 | lsacwide$i_housing_security == 0, 1, 0)
tab1(lsacwide$any_housing_insecruity)


#### Parental mental health / substance use ####
#### Pre-natal alcohol exposure ####
# Assessed age 4-5
# zhb17a1
tab1(K4$zhb17a1, cum.percent = TRUE)
tab1(lsacwide$zhb17a1, cum.percent = TRUE)
# Compute PAE vs none
lsacwide$PAE <- ifelse(lsacwide$zhb17a1 %in% 1:2, 1,
                       ifelse(lsacwide$zhb17a1 == -9, NA, 0))
tab1(lsacwide$PAE, cum.percent = TRUE)

#### Parental mental illness ####
# 1 No, never; 2 Yes, as a child only; 3 Yes, as an adult only; 4 Yes, as a child and as an adult
# Might have to compute for Parent 1, Parent 2 (other household parent/step-parent), and parent living elsewhere
# Parent 1, child age 12
tab1(lsacwide$ghs48a32, cum.percent = TRUE) # Have you ever had depression?
tab1(lsacwide$ghs48a33, cum.percent = TRUE) # Have you ever had scizophrenia?
tab1(lsacwide$ghs48a34, cum.percent = TRUE) # Ever had bipolar
tab1(lsacwide$ghs48a35, cum.percent = TRUE) # Postnatal depression
tab1(lsacwide$ghs48a36, cum.percent = TRUE) # Other mental illness
tab1(lsacwide$gak6g, cum.percent = TRUE)
# Parent 1, child age 16
tab1(lsacwide$ihs48a32, cum.percent = TRUE) # Have you ever had depression?
tab1(lsacwide$ihs48a33, cum.percent = TRUE) # Have you ever had scizophrenia?
tab1(lsacwide$ihs48a34, cum.percent = TRUE) # Ever had bipolar
tab1(lsacwide$ihs48a35, cum.percent = TRUE) # Postnatal depression
tab1(lsacwide$ihs48a36, cum.percent = TRUE) # Other mental illness

# Parent 1 - by child age 16
# Depression
lsacwide$parent1_dep <- ifelse(lsacwide$ihs48a32 %in% 2:4, 1,
                               ifelse(lsacwide$ihs48a32 == -9 | lsacwide$ihs48a32 == -3 | lsacwide$ihs48a32 == -2, NA, 0))
tab1(lsacwide$parent1_dep, cum.percent = TRUE) 
# Schizophrenia
tab1(lsacwide$ihs48a33, cum.percent = TRUE) 
lsacwide$parent1_schiz <- ifelse(lsacwide$ihs48a33 %in% 2:4, 1,
                                 ifelse(lsacwide$ihs48a33 == -9 | lsacwide$ihs48a33 == -3 | lsacwide$ihs48a33 == -2, NA, 0))
tab1(lsacwide$parent1_schiz, cum.percent = TRUE) 
# Bipolar
tab1(lsacwide$ihs48a34, cum.percent = TRUE) # Ever had bipolar
lsacwide$parent1_bipolar <- ifelse(lsacwide$ihs48a34 %in% 2:4, 1,
                                   ifelse(lsacwide$ihs48a34 == -9 | lsacwide$ihs48a34 == -3 | lsacwide$ihs48a34 == -2, NA, 0))
tab1(lsacwide$parent1_bipolar, cum.percent = TRUE) 
# Postnatal depression
tab1(lsacwide$ihs48a35, cum.percent = TRUE) # Postnatal depression
lsacwide$parent1_postnatal <- ifelse(lsacwide$ihs48a35 %in% 2:4, 1,
                                     ifelse(lsacwide$ihs48a35 == -9 | lsacwide$ihs48a35 == -3 | lsacwide$ihs48a35 == -2, NA, 0))
tab1(lsacwide$parent1_postnatal, cum.percent = TRUE) 
# Other mental illness
tab1(lsacwide$ihs48a36, cum.percent = TRUE) # Other mental illness
lsacwide$parent1_otherMI <- ifelse(lsacwide$ihs48a36 %in% 2:4, 1,
                                   ifelse(lsacwide$ihs48a36 == -9 | lsacwide$ihs48a36 == -3 | lsacwide$ihs48a36 == -2, NA, 0))
tab1(lsacwide$parent1_otherMI, cum.percent = TRUE) 

# Parent 1 - any mental disorder
lsacwide$parent1_MHdisorder <- ifelse(lsacwide$parent1_dep == 1 | lsacwide$parent1_schiz == 1 | lsacwide$parent1_bipolar == 1 | 
                                        lsacwide$parent1_postnatal == 1 | lsacwide$parent1_otherMI == 1, 1, 0)
tab1(lsacwide$parent1_MHdisorder, cum.percent = TRUE) 

# Parent 2
# Parent 2, child age 16
tab1(lsacwide$ihs48b32, cum.percent = TRUE) # Have you ever had depression?
tab1(lsacwide$ihs48b33, cum.percent = TRUE) # Have you ever had scizophrenia?
tab1(lsacwide$ihs48b34, cum.percent = TRUE) # Ever had bipolar
tab1(lsacwide$ihs48b35, cum.percent = TRUE) # Postnatal depression
tab1(lsacwide$ihs48b36, cum.percent = TRUE) # Other mental illness

# Parent 2 - by child age 16
# Depression
lsacwide$parent2_dep <- ifelse(lsacwide$ihs48b32 %in% 2:4, 1,
                               ifelse(lsacwide$ihs48b32 == -9 | lsacwide$ihs48b32 == -3 | lsacwide$ihs48b32 == -2, NA, 0))
tab1(lsacwide$parent2_dep, cum.percent = TRUE) 
# Schizophrenia
tab1(lsacwide$ihs48b33, cum.percent = TRUE) 
lsacwide$parent2_schiz <- ifelse(lsacwide$ihs48b33 %in% 2:4, 1,
                                 ifelse(lsacwide$ihs48b33 == -9 | lsacwide$ihs48b33 == -3 | lsacwide$ihs48b33 == -2, NA, 0))
tab1(lsacwide$parent2_schiz, cum.percent = TRUE) 
# Bipolar
tab1(lsacwide$ihs48b34, cum.percent = TRUE) # Ever had bipolar
lsacwide$parent2_bipolar <- ifelse(lsacwide$ihs48b34 %in% 2:4, 1,
                                   ifelse(lsacwide$ihs48b34 == -9 | lsacwide$ihs48b34 == -3 | lsacwide$ihs48b34 == -2, NA, 0))
tab1(lsacwide$parent2_bipolar, cum.percent = TRUE) 
# Postnatal depression
tab1(lsacwide$ihs48b35, cum.percent = TRUE) # Postnatal depression
lsacwide$parent2_postnatal <- ifelse(lsacwide$ihs48b35 %in% 2:4, 1,
                                     ifelse(lsacwide$ihs48b35 == -9 | lsacwide$ihs48b35 == -3 | lsacwide$ihs48b35 == -2, NA, 0))
tab1(lsacwide$parent2_postnatal, cum.percent = TRUE) 
# Other mental illness
tab1(lsacwide$ihs48b36, cum.percent = TRUE) # Other mental illness
lsacwide$parent2_otherMI <- ifelse(lsacwide$ihs48b36 %in% 2:4, 1,
                                   ifelse(lsacwide$ihs48b36 == -9 | lsacwide$ihs48b36 == -3 | lsacwide$ihs48b36 == -2, NA, 0))
tab1(lsacwide$parent2_otherMI, cum.percent = TRUE) 

# Parent 2 - any mental disorder
lsacwide$parent2_MHdisorder <- ifelse(lsacwide$parent2_dep == 1 | lsacwide$parent2_schiz == 1 | lsacwide$parent2_bipolar == 1 | 
                                        lsacwide$parent2_postnatal == 1 | lsacwide$parent2_otherMI == 1, 1, 0)
tab1(lsacwide$parent2_MHdisorder, cum.percent = TRUE)

# Household parent MH disorder
lsacwide$HHparent_MHdisorder <- ifelse(lsacwide$parent1_MHdisorder == 1 | 
                                         lsacwide$parent2_MHdisorder == 1, 1, 0)
tab1(lsacwide$HHparent_MHdisorder, cum.percent = TRUE)

#### Parent alcohol use ####
# Measured at all timepoints from age 4 to age 14
# Age 4
tab1(lsacwide$chb16a1, cum.percent = TRUE) # Have you ever drunk alcohol? 1 yes; 2 no
tab1(lsacwide$chb16a2, cum.percent = TRUE) # How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday
tab1(lsacwide$chb16a3, cum.percent = TRUE) # How many standard drinks do you have on a typical day when you are drinking?
tab1(lsacwide$chb16a4, cum.percent = TRUE) # How often do you have 5 or more standard drinks on one occasion (if female) or 7 or more standard drinks on one occasion (if male)?
tab1(lsacwide$chb16a5, cum.percent = TRUE) # How often do you have 5 or more standard drinks on one occasion (if female) or 7 or more standard drinks on one occasion (if male)?
tab1(lsacwide$aavdac, cum.percent = TRUE) # Average daily alcohol consumption using midpoints of categories for chb16a2 and chb16a3
tab1(lsacwide$aalcgp, cum.percent = TRUE) # Parent 1 average daily alcohol consumption using midpoints of categories for apw23b2a and apw23b2b
tab1(lsacwide$ahvdac, cum.percent = TRUE) # Average daily alcohol consumption >4 drinks for men and >2 for women
tab1(lsacwide$abinge, cum.percent = TRUE) # Have an alcohol binge (5+ drinks in a sitting for women, 7+ for men) 2 to 3 times per month or more commonly
tab1(lsacwide$caalcp, cum.percent = TRUE) # Problematic alcohol use, defined as having heavy daily alcohol consumption (>4 drinks for men >2 for women) or frequent binge drinking (7+ drinks in a sitting for men 5+ for women 2 to 3 times a month or more often)

# aalcp 
# 1 Yes; 2 No
tab1(lsacwide$caalcp, cum.percent = TRUE)
tab1(lsacwide$daalcp, cum.percent = TRUE)
tab1(lsacwide$eaalcp, cum.percent = TRUE)
tab1(lsacwide$faalcp, cum.percent = TRUE)
tab1(lsacwide$gaalcp, cum.percent = TRUE)
tab1(lsacwide$haalcp, cum.percent = TRUE)
# f, g, and h have -9s
# Recode first 3 to 0 No, 1 Yes
lsacwide$cp1_alcp <- ifelse(lsacwide$caalcp == 2, 0, lsacwide$caalcp)
tab1(lsacwide$cp1_alcp, cum.percent = TRUE)

lsacwide$dp1_alcp <- ifelse(lsacwide$daalcp == 2, 0, lsacwide$daalcp)
tab1(lsacwide$dp1_alcp, cum.percent = TRUE)

lsacwide$ep1_alcp <- ifelse(lsacwide$eaalcp == 2, 0, lsacwide$eaalcp)
tab1(lsacwide$ep1_alcp, cum.percent = TRUE)

# Age 10 (f)
tab1(lsacwide$fhb16a2, cum.percent = TRUE) # How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday
tab1(lsacwide$fhb16a3, cum.percent = TRUE) # How many standard drinks do you have on a typical day when you are drinking?
tab1(lsacwide$chb16a4, cum.percent = TRUE) # How often do you have 5 or more standard drinks on one occasion (if female) or 7 or more standard drinks on one occasion (if male)?
tab1(lsacwide$chb16a5, cum.percent = TRUE) # How often do you have 5 or more standard drinks on one occasion (if female) or 7 or more standard drinks on one occasion (if male)?
tab1(lsacwide$aavdac, cum.percent = TRUE) # Average daily alcohol consumption using midpoints of categories for chb16a2 and chb16a3
tab1(lsacwide$aalcgp, cum.percent = TRUE) # Parent 1 average daily alcohol consumption using midpoints of categories for apw23b2a and apw23b2b
tab1(lsacwide$ahvdac, cum.percent = TRUE) # Average daily alcohol consumption >4 drinks for men and >2 for women
tab1(lsacwide$abinge, cum.percent = TRUE) # Have an alcohol binge (5+ drinks in a sitting for women, 7+ for men) 2 to 3 times per month or more commonly
tab1(lsacwide$faalcp, cum.percent = TRUE) # Problematic alcohol use, defined as having heavy daily alcohol consumption (>4 drinks for men >2 for women) or frequent binge drinking (7+ drinks in a sitting for men 5+ for women 2 to 3 times a month or more often)

# Need to create new variables to get rid of the -9s
# F - age 10
lsacwide$fp1_alcp <- ifelse(lsacwide$fhb16a2 == 0, 0,
                            ifelse(lsacwide$faalcp == 2, 0, lsacwide$faalcp))
tab1(lsacwide$fp1_alcp, cum.percent = TRUE)

# G - age 12
tab1(lsacwide$ghb16a2, cum.percent = TRUE) # How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday
tab1(lsacwide$gaalcp, cum.percent = TRUE) # Problematic alcohol use, defined as having heavy daily alcohol consumption (>4 drinks for men >2 for women) or frequent binge drinking (7+ drinks in a sitting for men 5+ for women 2 to 3 times a month or more often)

lsacwide$gp1_alcp <- ifelse(lsacwide$gaalcp == 2, 0,
                            ifelse(lsacwide$gaalcp == -9, NA, lsacwide$gaalcp))
tab1(lsacwide$gp1_alcp, cum.percent = TRUE)
# H - age 14
tab1(lsacwide$hhb16a2, cum.percent = TRUE) # How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday
tab1(lsacwide$haalcp, cum.percent = TRUE) # Problematic alcohol use, defined as having heavy daily alcohol consumption (>4 drinks for men >2 for women) or frequent binge drinking (7+ drinks in a sitting for men 5+ for women 2 to 3 times a month or more often)

lsacwide$hp1_alcp <- ifelse(lsacwide$haalcp == 2, 0,
                            ifelse(lsacwide$haalcp == -9, NA, lsacwide$haalcp))
tab1(lsacwide$hp1_alcp, cum.percent = TRUE)

# Create a new variable indicating whether participant has available data on 5 out of 6 variables
lsacwide$available_p1_alcp <- rowSums(!is.na(lsacwide[c("cp1_alcp", "dp1_alcp", "ep1_alcp", "fp1_alcp", "gp1_alcp", "hp1_alcp")])) >= 5

# Calculate score for participants with available data on  out of 6 variables
lsacwide$nwaves_p1_alcp <- ifelse(lsacwide$available_p1_alcp,
                                  rowSums(lsacwide[c("cp1_alcp", "dp1_alcp", "ep1_alcp", "fp1_alcp", "gp1_alcp", "hp1_alcp")], na.rm = TRUE),
                                  NA)

tab1(lsacwide$nwaves_p1_alcp, cum.percent = TRUE)
lsacwide$p1_alcp <- ifelse(lsacwide$nwaves_p1_alcp %in% 0:1, 0,
                           ifelse(lsacwide$nwaves_p1_alcp %in% 2:6, 1, NA))

tab1(lsacwide$p1_alcp, cum.percent = TRUE)

# Need to do the same for parent 2
# Age 4
tab1(lsacwide$chb16b1, cum.percent = TRUE) # Have you ever drunk alcohol? 1 yes; 2 no
tab1(lsacwide$chb16b2, cum.percent = TRUE) # How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday

# aalcp 
# 1 Yes; 2 No
tab1(lsacwide$cbalcp, cum.percent = TRUE)
tab1(lsacwide$dbalcp, cum.percent = TRUE)
tab1(lsacwide$ebalcp, cum.percent = TRUE)
tab1(lsacwide$fbalcp, cum.percent = TRUE)
tab1(lsacwide$gbalcp, cum.percent = TRUE)
tab1(lsacwide$hbalcp, cum.percent = TRUE)
# f, g, and h have -9s
# Recode first 3 to 0 No, 1 Yes
lsacwide$cp2_alcp <- ifelse(lsacwide$cbalcp == 2, 0, lsacwide$cbalcp)
tab1(lsacwide$cp2_alcp, cum.percent = TRUE)

lsacwide$dp2_alcp <- ifelse(lsacwide$dbalcp == 2, 0, lsacwide$dbalcp)
tab1(lsacwide$dp2_alcp, cum.percent = TRUE)

lsacwide$ep2_alcp <- ifelse(lsacwide$ebalcp == 2, 0, lsacwide$ebalcp)
tab1(lsacwide$ep2_alcp, cum.percent = TRUE)

# Need to create new variables to get rid of the -9s
# F - age 10
tab1(lsacwide$fhb16b2, cum.percent = TRUE)
# How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday
tab1(lsacwide$fbalcp, cum.percent = TRUE) # Problematic alcohol use, defined as having heavy daily alcohol consumption (>4 drinks for men >2 for women) or frequent binge drinking (7+ drinks in a sitting for men 5+ for women 2 to 3 times a month or more often)

lsacwide$fp2_alcp <- ifelse(lsacwide$fbalcp == 2, 0,
                            ifelse(lsacwide$fbalcp == -9, NA, lsacwide$fbalcp))
tab1(lsacwide$fp2_alcp, cum.percent = TRUE)

# G - age 12
tab1(lsacwide$ghb16b2, cum.percent = TRUE) # How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday
tab1(lsacwide$gbalcp, cum.percent = TRUE) # Problematic alcohol use, defined as having heavy daily alcohol consumption (>4 drinks for men >2 for women) or frequent binge drinking (7+ drinks in a sitting for men 5+ for women 2 to 3 times a month or more often)

lsacwide$gp2_alcp <- ifelse(lsacwide$gbalcp == 2, 0,
                            ifelse(lsacwide$gbalcp == -9, NA, lsacwide$gbalcp))
tab1(lsacwide$gp2_alcp, cum.percent = TRUE)
# H - age 14
tab1(lsacwide$hhb16b2, cum.percent = TRUE) # How often do you have a drink containing alcohol? 
# 0 Never; 1 Not in the last year; 2 Monthly or less; 3 2 to 3 times a month; 4 Once a week; 5 2 to 3 times a week; 6 4 to 6 times a week; 7 Everyday
tab1(lsacwide$hbalcp, cum.percent = TRUE) # Problematic alcohol use, defined as having heavy daily alcohol consumption (>4 drinks for men >2 for women) or frequent binge drinking (7+ drinks in a sitting for men 5+ for women 2 to 3 times a month or more often)

lsacwide$hp2_alcp <- ifelse(lsacwide$hbalcp == 2, 0,
                            ifelse(lsacwide$hbalcp == -9, NA, lsacwide$hbalcp))
tab1(lsacwide$hp2_alcp, cum.percent = TRUE)

# Create a new variable indicating whether participant has available data on 5 out of 6 variables
lsacwide$available_p2_alcp <- rowSums(!is.na(lsacwide[c("cp2_alcp", "dp2_alcp", "ep2_alcp", "fp2_alcp", "gp2_alcp", "hp2_alcp")])) >= 5

# Calculate score for participants with available data on  out of 6 variables
lsacwide$nwaves_p2_alcp <- ifelse(lsacwide$available_p2_alcp,
                                  rowSums(lsacwide[c("cp2_alcp", "dp2_alcp", "ep2_alcp", "fp2_alcp", "gp2_alcp", "hp2_alcp")], na.rm = TRUE),
                                  NA)

tab1(lsacwide$nwaves_p2_alcp, cum.percent = TRUE)
lsacwide$p2_alcp <- ifelse(lsacwide$nwaves_p2_alcp %in% 0:1, 0,
                           ifelse(lsacwide$nwaves_p2_alcp %in% 2:6, 1, NA))

tab1(lsacwide$p2_alcp, cum.percent = TRUE)
# Either parent with problematic use
lsacwide$hh_alcp <- ifelse(lsacwide$p2_alcp == 1 | lsacwide$p1_alcp == 1, 1, 0)

tab1(lsacwide$hh_alcp, cum.percent = TRUE)

#### Parent SUD ####
# Age 12
tab1(lsacwide$ghs48a31, cum.percent = TRUE) # Have you ever had any of the following conditions? Drug addiction
# 1 No, never; 2 Yes, as a child only; 3 Yes, as an adult only; 4 Yes, as a child and as an adult
lsacwide$gp1_dud <- ifelse(lsacwide$ghs48a31 == 1, 0,
                           ifelse(lsacwide$ghs48a31 %in% 2:4, 1, NA))

tab1(lsacwide$gp1_dud, cum.percent = TRUE)

# Age 14
tab1(lsacwide$ihs48a31, cum.percent = TRUE) # Have you ever had any of the following conditions? Drug addiction
# 1 No, never; 2 Yes, as a child only; 3 Yes, as an adult only; 4 Yes, as a child and as an adult
lsacwide$ip1_dud <- ifelse(lsacwide$ihs48a31 == 1, 0,
                           ifelse(lsacwide$ihs48a31 %in% 2:4, 1, NA))

tab1(lsacwide$ip1_dud, cum.percent = TRUE)

# Parent 2
# Age 12
tab1(lsacwide$ghs48b31b, cum.percent = TRUE) # Have you ever had any of the following conditions? Drug addiction
# 1 No, never; 2 Yes, as a child only; 3 Yes, as an adult only; 4 Yes, as a child and as an adult
lsacwide$gp2_dud <- ifelse(lsacwide$ghs48b31b == 1, 0,
                           ifelse(lsacwide$ghs48b31b %in% 2:4, 1, NA))

tab1(lsacwide$gp2_dud, cum.percent = TRUE)

# Age 14
tab1(lsacwide$ihs48b31, cum.percent = TRUE) # Have you ever had any of the following conditions? Drug addiction
# 1 No, never; 2 Yes, as a child only; 3 Yes, as an adult only; 4 Yes, as a child and as an adult
lsacwide$ip2_dud <- ifelse(lsacwide$ihs48b31 == 1, 0,
                           ifelse(lsacwide$ihs48b31 %in% 2:4, 1, NA))

tab1(lsacwide$ip2_dud, cum.percent = TRUE)

# Household parent SUD
lsacwide$hh_dud <- ifelse(lsacwide$gp1_dud == 1 | lsacwide$ip1_dud == 1 | lsacwide$gp2_dud == 1 | lsacwide$ip2_dud == 1, 1, 0)
tab1(lsacwide$hh_dud, cum.percent = TRUE)


#### Parent 1 K6 ####
tab1(lsacwide$cak6s)
tab1(lsacwide$dak6s)
tab1(lsacwide$eak6s)
tab1(lsacwide$fak6s)
tab1(lsacwide$gak6s)
tab1(lsacwide$hak6s)
tab1(lsacwide$iak6s)



#### Parenting ####
#### Parental monitoring ####
tab1(lsacwide$eaparmon, cum.percent = TRUE) 
tab1(lsacwide$faparmon, cum.percent = TRUE) 
tab1(lsacwide$gaparmon, cum.percent = TRUE) 
tab1(lsacwide$haparmonb, cum.percent = TRUE) 
#### Parental warmth ####
# c - h
tab1(lsacwide$cawarm, cum.percent = TRUE) 
tab1(lsacwide$dawarm, cum.percent = TRUE) 
tab1(lsacwide$eawarm, cum.percent = TRUE) 
tab1(lsacwide$fawarm, cum.percent = TRUE) 
tab1(lsacwide$gawarm, cum.percent = TRUE) 
tab1(lsacwide$hawarm, cum.percent = TRUE) 
# Compute max if they have 4 or more variables with data
# Create a new variable indicating whether participant has available data on at least 4 out of 6 variables
lsacwide$available_pwarmth <- rowSums(!is.na(lsacwide[c("cawarm", "dawarm", "eawarm", "fawarm", "gawarm", "hawarm")])) >= 4

# Calculate max for participants with available data on 2 out of 3 variables
lsacwide$max_pwarmth <- ifelse(lsacwide$available_pwarmth,
                               pmax(lsacwide$cawarm, lsacwide$dawarm, lsacwide$eawarm, 
                                    lsacwide$fawarm, lsacwide$gawarm, lsacwide$hawarm, na.rm = TRUE),
                               NA)
tab1(lsacwide$max_pwarmth)
#### Close to parent ####
# 1 Very close; 2 Quite close; 3 Not very close; 4 Not close at all
#Age 12/13
tab1(lsacwide$gpa21a4, cum.percent = TRUE)  # How close do you feel to your mum?
tab1(lsacwide$gpa21b4, cum.percent = TRUE)  # How close do you feel to your dad
tab1(lsacwide$gpa21p4, cum.percent = TRUE)  # PLE

lsacwide$close_mum12 <- ifelse(lsacwide$gpa21a4 %in% 1:2, 1, 
                               ifelse(lsacwide$gpa21a4 %in% 3:4, 0, NA))

tab1(lsacwide$close_mum12, cum.percent = TRUE) 

lsacwide$close_dad12 <- ifelse(lsacwide$gpa21b4 %in% 1:2, 1, 
                               ifelse(lsacwide$gpa21b4 %in% 3:4, 0, NA))

tab1(lsacwide$close_dad12, cum.percent = TRUE) 
#Age 14/15
tab1(lsacwide$hpa21a4, cum.percent = TRUE) # mum
tab1(lsacwide$hpa21b4, cum.percent = TRUE) # dad
lsacwide$close_mum14 <- ifelse(lsacwide$hpa21a4 %in% 1:2, 1, 
                               ifelse(lsacwide$hpa21a4 %in% 3:4, 0, NA))

tab1(lsacwide$close_mum14, cum.percent = TRUE) 

lsacwide$close_dad14 <- ifelse(lsacwide$hpa21b4 %in% 1:2, 1, 
                               ifelse(lsacwide$hpa21b4 %in% 3:4, 0, NA))

tab1(lsacwide$close_dad14, cum.percent = TRUE) 

#Age 16/17
tab1(lsacwide$ipa21a4, cum.percent = TRUE) 
tab1(lsacwide$ipa21b4, cum.percent = TRUE) 

lsacwide$close_mum16 <- ifelse(lsacwide$ipa21a4 %in% 1:2, 1, 
                               ifelse(lsacwide$ipa21a4 %in% 3:4, 0, NA))

tab1(lsacwide$close_mum16, cum.percent = TRUE) 

lsacwide$close_dad16 <- ifelse(lsacwide$ipa21b4 %in% 1:2, 1, 
                               ifelse(lsacwide$ipa21b4 %in% 3:4, 0, NA))

tab1(lsacwide$close_dad16, cum.percent = TRUE) 
# At least one close parent at each timepoint
lsacwide$close_parent12 <- ifelse(lsacwide$close_mum12 == 1 | lsacwide$close_dad12 == 1, 1, 
                                  ifelse(lsacwide$close_mum12 == 0 & lsacwide$close_dad12 == 0, 0, NA))
tab1(lsacwide$close_parent12, cum.percent = TRUE) 

lsacwide$close_parent14 <- ifelse(lsacwide$close_mum14 == 1 | lsacwide$close_dad14 == 1, 1, 
                                  ifelse(lsacwide$close_mum14 == 0 & lsacwide$close_dad14 == 0, 0, NA))

tab1(lsacwide$close_parent14, cum.percent = TRUE) 

lsacwide$close_parent16 <- ifelse(lsacwide$close_mum16 == 1 | lsacwide$close_dad16 == 1, 1, 
                                  ifelse(lsacwide$close_mum16 == 0 & lsacwide$close_dad16 == 0, 0, NA))
tab1(lsacwide$close_parent16, cum.percent = TRUE) 

lsacwide$close_parent <- rowSums(lsacwide[c("close_parent12", "close_parent14", "close_parent16")])

tab1(lsacwide$close_parent, cum.percent = TRUE) 

lsacwide$close2parent <- ifelse(lsacwide$close_parent == 3, 1,
                                ifelse(lsacwide$close_parent %in% 0:2, 0, NA))
tab1(lsacwide$close2parent, cum.percent = TRUE) 

#### Parental support to child ####
tab1(lsacwide$hasupport, cum.percent = TRUE) 
tab1(lsacwide$iasupport, cum.percent = TRUE) 
print(cor(lsacwide$max_pwarmth, lsacwide$hasupport, use = "complete.obs"))
print(cor(lsacwide$max_pwarmth, lsacwide$iasupport, use = "complete.obs"))

# Create a new variable indicating whether participant has available data on at least 1 out of 2 variables
lsacwide$available_psupport <- rowSums(!is.na(lsacwide[c("hasupport", "iasupport")])) >= 1

# Calculate max for participants with available data on 2 out of 3 variables
lsacwide$max_psupport <- ifelse(lsacwide$available_psupport,
                                pmax(lsacwide$hasupport, lsacwide$iasupport, na.rm = TRUE),
                                NA)
tab1(lsacwide$max_psupport)


#### Angry parenting scale - parent 1 ####
tab1(lsacwide$caang)
tab1(lsacwide$daang)
tab1(lsacwide$eaang)
tab1(lsacwide$faang)
tab1(lsacwide$gaang)
tab1(lsacwide$haang)
tab1(lsacwide$iaang)
#### Angry parenting scale - parent 2 ####
tab1(lsacwide$cbang)
tab1(lsacwide$dbang)
tab1(lsacwide$ebang)
tab1(lsacwide$fbang)
tab1(lsacwide$gbang)
tab1(lsacwide$hbang)
tab1(lsacwide$ibang)
# f has a -9
lsacwide$f_bang <- ifelse(lsacwide$fbang == -9, NA, lsacwide$fbang)
tab1(lsacwide$f_bang)
##### Consistent parenting ####
tab1(lsacwide$cacons)
tab1(lsacwide$dacons)
tab1(lsacwide$eacons)
tab1(lsacwide$facons)
tab1(lsacwide$gacons)
tab1(lsacwide$hacons)
tab1(lsacwide$iacons)
# Consistent parenting - parent 2
tab1(lsacwide$cbcons)

#### Unsupervised time ####
tab1(lsacwide$gpa23a1, cum.percent = TRUE)
lsacwide$g_unsupervisedtime <- ifelse(lsacwide$gpa23a1 == -9, NA, lsacwide$gpa23a1)
tab1(lsacwide$g_unsupervisedtime, cum.percent = TRUE)
tab1(lsacwide$hpa23a1, cum.percent = TRUE)
lsacwide$h_unsupervisedtime <- ifelse(lsacwide$hpa23a1 == -9, NA, lsacwide$hpa23a1)
tab1(lsacwide$h_unsupervisedtime, cum.percent = TRUE)

print(cor(lsacwide$gpa23a1, lsacwide$max_pwarmth, use = "complete.obs"))
print(cor(lsacwide$g_unsupervisedtime, lsacwide$risky_alc, use = "complete.obs"))
tab1(lsacwide$gpa23b1, cum.percent = TRUE)

lsacwide$unsupervisedtime <- lsacwide$g_unsupervisedtime + lsacwide$h_unsupervisedtime
tab1(lsacwide$unsupervisedtime, cum.percent = TRUE)
median(lsacwide$unsupervisedtime, na.rm = TRUE)

# Create a new variable indicating whether participant has available data on 3 out of 4 variables
lsacwide$available_unsuptime <- rowSums(!is.na(lsacwide[c("g_unsupervisedtime", "h_unsupervisedtime")])) >= 1

# Calculate f_conduct_total only for participants with available data on 3 out of 4 variables
lsacwide$unsup_time <- ifelse(lsacwide$available_unsuptime,
                              rowSums(lsacwide[c("g_unsupervisedtime", "h_unsupervisedtime")], na.rm = TRUE),
                              NA)
tab1(lsacwide$unsup_time, cum.percent = TRUE)

# Compute new dummy variables 
lsacwide$unsuptime1416 <- ifelse(lsacwide$unsupervisedtime %in% 0:1, 0,
                                 ifelse(lsacwide$unsupervisedtime %in% 2:6, 1,
                                        ifelse(lsacwide$unsupervisedtime %in% 7:10, 2, NA)))
tab1(lsacwide$unsuptime1416, cum.percent = TRUE)
# 14
lsacwide$unsuptime14 <- ifelse(lsacwide$g_unsupervisedtime ==0, 0,
                               ifelse(lsacwide$g_unsupervisedtime %in% 1:4, 1, 
                                      ifelse(lsacwide$g_unsupervisedtime == 5, 2, NA)))

tab1(lsacwide$unsuptime14, cum.percent = TRUE)
# 16
lsacwide$unsuptime16 <- ifelse(lsacwide$h_unsupervisedtime ==0, 0,
                               ifelse(lsacwide$h_unsupervisedtime %in% 1:4, 1, 
                                      ifelse(lsacwide$h_unsupervisedtime == 5, 2, NA)))

tab1(lsacwide$unsuptime16, cum.percent = TRUE)
lsacwide$unsupervised_time <- lsacwide$unsuptime14 + lsacwide$unsuptime16
tab1(lsacwide$unsupervised_time, cum.percent = TRUE)

lsacwide$unsupervised_time1416 <- ifelse(lsacwide$unsupervised_time ==0, 0,
                                         ifelse(lsacwide$unsupervised_time %in% 1:2, 1,
                                                ifelse(lsacwide$unsupervised_time %in% 3:4, 2, NA)))
tab1(lsacwide$unsupervised_time1416, cum.percent = TRUE)

# Create dummy variables
lsacwide$no_unsupervised_time <- ifelse(lsacwide$unsupervised_time1416 ==0, 1,
                                        ifelse(lsacwide$unsupervised_time1416 %in% 1:2, 0, NA))
lsacwide$some_unsupervised_time <- ifelse(lsacwide$unsupervised_time1416 ==1, 1,
                                          ifelse(lsacwide$unsupervised_time1416 == 2 | lsacwide$unsupervised_time1416 == 0, 0, NA))
lsacwide$much_unsupervised_time <- ifelse(lsacwide$unsupervised_time1416 ==2, 1,
                                          ifelse(lsacwide$unsupervised_time1416 == 0 | lsacwide$unsupervised_time1416 == 1, 0, NA))
tab1(lsacwide$no_unsupervised_time, cum.percent = TRUE)
tab1(lsacwide$some_unsupervised_time, cum.percent = TRUE)
tab1(lsacwide$much_unsupervised_time, cum.percent = TRUE)



##### Home activities index mean available from 4 - 10 ####
tab1(lsacwide$cahact)
tab1(lsacwide$dahact)
tab1(lsacwide$eahacte)
tab1(lsacwide$fahactd)
#### Out of home activities index (v2) available from 4 - 14. Number of yes responses ####
# Higher scores = more out of home activities
tab1(lsacwide$coohactb)
tab1(lsacwide$doohactb)
tab1(lsacwide$eoohactb)
tab1(lsacwide$foohactb)
tab1(lsacwide$goohactb)
tab1(lsacwide$hoohactb)

##### Parental self-efficacy ####
tab1(lsacwide$cpa01b)
tab1(lsacwide$dpa01b)
tab1(lsacwide$epa01b)
tab1(lsacwide$fpa01b)
tab1(lsacwide$gpa01b)
tab1(lsacwide$hpa01b)
tab1(lsacwide$ipa01b)

lsacwide$cPSE <- ifelse(lsacwide$cpa01b < 0, NA, lsacwide$cpa01b)
lsacwide$dPSE <- ifelse(lsacwide$dpa01b < 0, NA, lsacwide$dpa01b)
lsacwide$ePSE <- ifelse(lsacwide$epa01b < 0, NA, lsacwide$epa01b)
lsacwide$fPSE <- ifelse(lsacwide$fpa01b < 0, NA, lsacwide$fpa01b)
lsacwide$gPSE <- ifelse(lsacwide$gpa01b < 0, NA, lsacwide$gpa01b)
lsacwide$hPSE <- ifelse(lsacwide$hpa01b < 0, NA, lsacwide$hpa01b)
lsacwide$iPSE <- ifelse(lsacwide$ipa01b < 0, NA, lsacwide$ipa01b)
tab1(lsacwide$cPSE)


#Need to update to parent 1
tab1(lsacwide$cpa01a)
tab1(lsacwide$dpa01a)
tab1(lsacwide$epa01a)
tab1(lsacwide$fpa01a)
tab1(lsacwide$gpa01a)
tab1(lsacwide$hpa01a)
tab1(lsacwide$ipa01a)

lsacwide$cP1SE <- ifelse(lsacwide$cpa01a < 0, NA, lsacwide$cpa01a)
lsacwide$dP1SE <- ifelse(lsacwide$dpa01a < 0, NA, lsacwide$dpa01a)
lsacwide$eP1SE <- ifelse(lsacwide$epa01a < 0, NA, lsacwide$epa01a)
lsacwide$fP1SE <- ifelse(lsacwide$fpa01a < 0, NA, lsacwide$fpa01a)
lsacwide$gP1SE <- ifelse(lsacwide$gpa01a < 0, NA, lsacwide$gpa01a)
lsacwide$hP1SE <- ifelse(lsacwide$hpa01a < 0, NA, lsacwide$hpa01a)
lsacwide$iP1SE <- ifelse(lsacwide$ipa01a < 0, NA, lsacwide$ipa01a)

# Reduce sparseness by combining responses 1 and 2
lsacwide$cP1SE <- ifelse(lsacwide$cpa01a < 0, NA, 
                       ifelse(lsacwide$cpa01a %in% 1:2, 1, 
                              ifelse(lsacwide$cpa01a == 3, 2,
                                     ifelse(lsacwide$cpa01a == 4, 3,
                                            ifelse(lsacwide$cpa01a == 5, 4, NA)))))

tab1(lsacwide$cP1SE)

lsacwide$dP1SE <- ifelse(lsacwide$dpa01a < 0, NA, 
                         ifelse(lsacwide$dpa01a %in% 1:2, 1, 
                                ifelse(lsacwide$dpa01a == 3, 2,
                                       ifelse(lsacwide$dpa01a == 4, 3,
                                              ifelse(lsacwide$dpa01a == 5, 4, NA)))))

tab1(lsacwide$dP1SE)

lsacwide$eP1SE <- ifelse(lsacwide$epa01a < 0, NA, 
                         ifelse(lsacwide$epa01a %in% 1:2, 1, 
                                ifelse(lsacwide$epa01a == 3, 2,
                                       ifelse(lsacwide$epa01a == 4, 3,
                                              ifelse(lsacwide$epa01a == 5, 4, NA)))))

tab1(lsacwide$eP1SE)

lsacwide$fP1SE <- ifelse(lsacwide$fpa01a < 0, NA, 
                         ifelse(lsacwide$fpa01a %in% 1:2, 1, 
                                ifelse(lsacwide$fpa01a == 3, 2,
                                       ifelse(lsacwide$fpa01a == 4, 3,
                                              ifelse(lsacwide$fpa01a == 5, 4, NA)))))

tab1(lsacwide$fP1SE)

lsacwide$gP1SE <- ifelse(lsacwide$gpa01a < 0, NA, 
                         ifelse(lsacwide$gpa01a %in% 1:2, 1, 
                                ifelse(lsacwide$gpa01a == 3, 2,
                                       ifelse(lsacwide$gpa01a == 4, 3,
                                              ifelse(lsacwide$gpa01a == 5, 4, NA)))))

tab1(lsacwide$gP1SE)

lsacwide$hP1SE <- ifelse(lsacwide$hpa01a < 0, NA, 
                         ifelse(lsacwide$hpa01a %in% 1:2, 1, 
                                ifelse(lsacwide$hpa01a == 3, 2,
                                       ifelse(lsacwide$hpa01a == 4, 3,
                                              ifelse(lsacwide$hpa01a == 5, 4, NA)))))
tab1(lsacwide$hP1SE)

lsacwide$iP1SE <- ifelse(lsacwide$ipa01a < 0, NA, 
                         ifelse(lsacwide$ipa01a %in% 1:2, 1, 
                                ifelse(lsacwide$ipa01a == 3, 2,
                                       ifelse(lsacwide$ipa01a == 4, 3,
                                              ifelse(lsacwide$ipa01a == 5, 4, NA)))))
tab1(lsacwide$iP1SE)




#### Parent stress ####
#### Parental separation ####
# pe23d - have in K16 for waves 4-7 (ages 10-16)
# pe23a - does the child have biological parent living elsewhere? only asked at 6/7
# pe02c - does the child have a parent living elsewhere? asked from 10-16
tab1(K16$ipe23d, cum.percent = TRUE)
tab1(lsac2$ipe23d, cum.percent = TRUE)
tab1(K6$dpe23a, cum.percent = TRUE)
tab1(lsac$dpe23a, cum.percent = TRUE)
tab1(K10$fpe02c, cum.percent = TRUE)
tab1(lsacwide$fpe02c, cum.percent = TRUE)
tab1(K16$ipe02c, cum.percent = TRUE)
tab1(lsacwide$ipe02c, cum.percent = TRUE)
tab1(lsacwide$gpe02c, cum.percent = TRUE)
tab1(lsacwide$hpe02c, cum.percent = TRUE)

lsacwide$parent_sep <- ifelse(lsacwide$ipe02c == 1, 1,
                              ifelse(lsacwide$ipe02c == -9, NA, 0))
tab1(lsacwide$parent_sep, cum.percent = TRUE)

lsacwide$parent_sep10 <- ifelse(lsacwide$fpe02c == 1, 1,
                                ifelse(lsacwide$fpe02c == -9, NA, 0))
tab1(lsacwide$parent_sep16, cum.percent = TRUE)

lsacwide$parent_sep12 <- ifelse(lsacwide$gpe02c == 1, 1,
                                ifelse(lsacwide$gpe02c == -9, NA, 0))

lsacwide$parent_sep14 <- ifelse(lsacwide$hpe02c == 1, 1,
                                ifelse(lsacwide$hpe02c == -9, NA, 0))

lsacwide$parent_sep16 <- lsacwide$parent_sep

#### Parent financial stress ####
tab1(lsacwide$ifn10a, cum.percent = TRUE) # Ability to raise $2k in emergency
tab1(lsacwide$chship, cum.percent = TRUE)
tab1(lsacwide$chshipb, cum.percent = TRUE)

# Waves 2-7
tab1(lsacwide$dfn10a, cum.percent = TRUE) # no -9s
tab1(lsacwide$efn10a, cum.percent = TRUE) # no -9s
tab1(lsacwide$ffn10a, cum.percent = TRUE)
tab1(lsacwide$gfn10a, cum.percent = TRUE)
tab1(lsacwide$hfn10a, cum.percent = TRUE)
tab1(lsacwide$ifn10a, cum.percent = TRUE)

# Wave 2
lsacwide$d_finstress <- ifelse(lsacwide$dfn10a == 4, 1, 0)
tab1(lsacwide$d_finstress, cum.percent = TRUE)
# Wave 3
lsacwide$e_finstress <- ifelse(lsacwide$efn10a == 4, 1, 0)
tab1(lsacwide$e_finstress, cum.percent = TRUE)
# Wave 4
lsacwide$f_finstress <- ifelse(lsacwide$ffn10a == -9, NA, 
                               ifelse(lsacwide$ffn10a == 4, 1, 0))
tab1(lsacwide$f_finstress, cum.percent = TRUE)
# Wave 5
lsacwide$g_finstress <- ifelse(lsacwide$gfn10a == -9, NA, 
                               ifelse(lsacwide$gfn10a == 4, 1, 0))
tab1(lsacwide$g_finstress, cum.percent = TRUE)
# Wave 6
lsacwide$h_finstress <- ifelse(lsacwide$hfn10a == -9, NA, 
                               ifelse(lsacwide$hfn10a == 4, 1, 0))
tab1(lsacwide$h_finstress, cum.percent = TRUE)
# Wave 7
lsacwide$i_finstress <- ifelse(lsacwide$ifn10a == -9, NA, 
                               ifelse(lsacwide$ifn10a == 4, 1, 0))
tab1(lsacwide$i_finstress, cum.percent = TRUE)

# Overall
lsacwide$finstress <- ifelse(lsacwide$d_finstress == 1 | lsacwide$e_finstress == 1 | lsacwide$f_finstress == 1 |
                               lsacwide$g_finstress == 1 | lsacwide$h_finstress == 1 | lsacwide$i_finstress == 1, 1, 0)
tab1(lsacwide$finstress, cum.percent = TRUE)

# Also code continuous variable at each timepoint
# Wave 4
lsacwide$f_finstress_cont <- ifelse(lsacwide$ffn10a == -9, NA, lsacwide$ffn10a)
tab1(lsacwide$f_finstress_cont, cum.percent = TRUE)
# Wave 5
lsacwide$g_finstress_cont <- ifelse(lsacwide$gfn10a == -9, NA, lsacwide$gfn10a)
tab1(lsacwide$g_finstress_cont, cum.percent = TRUE)
# Wave 6
lsacwide$h_finstress_cont <- ifelse(lsacwide$hfn10a == -9, NA, lsacwide$hfn10a)
tab1(lsacwide$h_finstress_cont, cum.percent = TRUE)
# Wave 7
lsacwide$i_finstress_cont <- ifelse(lsacwide$ifn10a == -9, NA, lsacwide$ifn10a)
tab1(lsacwide$i_finstress_cont, cum.percent = TRUE)


#### Parent stressful life events ####
tab1(lsacwide$csle)
tab1(lsacwide$cslei)
tab1(lsacwide$dsle)
tab1(lsacwide$esle)
tab1(lsacwide$fsle)
tab1(lsacwide$gsle)
tab1(lsacwide$hsle)
tab1(lsacwide$isle)

lsacwide$h_sle <- ifelse(lsacwide$hsle ==-9, NA, lsacwide$hsle)
tab1(lsacwide$h_sle)

# Create a new variable indicating whether participant has available data on 5 out of 7 variables
lsacwide$available_sle <- rowSums(!is.na(lsacwide[c("csle", "dsle", "esle", "fsle", "gsle", "h_sle", "isle")])) >= 5

# Calculate sum for participants with available data on 5 out of 7 variables
lsacwide$parent_sle <- ifelse(lsacwide$available_sle,
                              rowSums(lsacwide[c("csle", "dsle", "esle", "fsle", "gsle", "h_sle", "isle")], na.rm = TRUE),
                              NA)
tab1(lsacwide$parent_sle, cum.percent = TRUE)



#### Difficulty of life at present ####
tab1(lsacwide$chs26a1)
tab1(lsacwide$dhs26a1)
tab1(lsacwide$ehs26a1)
tab1(lsacwide$fhs26a1)
tab1(lsacwide$ghs26a1)
tab1(lsacwide$hhs26a1)
tab1(lsacwide$ihs26a1)

lsacwide$c_difficulty <- ifelse(lsacwide$chs26a1 < 0, NA, lsacwide$chs26a1)
tab1(lsacwide$c_difficulty)

lsacwide$d_difficulty <- ifelse(lsacwide$dhs26a1 < 0, NA, lsacwide$dhs26a1)
tab1(lsacwide$d_difficulty)

lsacwide$e_difficulty <- ifelse(lsacwide$ehs26a1 < 0, NA, lsacwide$ehs26a1)
tab1(lsacwide$e_difficulty)

lsacwide$f_difficulty <- ifelse(lsacwide$fhs26a1 < 0, NA, lsacwide$fhs26a1)
tab1(lsacwide$f_difficulty)

lsacwide$g_difficulty <- ifelse(lsacwide$ghs26a1 < 0, NA, lsacwide$ghs26a1)
tab1(lsacwide$g_difficulty)

lsacwide$h_difficulty <- ifelse(lsacwide$hhs26a1 < 0, NA, lsacwide$hhs26a1)
tab1(lsacwide$h_difficulty)

lsacwide$i_difficulty <- ifelse(lsacwide$ihs26a1 < 0, NA, lsacwide$ihs26a1)
tab1(lsacwide$i_difficulty)

#### Social support to parent ####
# How often do you feel you need support but can't get it from anyone
# 1 Very often; 2 Often; 3 Sometimes; 4 Never; (-1 I don't need it)
tab1(lsacwide$csc08a)
lsacwide$c_support_avail_to_parent <- ifelse(lsacwide$csc08a == -9, NA, 
                                             ifelse(lsacwide$csc08a == -1, 4, lsacwide$csc08a))

tab1(lsacwide$c_support_avail_to_parent)

tab1(lsacwide$dsc08a)
tab1(lsacwide$esc08a)
tab1(lsacwide$gsc08a)
lsacwide$d_support_avail_to_parent <- ifelse(lsacwide$dsc08a < 0, NA, lsacwide$dsc08a)
tab1(lsacwide$d_support_avail_to_parent)

lsacwide$e_support_avail_to_parent <- ifelse(lsacwide$esc08a < 0, NA, lsacwide$esc08a)
tab1(lsacwide$e_support_avail_to_parent)

lsacwide$g_support_avail_to_parent <- ifelse(lsacwide$gsc08a < 0, NA, lsacwide$gsc08a)
tab1(lsacwide$g_support_avail_to_parent)

#### Domestic violence ####
# How often do you have arguments with your partner that end up with people pushing, hitting, kicking or shoving?
# 1 Never; 2 Rarely; 3 Sometimes; 4 Often; 5 Always
tab1(lsacwide$cre15a5)
tab1(lsacwide$dre15a5)
tab1(lsacwide$ere15a5)
tab1(lsacwide$fre15a5)
tab1(lsacwide$gre15a5)
tab1(lsacwide$hre15a5)
tab1(lsacwide$ire15a5)

lsacwide$cDV <- ifelse(lsacwide$cre15a5 < 0, NA, 
                       ifelse(lsacwide$cre15a5 == 1, 0,
                       ifelse(lsacwide$cre15a5 %in% 2:5, 1, NA)))

tab1(lsacwide$cDV)

lsacwide$dDV <- ifelse(lsacwide$dre15a5 < 0, NA, 
                       ifelse(lsacwide$dre15a5 == 1, 0,
                              ifelse(lsacwide$dre15a5 %in% 2:5, 1, NA)))

tab1(lsacwide$dDV)

lsacwide$eDV <- ifelse(lsacwide$ere15a5 < 0, NA, 
                       ifelse(lsacwide$ere15a5 == 1, 0,
                              ifelse(lsacwide$ere15a5 %in% 2:4, 1, NA)))
tab1(lsacwide$eDV)

lsacwide$fDV <- ifelse(lsacwide$fre15a5 < 0, NA, 
                       ifelse(lsacwide$fre15a5 == 1, 0,
                              ifelse(lsacwide$fre15a5 %in% 2:5, 1, NA)))

tab1(lsacwide$fDV)

lsacwide$gDV <- ifelse(lsacwide$gre15a5 < 0, NA, 
                       ifelse(lsacwide$gre15a5 == 1, 0,
                              ifelse(lsacwide$gre15a5 %in% 2:5, 1, NA)))
tab1(lsacwide$gDV)
lsacwide$hDV <- ifelse(lsacwide$hre15a5 < 0, NA, 
                       ifelse(lsacwide$hre15a5 == 1, 0,
                              ifelse(lsacwide$hre15a5 %in% 2:5, 1, NA)))
tab1(lsacwide$hDV)
lsacwide$iDV <- ifelse(lsacwide$ire15a5 < 0, NA, 
                       ifelse(lsacwide$ire15a5 == 1, 0,
                              ifelse(lsacwide$ire15a5 %in% 2:5, 1, NA)))
tab1(lsacwide$iDV)


lsacwide$anyDV <- ifelse(lsacwide$cDV != 1 | lsacwide$dDV != 1 | lsacwide$eDV != 1 | 
                           lsacwide$fDV != 1 | lsacwide$gDV != 1 | lsacwide$hDV != 1 | 
                           lsacwide$iDV != 1, 1, 0)

tab1(lsacwide$anyDV)

##### Couple arguments # Higher scores = greater frequency of arguments ####
tab1(lsacwide$caarga)
tab1(lsacwide$daarga)
tab1(lsacwide$eaarga)
tab1(lsacwide$faarga)
tab1(lsacwide$gaarga)
tab1(lsacwide$haarga)
tab1(lsacwide$iaarga)

##### Hendrick relationship quality scale ####
tab1(lsacwide$cahend)
tab1(lsacwide$eahend)
tab1(lsacwide$fahend)
tab1(lsacwide$gahend)
tab1(lsacwide$hahend)



#### Child mental health / behaviours ####
#### Alcohol ####
tab1(K18$jhb16c11, cum.percent = TRUE)
tab1(lsac2$jhb16c11, cum.percent = TRUE)
tab1(K18$jhb16c11a, cum.percent = TRUE)
tab1(K18$jhb16c13, cum.percent = TRUE)
tab1(lsac2$jhb16c13, cum.percent = TRUE)
tab1(K18$jhb16c9, cum.percent = TRUE)
tab1(K18$jhb16c10, cum.percent = TRUE)
mean(K18$jhb16c10, na.rm = TRUE)
tab1(lsac2$jhb16c10, cum.percent = TRUE)
mean(lsac2$jhb16c10, na.rm = TRUE)
tab1(K14$hhb16c11, cum.percent = TRUE)
tab1(lsac2$hhb16c11, cum.percent = TRUE)
tab1(K12$ghb16c11, cum.percent = TRUE)
tab1(lsac2$ghb16c11, cum.percent = TRUE)


tab1(lsacwide$jhb16c11, cum.percent = TRUE) # ever had alcoholic drink
tab1(lsacwide$jhb16c11a, cum.percent = TRUE) # ever had alcoholic drink, combined from waves 5-8
tab1(lsacwide$jhb16c13, cum.percent = TRUE) # had alcoholic drink in last 12 months
tab1(lsacwide$jhb16c9, cum.percent = TRUE) # had alcoholic drink in last 4 weeks
tab1(lsacwide$jhb16c10, cum.percent = TRUE) # number of alcoholic drinks in past 7 days
tab1(lsacwide$jhb16c12, cum.percent = TRUE) 
tab1(lsacwide$jhb16c12a, cum.percent = TRUE)  

# Compute past week alcohol use, handling skip logic
# Here, those who responded no to ever had alcoholic drink, past 12 month alcohol use or past 4 weeks alcohol use
# Recoded to 0 drinks in past 7 days rather than -9
lsacwide$Npastweekdrinks <- ifelse(lsacwide$jhb16c9 == 2, 0,
                             ifelse(lsacwide$jhb16c13 == 2, 0, 
                                    ifelse(lsacwide$jhb16c10 == -5, NA,
                                           ifelse(lsacwide$jhb16c10 == -3, NA, 
                                                  ifelse(lsacwide$jhb16c11a ==2,0, 
                                                         ifelse(lsacwide$jhb16c10 ==-9,NA, lsacwide$jhb16c10))))))

tab1(lsacwide$Npastweekdrinks, cum.percent = TRUE)

# From there can compute risky alcohol use as >10 per week

lsacwide$risky_alc <- ifelse(lsacwide$Npastweekdrinks %in% 0:10, 0,
                        ifelse(lsacwide$Npastweekdrinks %in% 11:43, 1, NA))

tab1(lsacwide$risky_alc, cum.percent = TRUE)

#### Early onset of alcohol use ####
# Compute early onset of alcohol use as lifetime more than a few sips at age 14/15
lsacwide$earlyalc <- ifelse(lsacwide$hhb16c11 %in% 3:4, 1,
                       ifelse(lsacwide$hhb16c11 ==-9, NA, 
                              ifelse(lsacwide$hhb16c11 == -3, NA, 0)))
tab1(lsacwide$earlyalc, cum.percent = TRUE)

#### Alcohol use in adolescence ####
tab1(lsacwide$hhb16c11, cum.percent = TRUE) # ever had alcoholic drink, age 14
tab1(lsacwide$ihb16c11, cum.percent = TRUE) # ever had alcoholic drink, age 16
tab1(lsacwide$ihb16c11a, cum.percent = TRUE) # ever had alcoholic drink, combined from waves 5-7 (age 16)
tab1(lsacwide$hhb16c13, cum.percent = TRUE) # had alcoholic drink in last 12 months, age 14
tab1(lsacwide$ihb16c13, cum.percent = TRUE) # had alcoholic drink in last 12 months, age 16
tab1(lsacwide$hhb16c9, cum.percent = TRUE) # had alcoholic drink in last 4 weeks, age 14
tab1(lsacwide$ihb16c9, cum.percent = TRUE) # had alcoholic drink in last 4 weeks, age 16
tab1(lsacwide$hhb16c10, cum.percent = TRUE) # number of alcoholic drinks in past 7 days, age 14
tab1(lsacwide$ihb16c10, cum.percent = TRUE) # number of alcoholic drinks in past 7 days, age 16

# Compute past week alcohol use at age 14, handling skip logic
# Here, those who responded no to ever had alcoholic drink, past 12 month alcohol use or past 4 weeks alcohol use
# Recoded to 0 drinks in past 7 days rather than -9
lsacwide$Npastweekdrinks14 <- ifelse(lsacwide$hhb16c9 == 2, 0,
                                   ifelse(lsacwide$hhb16c13 == 2, 0, 
                                          ifelse(lsacwide$hhb16c10 == -5, NA,
                                                 ifelse(lsacwide$hhb16c10 == -3, NA, 
                                                        ifelse(lsacwide$hhb16c11 ==1,0, 
                                                               ifelse(lsacwide$hhb16c10 ==-9,NA, lsacwide$hhb16c10))))))


tab1(lsacwide$Npastweekdrinks14, cum.percent = TRUE)

# Compute past week alcohol use at age 16, handling skip logic
# Here, those who responded no to ever had alcoholic drink, past 12 month alcohol use or past 4 weeks alcohol use
# Recoded to 0 drinks in past 7 days rather than -9
lsacwide$Npastweekdrinks16 <- ifelse(lsacwide$ihb16c9 == 2, 0,
                                     ifelse(lsacwide$ihb16c13 == 2, 0, 
                                            ifelse(lsacwide$ihb16c10 == -5, NA,
                                                   ifelse(lsacwide$ihb16c10 == -3, NA, 
                                                          ifelse(lsacwide$ihb16c11a ==2,0, 
                                                                 ifelse(lsacwide$ihb16c10 ==-9,NA, lsacwide$ihb16c10))))))


tab1(lsacwide$Npastweekdrinks16, cum.percent = TRUE)

# Compute weekly drinking at age 14
lsacwide$weeklydrink14 <- ifelse(lsacwide$Npastweekdrinks14 > 0, 1,
                                     ifelse(lsacwide$Npastweekdrinks14 == 0, 0, NA))
                                           
tab1(lsacwide$weeklydrink14, cum.percent = TRUE)

# Compute weekly drinking at age 16
lsacwide$weeklydrink16 <- ifelse(lsacwide$Npastweekdrinks16 > 0, 1,
                                 ifelse(lsacwide$Npastweekdrinks16 == 0, 0, NA))

tab1(lsacwide$weeklydrink16, cum.percent = TRUE)

#### Other drug use ####
# Lifetime
# hb26c1 - cannabis
# hb28c1	- other drugs
# 1 = yes; 2 = no
tab1(lsacwide$ihb26c1, cum.percent = TRUE) # ever had cannabis, answered at 16
tab1(lsacwide$ihb26c1a, cum.percent = TRUE) # ever had cannabis, across waves 5-7
tab1(lsacwide$ihb28c1, cum.percent = TRUE) # ever had other drugs, answered at 16
tab1(lsacwide$ihb28c1a, cum.percent = TRUE) # ever had other drugs, across wave 5-7

# Compute lifetime use of cannabis with correct NA
lsacwide$cannabis <- ifelse(lsacwide$ihb26c1a ==1, 1,
                           ifelse(lsacwide$ihb26c1a == -9, NA, 
                                  ifelse(lsacwide$ihb26c1a == -3, NA, 0)))
tab1(lsacwide$cannabis, cum.percent = TRUE)
# Compute lifetime use of other drugs with correct NA
lsacwide$otherdrugs <- ifelse(lsacwide$ihb28c1a ==1, 1,
                            ifelse(lsacwide$ihb28c1a == -9, NA, 
                                   ifelse(lsacwide$ihb28c1a == -3, NA, 0)))

tab1(lsacwide$otherdrugs, cum.percent = TRUE)
# Compute lifetime use of other drugs (inc. cannabis) at age 16
lsacwide$druguse <- ifelse(lsacwide$ihb26c1a ==1 | lsacwide$ihb28c1a ==1, 1,
                        ifelse(lsacwide$ihb26c1a == -9 & lsacwide$ihb28c1 == -9, NA, 
                               ifelse(lsacwide$ihb26c1a == -3 & lsacwide$ihb28c1 == -3, NA, 0)))

tab1(lsacwide$druguse, cum.percent = TRUE)

lsacwide$druguse <- ifelse(lsacwide$cannabis ==1 | lsacwide$otherdrugs ==1, 1, 
                           ifelse(lsacwide$cannabis ==0 & lsacwide$otherdrugs ==0, 0, NA))
#### Current drug use ####
# Current cannabis use; 1 yes; 2 no
tab1(lsacwide$ihb26c4, cum.percent = TRUE) # Past month cannabis use
tab1(lsacwide$ihb26c3, cum.percent = TRUE)  # Past year cannabis use
# Compute past moonth cannabis use at age 16, handling skip logic
# Here, those who responded no to ever had cannabis or no to past 12 month cannabis use
# Recoded to 0 cannabis use in past month rather than -9
lsacwide$pastmonthcannabis <- ifelse(lsacwide$ihb26c1a == 2, 0,
                                     ifelse(lsacwide$ihb26c3 == 2, 0, 
                                            ifelse(lsacwide$ihb26c4 == -5, NA,
                                                   ifelse(lsacwide$ihb26c4 == -3, NA, 
                                                          ifelse(lsacwide$ihb26c4 ==2,0, 
                                                                 ifelse(lsacwide$ihb26c4 ==-9, NA, lsacwide$ihb26c4))))))

tab1(lsacwide$pastmonthcannabis, cum.percent = TRUE)

# Current other drug use; 1 yes; 2 no
tab1(lsacwide$ihb28c5, cum.percent = TRUE) # Past month other drug use
tab1(lsacwide$ihb28c2a, cum.percent = TRUE)  # Past 2 year other drug use
tab1(lsacwide$ihb28c4, cum.percent = TRUE) # Past year other drug use
tab1(lsacwide$ihb28c1, cum.percent = TRUE) # Ever had other drugs
tab1(lsacwide$ihb28c1a, cum.percent = TRUE) # Ever had other drugs, across waves 5-7

# Compute past month other drug use at age 16, handling skip logic
# Here, those who responded no to ever had other drugs, past 2 year drug use, past 12 month other drug use
# Recoded to 0 other drug use in past month rather than -9
lsacwide$pastmonthotherdrugs <- ifelse(lsacwide$ihb28c1a == 2, 0,
                                     ifelse(lsacwide$ihb28c2a == 2, 0, 
                                            ifelse(lsacwide$ihb28c4 == 2, 0,
                                            ifelse(lsacwide$ihb28c5 == 2, 0, 
                                                   ifelse(lsacwide$ihb28c5 ==-9, NA, lsacwide$ihb28c5)))))

tab1(lsacwide$pastmonthotherdrugs, cum.percent = TRUE)

# Combine cannabis and other drugs into one variable for any past month drug use at 16
lsacwide$currentdruguse <- ifelse(lsacwide$pastmonthcannabis ==1 | lsacwide$pastmonthotherdrugs ==1, 1, 
                           ifelse(lsacwide$pastmonthcannabis ==0 & lsacwide$pastmonthotherdrugs ==0, 0, NA))

tab1(lsacwide$currentdruguse, cum.percent = TRUE)

#### ADHD ####
# hs17l 0 = no; 1 = yes
tab1(lsacwide$chs17l, cum.percent = TRUE) # age 4/5
tab1(lsacwide$dhs17l, cum.percent = TRUE) # age 6/7
tab1(lsacwide$ehs17l, cum.percent = TRUE) # age 8
tab1(lsacwide$fhs17l, cum.percent = TRUE) # age 10
tab1(lsacwide$ghs17l, cum.percent = TRUE) # age 12
tab1(lsacwide$hhs17l, cum.percent = TRUE) # age 14
tab1(lsacwide$ihs17l, cum.percent = TRUE) # age 16

lsacwide$lifetimeADHD <- ifelse(lsacwide$chs17l ==1 | lsacwide$dhs17l ==1 | lsacwide$ehs17l ==1 | lsacwide$fhs17l ==1
                                | lsacwide$ghs17l ==1 | lsacwide$hhs17l ==1 | lsacwide$ihs17l ==1, 1, 
                                ifelse(lsacwide$chs17l ==0 & lsacwide$dhs17l ==0 & lsacwide$ehs17l ==0 & lsacwide$fhs17l ==0
                                       & lsacwide$ghs17l ==0 & lsacwide$hhs17l ==0 & lsacwide$ihs17l ==0, 0, NA))

tab1(lsacwide$lifetimeADHD, cum.percent = TRUE)
# Number of ADHD timepoints
# But waves g-i have -9s, need to recode to NAs
lsacwide <- lsacwide %>%
  mutate(gADHD = across(ghs17l, ~ na_if(., -9)))

tab1(lsacwide$gADHD, cum.percent = TRUE)

lsacwide <- lsacwide %>%
  mutate(hADHD = across(hhs17l, ~ na_if(., -9)))

tab1(lsacwide$hADHD, cum.percent = TRUE)

lsacwide <- lsacwide %>%
  mutate(iADHD = across(ihs17l, ~ na_if(., -9)))

tab1(lsacwide$iADHD, cum.percent = TRUE)

lsacwide$ADHDwaves <- rowSums(lsacwide[c("chs17l", "dhs17l", "ehs17l", "fhs17l", "gADHD", "hADHD", "iADHD")], na.rm = TRUE)
tab1(lsacwide$ADHDwaves, cum.percent = TRUE)

#### Conduct problems ####
# Use lsac original variable of ccondb
# But change -9 to missing
lsacwide$fconduct_tot <- ifelse(lsacwide$fccondb == -9, NA, lsacwide$fccondb)
tab1(lsacwide$fconduct_tot, cum.percent = TRUE)

#Compute elevated conduct problems at 10
lsacwide$fconduct_high <- ifelse(lsacwide$fconduct_tot >=5, 1,
                                 ifelse(lsacwide$fconduct_tot < 5, 0, NA))
tab1(lsacwide$fconduct_high, cum.percent = TRUE)

# Age 12
# Use lsac original variable of ccondb
# But change -9 to missing
tab1(lsacwide$gccondb, cum.percent = TRUE) 
# No -9
#Compute elevated conduct problems at 12
lsacwide$gconduct_high <- ifelse(lsacwide$gccondb >=5, 1,
                                 ifelse(lsacwide$gccondb < 5, 0, NA))
tab1(lsacwide$gconduct_high, cum.percent = TRUE)

# Age 14
# Use lsac original variable of ccondb
# But change -9 to missing
tab1(lsacwide$hccondb, cum.percent = TRUE) 
# No -9
#Compute elevated conduct problems at 14
lsacwide$hconduct_high <- ifelse(lsacwide$hccondb >=5, 1,
                                 ifelse(lsacwide$hccondb < 5, 0, NA))
tab1(lsacwide$hconduct_high, cum.percent = TRUE)

# Age 16
# Use lsac original variable of ccondb
# But change -9 to missing
tab1(lsacwide$iccondb, cum.percent = TRUE) 
# No -9
#Compute elevated conduct problems at 16
lsacwide$iconduct_high <- ifelse(lsacwide$iccondb >=5, 1,
                                 ifelse(lsacwide$iccondb < 5, 0, NA))
tab1(lsacwide$iconduct_high, cum.percent = TRUE)

# Compute conduct problems across adolescence
# Create a new variable indicating whether participant has available data on 3 out of 4 variables
lsacwide$available_data <- rowSums(!is.na(lsacwide[c("fconduct_high", "gconduct_high", "hconduct_high", "iconduct_high")])) >= 3

# Calculate f_conduct_total only for participants with available data on 3 out of 4 variables
lsacwide$nwaves_conductprobs3 <- ifelse(lsacwide$available_data,
                                   rowSums(lsacwide[c("fconduct_high", "gconduct_high", "hconduct_high", "iconduct_high")], na.rm = TRUE),
                                   NA)

tab1(lsacwide$nwaves_conductprobs3, cum.percent = TRUE)

# Compute 2 or more waves of problems
lsacwide$conductprobs <- ifelse(lsacwide$nwaves_conductprobs3 >=2, 1,
                                 ifelse(lsacwide$nwaves_conductprobs3 < 2, 0, NA))
tab1(lsacwide$conductprobs, cum.percent = TRUE)
# Compute 1 or more waves of problems
lsacwide$conductprobs_1wave <- ifelse(lsacwide$nwaves_conductprobs3 >=1, 1,
                                ifelse(lsacwide$nwaves_conductprobs3 < 1, 0, NA))
tab1(lsacwide$conductprobs_1wave, cum.percent = TRUE)

#### Hyperactivity ####
# 7-10 considered abnormal
# chypr
tab1(lsacwide$fchypr, cum.percent = TRUE)
tab1(lsacwide$gchypr, cum.percent = TRUE)
tab1(lsacwide$hchypr, cum.percent = TRUE)
tab1(lsacwide$ichypr, cum.percent = TRUE)

# Use lsac original variable of chypr
# But change -9 to missing
lsacwide$fhyper_tot <- ifelse(lsacwide$fchypr == -9, NA, lsacwide$fchypr)
tab1(lsacwide$fhyper_tot, cum.percent = TRUE)

#Compute elevated hyper problems at 10
lsacwide$fhyper_high <- ifelse(lsacwide$fhyper_tot >=7, 1,
                                 ifelse(lsacwide$fhyper_tot < 7, 0, NA))
tab1(lsacwide$fhyper_high, cum.percent = TRUE)

#Compute elevated hyper problems at 12
lsacwide$ghyper_high <- ifelse(lsacwide$gchypr >=7, 1,
                               ifelse(lsacwide$gchypr < 7, 0, NA))
tab1(lsacwide$ghyper_high, cum.percent = TRUE)

#Compute elevated hyper problems at 14
lsacwide$hhyper_high <- ifelse(lsacwide$hchypr >=7, 1,
                               ifelse(lsacwide$hchypr < 7, 0, NA))
tab1(lsacwide$hhyper_high, cum.percent = TRUE)

#Compute elevated hyper problems at 16
lsacwide$ihyper_high <- ifelse(lsacwide$ichypr >=7, 1,
                               ifelse(lsacwide$ichypr < 7, 0, NA))
tab1(lsacwide$ihyper_high, cum.percent = TRUE)

# Create a new variable indicating whether participant has available data on 3 out of 4 variables
lsacwide$available_hyper <- rowSums(!is.na(lsacwide[c("fhyper_high", "ghyper_high", "hhyper_high", "ihyper_high")])) >= 3

# Calculate f_conduct_total only for participants with available data on 3 out of 4 variables
lsacwide$nwaves_hyperprobs <- ifelse(lsacwide$available_hyper,
                                        rowSums(lsacwide[c("fhyper_high", "ghyper_high", "hhyper_high", "ihyper_high")], na.rm = TRUE),
                                        NA)

tab1(lsacwide$nwaves_hyperprobs, cum.percent = TRUE)

# Compute 2 or more waves of problems
lsacwide$hyperprobs <- ifelse(lsacwide$nwaves_hyperprobs >=2, 1,
                                ifelse(lsacwide$nwaves_hyperprobs < 2, 0, NA))
tab1(lsacwide$hyperprobs, cum.percent = TRUE)
# Compute 1 or more waves of problems
lsacwide$hyperprobs_1wave <- ifelse(lsacwide$nwaves_hyperprobs >=1, 1,
                                      ifelse(lsacwide$nwaves_hyperprobs < 1, 0, NA))
tab1(lsacwide$hyperprobs_1wave, cum.percent = TRUE)

#### Delinquency ####
# Measured at Waves 5-8 (age 12 - 18)
# se20a1-19
se20a1
se20a2
se20a3
se20a4
se20a5
se20a6
se20a7
se20a8
se20a9
se20a10
se20a11
se20a12
se20a13
se20a14
se20a15
se20a16
se20a17
se20a18
se20a19

# We're interested in ages 12-16, so g, h, and i
tab1(lsacwide$gse20a1, cum.percent = TRUE)
tab1(lsacwide$hse20a1, cum.percent = TRUE)
tab1(lsacwide$ise20a1, cum.percent = TRUE)

# Compute delinquency total score
# we're making a new variable called delinquency from the sum of our items
# First need to handle negative (missing) values so they don't affect the sum
# Age 12
vars <- c("gse20a1",
          "gse20a2",
          "gse20a3",
          "gse20a4",
          "gse20a5",
          "gse20a6",
          "gse20a7",
          "gse20a8",
          "gse20a9",
          "gse20a10",
          "gse20a11",
          "gse20a12",
          "gse20a13",
          "gse20a14",
          "gse20a15",
          "gse20a16",
          "gse20a17")

lsacwide <- lsacwide %>%
  mutate_at(vars, ~ ifelse(. %in% c(-9, -3, -5), NA, .))

# lsacwide$delinq_12 <- rowSums(lsacwide[vars], na.rm = TRUE) # this gives everyone a total score even if they only answered one item

tab1(lsacwide$delinq_12, cum.percent = TRUE)

# First want to create a new variable indicating whether participant has available data on at least 15 out of 17 variables
lsacwide$available_delinq12 <- rowSums(!is.na(lsacwide[vars])) >= 15

# Calculate gdelinquency_tot only for participants with available data on at least 15 out of 17 variables
lsacwide$gdelinquency_tot <- ifelse(lsacwide$available_delinq12, 
                             rowSums(lsacwide[vars], na.rm = TRUE), 
                             NA)

tab1(lsacwide$gdelinquency_tot, cum.percent = TRUE)

# Age 14
vars_h <- c("hse20a1",
          "hse20a2",
          "hse20a3",
          "hse20a4",
          "hse20a5",
          "hse20a6",
          "hse20a7",
          "hse20a8",
          "hse20a9",
          "hse20a10",
          "hse20a11",
          "hse20a12",
          "hse20a13",
          "hse20a14",
          "hse20a15",
          "hse20a16",
          "hse20a17")

lsacwide <- lsacwide %>%
  mutate_at(vars_h, ~ ifelse(. %in% c(-9, -3, -5), NA, .))

# First want to create a new variable indicating whether participant has available data on at least 15 out of 17 variables
lsacwide$available_delinq14 <- rowSums(!is.na(lsacwide[vars_h])) >= 15

# Calculate gdelinquency_tot only for participants with available data on at least 15 out of 17 variables
lsacwide$hdelinquency_tot <- ifelse(lsacwide$available_delinq14, 
                                    rowSums(lsacwide[vars_h], na.rm = TRUE), 
                                    NA)

tab1(lsacwide$hdelinquency_tot, cum.percent = TRUE)

# Age 16
vars_i <- c("ise20a1",
          "ise20a2",
          "ise20a3",
          "ise20a4",
          "ise20a5",
          "ise20a6",
          "ise20a7",
          "ise20a8",
          "ise20a9",
          "ise20a10",
          "ise20a11",
          "ise20a12",
          "ise20a13",
          "ise20a14",
          "ise20a15",
          "ise20a16",
          "ise20a17")

lsacwide <- lsacwide %>%
  mutate_at(vars_i, ~ ifelse(. %in% c(-9, -3, -5), NA, .))

# First want to create a new variable indicating whether participant has available data on at least 15 out of 17 variables
lsacwide$available_delinq16 <- rowSums(!is.na(lsacwide[vars_i])) >= 15

# Calculate idelinquency_tot only for participants with available data on at least 15 out of 17 variables
lsacwide$idelinquency_tot <- ifelse(lsacwide$available_delinq16, 
                                    rowSums(lsacwide[vars_i], na.rm = TRUE), 
                                    NA)

tab1(lsacwide$idelinquency_tot, cum.percent = TRUE)

# Create maximum delinquency score across the 3 waves of adolescence
lsacwide$max_delinq <- pmax(lsacwide$gdelinquency_tot, lsacwide$hdelinquency_tot, lsacwide$idelinquency_tot)
tab1(lsacwide$max_delinq)

# Create a new variable indicating whether participant has available data on 2 out of 3 variables
lsacwide$available_delinq <- rowSums(!is.na(lsacwide[c("gdelinquency_tot", "hdelinquency_tot", "idelinquency_tot")])) >= 2

# Calculate max delinquency for participants with available data on 2 out of 3 variables
lsacwide$max_delinquency <- ifelse(lsacwide$available_delinq,
                                   pmax(lsacwide$gdelinquency_tot, lsacwide$hdelinquency_tot, lsacwide$idelinquency_tot, na.rm = TRUE),
                                   NA)

tab1(lsacwide$max_delinquency)


#### Emotional problems ####
# cemot 7-10 considered abnormal
# Self-report from age 10
tab1(lsacwide$fcemot, cum.percent = TRUE)
tab1(lsacwide$gcemot, cum.percent = TRUE)
tab1(lsacwide$hcemot, cum.percent = TRUE)
tab1(lsacwide$icemot, cum.percent = TRUE)

# But change -9 to missing
lsacwide$femot_tot <- ifelse(lsacwide$fcemot == -9, NA, lsacwide$fcemot)
tab1(lsacwide$femot_tot, cum.percent = TRUE)

#Compute elevated int problems at 10
lsacwide$femot_high <- ifelse(lsacwide$femot_tot >=7, 1,
                               ifelse(lsacwide$femot_tot < 7, 0, NA))
tab1(lsacwide$femot_high, cum.percent = TRUE)

#Compute elevated int problems at 12
lsacwide$gemot_high <- ifelse(lsacwide$gcemot >=7, 1,
                               ifelse(lsacwide$gcemot < 7, 0, NA))
tab1(lsacwide$gemot_high, cum.percent = TRUE)

#Compute elevated int problems at 14
lsacwide$hemot_high <- ifelse(lsacwide$hcemot >=7, 1,
                               ifelse(lsacwide$hcemot < 7, 0, NA))
tab1(lsacwide$hemot_high, cum.percent = TRUE)

#Compute elevated int problems at 16
lsacwide$iemot_high <- ifelse(lsacwide$icemot >=7, 1,
                               ifelse(lsacwide$icemot < 7, 0, NA))
tab1(lsacwide$iemot_high, cum.percent = TRUE)

# Create a new variable indicating whether participant has available data on 3 out of 4 variables
lsacwide$available_emot <- rowSums(!is.na(lsacwide[c("femot_high", "gemot_high", "hemot_high", "iemot_high")])) >= 3

# Calculate emot total only for participants with available data on 3 out of 4 variables
lsacwide$nwaves_emotprobs <- ifelse(lsacwide$available_emot,
                                     rowSums(lsacwide[c("femot_high", "gemot_high", "hemot_high", "iemot_high")], na.rm = TRUE),
                                     NA)

tab1(lsacwide$nwaves_emotprobs, cum.percent = TRUE)

# Compute 2 or more waves of problems
lsacwide$emotprobs <- ifelse(lsacwide$nwaves_emotprobs >=2, 1,
                              ifelse(lsacwide$nwaves_emotprobs < 2, 0, NA))
tab1(lsacwide$emotprobs, cum.percent = TRUE)
# Compute 1 or more waves of problems
lsacwide$emotprobs_1wave <- ifelse(lsacwide$nwaves_emotprobs >=1, 1,
                                    ifelse(lsacwide$nwaves_emotprobs < 1, 0, NA))
tab1(lsacwide$emotprobs_1wave, cum.percent = TRUE)

#### Personality ####
tab1(lsacwide$icneuro, cum.percent = TRUE) 
tab1(lsacwide$icopen, cum.percent = TRUE) 
tab1(lsacwide$icagree)
tab1(lsacwide$icconsc)

#### Sleep ####
# 1 Plenty; 2 Just enough; 3 Not quite enough; 4 Not nearly enough
tab1(lsacwide$fhs20c3)
tab1(lsacwide$ghs20c3)
tab1(lsacwide$hhs20c3)
tab1(lsacwide$ihs20c3)

# Recode, 0 is now not getting enough sleep, 1 is getting enough
lsacwide <- lsacwide %>%
  mutate(across(c(fhs20c3, ghs20c3, hhs20c3, ihs20c3), 
                ~ ifelse(. %in% c(-9, -3), NA, ifelse(. %in% c(3, 4), 0, ifelse(. %in% c(1, 2), 1, .))),
                .names = "{.col}_new"))

tab1(lsacwide$fhs20c3_new)
tab1(lsacwide$ghs20c3_new)
tab1(lsacwide$hhs20c3_new)
tab1(lsacwide$ihs20c3_new)

# Compute variable for the number of waves of getting enough sleep
lsacwide$waves_sleep <- lsacwide$fhs20c3_new + lsacwide$ghs20c3_new + lsacwide$hhs20c3_new + 
  lsacwide$ihs20c3_new
tab1(lsacwide$waves_sleep)

# Create a new variable indicating whether participant has available data on 3 out of 4 variables
lsacwide$available_sleep <- rowSums(!is.na(lsacwide[c("fhs20c3_new", "ghs20c3_new", "hhs20c3_new", "ihs20c3_new")])) >= 3

# Calculate total only for participants with available data on 3 out of 4 variables
lsacwide$nwaves_enoughsleep <- ifelse(lsacwide$available_sleep,
                                      rowSums(lsacwide[c("fhs20c3_new", "ghs20c3_new", "hhs20c3_new", "ihs20c3_new")], na.rm = TRUE),
                                      NA)

tab1(lsacwide$nwaves_enoughsleep)
# Compute variable representing multiple waves of not enough sleep
lsacwide$insufficient_sleep <- ifelse(lsacwide$nwaves_enoughsleep >=3, 0,
                                      ifelse(lsacwide$nwaves_enoughsleep < 3, 1, NA))

tab1(lsacwide$insufficient_sleep)

# Also code continuous variables at each wave
# Recode to change -9s and -3s into NAs
lsacwide <- lsacwide %>%
  mutate(across(c(fhs20c3, ghs20c3, hhs20c3, ihs20c3), 
                ~ ifelse(. %in% c(-9, -3), NA, .),
                .names = "{.col}_NA"))

tab1(lsacwide$fhs20c3_NA)
tab1(lsacwide$ghs20c3_NA)
tab1(lsacwide$hhs20c3_NA)
tab1(lsacwide$ihs20c3_NA)


#### Are you active in a religious group 1 yes; 2 no ####
tab1(lsacwide$hfd13c2)
lsacwide$religion <- ifelse(lsacwide$hfd13c2 <0, NA, 
                            ifelse(lsacwide$hfd13c2 == 2, 0, lsacwide$hfd13c2))
tab1(lsacwide$religion)

#### Child sleep problems ####
tab1(lsacwide$chs20b)
tab1(lsacwide$dhs20b)
tab1(lsacwide$ehs20b)
tab1(lsacwide$fhs20b)
tab1(lsacwide$ghs20b)
tab1(lsacwide$hhs20b)

lsacwide$c_sleepprobs <- ifelse(lsacwide$chs20b < 0, NA, 
                                ifelse(lsacwide$chs20b == 2, 0, lsacwide$chs20b))
tab1(lsacwide$c_sleepprobs)

lsacwide$d_sleepprobs <- ifelse(lsacwide$dhs20b < 0, NA, 
                                ifelse(lsacwide$dhs20b == 2, 0, lsacwide$dhs20b))

lsacwide$e_sleepprobs <- ifelse(lsacwide$ehs20b < 0, NA, 
                                ifelse(lsacwide$ehs20b == 2, 0, lsacwide$ehs20b))

lsacwide$f_sleepprobs <- ifelse(lsacwide$fhs20b < 0, NA, 
                                ifelse(lsacwide$fhs20b == 2, 0, lsacwide$fhs20b))

lsacwide$g_sleepprobs <- ifelse(lsacwide$ghs20b < 0, NA, 
                                ifelse(lsacwide$ghs20b == 2, 0, lsacwide$ghs20b))

lsacwide$h_sleepprobs <- ifelse(lsacwide$hhs20b < 0, NA, 
                                ifelse(lsacwide$hhs20b == 2, 0, lsacwide$hhs20b))
tab1(lsacwide$h_sleepprobs)


#### Social support ####
# No one to talk to if had a problem
tab1(lsacwide$fsc21c1h)
tab1(lsacwide$gsc21c1h)
lsacwide$f_noone <- ifelse(lsacwide$fsc21c1h < 0, NA, lsacwide$fsc21c1h)
lsacwide$g_noone <- ifelse(lsacwide$gsc21c1h < 0, NA, lsacwide$gsc21c1h)
tab1(lsacwide$f_noone)
tab1(lsacwide$g_noone)


#### Short mood and feelings questionnaire ####
tab1(lsacwide$gsmfq)
tab1(lsacwide$hsmfq)
tab1(lsacwide$ismfq)

#### Spence anxiety scale ####
tab1(lsacwide$gspenceanx)
tab1(lsacwide$hspenceanx)
tab1(lsacwide$ispenceanx)

#### Suicide ####
# 14/15 - ACASK 31.4.1 - Have you thought about hurting yourself on purpose
# 14/15 - ACASK 31.4.2 - Have you hurt yourself on purpose in any way
# 14/15 - ACASK 31.5.1 - Consider attempting suicide
# 14/15 - ACASK 31.5.2 - Make a plan about how you would attempt suicide
tab1(lsacwide$hhs54a) # Self-harm ideation
tab1(lsacwide$hhs54b) # Self-harm
tab1(lsacwide$hhs54c) # Suicide ideation
tab1(lsacwide$hhs54d) # Suicide plan
tab1(lsacwide$hhs54e) # Number of times attempted suicide in past 12 months
# 0 0 times; 1 1 time; 2 2 or 3 times; 3 4 or 5 times; 4 6 or more times
lsacwide$h_selfharm_idea <- ifelse(lsacwide$hhs54a < 0, NA, 
                                   ifelse(lsacwide$hhs54a == 2, 0, lsacwide$hhs54a))

lsacwide$h_selfharm <- ifelse(lsacwide$hhs54b < 0, NA, 
                              ifelse(lsacwide$hhs54b == 2, 0, lsacwide$hhs54b))

lsacwide$h_suicide_idea <- ifelse(lsacwide$hhs54c < 0, NA, 
                                  ifelse(lsacwide$hhs54c == 2, 0, lsacwide$hhs54c))

lsacwide$h_suicide_plan <- ifelse(lsacwide$hhs54d < 0, NA, 
                                  ifelse(lsacwide$hhs54d == 2, 0, lsacwide$hhs54d))

lsacwide$h_suicide_attempt <- ifelse(lsacwide$hhs54e > 0, 1, 
                                     ifelse(lsacwide$hhs54e <0, NA, lsacwide$hhs54e))


tab1(lsacwide$ihs54a)
tab1(lsacwide$ihs54b)
tab1(lsacwide$ihs54c)
tab1(lsacwide$ihs54d)
tab1(lsacwide$ihs54e)

lsacwide$i_selfharm_idea <- ifelse(lsacwide$ihs54a < 0, NA, 
                                   ifelse(lsacwide$ihs54a == 2, 0, lsacwide$ihs54a))

lsacwide$i_selfharm <- ifelse(lsacwide$ihs54b < 0, NA, 
                              ifelse(lsacwide$ihs54b == 2, 0, lsacwide$ihs54b))

lsacwide$i_suicide_idea <- ifelse(lsacwide$ihs54c < 0, NA, 
                                  ifelse(lsacwide$ihs54c == 2, 0, lsacwide$ihs54c))

lsacwide$i_suicide_plan <- ifelse(lsacwide$ihs54d < 0, NA, 
                                  ifelse(lsacwide$ihs54d == 2, 0, lsacwide$ihs54d))

lsacwide$i_suicide_attempt <- ifelse(lsacwide$ihs54e > 0, 1, 
                                     ifelse(lsacwide$ihs54e <0, NA, lsacwide$ihs54e))

tab1(lsacwide$i_selfharm_idea)
tab1(lsacwide$i_selfharm)
tab1(lsacwide$i_suicide_idea)
tab1(lsacwide$i_suicide_plan)
tab1(lsacwide$i_suicide_attempt)


#### Overall happiness ####
# In general, I am happy with how things are for me in my life right now
# 1 Strongly disagree; 2 Disagree; 3 Neither agree nor disagree; 4 Agree; 5 Strongly agree
tab1(lsacwide$gse21b1)
tab1(lsacwide$hse21b1)
tab1(lsacwide$ise21b1)
lsacwide$g_happy <- ifelse(lsacwide$gse21b1 < 0, NA, lsacwide$gse21b1)
lsacwide$h_happy <- ifelse(lsacwide$hse21b1 < 0, NA, lsacwide$hse21b1)
lsacwide$i_happy <- ifelse(lsacwide$ise21b1 < 0, NA, lsacwide$ise21b1)
tab1(lsacwide$g_happy)
tab1(lsacwide$h_happy)
tab1(lsacwide$i_happy)



#### School ####
#### School belonging ####
# 12 items, scored 1 Not at all true;2 Not very true;3 Neither not at all true nor completely true; 4 Somewhat true;5 Completely true
tab1(lsacwide$gpssm, cum.percent = TRUE) # Sum of gpc58h1--gpch12, with h2, h5, h7 and h11 reverse coded
tab1(lsacwide$hpssm, cum.percent = TRUE)
tab1(lsacwide$ipssm, cum.percent = TRUE)

# Compute max score across all timepoints
# Create a new variable indicating whether participant has available data on at least 1 out of 3 variables
lsacwide$available_pssm <- rowSums(!is.na(lsacwide[c("gpssm", "hpssm", "ipssm")])) >= 1

# Calculate max pssm for participants with available data on 2 out of 3 variables
lsacwide$max_pssm <- ifelse(lsacwide$available_pssm,
                                  pmax(lsacwide$gpssm, lsacwide$hpssm, lsacwide$ipssm, na.rm = TRUE),
                                  NA)

tab1(lsacwide$max_pssm)
lsacwide$max_pssm1 <- ifelse(lsacwide$max_pssm == 2, NA, lsacwide$max_pssm)
tab1(lsacwide$max_pssm1)
# hpssm has a value of 2 - change to NA
lsacwide$hpssm1 <- ifelse(lsacwide$hpssm == 2, NA, lsacwide$hpssm)
tab1(lsacwide$hpssm1)

#### Overall school achievement ####
tab1(lsacwide$dlc08a3a) # How would you describe study child’s overall achievement at school?
# 1 Excellent; 2 Above average; 3 Average; 4 Below average; 5 Well below average
tab1(lsacwide$elc08a3a)
tab1(lsacwide$flc08a3a)
tab1(lsacwide$glc08a3a)
tab1(lsacwide$hlc08a3a)
# Recode so that higher scores indicate greater achievement, and -9s and -2s are missing
lsacwide$flc08a3aR <- ifelse(lsacwide$flc08a3a == 1, 5,
                             ifelse(lsacwide$flc08a3a == 2,4,
                                    ifelse(lsacwide$flc08a3a == 4, 2, 
                                           ifelse(lsacwide$flc08a3a == 5, 1, 
                                                  ifelse(lsacwide$flc08a3a == -9, NA,
                                                         ifelse(lsacwide$flc08a3a == -2, NA, lsacwide$flc08a3a))))))

tab1(lsacwide$flc08a3aR)

lsacwide$glc08a3aR <- ifelse(lsacwide$glc08a3a == 1, 5,
                             ifelse(lsacwide$glc08a3a == 2,4,
                                    ifelse(lsacwide$glc08a3a == 4, 2, 
                                           ifelse(lsacwide$glc08a3a == 5, 1, 
                                                  ifelse(lsacwide$glc08a3a == -9, NA,
                                                         ifelse(lsacwide$glc08a3a == -2, NA, lsacwide$glc08a3a))))))

tab1(lsacwide$glc08a3aR)

lsacwide$hlc08a3aR <- ifelse(lsacwide$hlc08a3a == 1, 5,
                             ifelse(lsacwide$hlc08a3a == 2,4,
                                    ifelse(lsacwide$hlc08a3a == 4, 2, 
                                           ifelse(lsacwide$hlc08a3a == 5, 1, 
                                                  ifelse(lsacwide$hlc08a3a == -9, NA,
                                                         ifelse(lsacwide$hlc08a3a == -2, NA, lsacwide$hlc08a3a))))))

tab1(lsacwide$hlc08a3aR)

# Create a new variable indicating whether participant has available data on at least 2 out of 3 variables
lsacwide$available_school_achievementR <- rowSums(!is.na(lsacwide[c("flc08a3aR", "glc08a3aR", "hlc08a3aR")])) >= 2

# Calculate max school achievement for participants with available data on 2 out of 3 variables
lsacwide$max_school_achievement <- ifelse(lsacwide$available_school_achievementR,
                            pmax(lsacwide$flc08a3aR, lsacwide$glc08a3aR, lsacwide$hlc08a3aR, na.rm = TRUE),
                            NA)
tab1(lsacwide$max_school_achievement)


#### Academic pressure ####
tab1(lsacwide$ipa29c2, cum.percent = TRUE)
lsacwide$academic_pressure <- ifelse(lsacwide$ipa29c2 %in% 1:2, 1,
                       ifelse(lsacwide$ipa29c2 == -9 | lsacwide$ipa29c2 == -3, NA, 0))

tab1(lsacwide$academic_pressure, cum.percent = TRUE)

##### Number of schools attended ####
tab1(lsacwide$hpc44a2)
lsacwide$n_schools <- ifelse(lsacwide$hpc44a2 < 0, NA, lsacwide$hpc44a2)
tab1(lsacwide$n_schools)


#### Parent interest in education ####
tab1(lsacwide$hhe38c1) 
tab1(lsacwide$hhe38c2) 
lsacwide$h_mum_interest_ed <- ifelse(lsacwide$hhe38c1 < 0, NA, lsacwide$hhe38c1)
lsacwide$h_dad_interest_ed <- ifelse(lsacwide$hhe38c2 < 0, NA, lsacwide$hhe38c2)
tab1(lsacwide$h_mum_interest_ed) 
tab1(lsacwide$h_dad_interest_ed) 
tab1(lsacwide$ghe38c1) 
tab1(lsacwide$ghe38c2) 
lsacwide$g_mum_interest_ed <- ifelse(lsacwide$ghe38c1 < 0, NA, lsacwide$ghe38c1)
lsacwide$g_dad_interest_ed <- ifelse(lsacwide$ghe38c2 < 0, NA, lsacwide$ghe38c2)
tab1(lsacwide$g_mum_interest_ed) 
tab1(lsacwide$g_dad_interest_ed) 

# Reduce sparsity in 4 by combining with 3
lsacwide$h_mum_interest_ed <- ifelse(lsacwide$hhe38c1 < 0, NA, 
                         ifelse(lsacwide$hhe38c1 %in% 3:4, 3, lsacwide$hhe38c1))

lsacwide$g_mum_interest_ed <- ifelse(lsacwide$ghe38c1 < 0, NA, 
                                     ifelse(lsacwide$ghe38c1 %in% 3:4, 3, lsacwide$ghe38c1))

#### Peers ####
#### Antisocial peers ####
# peermoral, coded such that higher scores indicate more moral peers
tab1(lsacwide$ipeermoral)
tab1(lsacwide$hpeermoral)
tab1(lsacwide$gpeermoral)


#### Bullying ####
# Victimisation
# Age 14
# Coded as 1 yes; 2 no
tab1(lsacwide$hre22a, cum.percent = TRUE) 
tab1(lsacwide$hre22b, cum.percent = TRUE) 
tab1(lsacwide$hre22c, cum.percent = TRUE) 
tab1(lsacwide$hre22d, cum.percent = TRUE) 
tab1(lsacwide$hre22e, cum.percent = TRUE) 

hre22f
hre22g
hre22h
hre22i
hre22j
hre22k
# Recode so that -9s and -3s are NAs, and that 2s (which was "No") are now 0
lsacwide <- lsacwide %>%
  mutate(across(c(hre22a, hre22b, hre22c, hre22d, hre22e, hre22f, hre22g, hre22h, hre22i, hre22j, hre22k), 
                ~ ifelse(. %in% c(-9, -3), NA, 
                                ifelse(. == 2, 0, .)), 
                .names = "new_{.col}"))
# New variables are now coded 0 - no bullying, 1 bullying
tab1(lsacwide$new_hre22a, cum.percent = TRUE) 
tab1(lsacwide$new_hre22b, cum.percent = TRUE) 
tab1(lsacwide$new_hre22c, cum.percent = TRUE) 
tab1(lsacwide$new_hre22d, cum.percent = TRUE) 
tab1(lsacwide$new_hre22e, cum.percent = TRUE) 
# Now we can sum them to get a total victimisation score at 14
lsacwide$bullyvictim14_tot <- lsacwide$new_hre22a + lsacwide$new_hre22b + lsacwide$new_hre22c + lsacwide$new_hre22d + lsacwide$new_hre22e + lsacwide$new_hre22f + lsacwide$new_hre22g + lsacwide$new_hre22h + lsacwide$new_hre22i + lsacwide$new_hre22j + lsacwide$new_hre22k
tab1(lsacwide$bullyvictim14_tot, cum.percent = TRUE)

# Age 16
# Coded as 1 yes; 2 no
ire22a
ire22b
ire22c
ire22e
ire22f
ire22g
ire22x
ire22y
ire22z
tab1(lsacwide$ire22a, cum.percent = TRUE) 
tab1(lsacwide$ire22b, cum.percent = TRUE) 
tab1(lsacwide$ire22c, cum.percent = TRUE) 
tab1(lsacwide$ire22e, cum.percent = TRUE) 
tab1(lsacwide$ire22f, cum.percent = TRUE) 
# Recode so that -9s and -3s are NAs, and that 2s (which was "No") are now 0
lsacwide <- lsacwide %>%
  mutate(across(c(ire22a,
                  ire22b,
                  ire22c,
                  ire22e,
                  ire22f,
                  ire22g,
                  ire22x,
                  ire22y,
                  ire22z), 
                ~ ifelse(. %in% c(-9, -3), NA, 
                         ifelse(. == 2, 0, .)), 
                .names = "new_{.col}"))
# New variables are now coded 0 - no bullying, 1 bullying
tab1(lsacwide$new_ire22a, cum.percent = TRUE) 
tab1(lsacwide$new_ire22b, cum.percent = TRUE) 
tab1(lsacwide$new_ire22c, cum.percent = TRUE) 
tab1(lsacwide$new_ire22e, cum.percent = TRUE) 
tab1(lsacwide$new_ire22f, cum.percent = TRUE) 
# Now we can sum them to get a total victimisation score at 16
lsacwide$bullyvictim16_tot <- lsacwide$new_ire22a + lsacwide$new_ire22b + lsacwide$new_ire22c + lsacwide$new_ire22e + lsacwide$new_ire22f + lsacwide$new_ire22g + lsacwide$new_ire22x + lsacwide$new_ire22y + lsacwide$new_ire22z
tab1(lsacwide$bullyvictim16_tot, cum.percent = TRUE)

# Perpetration
# Age 14
# Coded as 1 yes; 2 no
hre22l
hre22m
hre22n
hre22o
hre22p
hre22q
hre22r
hre22s
hre22t
hre22u
hre22v
tab1(lsacwide$hre22l, cum.percent = TRUE) 
tab1(lsacwide$hre22m, cum.percent = TRUE) 
tab1(lsacwide$hre22n, cum.percent = TRUE) 
tab1(lsacwide$hre22o, cum.percent = TRUE) 
tab1(lsacwide$hre22p, cum.percent = TRUE) 
# Recode so that -9s and -3s are NAs, and that 2s (which was "No") are now 0
lsacwide <- lsacwide %>%
  mutate(across(c(hre22l,
                  hre22m,
                  hre22n,
                  hre22o,
                  hre22p,
                  hre22q,
                  hre22r,
                  hre22s,
                  hre22t,
                  hre22u,
                  hre22v), 
                ~ ifelse(. %in% c(-9, -3), NA, 
                         ifelse(. == 2, 0, .)), 
                .names = "new_{.col}"))
# New variables are now coded 0 - no bullying, 1 bullying
tab1(lsacwide$new_hre22l, cum.percent = TRUE) 
tab1(lsacwide$new_hre22m, cum.percent = TRUE) 
tab1(lsacwide$new_hre22n, cum.percent = TRUE) 
tab1(lsacwide$new_hre22o, cum.percent = TRUE) 
tab1(lsacwide$new_hre22p, cum.percent = TRUE) 
# Now we can sum them to get a total perpetration score at 14
lsacwide$bullyperp14_tot <- lsacwide$new_hre22l + lsacwide$new_hre22m + lsacwide$new_hre22n + lsacwide$new_hre22o + lsacwide$new_hre22p + lsacwide$new_hre22q + lsacwide$new_hre22r + lsacwide$new_hre22s + lsacwide$new_hre22t + lsacwide$new_hre22u + lsacwide$new_hre22v
tab1(lsacwide$bullyperp14_tot, cum.percent = TRUE)
# Age 16
# Coded 1 yes; 2 no
ire22l
ire22m
ire22n
ire22p
ire22q
ire22r
ire22xa
ire22ya
ire22za
tab1(lsacwide$ire22l, cum.percent = TRUE) 
tab1(lsacwide$ire22m, cum.percent = TRUE) 
tab1(lsacwide$ire22n, cum.percent = TRUE) 
tab1(lsacwide$ire22p, cum.percent = TRUE) 
tab1(lsacwide$ire22q, cum.percent = TRUE) 
# Recode so that -9s and -3s are NAs, and that 2s (which was "No") are now 0
lsacwide <- lsacwide %>%
  mutate(across(c(ire22l,
                  ire22m,
                  ire22n,
                  ire22p,
                  ire22q,
                  ire22r,
                  ire22xa,
                  ire22ya,
                  ire22za), 
                ~ ifelse(. %in% c(-9, -3), NA, 
                         ifelse(. == 2, 0, .)), 
                .names = "new_{.col}"))
# New variables are now coded 0 - no bullying, 1 bullying
tab1(lsacwide$new_ire22l, cum.percent = TRUE) 
tab1(lsacwide$new_ire22m, cum.percent = TRUE) 
tab1(lsacwide$new_ire22n, cum.percent = TRUE) 
tab1(lsacwide$new_ire22p, cum.percent = TRUE) 
tab1(lsacwide$new_ire22q, cum.percent = TRUE) 
# Now we can sum them to get a total perpetration score at 16
lsacwide$bullyperp16_tot <- lsacwide$new_ire22l + lsacwide$new_ire22m + lsacwide$new_ire22n + lsacwide$new_ire22p + lsacwide$new_ire22q + lsacwide$new_ire22r + lsacwide$new_ire22xa + lsacwide$new_ire22ya + lsacwide$new_ire22za
tab1(lsacwide$bullyperp16_tot, cum.percent = TRUE)

#### Stressful life events hs27

#### Peer problems ####
# From SDQ peer problems scale, self-reported from age 10 - 16 (wave 4-7)
# 1 Not true; 2 Somewhat true; 3 Certainly true
tab1(lsacwide$fse03c5b)
tab1(lsacwide$gse03c5b)
tab1(lsacwide$hse03c5b)
tab1(lsacwide$ise03c5b)
# Or use cpeer
tab1(lsacwide$fcpeer)
tab1(lsacwide$gcpeer)
tab1(lsacwide$hcpeer)
tab1(lsacwide$icpeer)

# Recode -9s from f
# But change -9 to missing
lsacwide$f_cpeer <- ifelse(lsacwide$fcpeer == -9, NA, lsacwide$fcpeer)
tab1(lsacwide$f_cpeer, cum.percent = TRUE)

# 6-10 considered abnormal

#Compute elevated peer problems at 10
lsacwide$fcpeer_high <- ifelse(lsacwide$f_cpeer >=6, 1,
                              ifelse(lsacwide$f_cpeer < 6, 0, NA))
tab1(lsacwide$fcpeer_high, cum.percent = TRUE)

#Compute elevated peer problems at 12
lsacwide$gcpeer_high <- ifelse(lsacwide$gcpeer >=6, 1,
                              ifelse(lsacwide$gcpeer < 6, 0, NA))
tab1(lsacwide$gcpeer_high, cum.percent = TRUE)

#Compute elevated peer problems at 14
lsacwide$hcpeer_high <- ifelse(lsacwide$hcpeer >=6, 1,
                              ifelse(lsacwide$hcpeer < 6, 0, NA))
tab1(lsacwide$hcpeer_high, cum.percent = TRUE)

#Compute elevated peer problems at 16
lsacwide$icpeer_high <- ifelse(lsacwide$icpeer >=6, 1,
                              ifelse(lsacwide$icpeer < 6, 0, NA))
tab1(lsacwide$icpeer_high, cum.percent = TRUE)

# Create a new variable indicating whether participant has available data on 3 out of 4 variables
lsacwide$available_peer <- rowSums(!is.na(lsacwide[c("fcpeer_high", "gcpeer_high", "hcpeer_high", "icpeer_high")])) >= 3

# Calculate peer total only for participants with available data on 3 out of 4 variables
lsacwide$nwaves_peerprobs <- ifelse(lsacwide$available_peer,
                                    rowSums(lsacwide[c("fcpeer_high", "gcpeer_high", "hcpeer_high", "icpeer_high")], na.rm = TRUE),
                                    NA)

tab1(lsacwide$nwaves_peerprobs, cum.percent = TRUE)

# Compute 2 or more waves of problems
lsacwide$peerprobs <- ifelse(lsacwide$nwaves_peerprobs >=2, 1,
                             ifelse(lsacwide$nwaves_peerprobs < 2, 0, NA))
tab1(lsacwide$peerprobs, cum.percent = TRUE)
# Compute 1 or more waves of problems
lsacwide$peerprobs_1wave <- ifelse(lsacwide$nwaves_peerprobs >=1, 1,
                                   ifelse(lsacwide$nwaves_peerprobs < 1, 0, NA))
tab1(lsacwide$peerprobs_1wave, cum.percent = TRUE)

#### Discrimination ####
# Total number of discrimination events across all timepoints (12/13-16/17)
# 5 items at age 12
# 8 items at age 14
# 9 items at age 16
# 1 Yes; 2 No
# Age 12
tab1(lsacwide$gsc26c1)
tab1(lsacwide$gsc26c2)
tab1(lsacwide$gsc26c3)
tab1(lsacwide$gsc26c4)
tab1(lsacwide$gsc26c5)


# Recode -9 and -3 to NA, and 2 to 0 for specified columns and create new variables
lsacwide <- lsacwide %>%
  mutate(across(c(gsc26c1, gsc26c2, gsc26c3, gsc26c4, gsc26c5), 
                ~ ifelse(. %in% c(-9, -3), NA, ifelse(. == 2, 0, .)),
                .names = "{.col}_new"))

tab1(lsacwide$gsc26c1_new)
tab1(lsacwide$gsc26c2_new)
tab1(lsacwide$gsc26c3_new)
tab1(lsacwide$gsc26c4_new)
tab1(lsacwide$gsc26c5_new)
# Compute discrimination score
lsacwide$g_discrim_tot <- lsacwide$gsc26c1_new + lsacwide$gsc26c2_new + lsacwide$gsc26c3_new + 
  lsacwide$gsc26c4_new + lsacwide$gsc26c5_new
tab1(lsacwide$g_discrim_tot)

#Age 14
tab1(lsacwide$hsc26c1)
tab1(lsacwide$hsc26c2)
tab1(lsacwide$hsc26c3)
tab1(lsacwide$hsc26c4)
tab1(lsacwide$hsc26c5)
tab1(lsacwide$hsc26c6)
tab1(lsacwide$hsc26c7)
tab1(lsacwide$hsc26c8)

# Recode -9 and -3 to NA, and 2 to 0 for specified columns and create new variables
lsacwide <- lsacwide %>%
  mutate(across(c(hsc26c1, hsc26c2, hsc26c3, hsc26c4, hsc26c5, hsc26c6, hsc26c7, hsc26c8), 
                ~ ifelse(. %in% c(-9, -3), NA, ifelse(. == 2, 0, .)),
                .names = "{.col}_new"))

tab1(lsacwide$hsc26c1_new)
tab1(lsacwide$hsc26c2_new)
tab1(lsacwide$hsc26c3_new)
tab1(lsacwide$hsc26c4_new)
tab1(lsacwide$hsc26c5_new)
tab1(lsacwide$hsc26c6_new)
tab1(lsacwide$hsc26c7_new)
tab1(lsacwide$hsc26c8_new)
# Compute discrimination score
lsacwide$h_discrim_tot <- lsacwide$hsc26c1_new + lsacwide$hsc26c2_new + lsacwide$hsc26c3_new + 
  lsacwide$hsc26c4_new + lsacwide$hsc26c5_new + lsacwide$hsc26c6_new + lsacwide$hsc26c7_new + lsacwide$hsc26c8_new
tab1(lsacwide$h_discrim_tot)

# Age 16
tab1(lsacwide$isc26c1)
tab1(lsacwide$isc26c2)
tab1(lsacwide$isc26c3)
tab1(lsacwide$isc26c4)
tab1(lsacwide$isc26c5)
tab1(lsacwide$isc26c6)
tab1(lsacwide$isc26c7)
tab1(lsacwide$isc26c8)
tab1(lsacwide$isc26c9)

# Recode -9 and -3 to NA, and 2 to 0 for specified columns and create new variables
lsacwide <- lsacwide %>%
  mutate(across(c(isc26c1, isc26c2, isc26c3, isc26c4, isc26c5, isc26c6, isc26c7, isc26c8, isc26c9), 
                ~ ifelse(. %in% c(-9, -3), NA, ifelse(. == 2, 0, .)),
                .names = "{.col}_new"))

tab1(lsacwide$isc26c1_new)
tab1(lsacwide$isc26c2_new)
tab1(lsacwide$isc26c3_new)
tab1(lsacwide$isc26c4_new)
tab1(lsacwide$isc26c5_new)
tab1(lsacwide$isc26c6_new)
tab1(lsacwide$isc26c7_new)
tab1(lsacwide$isc26c8_new)
tab1(lsacwide$isc26c9_new)
# Compute discrimination score
lsacwide$i_discrim_tot <- lsacwide$isc26c1_new + lsacwide$isc26c2_new + lsacwide$isc26c3_new + 
  lsacwide$isc26c4_new + lsacwide$isc26c5_new + lsacwide$isc26c6_new + lsacwide$isc26c7_new + 
  lsacwide$isc26c8_new + lsacwide$isc26c9_new
tab1(lsacwide$i_discrim_tot)

# All waves
lsacwide$discrim_tot <- lsacwide$g_discrim_tot + lsacwide$h_discrim_tot + lsacwide$i_discrim_tot
tab1(lsacwide$discrim_tot)

#### Friend relationship quality ####
tab1(lsacwide$gippatrust)
tab1(lsacwide$gippacomm)
tab1(lsacwide$hippatrust)
tab1(lsacwide$hippacomm)
tab1(lsacwide$iippatrust)
tab1(lsacwide$iippacomm)



#### Create a new data frame with our predictors and outcome ####
lsac_analysis <- lsacwide[, c("trans", "cismale", "cisfemale", "csep", "ccnfsad", 
                                "dcnfsad", "ecnfsad", "fcnfsad", "gcnfsad2", "hcnfsad2", 
                                "icnfsad2", "university", "diploma", 
                              "homes_since_birth",
                                "cnliveb1", "dnliveb1", "enliveb1", "fnliveb1", "hnliveb1",
                                "g_housing_security", "h_housing_security", "i_housing_security", "any_housing_insecruity",
                                "PAE", "HHparent_MHdisorder", 
                                "hh_alcp", "p1_alcp", "p2_alcp", "hh_dud","cak6s", "dak6s", "eak6s", "fak6s",
                                "gak6s", "hak6s", "iak6s", "g_unsupervisedtime", "h_unsupervisedtime",
                                "cawarm", "dawarm", "eawarm", "fawarm", "gawarm", "hawarm", "max_pwarmth",
                                "eaparmon", "faparmon", "gaparmon", "haparmonb", "close_mum12", 
                                "close_mum14", "close_mum16", 
                                "close_dad12", "close_dad14", "close_dad16", "max_psupport", 
                                "hasupport", "iasupport", "cP1SE",
                                "dP1SE", "eP1SE", "fP1SE", "gP1SE",
                                "hP1SE", "iP1SE","cacons", "dacons", "eacons", "facons", "gacons", 
                                "hacons", "iacons", "cahact", "dahact", "eahacte", "fahactd", "coohactb", 
                                "doohactb", "eoohactb", "foohactb", "goohactb", "hoohactb", 
                                "caang", "daang", "eaang", "faang", "gaang", "haang", 
                                "iaang", "cDV",
                                "dDV", "eDV", "fDV", "gDV",
                                "hDV", "iDV", "anyDV",
                                "parent_sep16", "cahend", "eahend", "fahend", "gahend", "hahend",
                                "parent_sle", 
                                "csle", "dsle", "esle", "fsle", "gsle", "h_sle", "isle", 
                                "c_difficulty", "d_difficulty", "e_difficulty", "f_difficulty",
                                "g_difficulty", "h_difficulty", "i_difficulty",
                                "d_support_avail_to_parent", "g_support_avail_to_parent", "e_support_avail_to_parent", 
                                "c_support_avail_to_parent",
                                "caarga", "daarga", "eaarga", "faarga", "gaarga", "haarga", "iaarga",
                                "Npastweekdrinks", "earlyalc", 
                                "weeklydrink16", "cannabis", "otherdrugs", "lifetimeADHD", 
                                "fconduct_tot", "gccondb", "hccondb", "iccondb", "fhyper_tot", "gchypr", "hchypr", "ichypr", 
                                "gdelinquency_tot", "hdelinquency_tot", 
                                "idelinquency_tot", "femot_high", "gemot_high", "hemot_high", 
                                "iemot_high", "icneuro", "icextra", "icopen", 
                                "icagree", "icconsc", "h_selfharm_idea", "h_selfharm",
                                "h_suicide_idea", "h_suicide_plan", "h_suicide_attempt", "i_selfharm_idea",
                                "i_selfharm", "i_suicide_idea", "i_suicide_plan", "i_suicide_attempt", "religion", 
                                "c_sleepprobs", "d_sleepprobs",
                                "e_sleepprobs", "f_sleepprobs", "g_sleepprobs", "h_sleepprobs",
                                "fhs20c3_NA", "ghs20c3_NA", "hhs20c3_NA", 
                                "ihs20c3_NA", "gsmfq", "hsmfq", "ismfq", "gspenceanx",
                                "hspenceanx", "ispenceanx", "g_happy", "h_happy", "i_happy", 
                                "f_noone", "g_noone", "max_school_achievement",
                                "gpssm", "hpssm1", "ipssm", "academic_pressure", "n_schools", 
                                "h_mum_interest_ed", "h_dad_interest_ed", "g_mum_interest_ed", "g_dad_interest_ed",
                                "ipeermoral", "gpeermoral", "hpeermoral", "f_cpeer", "gcpeer", "hcpeer", "icpeer",
                                "discrim_tot", "g_discrim_tot", "h_discrim_tot", "i_discrim_tot", 
                                "bullyvictim14_tot", "bullyvictim16_tot", 
                                "bullyperp14_tot", "bullyperp16_tot", "gippatrust", "gippacomm", "hippatrust", "hippacomm", 
                                "iippatrust", "iippacomm",  "finstress", "f_finstress_cont", "g_finstress_cont", 
                                "h_finstress_cont", "i_finstress_cont", "risky_alc"
                                
)]


#### Write to an SPSS file ####
write_sav(lsac_analysis, "lsac_analysis.sav")
