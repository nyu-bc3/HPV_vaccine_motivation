## ----setup, include=FALSE-------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 


## -------------------------------------------------------------------------------------
library(ICC)           # Intra-class correlation
library(psychometric)  # Pearson’s correlation functions
library(dplyr)         # Data manipulation
library(scales)        # ggplot2 scales
library(tidyr)         # Data tidying
library(patchwork)     # Combining ggplots
library(gtsummary)     # Publication-ready tables
library(psych)         # Item correlations & EFA
library(polycor)       # Polychoric correlations
library(haven)         # Read SPSS/Stata/SAS files
library(lavaan)        # Exploratory factor analysis
library(writexl)       # Write Excel files
library(corrplot)      # Correlation plots
library(mice)          # Multiple imputation
library(readxl)        # Read Excel files
library(readr)
library(miceadds)      # Extensions for mice
library(VIM)           # Missing data visualization
library(mifa)          # Imputation for EFA (psych)
library(GPArotation)   # Rotation methods for EFA
library(reshape2)      # Melt/cast data
library(officer)       # Word/PowerPoint manipulation
library(flextable)     # Flexible tables for Word/PowerPoint
library(ggplot2)       # Data visualization
library(pheatmap)      # Heatmaps
library(tidyverse)     # Collection of core tidy packages
library(parameters)    # Model parameters extraction
library(gridExtra)     # Arranging grid-based plots
library(grid)          # Low-level grid functions
library(gtable)        # Arrange grobs
library(DiagrammeR)    # Graph diagrams
library(RColorBrewer)  # Color palettes
library(tibble)        # Modern data frames


dat <- read_dta(file=("./hpvaxdataset20230113_1347.dta"))
head(dat)


## -------------------------------------------------------------------------------------
dat <- select(dat,age,sec1_q4, sec1_q5, sec1_q6,sec1_q7,sec1_q9,sec3_q28,sec3_q29,sec3_q30, sec3_q31,sec3_q32,sec3_q33,sec3_q46,sec3_q47,sec3_q48,sec3_q37,sec3_q38,sec3_q39,sec3_q40,sec3_q41,sec3_q42,sec6_S_q67, sec6_S_q68, sec6_S_q69, sec6_S_q70, sec6_S_q71, sec6_S_q72, sec6_q73,sec6_q74, sec6_q75, sec6_q76,sec3_q34, sec3_q35, sec3_q36, sec3_q43, sec3_q44, sec3_q45, sec5_q59,sec5_q60,sec5_q61,sec5_q62,sec5_q63,sec5_q64,sec5_q66, sec4_q50, sec4_q51, sec4_q52,sec4_q53, sec4_q54, sec4_q55, sec4_q56, sec4_q58, HPV_VAX_attitu_s35,Child_VAX_attitu_s35,TRUST_Score,KnowledgeScore,Correct_A_q77,Correct_A_q78,Correct_A_q79,Correct_A_q80,Correct_A_q81,Correct_A_q82, caseid)

#see how all variables and their reponses are laid out 
#lapply(dat, attributes)

# Create the summary table without altering placeholder codes or converting them to NA
response_summary <- dat %>%
  select(sec5_q59:sec5_q66,
         sec3_q28:sec3_q33,
         sec3_q46:sec3_q48,
         sec3_q37:sec3_q42,
         sec6_S_q67:sec6_q76,
         Correct_A_q77:Correct_A_q82,
         sec3_q34:sec3_q36,
         sec3_q43:sec3_q45,
         sec4_q50:sec4_q56,
         sec4_q58) %>%
  tbl_summary(
    missing = "ifany",        # Include true NA values in the table
    missing_text = "NA"       # Label true NA values as "NA"
  )

# Convert the table to a flextable for Word compatibility
# ft <- as_flex_table(response_summary)

# Export to a Word document
#doc <- read_docx() %>%           # Create a new Word document
#  body_add_flextable(ft) %>%      # Add the flextable to the document
#  body_add_par("Table: Response Summary", style = "heading 1") %>%
#  print(target = "response_summary.docx")


## -------------------------------------------------------------------------------------
#higher answer should be higher positive view of vaccine (less hesitancy)
#general vaccine attitude
attributes(dat$sec3_q28)#need to exclude -777 and 88 
dat$sec3_q28[dat$sec3_q28 == 88] <- NA
dat$sec3_q28[dat$sec3_q28 == -777] <- NA
attributes(dat$sec3_q29)#need to exclude -777 and 88 
dat$sec3_q29[dat$sec3_q29 == 88] <- NA
dat$sec3_q29[dat$sec3_q29 == -777] <- NA
attributes(dat$sec3_q30)#need to exclude -777 and 88 and reverse the order
dat$sec3_q30[dat$sec3_q30 == 88] <- NA
dat$sec3_q30[dat$sec3_q30 == -777] <- NA
dat$sec3_q30 <- case_match(dat$sec3_q30, 1~4,2~3,3~2,4~1)
attributes(dat$sec3_q31)#need to exclude -777 and 88 and reverse the order
dat$sec3_q31[dat$sec3_q31 == 88] <- NA
dat$sec3_q31[dat$sec3_q31 == -777] <- NA
dat$sec3_q31 <- case_match(dat$sec3_q31, 1~4,2~3,3~2,4~1)
attributes(dat$sec3_q32)#need to exclude -777 and 88and reverse the order
dat$sec3_q32[dat$sec3_q32 == 88] <- NA
dat$sec3_q32[dat$sec3_q32 == -777] <- NA
dat$sec3_q32 <- case_match(dat$sec3_q32, 1~4,2~3,3~2,4~1)
attributes(dat$sec3_q33)#need to exclude -777 and 88 and reverse the order 
dat$sec3_q33[dat$sec3_q33 == 88] <- NA
dat$sec3_q33[dat$sec3_q33 == -777] <- NA
dat$sec3_q33 <- case_match(dat$sec3_q33, 1~4,2~3,3~2,4~1)
#COVID vx atts
attributes(dat$sec3_q46)#need to make current score of 3 in the middle, then reverse them all
dat$sec3_q46 <- case_match(dat$sec3_q46, 1~1,2~3,3~2)
dat$sec3_q46 <- case_match(dat$sec3_q46, 1~3,2~2,3~1)
attributes(dat$sec3_q47)#need to make current score of 3 in the middle, then reverse them all
dat$sec3_q47 <- case_match(dat$sec3_q47, 1~1,2~3,3~2)
dat$sec3_q47 <- case_match(dat$sec3_q47, 1~3,2~2,3~1)
attributes(dat$sec3_q48)#need to make current score of 3 in the middle, then reverse them all
dat$sec3_q48 <- case_match(dat$sec3_q48, 1~1,2~3,3~2)
dat$sec3_q48 <- case_match(dat$sec3_q48, 1~3,2~2,3~1)
#HPV and vx attitude 
attributes(dat$sec3_q37)#need to exclude -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q37[dat$sec3_q37 == -777] <- NA
dat$sec3_q37 <- case_match(dat$sec3_q37, 4~1,2~2,1~3,3~4)
attributes(dat$sec3_q38)#need to exclude -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q38[dat$sec3_q38 == -777] <- NA
dat$sec3_q38 <- case_match(dat$sec3_q38, 4~1,2~2,1~3,3~4)
attributes(dat$sec3_q39)#need to exclude -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4, then all need to be reversed
dat$sec3_q39[dat$sec3_q39 == -777] <- NA
dat$sec3_q39 <- case_match(dat$sec3_q39, 4~1,2~2,1~3,3~4)
dat$sec3_q39 <- case_match(dat$sec3_q39, 1~4,2~3,3~2,4~1)
attributes(dat$sec3_q40)#need to exclude -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4, then all need to be reversed
dat$sec3_q40[dat$sec3_q40 == -777] <- NA
dat$sec3_q40 <- case_match(dat$sec3_q40, 4~1,2~2,1~3,3~4)
dat$sec3_q40 <- case_match(dat$sec3_q40, 1~4,2~3,3~2,4~1)
attributes(dat$sec3_q41)#need to exclude -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4, then all need to be reversed
dat$sec3_q41[dat$sec3_q41 == -777] <- NA
dat$sec3_q41 <- case_match(dat$sec3_q41, 4~1,2~2,1~3,3~4)
dat$sec3_q41 <- case_match(dat$sec3_q41, 1~4,2~3,3~2,4~1)
attributes(dat$sec3_q42)#need to exclude -777 and 88 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4, then all need to be reversed
dat$sec3_q42[dat$sec3_q42 == 88] <- NA
dat$sec3_q42[dat$sec3_q42 == -777] <- NA
dat$sec3_q42 <- case_match(dat$sec3_q42, 4~1,2~2,1~3,3~4)
dat$sec3_q42 <- case_match(dat$sec3_q42, 1~4,2~3,3~2,4~1)
#higher answers should be higher trust
#trust in institutions 
attributes(dat$sec6_S_q67)#current numbered 0, 1, 2, 3 (need numbered 1, 2, 3, 4)
dat$sec6_S_q67 <- dat$sec6_S_q67 + 1
attributes(dat$sec6_S_q68)#current numbered 0, 1, 2, 3 (need numbered 1, 2, 3, 4)
dat$sec6_S_q68 <- dat$sec6_S_q68 + 1
attributes(dat$sec6_S_q69)#current numbered 0, 1, 2, 3 (need numbered 1, 2, 3, 4)
dat$sec6_S_q69 <- dat$sec6_S_q69 + 1
attributes(dat$sec6_S_q70)#current numbered 0, 1, 2, 3 (need numbered 1, 2, 3, 4)
dat$sec6_S_q70 <- dat$sec6_S_q70 + 1
attributes(dat$sec6_S_q71)#current numbered 0, 1, 2, 3 (need numbered 1, 2, 3, 4)
dat$sec6_S_q71 <- dat$sec6_S_q71 + 1
attributes(dat$sec6_S_q72)#current numbered 0, 1, 2, 3 (need numbered 1, 2, 3, 4)
dat$sec6_S_q72 <- dat$sec6_S_q72 + 1
attributes(dat$sec6_q73)#remove -999 and reverse the order 
dat$sec6_q73[dat$sec6_q73 == -999] <- NA
dat$sec6_q73 <- case_match(dat$sec6_q73, 1~4,2~3,3~2,4~1)
attributes(dat$sec6_q74)#remove -999 and reverse the order 
dat$sec6_q74[dat$sec6_q74 == -999] <- NA
dat$sec6_q74 <- case_match(dat$sec6_q74, 1~4,2~3,3~2,4~1)
#trust in information from institutions 
attributes(dat$sec6_q75)#remove -999 and reverse the order 
dat$sec6_q75[dat$sec6_q75 == -999] <- NA
dat$sec6_q75 <- case_match(dat$sec6_q75, 1~4,2~3,3~2,4~1)
attributes(dat$sec6_q76)#remove -999 and reverse the order 
dat$sec6_q76[dat$sec6_q76 == -999] <- NA
dat$sec6_q76 <- case_match(dat$sec6_q76, 1~4,2~3,3~2,4~1)
#HPV vx knowledge - Correct_A is Correct vs Incorrect+DK
attributes(dat$Correct_A_q77)#no changes
dat$Correct_A_q77[is.na(dat$Correct_A_q77)] <- 0
attributes(dat$Correct_A_q78)#no changes
dat$Correct_A_q78[is.na(dat$Correct_A_q78)] <- 0
attributes(dat$Correct_A_q79)#no changes
dat$Correct_A_q79[is.na(dat$Correct_A_q79)] <- 0
attributes(dat$Correct_A_q80)#no changes
dat$Correct_A_q80[is.na(dat$Correct_A_q80)] <- 0
attributes(dat$Correct_A_q81)#no changes
dat$Correct_A_q81[is.na(dat$Correct_A_q81)] <- 0
attributes(dat$Correct_A_q82)#no changes
dat$Correct_A_q82[is.na(dat$Correct_A_q82)] <- 0
#attributes(dat$Correct_A_q83)#LABEL GOT CUT OFF YOU NEED TO CHECK WHAT VIA IS AND IF THIS NEEDS TO BE REVERSED (NO CHANGES IF CORRECT ANSWER IS TRUE)
#dat$Correct_A_q83[is.na(dat$Correct_A_q83)] <- 0
#Social processes influence for general vaccines
#higher score = higher do what i'm told about vaccines 
attributes(dat$sec3_q34)#exclude 88 and -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q34[dat$sec3_q34 == 88] <- NA
dat$sec3_q34[dat$sec3_q34 == -777] <- NA
dat$sec3_q34 <- case_match(dat$sec3_q34, 4~1,2~2,1~3,3~4)
attributes(dat$sec3_q35)#exclude 88 and -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q35[dat$sec3_q35 == 88] <- NA
dat$sec3_q35[dat$sec3_q35 == -777] <- NA
dat$sec3_q35 <- case_match(dat$sec3_q35, 4~1,2~2,1~3,3~4)
attributes(dat$sec3_q36)#exclude 88 and -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q36[dat$sec3_q36 == 88] <- NA
dat$sec3_q36[dat$sec3_q36 == -777] <- NA
dat$sec3_q36 <- case_match(dat$sec3_q36, 4~1,2~2,1~3,3~4)
#Social processes influence for HPV vaccines
attributes(dat$sec3_q43)#exclude 88 and -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q43[dat$sec3_q43 == 88] <- NA
dat$sec3_q43[dat$sec3_q43 == -777] <- NA
dat$sec3_q43 <- case_match(dat$sec3_q43, 4~1,2~2,1~3,3~4)
#Social processes influence for HPV vaccines
attributes(dat$sec3_q44)#exclude 88 and -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q44[dat$sec3_q44 == 88] <- NA
dat$sec3_q44[dat$sec3_q44 == -777] <- NA
dat$sec3_q44 <- case_match(dat$sec3_q44, 4~1,2~2,1~3,3~4)
#Social processes influence for HPV vaccines
attributes(dat$sec3_q45)#exclude 88 and -777 and change order. Current answers of 4 need to be changed to 1, current answers of 2 can stay 2, current answers of 1 need to be changed to 3, and current answers of 3 need to be changed to 4
dat$sec3_q45[dat$sec3_q45 
             == 88] <- NA
dat$sec3_q45[dat$sec3_q45 == -777] <- NA
dat$sec3_q45 <- case_match(dat$sec3_q45, 4~1,2~2,1~3,3~4)
#Social processes influence for HPV vaccines
#attributes(sec4$sec4_q49)#there is a TON of missing data - ask Corrina to exclude - I DID NOT DO ANY CLEANING ON THIS VARIABLE 
#social processes social norms 
#more comfortable with hpv vx is higher answer 
#more community members vxing higher answer 
attributes(dat$sec5_q59)#no change
attributes(dat$sec5_q60)#exclude -777
dat$sec5_q60[dat$sec5_q60 == -777] <- NA
attributes(dat$sec5_q61)#no change
attributes(dat$sec5_q62)#exclude -777
dat$sec5_q62[dat$sec5_q62 == -777] <- NA
attributes(dat$sec5_q63)#exclude -777, reverse order 
dat$sec5_q63[dat$sec5_q63 == -777] <- NA
dat$sec5_q63 <- case_match(dat$sec5_q63, 1~4,2~3,3~2,4~1)
attributes(dat$sec5_q64)#exclude -777, reverse order 
dat$sec5_q64[dat$sec5_q64 == -777] <- NA
dat$sec5_q64 <- case_match(dat$sec5_q64, 1~4,2~3,3~2,4~1)
attributes(dat$sec5_q66)#no change
#access 
#increased difficulty = higher answer 
#access to hpv vx 
attributes(dat$sec4_q50)#exclude -999 and -777 - this question is for people with 0 doses 
dat$sec4_q50[dat$sec4_q50 == -777] <- NA
dat$sec4_q50[dat$sec4_q50== -999] <- NA
attributes(dat$sec4_q51)#exclude -999 and -777 - this question is for people with 1 dose 
dat$sec4_q51[dat$sec4_q51 == -777] <- NA
dat$sec4_q51[dat$sec4_q51== -999] <- NA
attributes(dat$sec4_q52)#exclude -999 and -777 - this question is for people with 2 doses 
dat$sec4_q52[dat$sec4_q52 == -777] <- NA
dat$sec4_q52[dat$sec4_q52== -999] <- NA
attributes(dat$sec4_q53)#exclude -999 - this is for people who did try to get their daughter vxxed at some point (answer of 4 is did not even try yet)
dat$sec4_q53[dat$sec4_q53== -999] <- NA
attributes(dat$sec4_q54)#exclude -999 - this is for people who did try to get their daughter vxxed at some point (answer of 4 is did not even try yet) 
dat$sec4_q54[dat$sec4_q54== -999] <- NA
attributes(dat$sec4_q55)#exclude -999 - this is for people who did try to get their daughter vxxed at some point (answer of 4 is did not even try yet)
dat$sec4_q55[dat$sec4_q55== -999] <- NA
attributes(dat$sec4_q56)#exclude -999 - this is for people who did try to get their daughter vxxed at some point (answer of 4 is did not even try yet)
dat$sec4_q56[dat$sec4_q56== -999] <- NA
#access to childhood vx 
attributes(dat$sec4_q58)#exclude -999 and -777
dat$sec4_q58[dat$sec4_q58 == -777] <- NA
dat$sec4_q58[dat$sec4_q58== -999] <- NA

#add new columns for new summed scores if new columns are needed 
#hpv vx attitudes 
dat <- dat %>%
  mutate(hpv_vx_att_score = sec3_q37+sec3_q38+sec3_q39+sec3_q40+sec3_q41+sec3_q42+sec3_q43)
#general vx attitudes 
dat <- dat %>%
  mutate(gen_vx_att_score = sec3_q28+sec3_q29+sec3_q30+sec3_q31+sec3_q32+sec3_q33)
#covid vx attitudes 
dat <- dat %>%
  mutate(covid_vx_att_score = sec3_q46+sec3_q47+sec3_q48)

#change haven imported data with labels to regular non-labeled data for MICE 
write_xlsx(dat, "non_labeled_hpv_data.xlsx")#take a look at it 
dat <- read_excel("non_labeled_hpv_data.xlsx")

#exclude data where missing > 50% 
missing_percent <- c()
for(i in seq_along(dat)) {
        new_elements <- sum(is.na(dat[i]))
        # Use 'c' to combine the existing vector with the new_elements
        missing_percent <- c(missing_percent, new_elements)
    }
missing_percent <- (missing_percent/1347)*100
missing_percent
#(41 (sec4_q51, Overall how difficult do you feel it was to get the first dose of HPV vaccine for your daughter?), 42 (sec4_q52, Overall how difficult do you feel it was to get a second dose of the HPV vaccine for your daughter?) all have >50% missing 

impdat <- select(dat,sec3_q28,sec3_q29,sec3_q30,sec3_q31,sec3_q32,sec3_q33,sec3_q46,sec3_q47,sec3_q48,sec3_q37,sec3_q38,sec3_q39,sec3_q40,sec3_q41,sec3_q42,sec6_S_q67, sec6_S_q68, sec6_S_q69, sec6_S_q70, sec6_S_q71, sec6_S_q72, sec6_q73,sec6_q74, sec6_q75, sec6_q76,sec3_q34, sec3_q35, sec3_q36, sec3_q43, sec3_q44, sec3_q45, sec5_q59,sec5_q60,sec5_q61,sec5_q62,sec5_q63,sec5_q64,sec5_q66, sec4_q50, sec4_q53, sec4_q54, sec4_q55, sec4_q56, sec4_q58, sec3_q34,sec3_q35,sec3_q36,sec3_q43, sec3_q44, sec3_q45, sec5_q59, sec5_q60, sec5_q61, sec5_q62, sec5_q63, sec5_q64, hpv_vx_att_score,gen_vx_att_score,covid_vx_att_score,TRUST_Score,KnowledgeScore,Correct_A_q77,Correct_A_q78,Correct_A_q79,Correct_A_q80,Correct_A_q81,Correct_A_q82)

#vizualizing missingness 
#aggr_plot <- aggr(impdat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#aggr_plot

#MULTIPLE IMPUTATION FOR CORRELATIONS 
#Missing variables: MICE: The way it will work is you will get 5 different versions of the completed dataset.  You compute the correlation coefficient you're interested in for each version, which will give you 5 versions of the answer, and then you average those 5.  We may want confidence intervals at some point, in which case we can talk about how to get those.) For the SEM models, I think there should be a setting like "FIML = True" or something like that that you can use. - ask Alex about this - had to transform all varibales to continuous for the correlations so should we also do mie when they are continuous? Or before this step?
# Drop the two observations with caseid 23182 and 2514
dat <- dat[ ! dat$caseid %in% c(23182, 2514), ]

# Verify they’ve been removed
any(dat$caseid %in% c(23182, 2514))  # should return FALSE
nrow(dat)

imp <- mice(
  impdat,
  m=5,
  method="pmm")



## -------------------------------------------------------------------------------------
# Create a summary table for the specified variables
dat_fac <- dat %>%
  mutate(across(everything(), as.factor))

response_summary <- dat_fac %>%
  select(sec5_q59:sec5_q66,
         sec3_q28:sec3_q33,
         sec3_q46:sec3_q48,
         sec3_q37:sec3_q42,
         sec6_S_q67:sec6_q76,
         Correct_A_q77:Correct_A_q82,
         sec3_q34:sec3_q36,
         sec3_q43:sec3_q45,
         sec4_q50:sec4_q56,
         sec4_q58) %>%
  tbl_summary(
    missing = "ifany",        # Include true NA values in the table
    missing_text = "NA"       # Label true NA values as "NA"
  )

# Convert the table to a flextable for Word compatibility
ft <- as_flex_table(response_summary)

# Export to a Word document
doc <- read_docx() %>%           # Create a new Word document
  body_add_flextable(ft) %>%      # Add the flextable to the document
  body_add_par("Table: Response Summary", style = "heading 1") %>%
  print(target = "response summary after clean.docx")



## -------------------------------------------------------------------------------------
#trust
item_total_cor_ind <- with(imp,cor.test(TRUST_Score,sec6_S_q67))#this is to test to see if imputaton worked - this gets one individual correlation with the total from each imputed dataset, so if it worked correctly, there should be 5 individual correlations
trust_total_cors <- miceadds::micombine.cor(mi.res=imp, variables=c("TRUST_Score","sec6_S_q67","sec6_S_q68","sec6_S_q69","sec6_S_q70","sec6_S_q71","sec6_S_q72","sec6_q73","sec6_q74","sec6_q75","sec6_q76"))  %>%
  filter(grepl("TRUST_Score", variable2))
trust_total_cors #this should be like an average correlation matrix for all imputed data 
#knowledge 
know_total_cors <- miceadds::micombine.cor(mi.res=imp, variables=c("KnowledgeScore","Correct_A_q77","Correct_A_q78","Correct_A_q79","Correct_A_q80","Correct_A_q81","Correct_A_q82")) %>%
  filter(grepl("KnowledgeScore", variable2))
know_total_cors 
#HPV vaccine attitudes 
hpv_vx_att_cors <- miceadds::micombine.cor(mi.res=imp, variables=c("hpv_vx_att_score","sec3_q37","sec3_q38","sec3_q39","sec3_q40","sec3_q41","sec3_q42")) %>%
  filter(grepl("hpv_vx_att_score", variable2))
hpv_vx_att_cors
#General child vaccine attitudes 
gen_vx_att_cors <- miceadds::micombine.cor(mi.res=imp, variables=c("gen_vx_att_score","sec3_q28","sec3_q29","sec3_q30","sec3_q31","sec3_q32","sec3_q33")) %>%
  filter(grepl("gen_vx_att_score", variable2))
gen_vx_att_cors
#COVID-19 and HPV vaccine attitudes 
covid_vx_att_cors <- miceadds::micombine.cor(mi.res=imp, variables=c("covid_vx_att_score","sec3_q46","sec3_q47","sec3_q48")) %>%
  filter(grepl("covid_vx_att_score", variable2))
covid_vx_att_cors


## -------------------------------------------------------------------------------------
#trust in institutions 
trust_total_cors_matrix <- attr(trust_total_cors, "r_matrix")
corrplot(trust_total_cors_matrix , method="number",number.cex=0.6)
#knowledge 
know_total_cors_matrix <- attr(know_total_cors , "r_matrix")
corrplot(know_total_cors_matrix , method="number",number.cex=0.6)
#Vaccine attitudes 
hpv_vx_att_cors_matrix <- attr(hpv_vx_att_cors , "r_matrix")
corrplot(hpv_vx_att_cors_matrix , method="number",number.cex=0.6)
gen_vx_att_cors_matrix <- attr(gen_vx_att_cors  , "r_matrix")
corrplot(gen_vx_att_cors_matrix , method="number",number.cex=0.6)
covid_vx_att_cors_matrix <- attr(covid_vx_att_cors , "r_matrix")
corrplot(covid_vx_att_cors_matrix, method="number",number.cex=0.6)
#Social processes: 
soc_cors <- miceadds::micombine.cor(mi.res=imp, variables=c("sec3_q34","sec3_q35","sec3_q36","sec3_q43", "sec3_q44", "sec3_q45", "sec5_q59", "sec5_q60", "sec5_q61", "sec5_q62", "sec5_q63", "sec5_q64",  "sec5_q66")) 
soc_cors_matrix <- attr(soc_cors , "r_matrix")
corrplot(soc_cors_matrix, method="number",number.cex=0.6)
#Practical issues 
prac_cors <- miceadds::micombine.cor(mi.res=imp, variables=c("sec4_q50", "sec4_q53", "sec4_q54", "sec4_q55", "sec4_q56", "sec4_q58")) 
prac_cors_matrix <- attr(prac_cors , "r_matrix")
corrplot(prac_cors_matrix, method="number",number.cex=0.6)

#Some questions on the distribution for vars 59-65
# Create a summary table
#response_summary <- dat %>%
#  select(sec5_q59:sec5_q65) %>% 
#  pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
#  group_by(Question, Response) %>%
#  summarize(Count = n(), .groups = 'drop') %>%
#  pivot_wider(names_from = "Response", values_from = "Count", values_fill = 0)
# View the summary table
#write_xlsx(response_summary, "distribution_59_65.xlsx")#take a look at it

## -------------------------------------------------------------------------------------

# trust_total_cors_matrix 为例
pheatmap(trust_total_cors_matrix,
         display_numbers = TRUE,      
         number_format   = "%.2f",    
         cluster_rows    = TRUE,     
         cluster_cols    = TRUE,
         fontsize_number = 10         
)

pheatmap(know_total_cors_matrix,
         display_numbers = TRUE,      
         number_format   = "%.2f",    
         cluster_rows    = TRUE,     
         cluster_cols    = TRUE,
         fontsize_number = 10         
)

pheatmap(hpv_vx_att_cors_matrix,
         display_numbers = TRUE,      
         number_format   = "%.2f",    
         cluster_rows    = TRUE,     
         cluster_cols    = TRUE,
         fontsize_number = 10         
)

## -------------------------------------------------------------------------------------
# Remove bad samples
dat <- dat[ ! dat$caseid %in% c(23182, 2514), ]
impdat <- dat %>%
  select(
    sec3_q28,sec3_q29,sec3_q30,sec3_q31,sec3_q32,sec3_q33,
    sec3_q46,sec3_q47,sec3_q48,
    sec3_q37,sec3_q38,sec3_q39,sec3_q40,sec3_q41,sec3_q42,
    sec6_S_q67,sec6_S_q68,sec6_S_q69,sec6_S_q70,sec6_S_q71,sec6_S_q72,
    sec6_q73,sec6_q74,sec6_q75,sec6_q76,
    sec3_q34,sec3_q35,sec3_q36,sec3_q43,sec3_q44,sec3_q45,
    sec5_q59,sec5_q60,sec5_q61,sec5_q62,sec5_q63,sec5_q64,sec5_q66,
    sec4_q50,sec4_q53,sec4_q54,sec4_q55,sec4_q56,sec4_q58,
    TRUST_Score, KnowledgeScore,
    Correct_A_q77,Correct_A_q78,Correct_A_q79,Correct_A_q80,Correct_A_q81,Correct_A_q82
  ) %>%
  mutate(
    think_score  = rowSums(across(all_of(think_vars)),  na.rm = FALSE),
    social_score = rowSums(across(all_of(social_vars)), na.rm = FALSE)
  )

# Imputations
imp <- mice(impdat, m = 5, method = "pmm")

think_total_cors <- miceadds::micombine.cor(
  mi.res    = imp,
  variables = c("think_score", think_vars)
) %>%
  filter(grepl("think_score", variable2))

social_total_cors <- miceadds::micombine.cor(
  mi.res    = imp,
  variables = c("social_score", social_vars)
) %>%
  filter(grepl("social_score", variable2))

think_total_cors
social_total_cors



## ----fig.height=10--------------------------------------------------------------------
think_mat  <- attr(think_total_cors,  "r_matrix")
social_mat <- attr(social_total_cors, "r_matrix")
my_palette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(50)
heatmap_opts <- list(
  color           = my_palette,
  border_color    = NA,
  cluster_rows    = TRUE,
  cluster_cols    = TRUE,
  show_rownames   = TRUE,
  show_colnames   = TRUE,
  angle_col       = 45,
  fontsize        = 12,
  fontsize_row    = 12,
  fontsize_col    = 12,
  number_color = "black",
  display_numbers = TRUE,
  number_format   = "%.2f",
  fontsize_number = 10,
  cellwidth       = 20,
  cellheight      = 20
)
do.call(
  pheatmap,
  c(
    list(
      think_mat,
      main = "Thinking & Feeling\nItem Correlation"
    ),
    heatmap_opts
  )
)

do.call(
  pheatmap,
  c(
    list(
      social_mat,
      main = "Social Factors\nItem Correlation"
    ),
    heatmap_opts
  )
)


## ----fig.height=8---------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
# 0. Setup ----------------------------------------------------------------------


# define your item vectors
think_vars  <- c("sec3_q28","sec3_q29","sec3_q30","sec3_q31","sec3_q32","sec3_q33",
                 "sec3_q46","sec3_q47","sec3_q48",
                 "sec3_q37","sec3_q38","sec3_q39","sec3_q40","sec3_q41","sec3_q42")
social_vars <- c("sec6_S_q67","sec6_S_q68","sec6_S_q69","sec6_S_q70","sec6_S_q71","sec6_S_q72",
                 "sec6_q73","sec6_q74","sec6_q75","sec6_q76",
                 "sec3_q34","sec3_q35","sec3_q36","sec3_q43","sec3_q44","sec3_q45",
                 "sec5_q59","sec5_q60","sec5_q61","sec5_q62","sec5_q63","sec5_q64","sec5_q66",
                 "sec4_q50","sec4_q53","sec4_q54","sec4_q55","sec4_q56","sec4_q58")

# ──────────────────────────────────────────────────────────────────────────────
# 1. Remove cases ---------------------------------------------------------------
dat <- dat[ ! dat$caseid %in% c(23182, 2514), ]

# ──────────────────────────────────────────────────────────────────────────────
# 2. Build impdat & run MICE ---------------------------------------------------
impdat <- dat %>%
  select(
    all_of(think_vars),
    all_of(social_vars),
    TRUST_Score, KnowledgeScore,
    Correct_A_q77,Correct_A_q78,Correct_A_q79,
    Correct_A_q80,Correct_A_q81,Correct_A_q82
  )

# run predictive-mean-matching with 5 imputations
imp <- mice(impdat, m = 5, method = "pmm", seed = 2025)

# ──────────────────────────────────────────────────────────────────────────────
# 3. Pool inter‐item correlations ------------------------------------------------
think_cors  <- miceadds::micombine.cor(mi.res = imp, variables = think_vars)
social_cors <- miceadds::micombine.cor(mi.res = imp, variables = social_vars)

# extract the r matrices
think_mat  <- attr(think_cors,  "r_matrix")
social_mat <- attr(social_cors, "r_matrix")

# ──────────────────────────────────────────────────────────────────────────────
# 4. Heatmap color palette & common options -------------------------------------
#  red–white–blue palette
my_palette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(50)

heatmap_opts <- list(
  color           = my_palette,
  border_color    = NA,
  cluster_rows    = TRUE,
  cluster_cols    = TRUE,
  show_rownames   = TRUE,
  show_colnames   = TRUE,
  angle_col       = 45,
  fontsize        = 12,
  fontsize_row    = 12,
  fontsize_col    = 12,
  display_numbers = TRUE,
  number_color    = "black",
  number_format   = "%.2f",
  fontsize_number = 10,
  cellwidth       = 20,
  cellheight      = 20
)

# ──────────────────────────────────────────────────────────────────────────────
# 5. Plot the heatmaps ----------------------------------------------------------
# Thinking & Feeling items
do.call(
  pheatmap,
  c(
    list(
      think_mat,
      main = "Thinking & Feeling\nInter-Item Correlations"
    ),
    heatmap_opts
  )
)




## ----fig.height=10--------------------------------------------------------------------

# Social Factors items
do.call(
  pheatmap,
  c(
    list(
      social_mat,
      main = "Social Factors Inter-Item Correlations"
    ),
    heatmap_opts
  )
)


## -------------------------------------------------------------------------------------
think_ordered <- c(
  "sec3_q28","sec3_q29","sec3_q30","sec3_q31","sec3_q32","sec3_q33",
  "sec3_q37","sec3_q38","sec3_q39","sec3_q40","sec3_q41","sec3_q42",
  "sec3_q46","sec3_q47","sec3_q48",
  "Correct_A_q77","Correct_A_q78","Correct_A_q79",
  "Correct_A_q80","Correct_A_q81","Correct_A_q82"
)

social_ordered <- c(
  "sec3_q36","sec3_q45","sec3_q34","sec3_q35","sec3_q43","sec3_q44",
  "sec6_S_q67","sec6_S_q68","sec6_S_q69","sec6_S_q70","sec6_S_q71","sec6_S_q72",
  "sec6_q73","sec6_q74","sec6_q75","sec6_q76",
  "sec5_q59","sec5_q60","sec5_q61","sec5_q62","sec5_q63","sec5_q64","sec5_q66"
)

labels_map <- c(
  sec3_q28 = "vaccines are important",
  sec3_q29 = "vaccines offer protection",
  sec3_q30 = "concern: mod. short-term S/E vaccines",
  sec3_q31 = "concern: ser. short-term S/E vaccines",
  sec3_q32 = "concern: long-term S/E vaccines",
  sec3_q33 = "concern: too many vaccines",
  sec3_q37 = "HPV vaccine is important",
  sec3_q38 = "HPV vaccine offers protection",
  sec3_q39 = "concern: HPV vaccine is riskier",
  sec3_q40 = "concern: mod. short-term S/E HPV vax",
  sec3_q41 = "concern: ser. short-term S/E HPV vax",
  sec3_q42 = "concern: long-term S/E HPV vax",
  sec3_q46 = "covid changed opin. HPV importance",
  sec3_q47 = "covid changed opin. HPV safety",
  sec3_q48 = "covid changed opin. HPV effectiveness",
  Correct_A_q77 = "knowledge: HPV causes cancer",
  Correct_A_q78 = "knowledge: HPV is an STD",
  Correct_A_q79 = "knowledge: men can have HPV",
  Correct_A_q80 = "knowledge: HPV can have no symptoms",
  Correct_A_q81 = "knowledge: vax most effective timing",
  Correct_A_q82 = "knowledge: vax protects against cancer",
  sec3_q36 = "follow clinician recs vaccine",
  sec3_q45 = "follow clinician recs HPV vax",
  sec3_q34 = "follow rel. leader recs vaccines",
  sec3_q35 = "follow trad. leader recs vaccines",
  sec3_q43 = "follow rel. leader recs HPV vax",
  sec3_q44 = "follow trad. leader recs HPV vax",
  sec6_S_q67 = "trust community",
  sec6_S_q68 = "trust national government",
  sec6_S_q69 = "trust county government",
  sec6_S_q70 = "trust doctors & nurses",
  sec6_S_q71 = "trust community HC workers",
  sec6_S_q72 = "trust civil society / NGO",
  sec6_q73   = "trust traditional healers",
  sec6_q74   = "trust religious leaders",
  sec6_q75   = "trust HPV info: gov’t",
  sec6_q76   = "trust HPV info: clinicians",
  sec5_q59   = "talk about HPV with daughter",
  sec5_q60   = "comfortable talking w/ daughter",
  sec5_q61   = "talk about HPV with parents",
  sec5_q62   = "comfortable talking w/ parents",
  sec5_q63   = "other parents vaccinate",
  sec5_q64   = "other parents vaccinate HPV",
  sec5_q66   = "talk about HPV with doctor"
)

# ──────────────────────────────────────────────────────────────────────────────
# 2. Remove cases & run MICE ---------------------------------------------------

# drop unwanted IDs
dat <- dat[ ! dat$caseid %in% c(23182, 2514), ]

# select only your vars + knowledge items
impdat <- dat %>%
  select(
    all_of(think_ordered),
    all_of(social_ordered),
    TRUST_Score, KnowledgeScore,
    Correct_A_q77:Correct_A_q82
  )

# run 5‐fold PMM imputation
imp <- mice(impdat, m = 5, method = "pmm", seed = 2025)

# ──────────────────────────────────────────────────────────────────────────────
# 3. Pool & extract correlation matrices ---------------------------------------

think_cors  <- miceadds::micombine.cor(mi.res = imp, variables = think_ordered)
social_cors <- miceadds::micombine.cor(mi.res = imp, variables = social_ordered)

think_mat  <- attr(think_cors,  "r_matrix")
social_mat <- attr(social_cors, "r_matrix")

# reorder & relabel
think_mat  <- think_mat[think_ordered, think_ordered]
social_mat <- social_mat[social_ordered, social_ordered]

dimnames(think_mat)  <- list(labels_map[think_ordered],  labels_map[think_ordered])
dimnames(social_mat) <- list(labels_map[social_ordered], labels_map[social_ordered])

# ──────────────────────────────────────────────────────────────────────────────
# 4. Heatmap palette & plotting ------------------------------------------------

my_palette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(50)
breaks_vec <- seq(-1, 1, length.out = length(my_palette)+1)

heatmap_args <- list(
  color           = my_palette,
  breaks          = breaks_vec,
  border_color    = NA,
  cluster_rows    = FALSE,
  cluster_cols    = FALSE,
  show_rownames   = TRUE,
  show_colnames   = TRUE,
  angle_col       = 45,
  fontsize        = 12,
  fontsize_row    = 10,
  fontsize_col    = 10,
  display_numbers = TRUE,
  number_color    = "black",
  number_format   = "%.2f",
  fontsize_number = 8,
  cellwidth       = 18,
  cellheight      = 18
)





## ----fig.height=10--------------------------------------------------------------------
# Thinking & Feeling
do.call(
  pheatmap,
  c(
    list( think_mat, main = "Thinking & Feeling\nItem Correlations" ),
    heatmap_args
  )
)

# Social Factors
do.call(
  pheatmap,
  c(
    list( social_mat, main = "Social Factors\nItem Correlations" ),
    heatmap_args
  )
)

## -------------------------------------------------------------------------------------
# 直接保存为 PNG
pheatmap(
  think_mat,
  main        = "Thinking & Feeling\nItem Correlations",
  color       = my_palette,
  breaks      = breaks_vec,
  cluster_rows= FALSE,
  cluster_cols= FALSE,
  display_numbers = TRUE,
  filename    = ".//thinking_heatmap.png",   #
  width       = 10,        # 宽，单位由 units 决定（默认 inch）
  height      = 8,         # 高
  units       = "in",      # 单位：in, cm 或 mm
  dpi         = 300        # 分辨率：300 dpi
)
pheatmap(
  social_mat,
  main        = "Social Factors\nItem Correlations",
  color       = my_palette,
  breaks      = breaks_vec,
  cluster_rows= FALSE,
  cluster_cols= FALSE,
  display_numbers = TRUE,
  filename    = ".//social_heatmap.png",
  width       = 10,
  height      = 8,
  units       = "in",
  dpi         = 300
)


## -------------------------------------------------------------------------------------
think_vars <- c(
  "sec3_q28","sec3_q29","sec3_q30","sec3_q31","sec3_q32","sec3_q33",
  "sec3_q37","sec3_q38","sec3_q39","sec3_q40","sec3_q41","sec3_q42",
  "sec3_q46","sec3_q47","sec3_q48"
)
social_vars <- c(
  "sec6_S_q67","sec6_S_q68","sec6_S_q69","sec6_S_q70","sec6_S_q71","sec6_S_q72",
  "sec6_q73","sec6_q74","sec6_q75","sec6_q76",
  "sec3_q34","sec3_q35","sec3_q36","sec3_q43","sec3_q44","sec3_q45",
  "sec5_q59","sec5_q60","sec5_q61","sec5_q62","sec5_q63","sec5_q64","sec5_q66",
  "sec4_q50","sec4_q53","sec4_q54","sec4_q55","sec4_q56","sec4_q58"
)

dat <- dat[! dat$caseid %in% c(23182, 2514), ]
impdat <- dat %>%
  select(all_of(c(think_vars, social_vars)))
imp <- mice(impdat, m = 5, method = "pmm", seed = 2025)

think_raw  <- miceadds::micombine.cor(mi.res = imp, variables = think_vars)
social_raw <- miceadds::micombine.cor(mi.res = imp, variables = social_vars)

think_inter <- as_tibble(think_raw) %>%
  select(variable1, variable2, r, rse, fisher_r, fisher_rse, fmi) %>%
  filter(variable1 != variable2)    # drop diagonal if you like

social_inter <- as_tibble(social_raw) %>%
  select(variable1, variable2, r, rse, fisher_r, fisher_rse, fmi) %>%
  filter(variable1 != variable2)

print(think_inter)
print(social_inter)




## -------------------------------------------------------------------------------------
write.table(
  think_inter,
  file      = "think_inter.txt",
  sep       = "\t",
  quote     = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

write.table(
  social_inter,
  file      = "social_inter.txt",
  sep       = "\t",
  quote     = FALSE,
  row.names = FALSE,
  col.names = TRUE
)


write_tsv(think_inter,  ".//think_inter.txt")
write_tsv(social_inter, ".//social_inter.txt")



## -------------------------------------------------------------------------------------
efadat <- select(dat,sec3_q28, sec3_q29, sec3_q30, sec3_q31, sec3_q32, sec3_q33, sec3_q37,sec3_q38, sec3_q39,sec3_q40, sec3_q41, sec3_q42,sec6_S_q67, sec6_S_q68, sec6_S_q69, sec6_S_q70, sec6_S_q71, sec6_S_q72, sec6_q73, sec6_q74, sec6_q75, sec6_q76, sec3_q34, sec3_q35,sec3_q36,sec3_q43, sec3_q44,sec3_q45,sec5_q59,sec5_q60,sec5_q61, sec5_q62, sec5_q63,sec5_q64,sec5_q66,Correct_A_q77, Correct_A_q78, Correct_A_q79, Correct_A_q80, Correct_A_q81, Correct_A_q82)

names(efadat) <- c("Vxs important for my childrens' health","Getting vx is a good way to protect my child","concerned about mild to moderate short-term effects of childhood vx", "concerned about serious short-term effects of childhood vx", "concerned about long-term effects of childhood vx","Children receive too many vx","HPV Vxs important for my daughter’s health","Getting HPV vx is a good way to protect my daughter","HPV vx more risks than older vx","concerned about mild to moderate short-term effects of HPV vx","concerned about serious short-term effects of HPV vx","concerned about long-term effects of HPV vx", "trust people in your community", "trust national gov", "trust county gov", "trust docs and nurses", "trust community health workers/volunteers", "trust people who work at non-gov organizations/civil society", "trust traditional healers", "trust religious leaders", "trust HPV vx information & advice from the gov", "trust HPV vx information & advice from docs and nurses", "I do what my religious leader rec about vx","I do what my traditional leader rec about vx", "I do what the clinician rec about vx","I do what my religious leader rec about the HPV vx","I do what my traditional leader rec about HPV vx","I do what the clinican rec about HPV vx","ever talked about cervical cancer or HPV vx with your daughter?", "comfortable do you feel talking about cervical cancer or HPV vx w/ daughter", "talked about cervical cancer or HPV vx with other parents","comfortable do you feel talking about cervical cancer or HPV vx w/ other parents","other parents in my community are vaccinating their children with routine vx",  "other parents in my community are vaccinating their daughters against HPV","anyone you talk with regularly who had the opportunity to vaccinate their daughter against HPV but declined","Have you ever talked about cervical cancer or HPV vaccine with a doctor/nurse","Correct_q77", "Correct_q78", "Correct_q79", "Correct_q80", "Correct_q81", "Correct_q82", "Correct_q83")

#with pysch is below:https://www.uwo.ca/fhs/tc/labs/10.FactorAnalysis.pdf
scree(efadat)#Eigenvalues are a measure of the amount of variance accounted for by a factor, and so they can be useful in determining the number of factors that we need to extract. In a scree plot, we simply plot the eigenvalues for all of our factors, and then look to see where they drop off sharply.
fa.parallel(efadat)#A better method for evaluating the scree plot is within a parallel analysis. In addition to plotting the eigenvalues from our factor analysis (whether it’s based on principal axis or principal components extraction), a parallel analysis involves generating random correlation matrices and after factor analyzing them, comparing the resulting eigenvalues to the eigenvalues of the observed data. The idea behind this method is that observed eigenvalues that are higher than their corresponding random eigenvalues are more likely to be from “meaningful factors” than observed eigenvalues that are below their corresponding random eigenvalue.When looking at the parallel analysis scree plots, there are two places to look depending on which type of factor analysis you’re looking to run. The two blue lines show you the observed eigenvalues - they should look identical to the scree plots drawn by the scree function. The red dotted lines show you the random eigenvalues or the simulated data line. Each point on the blue line that lies above the corresponding simulated data line is a factor or component to extract. In this analysis, you can see that 6 factors in the “Factor Analysis” parallel analysis lie above the corresponding simulated data line and 6 components in the “Principal Components” parallel analysis lie above the corresponding simulated data line.

#run the efa IN PSYCH  

#2
fa2 <- fa(efadat,
nfactors = 2,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#3
fa3 <- fa(efadat,
nfactors = 3,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#4
fa4 <- fa(efadat,
nfactors = 4,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#5
fa5 <- fa(efadat,
nfactors = 5,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#6
fa6 <- fa(efadat,
nfactors = 6,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#7
fa7 <- fa(efadat,
nfactors = 7,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#8
fa8 <- fa(efadat,
nfactors = 8,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#9
fa9 <- fa(efadat,
nfactors = 9,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#10
fa10 <- fa(efadat,
nfactors = 10,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#11
fa11 <- fa(efadat,
nfactors = 11,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#12
fa12 <- fa(efadat,
nfactors = 12,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#13
fa13 <- fa(efadat,
nfactors = 13,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")
#14
fa14<- fa(efadat,
nfactors = 14,
fm="pa",
max.iter = 100,
missing=TRUE,
impute="median",
rotate = "oblimin")

#for ease of interpretation, set all values below 0.3 to zero 
fa4$loadings[fa4$loadings < 0.3] <- 0
fa4$loadings
fa5$loadings[fa5$loadings < 0.3] <- 0
fa5$loadings
fa6$loadings[fa6$loadings < 0.3] <- 0
fa6$loadings
fa7$loadings[fa7$loadings < 0.3] <- 0
fa7$loadings
fa8$loadings[fa8$loadings < 0.3] <- 0
fa8$loadings
fa9$loadings[fa9$loadings < 0.3] <- 0
fa9$loadings
fa10$loadings[fa10$loadings < 0.3] <- 0
fa10$loadings
fa11$loadings[fa11$loadings < 0.3] <- 0
fa11$loadings
fa12$loadings[fa12$loadings < 0.3] <- 0
fa12$loadings
fa13$loadings[fa13$loadings < 0.3] <- 0
fa13$loadings
fa14$loadings[fa14$loadings < 0.3] <- 0
fa14$loadings

#make a diagram of the efa 
pdf("66my_fa_diagram_5.pdf", height=15, width=30)
fa.diagram(fa5, cut=0.3)
dev.off()

pdf("66my_fa_diagra_6.pdf", height=15, width=30)
fa.diagram(fa6, cut=0.3)
dev.off()


pdf("66my_fa_diagram_7.pdf", height=15, width=30)
fa.diagram(fa7, cut=0.3)
dev.off()

pdf("66my_fa_diagram_8.pdf", height=15, width=30)
fa.diagram(fa8, cut=0.3)
dev.off()

pdf("66my_fa_diagram9.pdf", height=15, width=30)
fa.diagram(fa9, cut=0.3)
dev.off()

pdf("66my_fa_diagram10.pdf", height=15, width=30)
fa.diagram(fa10, cut=0.3)
dev.off()

pdf("66my_fa_diagram11.pdf", height=15, width=30)
fa.diagram(fa11, cut=0.3)
dev.off()

pdf("66my_fa_diagram12.pdf", height=15, width=30)
fa.diagram(fa12, cut=0.3)
dev.off()

pdf("66my_fa_diagram13.pdf", height=15, width=30)
fa.diagram(fa13, cut=0.3)
dev.off()

pdf("66my_fa_diagram14.pdf", height=15, width=30)
fa.diagram(fa14, cut=0.3)
dev.off()

#variables with communality between factors: 
#name_fa$communality

#loadings matrix for efa 
print(fa7$loadings, cutoff=0, digits=3)
fa7$loadings <- as.data.frame(fa7$loadings[,1:7])
loadings_fa7 <- fa7$loadings 
loadings_fa7$row_name <- rownames(fa7$loadings)
loadings_fa7 <- loadings_fa7[, c(8, 1:7)]
loadings_fa7 <- as.matrix(fa7$loadings)
factor_loadings_melted_fa7 <- melt(loadings_fa7)
# Plotting the heatmap
ggplot(factor_loadings_melted_fa7, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "Factors", y = "Items", fill = "Loading") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#just getting a table 
fa7_loadings_df <- as.data.frame(fa7$loadings[, 1:7])
fa7_loadings_df$row_name <- rownames(fa7$loadings)
fa7_loadings_df <- fa7_loadings_df[, c(ncol(fa7_loadings_df), 1:7)]
fa7_loadings_df <- fa7_loadings_df[, c("row_name", "PA1", "PA2", "PA3", "PA4", "PA5", "PA6", "PA7")]
fa7_loadings_df <- fa7_loadings_df %>%
  mutate(across(where(is.numeric), round, digits = 3))
fa7_table <- flextable(fa7_loadings_df)
doc <- read_docx() %>% 
  body_add_flextable(fa7_table) %>% 
  body_add_par("Factor Loadings Table", style = "heading 1")
print(doc, target = "fa7_loadings_table.docx")


## -------------------------------------------------------------------------------------

# Packages ----------------------------------------------------------------


# Data Preparation -------------------------------------------------------
efadat <- select(dat, sec3_q28, sec3_q29, sec3_q30, sec3_q31, sec3_q32, sec3_q33, 
                 sec3_q37, sec3_q38, sec3_q39, sec3_q40, sec3_q41, sec3_q42, 
                 sec6_S_q67, sec6_S_q68, sec6_S_q69, sec6_S_q70, sec6_S_q71, sec6_S_q72, 
                 sec6_q73, sec6_q74, sec6_q75, sec6_q76, sec3_q34, sec3_q35, sec3_q36, 
                 sec3_q43, sec3_q44, sec3_q45, sec5_q59, sec5_q60, sec5_q61, sec5_q62, 
                 sec5_q63, sec5_q64, sec5_q66, Correct_A_q77, Correct_A_q78, 
                 Correct_A_q79, Correct_A_q80, Correct_A_q81, Correct_A_q82)

# Rename variables for clarity
names(efadat) <- c("Vxs health importance", "Vxs protect child", "Concern mild effects", 
                   "Concern severe effects", "Concern long-term effects", "Too many vxs", 
                   "HPV vxs importance", "HPV vx protection", "HPV vx risks", 
                   "Concern mild HPV effects", "Concern severe HPV effects", "Concern long-term HPV effects", 
                   "Trust community", "Trust nat'l gov", "Trust county gov", "Trust doctors/nurses", 
                   "Trust CHWs", "Trust NGOs", "Trust traditional healers", "Trust religious leaders", 
                   "Trust HPV info from gov", "Trust HPV info from doctors", "Follow religious vx rec", 
                   "Follow traditional vx rec", "Follow doctor vx rec", "Follow religious HPV vx rec", 
                   "Follow traditional HPV vx rec", "Follow doctor HPV vx rec", "Discussed HPV with daughter", 
                   "Comfort discussing HPV with daughter", "Discussed HPV with parents", 
                   "Comfort discussing HPV with parents", "Community parents vxs", 
                   "Community parents HPV vxs", "Knows someone who declined HPV vx", 
                   "Discussed HPV with doctor", "Correct_q77", "Correct_q78", "Correct_q79", 
                   "Correct_q80", "Correct_q81", "Correct_q82", "Correct_q83")

# Descriptive Statistics -------------------------------------------------
summary(efadat)

# Exploratory Factor Analysis ---------------------------------------------

# Scree Plot
scree(efadat)

# Parallel Analysis
fa.parallel(efadat)

# Compute Correlation Matrix
efa_cor <- cor(efadat, use = "pairwise.complete.obs")

# Try multiple factor solutions explicitly
fa_4 <- fa(efadat, nfactors = 4, fm="pa", rotate = "oblimin", missing = TRUE)
fa_5 <- fa(efadat, nfactors = 5, fm="pa", rotate = "oblimin", missing = TRUE)
fa_6 <- fa(efadat, nfactors = 6, fm="pa", rotate = "oblimin", missing = TRUE)
fa_7 <- fa(efadat, nfactors = 7, fm="pa", rotate = "oblimin", missing = TRUE)

# Print Factor Loadings
print(fa_4$loadings, cutoff = 0.3)
print(fa_5$loadings, cutoff = 0.3)
print(fa_6$loadings, cutoff = 0.3)
print(fa_7$loadings, cutoff = 0.3)

# Possible Factors Identified ---------------------------------------------
# Factors identified from fa_4:
# - Trust in medical authorities: "Trust doctors/nurses", "Trust CHWs", "Trust HPV info from doctors"
# - Religious influence: "Follow religious vx rec", "Follow religious HPV vx rec", "Trust religious leaders"
# - Concerns about vaccine side effects: "Concern mild effects", "Concern severe effects", "Concern long-term effects"
# - Community influence: "Community parents vxs", "Community parents HPV vxs", "Trust community"

# Factors identified from fa_5:
# - Trust in government: "Trust nat'l gov", "Trust county gov", "Trust HPV info from gov"
# - Personal vaccine attitudes: "Vxs health importance", "Vxs protect child", "Too many vxs"
# - Parental influence: "Discussed HPV with daughter", "Comfort discussing HPV with daughter"
# - Misinformation susceptibility: "HPV vx risks", "Knows someone who declined HPV vx"
# - Social norms: "Discussed HPV with doctor", "Discussed HPV with parents"

# Factors identified from fa_6:
# - Vaccine safety concerns: "Concern mild HPV effects", "Concern severe HPV effects", "Concern long-term HPV effects"
# - Traditional beliefs: "Trust traditional healers", "Follow traditional vx rec", "Follow traditional HPV vx rec"
# - Medical trust: "Trust CHWs", "Trust doctors/nurses", "Trust NGOs"
# - Government messaging: "Trust nat'l gov", "Trust county gov", "Trust HPV info from gov"
# - Parental openness: "Comfort discussing HPV with parents", "Comfort discussing HPV with daughter"
# - Community impact: "Community parents vxs", "Community parents HPV vxs"

# Factors identified from fa_7:
# - Distrust in government: "Trust nat'l gov", "Trust county gov", "Trust NGOs"
# - Religious adherence: "Follow religious vx rec", "Follow religious HPV vx rec"
# - Concerns about vaccination: "Concern mild effects", "Concern severe effects", "Concern long-term effects"
# - Social discussions: "Discussed HPV with doctor", "Discussed HPV with parents"
# - Influence from peers: "Knows someone who declined HPV vx", "Community parents HPV vxs"
# - Doctor recommendations: "Trust doctors/nurses", "Follow doctor HPV vx rec"
# - Alternative medicine: "Trust traditional healers", "Follow traditional HPV vx rec"

# Visualization ----------------------------------------------------------
pdf("my_fa_diagram_4.pdf", height=15, width=30)
fa.diagram(fa_4, cut=0.3, main="EFA_4")
dev.off()
pdf("my_fa_diagram_5.pdf", height=15, width=30)
fa.diagram(fa_5, cut=0.3, main="EFA_5")
dev.off()
pdf("my_fa_diagram_6.pdf", height=15, width=30)
fa.diagram(fa_6, cut=0.3, main="EFA_6")
dev.off()
pdf("my_fa_diagram_7.pdf", height=15, width=30)
fa.diagram(fa_7, cut=0.3, main="EFA_7")
dev.off()

# Factor Loadings Table --------------------------------------------------
fa4_loadings_df <- as.data.frame(fa_4$loadings[, 1:4])
fa4_loadings_df$row_name <- rownames(fa_4$loadings)
fa4_loadings_df <- fa4_loadings_df[, c(ncol(fa4_loadings_df), 1:4)]
fa4_loadings_df <- fa4_loadings_df %>% mutate(across(where(is.numeric), round, digits = 3))

# Display Loadings Table
print(fa4_loadings_df)



## ----fig.height=10--------------------------------------------------------------------
# =============================================================================
# Exploratory Factor Analysis (EFA) with a Single Scree Plot on Page 1
# =============================================================================

# Load required packages

# -------------------------------
# 1. Define variable subsets
# -------------------------------
general_vaccine_vars <- c(
  "sec3_q28", "sec3_q29", "sec3_q30", "sec3_q31", "sec3_q32", "sec3_q33",
  "sec3_q46", "sec3_q47", "sec3_q48", "sec3_q37", "sec3_q38", "sec3_q39",
  "sec3_q40", "sec3_q41", "sec3_q42"
)
trust_vars <- c(
  "sec6_S_q67", "sec6_S_q68", "sec6_S_q69", "sec6_S_q70",
  "sec6_S_q71", "sec6_S_q72", "sec6_q73", "sec6_q74", "sec6_q75", "sec6_q76"
)
hpv_knowledge_vars <- c(
  "Correct_A_q77", "Correct_A_q78", "Correct_A_q79", "Correct_A_q80",
  "Correct_A_q81", "Correct_A_q82"
)
social_processes <- c(
  "sec3_q34", "sec3_q35", "sec3_q36", "sec3_q43", "sec3_q44", "sec3_q45",
  "sec5_q59", "sec5_q60", "sec5_q61", "sec5_q62", "sec5_q63", "sec5_q64",
  "sec5_q66"
)
all_vars <- c(
  general_vaccine_vars,
  trust_vars,
  hpv_knowledge_vars,
  social_processes
)

# -------------------------------------
# 2. Compute correlation matrix once
# -------------------------------------

# Compute correlation matrix for the variables of interest
cor_mat <- cor(dat[, all_vars], use = "pairwise.complete.obs")

# Plot a single scree plot
scree(cor_mat, factors = FALSE, main = "Scree Plot")

# -----------------------------------------------------
# 3. Helper to draw loadings as a two‐column table
# -----------------------------------------------------
plot_loading_table <- function(loadings_df, title_text) {
  n_rows      <- nrow(loadings_df)
  split_index <- ceiling(n_rows / 2)
  first_half  <- loadings_df[1:split_index, ]
  second_half <- loadings_df[(split_index + 1):n_rows, ]
  
  tbl1 <- tableGrob(first_half,  rows = NULL, theme = ttheme_minimal(base_size = 8))
  tbl2 <- tableGrob(second_half, rows = NULL, theme = ttheme_minimal(base_size = 8))
  title_grob <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  grid.arrange(
    title_grob,
    arrangeGrob(tbl1, tbl2, ncol = 2),
    ncol = 1,
    heights = unit.c(
      grobHeight(title_grob) + unit(5, "mm"),
      unit(1, "npc") - grobHeight(title_grob) - unit(5, "mm")
    )
  )
}

# -------------------------------------
# 4. Open a single PDF device
# -------------------------------------
pdf("combined_efa_results.pdf", width = 12, height = 10)

# -------------------------------------
# 5. Draw exactly one scree plot on page 1
# -------------------------------------
# Reset graphic parameters to a single panel
par(mfrow = c(1, 1))
scree(
  cor_mat,
  factors = FALSE,
  f       = TRUE,
  main    = "Scree Plot for All Variables (1–10 Factors)"
)

# ---------------------------------------------------------
# 6. Loop: EFA for factors = 2 to 10, diagram + loadings
# ---------------------------------------------------------
for (nf in 2:10) {
  efa_res <- fa(
    select(dat, all_of(all_vars)),
    nfactors = nf,
    fm       = "pa",
    rotate   = "oblimin",
    missing  = TRUE
  )
  
  # 6.1 Factor diagram
  fa.diagram(
    efa_res,
    main = paste0("Factor Diagram — ", nf, " Factors"),
    cex  = 0.6
  )
  
  # 6.2 Prepare loadings table and draw (no extra blank page)
  loadings_df <- as.data.frame(round(efa_res$loadings[], 2))
  loadings_df <- tibble(Variable = rownames(loadings_df), loadings_df)
  
  plot_loading_table(
    loadings_df,
    paste0("Loadings Table — ", nf, " Factors (cutoff = 0.30)")
  )
}

# -------------------------------------
# 7. Close the PDF device
# -------------------------------------
dev.off()




## -------------------------------------------------------------------------------------
model0 <- '
  # 1) Concerns for Vaccines
  Concerns =~ sec3_q30 + sec3_q31+ sec3_q32 + sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 5) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61 + sec5_q66'



model0.fit <- cfa(model0, data = dat, missing = "fiml")
summary(model0.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model0.fit, sort = TRUE)



## -------------------------------------------------------------------------------------
## Added covariance without removing anything

model1 <- '
  # 1) Concerns for Vaccines
  Concerns =~ sec3_q30+ sec3_q31+ sec3_q32 + sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42;
  
  # 2) Trust
  Trust =~ sec6_S_q67+ sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 5) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66


sec3_q30 ~~ sec3_q31
sec3_q31 ~~ sec3_q32
sec3_q35	~~	sec3_q44
sec3_q34	~~	sec3_q43
sec6_S_q68	~~	sec6_S_q69'

model1.fit <- cfa(model1, data = dat, missing = "fiml")
summary(model1.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model1.fit, sort = TRUE)



## -------------------------------------------------------------------------------------
## Added covariance without removing anything

model2 <- '
  # 1) Concerns for Vaccines
  Concerns =~ sec3_q30+ sec3_q31+ sec3_q32 + sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42;
  
  # 2) Trust
  Trust =~ sec6_S_q67+ sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q29 + sec3_q36 + sec3_q37 + sec3_q38 +sec3_q45;
  
  # 5) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66


sec3_q30 ~~ sec3_q31
sec3_q31 ~~ sec3_q32
sec3_q35	~~	sec3_q44
sec3_q34	~~	sec3_q43
sec6_S_q68	~~	sec6_S_q69'

model2.fit <- cfa(model2, data = dat, missing = "fiml")
summary(model2.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model2.fit, sort = TRUE)


## -------------------------------------------------------------------------------------
## Added covariance without removing anything

model3 <- '
  # 1) Concerns for Vaccines
  Concerns =~ sec3_q30+ sec3_q31+ sec3_q32 + sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42;
  
  # 2) Trust
  Trust =~ sec6_S_q67+ sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~  sec3_q36 + sec3_q37 + sec3_q38 +sec3_q45;
  
  # 5) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66


sec3_q30 ~~ sec3_q31
sec3_q31 ~~ sec3_q32
sec3_q35	~~	sec3_q44
sec3_q34	~~	sec3_q43
sec6_S_q68	~~	sec6_S_q69'

model3.fit <- cfa(model3, data = dat, missing = "fiml")
summary(model3.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model3.fit, sort = TRUE)



## -------------------------------------------------------------------------------------
outcome <- read_dta(file=(".//2022CaseIdAnyHPVDoses.dta"))
head(outcome)


## -------------------------------------------------------------------------------------

dat <- dat %>%
  left_join(outcome %>% select(caseid, AnyHPVdoses), by = "caseid")

if (any(is.na(dat$AnyHPVdoses))) {
  stop("Some Case_id not found")
}


## -------------------------------------------------------------------------------------
##Final Model

model_outcome <- '
  # 1) Concerns for Vaccines
  Concerns =~ sec3_q30+ sec3_q31+ sec3_q32 + sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42;
  
  # 2) Trust
  Trust =~ sec6_S_q67+ sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ +sec3_q29 + sec3_q36 + sec3_q37 + sec3_q38 +sec3_q45;
  
  # 5) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66

  
  AnyHPVdoses ~ Concerns + Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms
                
  sec3_q30 ~~ sec3_q31
  sec3_q31 ~~ sec3_q32
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model_outcome.fit, sort = TRUE)
model_parameters(model_outcome.fit, ci = 0.95, standardize = FALSE, component = c("regression"))
model_parameters(model_outcome.fit, ci = 0.95, standardize = TRUE, component = c("regression"))


## -------------------------------------------------------------------------------------

DiagrammeR("
graph LR;
    Concerns(Concerns for Vaccines) --> Vaccination[HPV Vaccination];
    Trust(Trust) --> Vaccination;
    Recommendation(Leadership Recommendation) --> Vaccination;
    Covid(Covid Changes) --> Vaccination;
    Attitudes(Vaccine Attitudes) --> Vaccination;
    Knowledge(HPV Knowledge) --> Vaccination;
    SocialNorms(Social Norms) --> Vaccination;

    style Concerns fill:#90EE90,stroke:#000,color:#000
    style Trust fill:#90EE90,stroke:#000,color:#000
    style Recommendation fill:#90EE90,stroke:#000,color:#000
    style Covid fill:#90EE90,stroke:#000,color:#000
    style Attitudes fill:#90EE90,stroke:#000,color:#000
    style Knowledge fill:#90EE90,stroke:#000,color:#000
    style SocialNorms fill:#90EE90,stroke:#000,color:#000
    style Vaccination fill:#7DA8B8,stroke:#000,color:#000
")



## -------------------------------------------------------------------------------------
model0_8<- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42'

model0_8.fit <- cfa(model0_8, data = dat, missing = "fiml")
summary(model0_8.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model0_8.fit, sort = TRUE)


## -------------------------------------------------------------------------------------
## Added modification indices Covariates
model1_8<- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61 + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model1_8.fit <- cfa(model1_8, data = dat, missing = "fiml")
summary(model1_8.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model1_8.fit, sort = TRUE)



## -------------------------------------------------------------------------------------

DiagrammeR("
graph LR;
    Concerns(Concerns for Vaccines in general) --> Vaccination;
    Concerns_hpv(Concerns for HPV Vaccines) --> Vaccination;
    Trust(Trust) --> Vaccination;
    Recommendation(Leadership Recommendation) --> Vaccination;
    Covid(Covid Changes) --> Vaccination;
    Attitudes(Vaccine Attitudes) --> Vaccination;
    Knowledge(HPV Knowledge) --> Vaccination;
    SocialNorms(Social Norms) --> Vaccination;

    style Concerns fill:#90EE90,stroke:#000,color:#000
    style Concerns_hpv fill:#90EE90,stroke:#000,color:#000
    style Trust fill:#90EE90,stroke:#000,color:#000
    style Recommendation fill:#90EE90,stroke:#000,color:#000
    style Covid fill:#90EE90,stroke:#000,color:#000
    style Attitudes fill:#90EE90,stroke:#000,color:#000
    style Knowledge fill:#90EE90,stroke:#000,color:#000
    style SocialNorms fill:#90EE90,stroke:#000,color:#000
    
    style Vaccination fill:#7DA8B8,stroke:#000,color:#000
")



## -------------------------------------------------------------------------------------
##Final Model

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61 + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(model_outcome.fit, sort = TRUE)


## -------------------------------------------------------------------------------------
model_parameters(model_outcome.fit, ci = 0.95, standardize = FALSE, component = c("regression"))
model_parameters(model_outcome.fit, ci = 0.95, standardize = TRUE, component = c("regression"))


## -------------------------------------------------------------------------------------
##Final Model + sec3_q33

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms + sec3_q33
                
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)


## -------------------------------------------------------------------------------------
##Final Model + sec5_q60

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61 + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms + sec5_q60
                
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)

## -------------------------------------------------------------------------------------
##Final Model + sec5_q62

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61 + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms + sec5_q62
                
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)

## -------------------------------------------------------------------------------------
##Final Model + sec5_q63

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms + sec5_q63
                
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)


## -------------------------------------------------------------------------------------
##Final Model + sec5_q64

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61 + + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms + sec5_q64
                
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)


## -------------------------------------------------------------------------------------
##Final Model + sec6_q73

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms + sec6_q73
                
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)


## -------------------------------------------------------------------------------------
##Final Model + sec6_q74

model_outcome <- '
  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  AnyHPVdoses ~ general_Concerns + HPV_concerns+ Trust + recommendation + Covid +
                attitudes + HPV_knowledge + Social_norms + sec6_q74
                
  sec3_q35	~~	sec3_q44
  sec3_q34	~~	sec3_q43
  sec6_S_q68	~~	sec6_S_q69
  sec6_q75	~~	sec6_q76
  sec3_q28	~~	sec3_q29
'

model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)


## -------------------------------------------------------------------------------------
model_outcome <- '

  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61  + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42

  AnyHPVdoses ~ HPV_concerns + attitudes + Social_norms
'
model_outcome.fit <- cfa(model_outcome, data = dat, missing = "fiml")
summary(model_outcome.fit, standardized = TRUE, fit.measures = TRUE)

## -------------------------------------------------------------------------------------
# Univariate logits for each singleton item
singletons <- c("sec5_q64", "sec5_q63", "sec6_q73", "sec6_q74",
                "sec3_q33", "sec5_q60", "sec5_q62")

univ_fits <- lapply(singletons, function(item) {
  glm(as.formula(paste0("AnyHPVdoses ~ ", item)),
      family = binomial(link = "logit"),
      data   = dat)
})
names(univ_fits) <- singletons

lapply(univ_fits, summary)

## -------------------------------------------------------------------------------------
# 16-predictor SEM: 8 latents + 8 singleton items
model16 <- paste0(model0_8, "

  # 1) Concerns for Vaccines
  general_Concerns =~ sec3_q30 + sec3_q31+ sec3_q32;
  
  # 2) Trust
  Trust =~ sec6_S_q67 + sec6_S_q68 + sec6_S_q69 + sec6_S_q70 + sec6_S_q71 + sec6_S_q72 + sec6_q75 + sec6_q76;
  
  # 3) leadership recommendation
  recommendation =~ sec3_q34 + sec3_q35 + sec3_q43 + sec3_q44;
  
  # 4) Covid Changes
  Covid =~ sec3_q46 + sec3_q47 + sec3_q48;
  
  # 5) Vaccines Attitude
  attitudes =~ sec3_q28+sec3_q29+sec3_q36+sec3_q37+sec3_q38+sec3_q45;
  
  # 6) HPV knowledge
  HPV_knowledge =~ Correct_A_q77 + Correct_A_q78 + Correct_A_q79 +
                   Correct_A_q80 + Correct_A_q81 + Correct_A_q82 
                   
                   
  # 7) Social norms
  Social_norms =~ sec5_q59 + sec5_q61 + + sec5_q66
  
  # 8) Concerns for HPV vaccines
  HPV_concerns =~  sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  
  sec3_q35 ~~ sec3_q44
  sec3_q34 ~~ sec3_q43
  sec6_S_q68 ~~ sec6_S_q69
  sec6_q75 ~~ sec6_q76
  sec3_q28 ~~ sec3_q29
  
  # regressions on outcome
  AnyHPVdoses ~ general_Concerns + Trust + recommendation +
                 Covid + attitudes + HPV_knowledge +
                 Social_norms + HPV_concerns +
                 sec5_q64 + sec5_q63 + sec6_q73 + sec6_q74 +
                 sec3_q33 + sec5_q60 + sec5_q62
")

fit16 <- sem(model16,
             data    = dat,
             missing = "fiml",
             fixed.x = FALSE)
summary(fit16, standardized = TRUE, fit.measures = TRUE)



## -------------------------------------------------------------------------------------
model_parameters(fit16, ci = 0.95, standardize = FALSE, component = c("regression"))
model_parameters(fit16, ci = 0.95, standardize = TRUE, component = c("regression"))

## -------------------------------------------------------------------------------------
modificationindices(fit16, sort = TRUE)


## -------------------------------------------------------------------------------------
# 1) Define the 3-factor + 1-singleton SEM
model3_1 <- '
  # latent factors
  Social_norms  =~ sec5_q59 + sec5_q61 + sec5_q66
  HPV_concerns  =~ sec3_q39 + sec3_q40 + sec3_q41 + sec3_q42
  attitudes     =~ sec3_q28 + sec3_q29 + sec3_q36 + sec3_q37 + sec3_q38 + sec3_q45
  sec3_q28	~~	sec3_q29
  # regressions on the binary outcome
  AnyHPVdoses ~ Social_norms + HPV_concerns + attitudes + sec5_q64
'

# 2) Fit the model (with FIML for missing data)
fit3_1 <- sem(model3_1,
            data    = dat,
            missing = "fiml",
            fixed.x = FALSE)

# 3) Show a full summary with fit measures and standardized estimates
summary(fit3_1,
        standardized = TRUE,
        fit.measures = TRUE)

# 4) Extract just the regression parameters (with 95% CIs and standardized effects)
model_parameters(fit3_1,
                 ci         = 0.95,
                 standardize = FALSE,
                 component   = "regression")
model_parameters(fit3_1,
                 ci         = 0.95,
                 standardize = TRUE,
                 component   = "regression")


## -------------------------------------------------------------------------------------
modificationindices(fit3_1, sort = TRUE)

