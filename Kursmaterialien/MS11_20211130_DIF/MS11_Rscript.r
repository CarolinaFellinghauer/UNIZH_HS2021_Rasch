#install and load following R-packages 
library(eRm)
library(iarm)
library(PP)



## Data Load

urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2021_Rasch/main/Data/SRG_Data_Course_UZH_HS2021.csv"

srg.data = read.csv(url(urlfile))

dim(srg.data)
colnames(srg.data)

#Items SRG 1 to 15
srg.items = paste("SRG", 1:15, sep = "")


#create age groups based < 30 < 45 < 60 < using cut()
#the break choice here is, admittedly, a bit random

#only adult participants
srg.data = srg.data[-which(srg.data$Age < 18),]

srg.data[, "Age_grp"] = cut(srg.data$Age, breaks = c(0,30,45,60, 85))

# exogenous variables to test DIF for
srg.pf = c("Age", "Age_grp", "Gender", "Completeness", "para.tetra_1", "traumatic_nontraumatic")

# dataset with  SRG items and the person factors
data.srg = srg.data[,c(srg.items, srg.pf)]

#check response coding (frequencies including missing values) for each SRG-item
#apply(data.srg, 2, table, useNA="always")

#recode traumatic_nontraumatic = 3 to NA

data.srg[which(data.srg$traumatic_nontraumatic == 3),"traumatic_nontraumatic"] = NA 


## ANOVA Funktion---------------
urldif <- "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/RFunctions/anova_DIF.r"

source(url(urldif))

## DIF for Gender : ANOVA of residuals----------------
DIF.srg.gender = anova_DIF(srg.data[,srg.items],
                    srg.data$Gender, 
                    model = "PCM",
                    nci = 9,  #number of score class interval
                    p.adj= "BH") #correction for multiple testing - advice Benjamini Hochberg
DIF.srg.gender


## DIF for para_tetra : ANOVA of residuals--------------------

DIF.srg.level = anova_DIF(d$$) #correction for multiple testing
DIF.srg.level


## ----ICC plot SRG12-----------------------------------
ICCplot(data = srg.data[, srg.items], itemnumber = 12, 
        method = "cut", cinumber = 9, dif = "yes", difvar =    as.factor(srg.data$Gender), diflabels = c("Male", "Female"))


## ----ICC plot SRG10--------------------------------------------------------------------------
ICCplot(data = srg.data[, srg.items], itemnumber = 10, 
        method = "cut", cinumber = 9, dif = "yes", difvar =    as.factor(srg.data$para.tetra_1), diflabels = c("Paraplegia", "Tetraplegia"))


## ----Item Split--------------------------------------------------------------------------------------------
#example of item split using the item 10 which showed very strong DIF for injury level (paraplegia and tetraplegia).

#here two new variables are created
#one for traumatic and one for non-traumatic injuries
#for each variable the options for the other injury group are set to missing.
#1 = para, 2 = tetra

srg.data[, "SRG10_P"] = $$
srg.data[which(srg.data[,"para.tetra_1"] == $$), "SRG10_P" ] = $$

srg.data[, "SRG10_T"] = $$
srg.data[$$, "SRG10_T" ] = NA


#now instead one SRG10 item, there are two, one for each level of the injury etiology variable.

#the item SRG10 has to be removed or the list of SRG items has to be redefined.

srg.items = c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10_P", "SRG10_T", "SRG11", "SRG12", "SRG13", "SRG14", "SRG15")

head(srg.data[, srg.items])

## ----rasch and person map-----------
$$

