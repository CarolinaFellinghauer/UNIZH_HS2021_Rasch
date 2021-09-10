
## ----SRG data-----------------------------------------------------------------------------
library(iarm)

urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/Data/SRG_Data_Course_UNIZH.csv?token=AB5GB47UIUWV7F5NMGA33T27K5IQ2"

srg.data=read.csv(url(urlfile))

dim(srg.data)
colnames(srg.data)

srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6", 
            "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", 
            "SRG13", "SRG14") # minus "SRG15"


# dataset with  SRG items and the person factors
data.srg = srg.data[,srg.items]



## ----Second Rasch analysis
library(eRm)
library(iarm)

PCM.srg.2 = PCM(data.srg[,srg.items], sum0 = TRUE)

#Targeting and reliability
scale.properties = test_prop(PCM.srg.2)
scale.properties[c(1,3)]

#Person-Item Map
plotPImap(PCM.srg.2, sort = TRUE, main = "SRG-metric")

PP.srg.2 = person.parameter(PCM.srg.2)
resid.srg.2 = residuals(PP.srg.2)

#Item Fit
eRm::itemfit(PP.srg.2)

#LID - local item dependencies
cor.resid.srg.2 = cor(resid.srg.2, use = "pairwise.complete.obs")

cor.resid.srg.2.tri = cor.resid.srg.2
cor.resid.srg.2.tri[upper.tri(cor.resid.srg.2.tri, diag = TRUE)] = NA

which(cor.resid.srg.2.tri > 0.2, arr.ind = TRUE)

#PCA  eigenvalues 
eigen(cor.resid.srg.2)$values

#thresholds
thres_map_fct = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/RFunctions/threshold_map_fct.r"

source(url(thres_map_fct))

ThresholdMap(thresholds(PCM.srg.2))

## ----transformation table
library(scales)

names(PP.srg.2)

T.Table = as.data.frame(cbind(PP.srg.2$pred.list[[1]]$x, PP.srg.2$pred.list[[1]]$y))
colnames(T.Table) = c("Row Score", "Logit Score")

#create a rescaled Rasch-Score in a convenient range, here from 0 to 100
Transformed_Score = scales::rescale(T.Table[,2], to = c(0, 100))

T.Table = cbind(T.Table, Transformed_Score)
colnames(T.Table) = c("Row Scores", "Logit Scores", "0-100 Scores")


#round to the second decimals of the two last columns
T.Table[,c(2,3)] = round(T.Table[, c(2,3)], 2)


T.Table






