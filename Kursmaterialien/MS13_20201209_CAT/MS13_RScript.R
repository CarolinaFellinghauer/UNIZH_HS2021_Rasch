
## ----SRG data-----------------------------------------------------------------------------

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

PCM.srg.2 = PCM(data.srg[,srg.items], sum0 = TRUE)

plotPImap(PCM.srg.2, sort = TRUE, main = "SRG-metric")

PP.srg.2 = person.parameter(PCM.srg.2)

SepRel(PP.srg.2)

itemfit(PP.srg.2)

resid_srg.2 = residuals(PP.srg.2)


urlfunction = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/RFunctions/LIDGraph.R?token=AB5GB42GG6I6YI2MF3KDINC7K5JG6"

source(urlfunction)

LIDgraph(PCM.srg.2, cut = 0.2, vertex.color = "pink", vertex.size = 40,
         vertex.label.dist = 0)

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




## ----CAT_infographics, echo = FALSE, fig.cap = "Boston University: School of Public Health"----

knitr::include_graphics("CAT-Infographic.png")


