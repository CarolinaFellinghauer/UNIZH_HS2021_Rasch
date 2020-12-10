#install and load following R-packages  
library(eRm)
library(iarm)





## ---- dataload and analysis--
# get the SRG dataset from GitHub

urlfile = $$$

srg.data=$$$

dim(srg.data)
colnames(srg.data)

srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", "SRG13", "SRG14", "SRG15")


# dataset with only the SRG items
data.srg=srg.data[,srg.items]

#check response coding (frequencies including missing values) for each SRG-item
apply(data.srg, 2, table, useNA="always")




## ---- Thresholds

PCM.srg <- $$$
Thres.srg <- $$$(PCM.srg)
Thres.srg



## --Location - the item difficulty = mean of the thresholds
$$$$

#or

$$$$

#mean of thresholds should correspond to the location in table


## ----thres_matrix--

names(Thres.srg) 

Thres.srg$$$

#to have a matrix format type:
Thres.srg$$$$

#to remove the locations type:
Thres.srg$$$$$




## -check order by rows
apply($$$$, $$$, $$$, decreasing = FALSE)


## ----download_thresmap_from_url------
thres_map_fct = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/RFunctions/threshold_map_fct.r"

source(url(thres_map_fct))


## ----threshold_map--

ThresholdMap($$$$)



## ---- person item thresholds-

$$$
  
##more about the interpretation of this plot in the workbook


## ---- ICC plot ordered example for item 5-

plotICC($$$, item.subset =$$$ ,  xlab = "SRG 5", ylab = "Probability", 
        legpos = FALSE, #no legend here
        ask = FALSE)  #for several plot, R interactive can make sense




## ----item recode---------------------------------------------------------------------------
#example of collapsing code
#let's imagine SRG-8 would have had disordered thresholds...

#if the analysis of thresholds would have shown that options 2 and 1 are reversed 
#response options 2 and 1 could be collapsed

$$$$

#frequency tables per column should show that SRG8 has now only 2 responses left
apply(data.srg, $$, $$, useNA="always")



#having done that the PCM analysis and diagnostics are simply re-run on the new sample

PCM.srg_rec=$$$
plotPImap(PCM.srg_rec, sorted = TRUE, main="SRG - short form")




