

## ---- 
library(eRm)

#load the LIDgraph function and source it to be able to run LIDGraphs.
urlfunction = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/RFunctions/LIDGraph.R"

$$$
        


## ----MDS Data-------------------------------------------------------------------------------------------------------

# get the MDS dataset from GitHub

$$$
  
MDS.data="https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/Data/WHO_MDS_course.csv"

# Select only the MDS items, these are all columns except the first four. 
# It starts with vision.

mds.items = colnames(MDS.data)[-(1:4)]
# data only with the items
data.mds = MDS.data[, mds.items]
# make them start at zero
data.mds = $$$$


## ----testlet12------------------------------------------------------------------------------------------------------
#
# Testlets can be created to solve LID, this consists in summing up correlated items.
# Find which items correlated highest in the exercise,
# and create testlets.

colnames(data.mds)

#testlet for Depressivity & Anxiety
data.mds[, "Testlet1"] = rowSums(data.mds[,c("Depressivity", "Anxiety")], na.rm = TRUE)

# other ways to sum up items
# this is same as 
# data.mds[, "Testlet1"] = data.mds[,"Depressivity"] + data.mds[, "Anxiety"] 
# or
# data.mds[, "Testlet1"] = apply(data.mds[,c("Depressivity", "Anxiety")], 1, sum, na.rm = TRUE)


#second testlet GoingOut and Shopping
data.mds[, "Testlet2"] = $$$$

# having a look at the frequency distribution in the testlet

$$$
$$$

# Now that the items are aggregated into testlets, the single items need to be removed before analysis.
# They are probably fancier ways to do it - thinking dplyr or else...

data.mds=data.mds[,-which(colnames(data.mds) %in% c("Depressivity", "Anxiety", "GoingOut", "Shopping")==TRUE)]

# check if testlet creation and removal of the LID items worked
# table of data.mds by column with apply

$$$

dim(data.mds)


## ----PCM of the data with the 2 testets

PCM.mds.testlet1 = $$$



## ----Make an LIDgraph with a cut-off of 0.7_LID

$$$



## ----Make an LIDgraph with a cut-off with the Q3* method

LIDgraph($$$)


## ----More testlets.. 
## residual correlation > 0.4

#what was the name of the variables...
colnames(data.mds)

#testlet for Toileting & Bed
$$$
  
#second testlet Feeding & Toileting
$$$

# having a look at the frequency distribution in the testlet

$$$
$$$

# Removing the 3 items from the dataset before running the PCM

$$$

# check if testlet creation and removal of the LID items worked
# table by column

apply(data.mds, 2, table, useNA = "always")

dim(data.mds)


## ----PCM2: another PCM with the new testlets

PCM.mds.testlet2 = $$$$



## ----LID with LIDgraph Q3* method

$$$

## --- Conclusion: this is a $$$ scale where the LID cannot be solved easily. 
  ## The scale may be multidimensional.
  ## The response options are known to be problematic. 
  ##  We stop here.
