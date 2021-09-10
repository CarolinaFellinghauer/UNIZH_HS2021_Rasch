
## ----libraries, warning = FALSE, message = FALSE----------------------------------------
library(mirt)


## Load the MDS-Data
urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2021_Rasch/main/Data/WHO_MDS_course.csv"
MDS.data=read.csv(url(urlfile), sep= ";")
dim(MDS.data)
colnames(MDS.data)

# Select only the MDS items, these are all columns except the first four. 
# It starts with vision.
mds.items=colnames(MDS.data)[-(1:4)]
data.mds=MDS.data[, mds.items]
data.mds=data.mds-1


## 1) Unidimensional Rasch Analysis

mds.1dim = mirt(data.mds, 1, itemtype = "Rasch", verbose = FALSE)


## PCA of the residuals correlations
cor.mds = residuals(mds.1dim, type = "Q3", verbose = FALSE) #gives Q3 the correlation of the standardized residuals

PCA.mds = eigen(cor.mds)  #PCA

PC1.neg = which(PCA.mds$vectors[,1] < 0) # negative sign on PC1
PC1.pos = which(PCA.mds$vectors[,1] >0) # positive sign on PC1


## Extract the coefficients
 coef.mds.1dim = coef(mds.1dim, simplify = TRUE)
 coef.mds.1dim


## Anchoring the items loading negatively

 #First run a Rasch analysis of the negatively loading items, as usual, but with option pars = "values"

  mod.mds.dim1 = mirt(data.mds[, PC1.neg], 1, itemtype = "Rasch", pars = "values") 

  #get the coefficient matrix and replace the values with those from the common calibration
  #to fix the first ,second, third, and fourth threshold.

  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d1"), "value"] = coef.mds.1dim$items[PC1.neg,"d1"]
  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d2"), "value"] = coef.mds.1dim$items[PC1.neg,"d2"]
  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d3"), "value"] = coef.mds.1dim$items[PC1.neg,"d3"]
  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d4"), "value"] = coef.mds.1dim$items[PC1.neg,"d4"]
  
  #finally set this, otherwise the anchored start values will not be kept fixed..
  mod.mds.dim1$est = FALSE
     
     
## Do the same with the items loading positively

  mod.mds.dim2 = mirt(data.mds[, PC1.pos], 1, itemtype = "Rasch", pars = "values") 
  
  #get the coefficient matrix and replace the values with those from the common calibration
  #to fix the first ,second, third, and fourth threshold.
  
  mod.mds.dim2[which(mod.mds.dim2[ ,"name"]=="d1"), "value"] = coef.mds.1dim$items[PC1.pos,"d1"]
  mod.mds.dim2[which(mod.mds.dim2[ ,"name"]=="d2"), "value"] = coef.mds.1dim$items[PC1.pos,"d2"]
  mod.mds.dim2[which(mod.mds.dim2[ ,"name"]=="d3"), "value"] = coef.mds.1dim$items[PC1.pos,"d3"]
  mod.mds.dim2[which(mod.mds.dim2[ ,"name"]=="d4"), "value"] = coef.mds.1dim$items[PC1.pos,"d4"]
  
  #finally set this, otherwise the anchored start values will not be kept fixed..
  mod.mds.dim2$est = FALSE
  


## Run the anchored analysis by setting pars = to the parameter estimates fixed previously
   
   mod.PCM.dim1 = mirt(data.mds[,PC1.neg], 1, itemtype = "Rasch", pars = mod.mds.dim1) #anchored analysis
   
   mod.PCM.dim2 = mirt(data.mds[,PC1.pos], 1, itemtype = "Rasch", pars = mod.mds.dim2) #anchored analysis



## Extract the Theta estimates including the SE using fscores()

Theta.dim1 = fscores(mod.PCM.dim1, full.scores.SE = TRUE)
Theta.dim2 = fscores(mod.PCM.dim2, full.scores.SE = TRUE)



## Making a scatterplot: thetas dim 1 versus thetas dim 2

plot(Theta.dim1[,"F1"], Theta.dim2[,"F1"],
     xlab = "Dim 1: Abilities for items PC1 negative loading",
     ylab = "Dim 2: Abilities for items PC1 positive loading",
     main = "Ability Estimates per Dimension",
     pch = 20, 
     col = "darkgrey")

#Trace a diagonal 

segments(x0=-2, y0 = -2, x1 = 2, y1 = 2, col = "red")


## Make the individual pairwise t-tests based on the previous formula

   T_test_abs = abs(Theta.dim1[,"F1"]- Theta.dim2[,"F1"])/sqrt(Theta.dim1[,"SE_F1"]^2 + Theta.dim2[, "SE_F1"]^2)



## Proportion above 2.5

sum(T_test_abs>2.5)/length(T_test_abs) * 100


