
## ---- load libraries---------------

library(eRm)
library(plotrix)
library(rgl) #library for 3-dimensional plotting functions



## ----data srg------------------------

urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/Data/SRG_Data_Course_UNIZH.csv?token=AB5GB47UIUWV7F5NMGA33T27K5IQ2"

srg.data=read.csv(url(urlfile))
srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", "SRG13", "SRG14", "SRG15")

# dataset with only the SRG items
data.srg=srg.data[,srg.items]

#check response coding (frequencies including missing values) for each SRG-item
apply(data.srg, 2, table, useNA="always")

#PCM analysis, residuals -------------------------------
PCM.srg=$$
PP.srg=$$
resid.srg=$$



## eigenvector and eigenvalue----
cor.srg = $$
PCA.srg = $$
Eigen.Vector.srg = round(PCA.srg$vectors,3)
colnames(Eigen.Vector.srg ) = paste("PC", 1:ncol(data.srg), sep = "")
rownames(Eigen.Vector.srg ) = $$

Eigen.Vector.srg


## eigenvalue table------------

Eigen.Value.srg = $$

Perc.Eigen.srg = Eigen.Value.srg/sum(Eigen.Value.srg)*100 
Cum.Perc.Eigen.srg = $$(Perc.Eigen.srg)
PCA.Table.srg = cbind($$,$$,$$)

PCA.Table.srg


## ----2_dimensional ----

##analysis of the PCA loading 
plot($$, $$, xlab="1st component", ylab="2nd component", main="SRG Item PCA-Loading")


###add labels to identify the item loading locations
plot($$, $$, xlab="1st component", ylab="2nd component", main="SRG Item PCA-Loading", col="white")
text($$, $$, colnames(data.srg), cex=0.7)

segments($$, col = "red", lty = "dotted")
segments($$, col = "orange", lty = "dotted")



## ----3_dimensional------------------
options(rgl.printRglwidget = TRUE)
plot3d(Eigen.Vector.srg[, "PC1"], Eigen.Vector.srg[, "PC2"], Eigen.Vector.srg[, "PC3"], xlab = "PC1", ylab = "PC2", zlab = "PC3",
       type = "s", radius = 0.00, col="white")

text3d($$, $$, $$, texts = rownames(Eigen.Vector.srg))



## ----PCA Barplot Screeplot, fig.cap = "Figure 6"------------------------
barplot($$, main="Screeplot", ylim = c(0, 2.5))

segments($$, col = "red", lty = "dotted") #add line for the common cut-off at 2
segments($$, col = "blue", lty = "dotted") #add line for Smith and Miao cut-off

