# get the MDS dataset from GitHub

library(mirt)
library(stats4)
library(rgl)

# get the MDS dataset from GitHub

urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/Data/WHO_MDS_course.csv?token=AB5GB46B55WMO5AU74G4CKK7K5JSG"

MDS.data=read.csv(url(urlfile), sep = ";")
colnames(MDS.data)

# Select only the MDS items, these are all columns except the first four. 
# It starts with vision.

mds.items=colnames(MDS.data)[-(1:4)]

data.mds=MDS.data[, mds.items]
data.mds=data.mds-1



## Unidimensional_Rasch_Model with mirt

mds.1dim = mirt($$$, $$$, itemtype = $$$, verbose = FALSE, $$$)

## mirt make a prcomp of the Q3 (residual() correlation)

cor.mds = r$$$($$$, type = $$$, verbose = FALSE) #gives the correlation of the standardized residuals

PCA.mds = $$$($$)  #eigendecomposition

#find the rows of PC1 which load negatively
which($$$) # negative sign on PC1

#find the rows of PC1 which load positively

which($$$) # positive sign on PC1


##Fixing slopes
##the multidimensional model does not exist for Rasch but for 2PL model, where the
##slopes can be fixed to 1.
##how to set which items are in which dimension and how to set the slopes fixed at one
##assumptions that the first (F1) and second (F2) dimensions are not independent COV = .


 spec1 <- "
              F1 = 1, 2, 4, 6, 12, 13, 14, 18, 19, 20, 22
              F2 = 3, 5, 7, 8, 9, 10, 11, 15, 16, 17, 21
              START = (1, 2, 4, 6, 12, 13, 14, 18, 19, 20, 22, a1, 1.0)
              START = (3, 5, 7, 8, 9, 10, 11, 15, 16, 17, 21, a2, 1.0)
              FIXED = (1, 2, 4, 6, 12, 13, 14, 18, 19, 20, 22, a1)
              FIXED = (3, 5, 7, 8, 9, 10, 11, 15, 16, 17, 21, a2)
              FREE = (GROUP, COV11)
              FREE = (GROUP, COV22)
              COV = F1*F2 "
  


## run the model with the specifications above. 

mds.2dim = mirt(data.mds, model = spec1, itemtype = "gpcm", verbose = FALSE)



## compare the two models 1dim against 2dim with anova

anova($$$, $$$, verbose = FALSE)


##The change is highly significant. Let's obsever the three first components and make
##assumptions of potential 3 dimensional conceptualization.
## 

PCA.mds$vectors[, c(1:3)]


## 2_dimensional_plot of PC1 and PC2 $
##with labels for better insight

Dataset=as.data.frame(PCA.mds$vectors)
colnames(Dataset) = paste("PC", 1:ncol(Dataset), sep = "")
rownames(Dataset) = mds.items


plot($$, $$, xlab="1st component", ylab="2nd component", main="MDS Item PCA-Loading", col="white")
text($$, $$, labels = , cex=0.7)



##3-Dimensional - plot with dots almost invisible or invisible to show the labels
##with text3d()


options(rgl.printRglwidget = TRUE)   
plot3d($$$, $$$, $$$, 
         xlab = $$$, ylab = $$$, zlab = $$$,
         type = "s", radius = 0.00, col="white")
text3d($$$, $$$, $$$, texts = $$$))



## another suggested conceptualization 
## Invitation to make it better.


 spec2 <- "
              F1 = 4, 6, 12, 20
              F2 = 1, 2, 13, 14, 18, 19, 22
              F3 = 3, 5, 7, 8, 9, 10, 11, 15, 16, 17, 21
              START = (4, 6, 12, 20, a1, 1.0)
              START = (1, 2, 13, 14, 18, 19, 22, a2, 1.0)
              START = (3, 5, 7, 8, 9, 10, 11, 15, 16, 17, 21, a3, 1.0)
              FIXED = (4, 6, 12, 20, a1)
              FIXED = (1, 2, 13, 14, 18, 19, 22, a2)
              FIXED = (3, 5, 7, 8, 9, 10, 11, 15, 16, 17, 21, a3)
              FREE = (GROUP, COV11)
              FREE = (GROUP, COV22)
              FREE = (GROUP, COV33)
              COV = F1*F2, F1*F3, F2*F3"
  


## Run the 3 dimensional Rasch-model with model specification spec2
mds.3dim = mirt($$$, model = $$$, itemtype = $$$, verbose = FALSE)



## Compare the 2 dimensional and the 3 dimensional model
anova($$$, $$$)


##  mirt also gives the PSI - reliability of the different models
empirical_rxx(fscores(mds.1dim, full.scores.SE = TRUE))
empirical_rxx(fscores(mds.2dim, full.scores.SE = TRUE))
empirical_rxx(fscores(mds.3dim, full.scores.SE = TRUE))


## item fit
itemfit($$ fit_stats = "infit")
itemfit($$, fit_stats = "infit")
itemfit($$, fit_stats = "infit")

