## Load eRm

$$$


## dataload and analysis
  # get the SRG data set from GitHub

urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/Data/SRG_Data_Course_UNIZH.csv"

srg.data = $$$

dim(srg.data)
colnames(srg.data)

srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", "SRG13", "SRG14", "SRG15")

# dataset with only the capacity items
data.srg=srg.data[,srg.items]



## ----PCM_start and person parameter

PCM.srg = $$$
PP.srg = $$$  



## ----Q3, warning = FALSE-----------------------------------------------------------------------

resid.srg = $$$ # Calculates the Rasch residuals.
d$$(resid.srg) # number of rows (persons) and number of columns(items) for the residuals



## ----extremes----------------------------------------------------------------------------------
Rows=rownames(resid.srg) # gives the original rownumber preceded with a P

# extracting the rownumber of the excluded persons
extrm=which(1:nrow(data.srg)%in%as.numeric(substr(Rows, 2,nchar(Rows)))==FALSE) 
data.srg[extrm,] # we see these are only persons with 0 or 2 answers
                  #minimum or maximum score


## ----correlations of the items - method spearman and use pairwise.complete.obs

cor.items.srg = $$$($$$) # item correlations

round(cor.items.srg, digits=2) # rounding to 2 decimals



## ----correlation of the standardized residuals

cor.resid.srg = $$$ # residual correlations

round(cor.resid.srg, digits=3) # rounding

#corrplot(cor.resid.srg) #displays graphically the correlation matrix



## ----code_LID----------------------------------------------------------------------------------


cor.resid.tri = cor.resid.srg  #to avoid overwriting the original matrix

# setting values of the lower triangle and the diagonal to missing with lower.tri
cor.resid.tri[$$$] = NA 

# gives the pairwise correlations
which(cor.resid.tri > 0.2, arr.ind = TRUE)


## ----residual_correlation_plot- set cut-off to 0.2

#put raw path to the LIDgraph function in Github

urlfunction = $$$

source(urlfunction)

$$$$($$$, cut =$$$, vertex.color = "pink", vertex.size = 40,
         vertex.label.dist = 0)




## ---find cut-off value using the mean of the cut-off + 0.2

#selecting the largest Q3 value
Q3.max.srg = max(cor.resid.srg[(cor.resid.srg < 1)])
Q3.max.srg #shows the largest Q3 value


Q3.average.srg = $$$ # calculates the average residual                                                           # correlation (without the diagonal)
Q3.average.srg #shows the average residual correlation

#calculates the difference between the largest Q3 value and the average Q3n; 
#if this value is bigger than .2 this would indicate presence of LID

Q3.star.srg = $$$
Q3.star.srg  



## ----mean_as_cut_off---------------------------------------------------------------------------

# using the mean as cut-off

cut.off.Q3 = Q3.average.srg + 0.2



which($$$ > $$$, arr.ind = TRUE)

## ----mean_as_cut_off_figure, warning = FALSE, message = FALSE----------------------------------

LID_Q3cutoff=LIDgraph($$$, cut = $$$, 
                      vertex.color = "pink", vertex.size = 40,
                        vertex.label.dist = 0, print.out = TRUE)

LID_Q3cutoff$cut.off

LID_Q3cutoff$LID



## ----mean_as_cut_off_2-

# In principle, it is not necessary to calculate that cut-off as it is the default setting in the LIDgraph
#check the function -> name of the function without the brackets.


LIDgraph(PCM.srg, cut = "Q3star", vertex.color="pink", vertex.size=40,
         vertex.label.dist = 0, print.out = TRUE)


LIDgraph(PCM.srg, vertex.color="pink", vertex.size=40,
         vertex.label.dist = 0, print.out = TRUE)



