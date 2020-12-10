###The syntax has gaps, indicated by $$$ which have to be filled. 

##MOST BASIC RASCH FUNCTIONS TO START WITH:

#########################################
#Dichotomous Rasch Model

#The most common Rasch model is the model for dichotomous items.

#install the package eRm and load it

i$$$
$$$$


# simulate 10 dichotomous items for 500 persons with sim.rasch
# set the seed to 1234

Data_Dicho <- sim.rasch($$$)


#run a Rasch model on Data_Dicho

RM_Dicho <- $$$
print(summary(RM_Dicho))


#plot a Person-Item Map and sort the responses by increasing difficulty

$$$

############################################################################3
####Polytomous Rasch Model

#install and load the package psych

$$$
  
#here a function to simulate polytomous data
  
set.seed(9876)  # reproducibility
Data_Poly <- sim.poly.npl(nvar = 8, n = 500, low = -4, high = 4, a = rep(1, 8), c = 0, 
                          z = 1, d = NULL, mu = 0, sd = 1.96, cat = 3)$items  
  
  
#######################################  
#Rating Scale Model

#run a Rating Scale Model with Data_Poly

RSM_Poly <- $$$
summary(RSM_Poly)



#########################################
#Partial Credit Model

PCM_Poly <- $$$
summary(PCM_Poly)



#####################################
#Loading a Function ThresholdMap from Github

thres_map_fct <- "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/RFunctions/threshold_map_fct.r"

source(url(thres_map_fct))

#install package tibble

$$$

##Run the ThresholdMap() function for the thresholds of RSM_Poly and PCM_Poly 
#(check function thresholds()). 

ThresholdMap($$$)
ThresholdMap($$$)

#What do we see?
  
###Comparing the fit of the RSM against the PCM with ANOVA

anova(RSM_Poly, PCM_Poly)

