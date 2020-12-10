# install and load following R-packages
library(eRm)
library(iarm)


#Load the data from Github (select raw and copy the link) using read.csv(url())

urlfile <- $$$
  
srg.data <- read.csv(url(urlfile))

dim(srg.data)


srg.items <- c("SRG1", "SRG2", "SRG3", "SRG4", "SRG5", "SRG6", "SRG7", "SRG8", "SRG9", 
               "SRG10", "SRG11", "SRG12", "SRG13", "SRG14", "SRG15")


# dataset with only the SRG items
data.srg <- srg.data[, srg.items]

# check response coding (frequencies including missing values) for each SRG-item
apply(data.srg, 2, table, useNA = "always")


###Calculating item fit with package eRm requires the function itemfit(), 
##which uses the output of the person.parameter() function.

# starting with the PCM analysis

PCM.srg <- $$$
  
# first apply the person.parameter from the PCM run previously

PP.srg <- $$$
  
#having a look at the expected response probabilities and the residuals is possible with:

pmat($$$)
residuals($$$)
  
  
# second apply itemfit()  
  
Fit <- $$$
Fit

#Do all items fit? 


#Let's look at ICCplots to learn more about underfit and overfit
# ICC plot is found in the package iarm

#first install and then load the package iarm if you have not done so yet.


#draw an ICC plot for data.srg and the first item, use method = "score"

ICCplot($$$, itemnumber = , method = "score")


#same using the method = "cut" and cinumber = 8

ICCplot($$$, $$$, $$$, $$$)


#now for item 7 with method = "score" and method "cut", using cinumber = 8


#now for item 15 with method = "score" and method "cut", using cinumber = 8


#it is maybe difficult to see, but overfit and underfit could
#be dectected with these curves.

  
  
  


