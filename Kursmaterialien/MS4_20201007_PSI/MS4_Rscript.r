


#install and load following R-packages  
library(eRm)
library(iarm)



## SRG Data
# get the SRG dataset from GitHub

urlfile = $$$

srg.data=read.csv(url(urlfile))

dim(srg.data)
colnames(srg.data)

srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", "SRG13", "SRG14", "SRG15")

# dataset with only the SRG items
data.srg=srg.data[,srg.items]

#check response coding (frequencies including missing values) for each SRG-item
apply(data.srg, 2, table, useNA="always")



## ----PCM and targeting

#starting with the PCM analysis
PCM.srg = $$$


##Find the difficulty parameter of the items (=Location in eRm) and calculate 
##for the mean location of the item difficulties use the parameterization found in the thresholds function
#and compute the mean location
  
  
thresh.srg = thresh$$$

# names(thresh.srg)

#provides the locations
i.dif.srg  = thresh.srg$$$
mean.dif.srg = mean($$$
sd.dif.srg = $$$


##Find the ability of the persons and calculate the mean and sd 

pp.srg <- $$$
abil.srg <- pp.srg$theta.table[,"Person Parameter"]
mean.abil.srg<-$$$
sd.abil.srg<-$$$

##rounded mean and sd difficulties and abilities
summary.targeting.srg <- c(Mean_Difficulty=$$$, SD_Difficulty=$$$, 
                    Mean_Abilities=$$$, SD_Abilities=$$$)


round(summary.targeting.srg,3)



## ----person item map to check the targeting

$$$


##Person Separation Reliability: 
##show the function code by typing the name of the function without brackets

$$$

#SSD.PersonScores <- var(PersonScores)
#MSE <- sum((StandardErrors)^2)/length(StandardErrors)
#separation.reliability <- (SSD.PersonScores - MSE)/SSD.PersonScores

#note that (SSD-MSE)/SSD = SSD/SSD - MSE/SSD = 1 - MSE/SSD


## ----person separation of the SRG

$$$



## ----reliability

##check the data distribution.
##load iarm

round(test_prop(PCM.srg),3)



## ----information_graphic "from scratch" 
#x-axis the theta levels
#y-axis 1/ThetaSE


plot(pp.srg$$$, 
     1/pp.srg$$$, type="h", xlab="Ability",    
     ylab="Information", main = "Information Function")



## ----------------------------------------------------------------------------------------------

# where is the smallest se.theta value or the highest information -put a segment in the plot
Minimum=$$$

segments(pp.srg$thetapar$NAgroup1[Minimum], 
         0,
         pp.srg$thetapar$NAgroup1[Minimum], 
         max(1/pp.srg$se.theta$NAgroup1^2),
         col = "red", lwd = 2.4)



## ----InfoPlot for the items

eRm::$$$(PCM.srg, type = $$$)




