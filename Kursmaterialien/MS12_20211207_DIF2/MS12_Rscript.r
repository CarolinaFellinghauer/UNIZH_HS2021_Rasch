

## ---- warning=FALSE, message=FALSE----------------------------------------
#install and load following R-packages  
#Additional packages for DIF analysis


library(psychotree)
library(colorspace)



## ---- dataload and analysis, warning = FALSE------------------------------
#get the SRG datasets

urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2021_Rasch/main/Data/SRG_Data_Course_UZH_HS2021.csv"

srg.data=read.csv(url(urlfile))


                        
dim(srg.data)
colnames(srg.data)

srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", "SRG13", "SRG14", "SRG15")


range(srg.data$Age, na.rm = TRUE)

#create age groups based < 30 < 45 < 60 <
srg.data[, "Age_grp"] = cut(srg.data[, "Age"], breaks = c(0, 30, 45, 60, 85)) 

# exogenous variables to test DIF for
#Completeness coded : 1 = complete, 0 = incomplete
#para.tetra_1 coded : 1 = paraplegia, 0 = tetraplegia
srg.pf = c("Age_grp", "Gender", "Completeness", "para.tetra_1", "traumatic_nontraumatic")


# dataset with only the SRG items
data.srg=srg.data[,c(srg.items, srg.pf)]

#check response coding (frequencies including missing values) for each SRG-item
#apply(data.srg, 2, table, useNA="always")


## ----Data_Preparation-----------------------------------------------------

# variable traumatic_nontraumatic is not taken into account because of the small # number of persons with nontraumatic injury.
##
data.srg.tree = data.srg

#make sure that the srg.items are numeric and the pf.factors are coded as factors
data.srg.tree[, srg.items] = apply(data.srg.tree[, srg.items], 2, as$$)

#data.frame data.srg.tree.final first only contains the DIF-variables 
data.srg.tree.final = data.frame(  #
           Age_grp = as.$$(data.srg.tree$$$), #
           Gender = as.$$(data.srg.tree$$$), #
           Completeness = as.$$(data.srg.tree$$$), #
           para.tetra_1 = as.$$(data.srg.tree$$$))



## ----Dataformat-----------------------------------------------------------
#adding the item matrix $srg  to data.srg.tree.final data.frame
data.srg.tree.final$srg = array(as.matrix(data.srg.tree[, srg.items]),  #
                                dim = c($$, $$), #
                                dimnames = list(N$$, $$))


## ----tree


##computes the Rasch tree for Gender, Age_grp, para.tetra_1, Completeness. 
##sample minimum size in the last nodes set to N = 50 with minsplit = 
srg.tree = pctree($$, data = )

#draws the Rasch-Tree plot
plot(srg.tree, "profile", col = rainbow_hcl(22, c = 65, l = 65)) 




## ----Item_Parameter-------------------------------------------------------
#item locations for the persons with complete injury aged above 30
itempar(srg.tree, node = $$) #item location
threshpar(srg.tree, node =$$) #item difficulty parameter



