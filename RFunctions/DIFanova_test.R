# data with items from Rasch analysis
# pf the vector for the exogenous variable to test for DIF
# model is PCM or RM -> for the moment only tested on PCM
# nci = number of class intervals, per default size is set to N = 50 per class
# p.adj = p-value adjustment "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", or "none", the default would be "none"

##need the class interval function to be run first ...

DIFanova = function(data = NULL, pf = NULL, model = NULL, nci = NULL, p.adj = NULL){

library(eRm)


if(is.null(pf)==TRUE){stop("please add an exogenous variable")}
if(is.null(data)==TRUE){stop("function needs data")}
if(is.null(nci)==TRUE){nci = round(nrow(data)/50)}
if(is.null(model)==TRUE){stop("specify the model: RM or PCM?")}
if(is.null(p.adj)==TRUE){p.adj = "none"}


if(model == "RM"){
  mod1 = RM(data, sum0 = TRUE)
  res.mod1 = residuals(person.parameter(mod1))
}else{
  mod1 = PCM(data, sum = TRUE)
  res.mod1 = residuals(person.parameter(mod1))
}

#which observations are in the data
colnames(res.mod1) <- paste(colnames(res.mod1), "_Res", sep="")
if(substr(rownames(res.mod1),1,1)[1]=="P"){
  IN <- as.numeric(substr(rownames(res.mod1),2,nchar(rownames(res.mod1))))
}else{
  IN <- rownames(res.mod1)  
}


###call the class interval function to create score groups
CI <- Class_Intervals(data, nci, IN)

data_DIF_class <- cbind(CI, res.mod1, data[IN,], PF = pf[IN])

DIF_aov <- list()

PCM_res_names <- colnames(res.mod1) #list of names of variable residuals

#for each variable 

for(i in 1:length(PCM_res_names)){ 
#i = 1
 
    ###Anova of item residual and dif group
      DIF_aov[[i]] <- try(matrix(unlist(summary(
        aov(
          as.numeric(data_DIF_class[, PCM_res_names[i]]) ~ # residual
            as.factor(data_DIF_class[, "PF"])  + #demographic
            as.factor(data_DIF_class[, "CI"]) +  #class interval
            as.factor(data_DIF_class[, "PF"]):as.factor(data_DIF_class[, "CI"]) #interaction between demo and CI
        )
      )), ncol = 5), silent = TRUE)
     
    
    
    if (class(DIF_aov[[i]])[1] != "try-error") {
      colnames(DIF_aov[[i]]) <-
        c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
      rownames(DIF_aov[[i]]) <-
        c(
          "PF",
          paste0("class interval_", colnames(data)[i]),
          paste0("PF:class interval_", colnames(data)[i]),
          paste0("residuals_", colnames(data)[i])
        )
        }
  
  
}

  DIF_aov <- do.call(rbind, DIF_aov) #stack info for each demo characteristic


names(DIF_aov) <- PCM_res_names
DIF_aov <- cbind(DIF_aov[,1], apply(DIF_aov[,c(2:5)],2, round, 4))

###simplified DIFrumm output with only the p-values

NonUniform = DIF_aov[seq(3, nrow(DIF_aov), 4),]
Uniform = DIF_aov[seq(1, nrow(DIF_aov), 4),]

##adjustment
NonUniform[,5] = round(p.adjust(NonUniform[,5], method = p.adj), 3)
Uniform[,5] = round(p.adjust(Uniform[,5], method = p.adj), 3)

rownames(Uniform) = colnames(data)
rownames(NonUniform) = colnames(data)

add_a_star = function(x){
x = as.data.frame(x)
x[,6] = ""
x[which(x[,"Pr(>F)"] < 0.05),6] = "*"
colnames(x)[6] = ""
return(x)
}

Uniform = add_a_star(Uniform)
NonUniform = add_a_star(NonUniform)



  return(list(All.no.padj = DIF_aov, Uniform.DIF.PF = Uniform, 
                  NonUniform.DIF = NonUniform))
 
}

