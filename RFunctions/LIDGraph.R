
# LIDgraph {R2R}

# Local Item Dependency (LID) Graph

# Description

# LIDgraph visualizes the association pattern in standardized Rasch residuals from a fitted Rasch
# model or partial credit model using the functions RM or PCM in package eRm.
# If not further specified, the outcome of a local item dependencies (LID) analysis with the Q3-Method (Yen, 1984) is returned. 
# Only associations above a cut-off are shown. 

# Usage

# LIDgraph(x, cut, ...)


# Arguments


# x           an `RM`` or `PCM` object.
#
# cut         Numeric values between -1 and 1 or "Q3star". Default cut-off "Q3star" (see Details).
#
# main        Title for the graph. Default is 'Item Dependencies'.
#
# cex         size of labels
#
# print.out   If 'TRUE' the correlation matrix for LID items and the LID cut-off are returned. Default is 'FALSE'.
#
# layout      Layout from layout `igraph`. Default is `layout.kamada.kawai`.
#
# ...         Further 'igraph' layout options : vertex.color, vertex.size, vertex.label.dist, edge.color.

# Value

# LIDgraph returns an association graph of class `igraph`

# Details

# LIDgraph returns an association graph if residual correlations above a cut-off are found, otherwise a message is returned. 
# For Rasch analysis, Marais (2013) recommends that LID should be considered relative to the average of the residual correlations, 
# because the magnitude of the residual correlations depends on the number of items. Christensen et al. (2017) formalized this, 
# illustrating that if the largest Q3 value is more than 0.2 above the average $$Q^{\star}_3 = Q_{3,max} - \bar{Q_3} > 0.2$$  
# scale items are not free of LID. Entering cut = 'Q3star', the default, uses the suggested cut-off, corresponding to $\bar{Q_3} + 0.2$.

# Value

# Returns an object of class `igraph` containing

# 'LID' upper triangle of a correlation matrix with values for locally dependent items

# 'cut.off' the size of the cut-off


# Author(s)
#
# Carolina Fellinghauer carolina.fellinghauer@uzh.ch
# Marianne Müller - mail

# References
#

# Christensen, K. B., Makransky, G., & Horton, M. (2017). Critical Values for Yen’s Q 3 : 
# Identification of Local Dependence in the Rasch Model Using Residual Correlations. 
# Applied Psychological Measurement, 41(3), 178-194. http://doi.org/10.1177/0146621616677520
#
# Marais, I. (2013). Local Dependence. In Rasch Models in Health (Vol. 44, pp. 111-130). Hoboken, NJ USA: 
# John Wiley & Sons, Inc. http://doi.org/10.1002/9781118574454.ch7
#
# Yen, Wendy M. 1984. Effects of Local Item Dependence on the Fit and Equating Performance of the 
# Three-Parameter Logistic Model. Applied Psychological Measurement 8 (2): 125-45. https://doi.org/10.1177/014662168400800201.
#
#

  LIDgraph=function(x, cut = NULL,
                   main = NULL, cex = NULL, print.out = FALSE, layout = NULL,
                    vertex.color = NULL, vertex.size = NULL, vertex.label.dist = NULL,
                     edge.color = NULL){
   
   
    
  library(igraph)
   
  x.pp=suppressWarnings(person.parameter(x))
  x.res=residuals(x.pp)
  
  x.1=cor(x.res, use = "pairwise.complete.obs")  


  if(is.null(cut) == TRUE){cut = mean(x.1[(x.1 < 1)]) + 0.2}
  if(cut == "Q3star"){cut = mean(x.1[(x.1 < 1)]) + 0.2}
  if(is.null(main) == TRUE){main = "Item Dependencies"}
  if(is.null(cex) == TRUE){cex = 1}
  if(is.null(layout) == TRUE){layout = layout.kamada.kawai}
  if(is.null(vertex.color) == TRUE){vertex.color = "lightgrey"}
  if(is.null(vertex.size) == TRUE){vertex.size = 4}
  if(is.null(vertex.label.dist) == TRUE){vertex.label.dist = 1}
  if(is.null(edge.color) == TRUE){edge.color = "lightgrey"}


  x.1[is.na(x.1)]=0
  x.1[which(x.1 <= cut)]=0

 Row.x.1=list()
 Unique.x.1=list()

for(i in 1:(dim(x.1)[2]-1)){
  if(mean(x.1[i,c((i+1):ncol(x.1))])!=0){
    Row.x.1[[i]]=(paste(colnames(x.1)[i], "---", paste(rownames(x.1)[i+which(x.1[i,c((i+1):ncol(x.1))]!=0)], collapse=":"),",", sep=""))
    Unique.x.1[[i]]=(paste(colnames(x.1)[i], ",", paste(rownames(x.1)[i+which(x.1[i,c((i+1):ncol(x.1))]!=0)], collapse=","),",", sep=""))
  }
}  

Insert_this=do.call(rbind, Row.x.1)
Unique_this=do.call(rbind, Unique.x.1)

if(length(Insert_this)!=0){
  
  Last_Insert=Insert_this[dim(Insert_this)[1]]
  Last_Insert_No_Comma=substr(Last_Insert, 1, nchar(Last_Insert)-1)
  Last_OK=c(Insert_this[1:dim(Insert_this)[1]-1],Last_Insert_No_Comma)

  And_This=unlist(strsplit(Last_OK, ","))
  
  Data.x.1=eval(parse(text=paste("graph.formula(", paste(And_This,collapse=","), ")", sep="") ))
  
  Last_Unique=Unique_this[dim(Unique_this)[1]]
  Last_Unique_No_Comma=substr(Last_Unique, 1, nchar(Last_Unique)-1)
  Ok_Unique=c(Unique_this[1:dim(Unique_this)[1]-1],Last_Unique_No_Comma)
  
  Strsplit_Unique=unlist(strsplit(Ok_Unique, ","))
  
  Data_Names=unique(Strsplit_Unique)
  
  par(mar=c(0.5,0.5,2,0.5))
  plot(Data.x.1, layout=layout, 
       vertex.label = V(Data.x.1)$name, 
       main = main,
       cex = cex,
       vertex.color = vertex.color, 
       vertex.label.color = "black", 
       edge.color = edge.color,
       vertex.label.dist = vertex.label.dist,
       vertex.size = vertex.size)
  
  }else{
  return(message(paste("No associations found above cut-off = ", round(cut,4), "!", sep="")))
  }
  
  x.1 = apply(x.1, 2, as.numeric)
  x.1 = round(x.1, 4)
  x.1[lower.tri(x.1, diag = TRUE)] = ""
  x.1[which(x.1==0, arr.ind=TRUE)]=""
  rownames(x.1) = colnames(x.1)
  
  if(print.out == TRUE){return(list(LID = x.1, 
                        cut.off = cut))}
}


