# Map 1-based optional input ports to variables
dataset1 <- maml.mapInputPort(1) # class: data.frame
dataset2 <- maml.mapInputPort(2)
library (arules)
library(arulesViz)
library(tidyr)
library(dplyr)


##USING THE TRAINING DATA TO GET THE ASSOCIATION RULES VIA APRIORI ALGORITHM.
data<-dataset2[,c(1,3)]
aggdata<-split(data$product_name,data$order_id)
tr<-as(aggdata,"transactions")
rules <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8,target="rules"))
rules <- sort(rules, by="lift", decreasing=TRUE)

##Define the Item Basket, the testing data.
dataset1 <- as.vector(t(dataset1))
Itembasket<-dataset1


##Find the common elements that exist in the rules so that we can subset those that match   


a<-intersect(itemLabels(rules),Itembasket)


 rulesMatchLHS <- subset(rules,lhs%ain% a)
 
 
 
 ##Conditional Output data frames
 if (nrow(rulesMatchLHS@quality)==0)
{
  
  OutputClient =data.frame(  lhs = c('The product does not match other item baskets'),                       
                             rhs = c('No recommendations available'))
  
} else if (nrow(rulesMatchLHS@quality)>0)
{
  
 
OutputClient =data.frame(  lhs = labels(lhs(rulesMatchLHS))$elements,                       
                             rhs = labels(rhs(rulesMatchLHS))$elements,
                             rulesMatchLHS@quality)
  
  
}
 
#  
#  ##ADDING THE COLLABORATIVE FILTERING LAYER
#  
#  gdata<-dataset2
#  
#  gdata_coll<-gdata[,c(2,3)]
# gdata_coll<-unique(gdata_coll)
# 
# ##Assign 1s if particular user has purchased the particular product.
# 
# gdata_coll$purchase<-1
# 
# ##Spread the dataframe to get the distance matrix between item purchases
# 
# g_matrix<-spread(gdata_coll,product_name,purchase,fill=0)
# 
# g_matrix<-g_matrix[,-c(1)]
# 
# 
# 
# ##Now to compute the cosine similarity between sets of items
# 
# 
# ##A function to calculate Cosine similarity- Source: http://www.salemmarafi.com/code/collaborative-filtering-r/
# 
# getcossim<-function(x,y){
#   
#  cosim<-sum(x*y)/sqrt((sum(x*x)+sum(y*y)))
#  return(cosim)
#  
# } 
# 
# ##Create a dummy matrix that will extract the similar products from this product list
# 
# phmatrix<-matrix(NA,
#                  nrow=ncol(g_matrix),
#                  ncol=ncol(g_matrix),
#                  dimnames=list(colnames(g_matrix),colnames(g_matrix)))
# 
# ##Fill up this dummy matrix with the similarities of the user products
# 
# 
# for (i in 1:nrow(phmatrix))
# {
#   
#   for (j in 1:ncol(phmatrix))
#     
#   {
#     
#     if(i==j){
#       phmatrix[i,j]<-"1"
#     } else {
#       
#       phmatrix[i,j]<-getcossim(as.matrix(g_matrix[i]),as.matrix(g_matrix[j]))
#     }
#     
#   }
#   
#   
# }
# 
# ##Convert Matrix output to Data Frame
# sim_matrix<-as.data.frame(phmatrix)
# 
# ##Get the list of 5 closest items in this list
# 
# closem<-matrix(NA, nrow=nrow(sim_matrix),ncol=6,dimnames=list(colnames(sim_matrix),
#               c("Item0","Item1","Item2","Item3","Item4","Item5")))
# 
# for (i in 1:nrow(closem))
#   
# {
#   
#   closem[i,]<-(t(head(n=6,rownames(sim_matrix[order(sim_matrix[,i],decreasing=TRUE),][i]))))
# }
#  
# 
# closem<-as.data.frame(closem)
# 
# ##Combining the two outputs obtained.
# 
# cflist<-closem[which(rownames(closem)%in%Itembasket),]
#  
# 
#  
# #  ##Conditional statement to handle no intersections between testing set and training rules.
# #  if (identical(a,character(0)))  
# #     {   
# # rhs=c('No recommendations available')
# # OutputClient=data.frame(rhs)
# # }
# 
# finaldf<-cbind(cflist,OutputClient)

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("OutputClient");

# Map 1-based optional input ports to variables
dataset1 <- maml.mapInputPort(1) # class: data.frame


data.set<-dataset1[,c(1,2)]

names(data.set)<-c("Product Name","Recommendations")
# 
# names(data.set)[1]<-"Product Name"
# 
# names(data.set)[7]<-"Additional Recommendations"

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("data.set");