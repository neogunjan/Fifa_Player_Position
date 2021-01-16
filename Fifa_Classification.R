setwd("~/Desktop/MSDS/6350 - Statistical Learning and Data Mining/Midterm")

library(dplyr) # To use filter function

fifa_19 <- read.csv("fifa_19_Final_Exam.csv")

### Q0 - Describe Dataset

## Cleaning data
str(fifa_19)
table(fifa_19$Position) # Classes and counts
sum(is.na(fifa_19))

which(grepl('C. Ronaldo', fifa_19$Body.Type))
which(grepl('Shaqiri', fifa_19$Body.Type))
which(grepl('Messi', fifa_19$Body.Type))
which(grepl('Akinfenwa', fifa_19$Body.Type))
fifa_19 <- fifa_19[-c(9167,8231,11408,11574),]
fifa_19 <- fifa_19[,-c(1)] #,9,10,11

# Remove "lbs" from weight column
library(stringr)
num_weight = str_sub(fifa_19$Weight, end=-4)
fifa_19$Weight = as.numeric(num_weight)

# Convert height to numeric inches
new_heights = numeric(dim(fifa_19)[1])
index = 0
for (height in fifa_19$Height) {
  index = index+1
  split_h = strsplit(height,split="'")[[1]]
  a = as.numeric(split_h[1]); b = as.numeric(split_h[2])
  height_inches = (a*12) + b
  new_heights[index] = height_inches
}
fifa_19$Height = new_heights

# Creating a binary column vector of length 3 for the Body.Type feature
for(unique_value in unique(fifa_19$Body.Type)){
  fifa_19[paste("Body.Type", unique_value, sep = ".")] <- ifelse(fifa_19$Body.Type == unique_value, 1, 0)}

str(fifa_19)

# Removing the original Body.Type column and the Position column
fifa_19.features = fifa_19[-c(4,5)]
str(fifa_19.features)

# Standardize each feature
SDATA = data.frame(scale(fifa_19.features)) # Scale feature for standardization
#fifa_19.features.s.wc = cbind(fifa_19.features.s,fifa_19['Position'] )


### Q1 Perform a PCA analysis on standardized features SDATA
CORR <- cor(SDATA)
write.csv(prob_dist_matrix,"~/Desktop/MSDS/6350 - Statistical Learning and Data Mining/Final/prob_dist_matrix.csv",row.names = F)
dim(CORR)

# Eigenvalues and vectors
Z <- eigen(CORR)
values <- Z$values
W <- Z$vectors # matrix W -> eigenvectors

# Decreasing curve eigenvalues (Lr) vs r
plot(seq(1,length(values)),values,main = 'eigenvalues (Lr) vs r',
     xlab = 'r', ylab = 'eigenvalues (Lr)',type = 'l',col = 'red',xaxt ='n')
axis(1, at = seq(0, 23, by = 1), las=2);

# Percentage of Variance Explained PVE(r) vs r
PVE.calc <- function(p,eigenvalues){
  values = eigenvalues
  PVE_vector = numeric(p)
  sum.e_val = 0
  for (i in seq(1,length(values))){
    sum.e_val = sum.e_val+values[i]
    PVE = sum.e_val/p
    PVE_vector[i] = PVE
  }
  return(PVE_vector)}
PVE_vector <- PVE.calc(23,values)
plot(seq(1,length(values)),PVE_vector*100,main = 'Percentage of Variance Explained PVE(r) vs r',
     xlab = 'r', ylab = 'PVE(r)',type = 'l',col = 'red',
     xaxt='n', ylim = c(0,100), yaxt='n')
axis(1, at = seq(0, 23, by = 1), las=2); axis(2, at = seq(0, 100, by = 10), las=2)

# First 3 eigenvalues
round(values[c(1,2,3)],2); round(PVE_vector[c(1,2,3)]*100,2)

# Finding at what r values is PVE(r) = .95
r_95 = which(PVE_vector >= .95)[1]; r_95 # r = 16

# Compute principal components for each case n
W_transpose = t(W)
new_features_vector = W_transpose %*% t(as.matrix(SDATA))
new_features_vector = t(new_features_vector)
new_features_vector <- data.frame(new_features_vector)[1:r_95]

# Adding class to the new features
new_features.c <- cbind(new_features_vector,fifa_19['Position'])

# Visualization
library(scatterplot3d)

new_features.c.CBFB = filter(new_features.c, Position == 'CB' | Position == 'FB')
new_features.c.CBCM = filter(new_features.c, Position == 'CB' | Position == 'CM')
new_features.c.CBST = filter(new_features.c, Position == 'CB' | Position == 'ST')
new_features.c.FBCM = filter(new_features.c, Position == 'FB' | Position == 'CM')
new_features.c.FBST = filter(new_features.c, Position == 'FB' | Position == 'ST')
new_features.c.CMST = filter(new_features.c, Position == 'CM' | Position == 'ST')

Q1_plot <- function(dataset, c1='CB', c2='FB', colors = c('red','purple')){
  scatterplot3d(dataset[,1], dataset[,2], dataset[,3],
                angle=55, pch=16, color= colors[factor(dataset$Position)],
                main= paste("First 3 PCA Eigenvectors for",c1, "&", c2),
                xlab = "V1", ylab = "V2", zlab = "V3") 
  legend("topright", legend = c(c1,c2),
         col = colors, 
         pch = 16, 
         bty = "o", 
         pt.cex = 1.2, 
         cex = .9, 
         text.col = "black", 
         horiz = F , 
         inset = c(.075, .045))
}

par(mfrow=c(3,2))
Q1_plot(new_features.c.CBFB, c1='CB', c2='FB', c('red','purple'))
Q1_plot(new_features.c.CBCM, c1='CB', c2='CM', c('red','cyan'))
Q1_plot(new_features.c.CBST, c1='CB', c2='ST', c('red','orange'))
Q1_plot(new_features.c.FBCM, c1='FB', c2='CM', c('purple','cyan'))
Q1_plot(new_features.c.FBST, c1='FB', c2='ST', c('purple','orange'))
Q1_plot(new_features.c.CMST, c1='CM', c2='ST', c('cyan','orange'))
par(mfrow=c(1,1))

k_values = c(14,15,16,17,18)#c(seq(1,9,3),seq(10,48,4),seq(50,100,10))
start_time <- Sys.time() # Start timer
GINI_indexes_per = numeric(length(k_values)) #pick_k
Qm <- numeric(length(k_values))
Perf.m <- numeric(length(k_values))
index = 0
for (k in k_values){ #seq(1,pick_k)
  index = index+1
  out <- kmeans(SDATA, k, nstart=100, iter.max = 100) 
  Q <- out$tot.withinss/out$totss
  Perf <- 1-(out$tot.withinss/out$totss)
  Qm[index] = Q
  Perf.m[index] = Perf
  
  # Calculating the GINI index for each K value
  clusters_size = out$size
  var1 = character(0)
  for(i in 1:k){
    var2 = paste('CLU', as.character(i), sep = "")
    var1 = c(var1, var2)
  }
  prob_dist_matrix = matrix(nrow = k, ncol = 4, dimnames = list(var1, c('CB%','FB%','CM%','ST%'))) #,'gini_index'
  cluster_num = 0
  for (i in clusters_size){
    cluster_num = cluster_num + 1 #which(clusters_size == i)
    cluster_index <- out$cluster == cluster_num # Indexes of cases belonging to a cluster
    cluster_cases <- SDATA[cluster_index,]
    cluster <- cbind(cluster_cases,'Position'= fifa_19[cluster_index,]$Position) # Adding column of class names
    n_total_cases = i #dim(cluster['Position'])[1]
    dist_CB = sum(cluster['Position'] == 'CB')/n_total_cases
    dist_FB = sum(cluster['Position'] == 'FB')/n_total_cases
    dist_CM = sum(cluster['Position'] == 'CM')/n_total_cases
    dist_ST = sum(cluster['Position'] == 'ST')/n_total_cases
    #gini_index = (dist_CB*(1-dist_CB))+(dist_FB*(1-dist_FB))+(dist_CM*(1-dist_CM))+(dist_ST*(1-dist_ST)) # Gini Index for a cluster. High purity when close to zero
    prob_dist_cluster <- c(dist_CB,dist_FB,dist_CM,dist_ST) #,gini_index
    prob_dist_matrix[cluster_num,] = prob_dist_cluster
  }
  ginis = numeric(k)
  for (i in c(1:k)){
    gini = prob_dist_matrix[i,] %*% (1-prob_dist_matrix[i,])
    ginis[i] = gini}
  prob_dist_matrix <- cbind(prob_dist_matrix,'gini_index'=ginis)
  
  impurity_clustering = sum(prob_dist_matrix[,'gini_index'])
  percent_impurity = impurity_clustering/(0.75*k)
  GINI_indexes_per[index] = percent_impurity
}
end_time <- Sys.time() # End timer
kmeans_time = end_time - start_time
print(kmeans_time)

par(mar=c( 5.1,4.1,4.1,3.1))
par(mfrow=c(3,1))
# Q(m) = sum(withinss)/total_disp
plot(k_values,Qm,xlab = 'k Clusters', ylab = 'Q(m)',
     main = 'Elbow Method for Clustering Quality',col='red', type='l',
     xaxt='n')
axis(1,at = seq(0, 12000, by = 5), las=2)
plot(k_values,Perf.m,xlab = 'k Clusters', ylab = 'Perf(k)',
     main = 'Clustering Performance vs Number of Clusters',col='red', type='l',
     xaxt = 'n')
axis(1,at = seq(0, 12000, by = 5), las=2)
plot(k_values,GINI_indexes_per,xlab = 'k Clusters', ylab = 'GINI(k) in %',
     main = 'GINI vs Number of Clusters',col='red', type='l',
     xaxt='n')
axis(1,at = seq(0, 12000, by = 5), las=2)
par(mfrow=c(1,1))


plot(k_values,GINI_indexes_per*(0.75*k_values),xlab = 'k Clusters', ylab = 'GINI(k) in %',
     main = 'GINI vs Number of Clusters',col='red', type='l',
     xaxt='n')
axis(1,at = seq(0, 12000, by = 5), las=2)



### Q 1.3
k_best = 17
start_time <- Sys.time() # Start timer
out_best <- kmeans(SDATA, k_best, nstart=100, iter.max = 200) 
end_time <- Sys.time() # End timer
kmeans_time = end_time - start_time
print(kmeans_time)

View(out_best$centers)
#write.csv(out_best$centers,"~/Desktop/MSDS/6350 - Statistical Learning and Data Mining/Final/centers_k17.csv",row.names = F)

# Matrix for probability distribution of classes.
var1 = character(0)
for(i in 1:k_best){
  var2 = paste('CLU', as.character(i), sep = "")
  var1 = c(var1, var2)
}
prob_dist_matrix = matrix(nrow = k_best, ncol = 6, dimnames = list(var1, c('Size','Disp','CB%','FB%','CM%','ST%')))
clusters_size = out_best$size
cluster_num = 0
for (i in clusters_size){
  cluster_num = cluster_num + 1
  cluster_index <- out_best$cluster == cluster_num # Indexes of cases belonging to a cluster
  cluster_cases <- SDATA[cluster_index,]
  cluster <- cbind(cluster_cases,'Position'= fifa_19[cluster_index,]$Position) # Adding column of class names
  n_total_cases = i #dim(cluster['Position'])[1]
  dist_CB = sum(cluster['Position'] == 'CB')/n_total_cases
  dist_FB = sum(cluster['Position'] == 'FB')/n_total_cases
  dist_CM = sum(cluster['Position'] == 'CM')/n_total_cases
  dist_ST = sum(cluster['Position'] == 'ST')/n_total_cases
  clus_size = clusters_size[cluster_num]; clus_disp = out_best$withinss[cluster_num];
  prob_dist_cluster <- c(clus_size,clus_disp,dist_CB,dist_FB,dist_CM,dist_ST)
  prob_dist_matrix[cluster_num,] = prob_dist_cluster
}
ginis = numeric(k_best)
for (i in c(1:k_best)){
  gini = prob_dist_matrix[i,][c(3:6)] %*% (1-prob_dist_matrix[i,][c(3:6)])
  ginis[i] = gini}
prob_dist_matrix <- cbind(prob_dist_matrix,'gini_index'=ginis)
impurity_clustering_best = sum(prob_dist_matrix[,'gini_index'])
impurity_clustering_best
View(round(prob_dist_matrix,2))

# 3D plot of clusters

CORR_center <- cor(out_best$centers)
# Eigenvalues and vectors
Z_c <- eigen(CORR_center)
W_c <- Z_c$vectors # matrix W -> eigenvectors
vectors_3D <- t(t(W_c[,1:3]) %*% t(out_best$centers))

new_features.c$cluster = as.factor(out_best$cluster)
colors17 <- c('gray52','lightgoldenrod','blue','yellow','purple','palevioletred1','black','burlywood',
              'tomato1','cyan','violet','orange','lawngreen','brown',
              'red','darkseagreen1','darkgreen')

par(mfrow=c(1,1))
scatterplot3d(vectors_3D[,1], vectors_3D[,2], vectors_3D[,3], angle=45, pch=16, color=colors17,
              main="3D Scatter Plot of Centroids",cex.symbols = 1.5,
              xlab = "V1",ylab = "V2",zlab = "V3",
              xlim =c(-10,10),ylim=c(-5,10),zlim=c(-4,4))
legend("topright",title = 'Centroid Number', legend = levels(new_features.c$cluster),
       col = colors17, pch = 16, bty = "o", pt.cex = 1.5, cex = .8, text.col = "black",
       horiz = F , inset = c(0.1, 0.1))


par(mfrow=c(2,2))
for (i in c(45,135,225,315)){
  scatterplot3d(new_features.c[,1], new_features.c[,2], new_features.c[,3],
                angle=45, pch=16, color= colors17[factor(new_features.c$cluster)],
                main= "K-means clustering",
                xlab = "V1", ylab = "V2", zlab = "V3")
}
par(mfrow=c(1,1))
legend("topright", pch=16,pt.cex = 1.5 ,cex = .8, legend = levels(new_features.c$cluster),
       col = colors17, #seq_along(levels(new_features.c$cluster))
       horiz = T, inset = c(.058, .041), title = 'Cluster Number')

clus_3.graph <- filter(new_features.c, cluster == 3)
scatterplot3d(clus_3.graph[,1], clus_3.graph[,2], clus_3.graph[,3],
              angle=28, pch=16, color= c('Red','Cyan','Purple','Orange')[factor(clus_3.graph$Position)],
              main= "K-means Cluster 3",
              xlab = "V1", ylab = "V2", zlab = "V3")
legend("topright", pch=16,pt.cex = 1.5 ,cex = .8, legend = levels(factor(clus_3.graph$Position)),
       col = c('Red','Cyan','Purple','Orange'), #seq_along(levels(new_features.c$cluster))
       horiz = F, inset = c(.058, .041), title = 'Class')
%

# Training and testing sets
full_data <- cbind(SDATA,'Position' = fifa_19$Position)
train_test_split <- function(new_features) {
  new_features_CB <- filter(new_features, Position == 'CB')
  n1 <- nrow(new_features_CB)
  train1 <- sample(1:n1, 0.8*n1)
  trainCB <- new_features_CB[train1,]
  testCB <- new_features_CB[-train1,]
  
  new_features_FB <- filter(new_features, Position == 'FB')
  n2 <- nrow(new_features_FB)
  train2 <- sample(1:n2, 0.8*n2)
  trainFB <- new_features_FB[train2,]
  testFB <- new_features_FB[-train2,]
  
  new_features_CM <- filter(new_features, Position == 'CM')
  n3 <- nrow(new_features_CM)
  train3 <- sample(1:n3, 0.8*n3)
  trainCM <- new_features_CM[train3,]
  testCM <- new_features_CM[-train3,]
  
  new_features_ST <- filter(new_features, Position == 'ST')
  n4 <- nrow(new_features_ST)
  train4 <- sample(1:n4, 0.8*n4)
  trainST <- new_features_ST[train4,]
  testST <- new_features_ST[-train4,]
  
  trainset <- rbind(trainCB,trainFB,trainCM,trainST)
  trainset$Position <- as.factor(trainset$Position) ##
  x.trainset <- trainset[-24] # Features only
  y.trainset <- as.factor(trainset[,24]) # Class only
  testset<- rbind(testCB,testFB,testCM,testST)
  testset$Position <- as.factor(testset$Position) ##
  x.testset <- testset[-24]
  y.testset <- as.factor(testset[,24])
  
  result <- list('trainset'=trainset, 'x.trainset'=x.trainset, 'y.trainset'=y.trainset,
                 'testset'=testset, 'x.testset'=x.testset, 'y.testset'=y.testset)
  # result <- list('trainCL1' = trainCL1,'trainCL2' = trainCL2,'trainCL3' = trainCL3,
  #                'testCL1' = testCL1,'testCL2' = testCL2,'testCL3' = testCL3)
  return(result)
}

data <- train_test_split(full_data)
table(data$y.trainset)
table(data$y.testset)
dim(data$x.testset)
dim(data$x.trainset)
#data2 <- train_test_split(new_features) # data with new test/train split

### Q5
# Random Forest
library(randomForest)
# function for confusion matrix in percentage
conf_percentage<- function(conf_name){
  confmat<- as.matrix(conf_name, rownames=TRUE, colnames=TRUE)
  conf_percent<- (prop.table(confmat,1))*100
  round(conf_percent,2)}     

start_time <- Sys.time()
trees_val = c(10,50,100,200,300,400,500,600)
train.accuracies <- numeric(length(trees_val))
test.accuracies <- numeric(length(trees_val))
c1 <- numeric(length(trees_val)); c2 <- numeric(length(trees_val));
c3 <- numeric(length(trees_val)); c4 <- numeric(length(trees_val))
count = 0
for (trees in trees_val){
  count = count+1
  #set.seed(2)
  rf_CL <- randomForest(Position~., data = data$trainset,
                        ntree = trees, mtry = round((sqrt(23)),0),
                        importance = F)
  train_acc = sum(diag(rf_CL$confusion[,c(1:4)]))/sum(rf_CL$confusion[,c(1:4)])
  train.accuracies[count] = train_acc
  rf_CL.test <- predict(rf_CL,newdata=data$x.testset)
  confusion_matrix = table(True = data$y.testset, Prediction = rf_CL.test)
  print(paste('conf',count,sep=""))
  print(conf_percentage(confusion_matrix))
  test.accuracies[count] = mean((rf_CL.test==data$y.testset))
  c1[count] = conf_percentage(confusion_matrix)[1,1]
  c2[count] = conf_percentage(confusion_matrix)[2,2]
  c3[count] = conf_percentage(confusion_matrix)[3,3]
  c4[count] = conf_percentage(confusion_matrix)[4,4]
}
end_time <- Sys.time() # End timer
RF_time = end_time - start_time
print(RF_time)

par(mar=c( 5.1,4.1,4.1,3.1))
plot(trees_val,train.accuracies*100, ylim=range(70,85), #ylim=range(train.accuracies*100,test.accuracies*100)
     xlab = 'ntrees', ylab = 'accuracy',
     main = 'Accuracy vs ntrees',col='blue', type='b', pch = 4)
par(new = TRUE)
plot(trees_val,test.accuracies*100, type='b',pch = 4, ylim=range(70,85),
     axes = FALSE, xlab = "", ylab = "", col='red')
legend("bottomright", legend = c("Training Accuracy","Testing Accuracy"), 
       col = c('blue', 'red'), bty = "o", pt.cex = 1, lwd = c(1,1),
       cex = 0.85, text.col = "black", horiz = F , inset = c(0.1, 0.1))

# Diagonal Terms Plot
plot(trees_val, c1, ylim=range(60,c(c1,c2,c3,c4,90)), type='b',col='blue',
     xlab = "ntrees", ylab = "class accuracy", main = 'Position Accuracy vs Number of Trees')
par(new = TRUE)
plot(trees_val, c2,ylim=range(60,c(c1,c2,c3,c4,90)), type='b',pch = 4,
     axes = FALSE, xlab = "", ylab = "", col='red')
par(new = TRUE)
plot(trees_val, c3,ylim=range(60,c(c1,c2,c3,c4,90)), type='b',pch = 5,
     axes = FALSE, xlab = "", ylab = "", col='darkgreen')
par(new = TRUE)
plot(trees_val, c4,ylim=range(60,c(c1,c2,c3,c4,90)), type='b',pch = 2,
     axes = FALSE, xlab = "", ylab = "", col='black')
legend("bottomright", legend = c("CB","CM",'FB','ST'), 
       col = c('blue', 'red','darkgreen','black'), pch = c(1,4,5,2), 
       bty = "n", pt.cex = 1, cex = 0.85, #lwd = c(1,1),
       text.col = "black", horiz = F , inset = c(0.1, 0.1,0.1))


### Q7
start_time <- Sys.time()
BNT = 400
best_RFCL <- randomForest(Position~., data = data$trainset,
                         ntree = BNT, mtry = round((sqrt(23)),0),
                         importance = TRUE)
end_time <- Sys.time() # End timer
RF_time = end_time - start_time
print(RF_time)
IMk = importance(best_RFCL,type = 1)
IMk = data.frame(IMk)
#View(IMk)
varImpPlot(best_RFCL)

### Q8 KS-test
data_CB = filter(full_data, Position == 'CB')
data_CM = filter(full_data, Position == 'CM')
data_FB = filter(full_data, Position == 'FB')
data_ST = filter(full_data, Position == 'ST')

par(mfrow=c(1,4))
hist_var = 'Movement'
hist(data_CB[[hist_var]],main=paste('CB-histogram of',hist_var),xlab=hist_var,col = 'red')
hist(data_FB[[hist_var]],main=paste('FB-histogram of',hist_var),xlab=hist_var,col = 'purple')
hist(data_CM[[hist_var]],main=paste('CM-histogram of',hist_var),xlab=hist_var,col = 'cyan')
hist(data_ST[[hist_var]],main=paste('ST-histogram of',hist_var),xlab=hist_var,col = 'orange')
par(mfrow=c(1,1))

ks_features = c('Movement','Body.Type.Normal','Body.Type.Lean','Body.Type.Stocky','Weak.Foot',
                'Skill.Moves','Reactions','Balance','Agility','Height','Weight')
disc_pwr_matrix = matrix(ncol = 6, nrow = length(ks_features))
index = 0
for (feature in ks_features){
  index = index+1
  d1 = 1-ks.test(data_CB[,feature],data_FB[,feature])$p.value
  d2 = 1-ks.test(data_CB[,feature],data_CM[,feature])$p.value
  d3 = 1-ks.test(data_CB[,feature],data_ST[,feature])$p.value
  d4 = 1-ks.test(data_FB[,feature],data_CM[,feature])$p.value
  d5 = 1-ks.test(data_FB[,feature],data_ST[,feature])$p.value
  d6 = 1-ks.test(data_CM[,feature],data_ST[,feature])$p.value
  disc_pwr_matrix[index,] = c(d1,d2,d3,d4,d5,d6)
}
disc_pwr_matrix <- data.frame(format(round(disc_pwr_matrix,4),nsmall=4))
cols = c('CB-FB','CB-CM','CB-ST','FB-CM','FB-ST','CM-ST')
rownames(disc_pwr_matrix) = ks_features
colnames(disc_pwr_matrix) = cols
View(disc_pwr_matrix)

pvalues_matrix = matrix(ncol = 6, nrow = length(ks_features))
index = 0
for (feature in ks_features){
  index = index+1
  p1 = ks.test(data_CB[,feature],data_FB[,feature])$p.value
  p2 = ks.test(data_CB[,feature],data_CM[,feature])$p.value
  p3 = ks.test(data_CB[,feature],data_ST[,feature])$p.value
  p4 = ks.test(data_FB[,feature],data_CM[,feature])$p.value
  p5 = ks.test(data_FB[,feature],data_ST[,feature])$p.value
  p6 = ks.test(data_CM[,feature],data_ST[,feature])$p.value
  pvalues_matrix[index,] = c(p1,p2,p3,p4,p5,p6)
}
#pvalues_matrix <- data.frame(format(round(pvalues_matrix,4),nsmall=4))
cols = c('CB-FB','CB-CM','CB-ST','FB-CM','FB-ST','CM-ST')
rownames(pvalues_matrix) = ks_features
colnames(pvalues_matrix) = cols
#View(pvalues_matrix)

library(reshape)
library(ggplot2)
data_melt = melt(pvalues_matrix)

ggp = ggplot(data_melt, aes(X1,X2)) + geom_tile(aes(fill=value)) +
  ggtitle("P-values heatmap") + xlab('features') + 
  ylab('classes compared') + theme(axis.text=element_text(size=12, face = 'bold'),
                                   axis.title=element_text(size=14))
ggp
### Q9
q9.data = cbind(full_data,'Cluster'=new_features.c$cluster)
clu3.data = filter(q9.data, Cluster == 3)
clu1.data = filter(q9.data, Cluster == 1)
clu2.data = filter(q9.data, Cluster == 2)
clu3.data = filter(q9.data, Cluster == 3)
clu3.data = filter(q9.data, Cluster == 3)

# smote clu1
clu1.data= clu1.data[,-25]; clu1.data$Position <- as.factor(clu1.data$Position)
table(clu1.data$Position)
#####
clu2.data= clu2.data[,-25]; clu2.data$Position <- as.factor(clu2.data$Position)
table(clu2.data$Position)
clu2.data.split <- train_test_split(clu2.data)
new.clu2.trainset <- SmoteClassif(Position ~ ., clu2.data.split$trainset,
                                  C.perc = list(CB=5,CM=.6,FB=.6,ST=0.6),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu2.testset <- SmoteClassif(Position ~ ., clu2.data.split$testset,
                                 C.perc = list(CB=5,CM=.6,FB=.6,ST=0.6),
                                 k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu2.trainset[,24])
table(new.clu2.testset[,24])
RF_clu2 <- randomForest(Position~., data = new.clu2.trainset,
                        ntree = BNT, mtry = round((sqrt(23)),0),
                        importance = TRUE)
RF_clu2.test <- predict(RF_clu2,newdata=new.clu2.testset[,-24])
confusion_matrix = table(True = new.clu2.testset[,24], Prediction = RF_clu2.test)
confusion_matrix
print(conf_percentage(confusion_matrix))
mean((RF_clu2.test==new.clu2.testset[,24]))
#####
clu7.data = filter(q9.data, Cluster == 7)
clu7.data= clu7.data[,-25]; clu7.data$Position <- as.factor(clu7.data$Position)
clu7.data.split <- train_test_split(clu7.data)
table(clu7.data.split$trainset[,24])
table(clu7.data.split$testset[,24])
new.clu7.trainset <- SmoteClassif(Position ~ ., clu7.data.split$trainset,
                                  C.perc = list(CB=.7,CM=3.5,FB=5,ST=1.7),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu7.testset <- SmoteClassif(Position ~ ., clu7.data.split$testset,
                                 C.perc = list(CB=.7,CM=3.5,FB=5,ST=1.7),
                                 k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu7.trainset[,24])
table(new.clu7.testset[,24])
RF_clu7 <- randomForest(Position~., data = new.clu7.trainset,
                        ntree = BNT, mtry = round((sqrt(23)),0),
                        importance = TRUE)
RF_clu7.test <- predict(RF_clu7,newdata=new.clu7.testset[,-24])
#####
clu8.data = filter(q9.data, Cluster == 8)
clu8.data= clu8.data[,-25]; clu8.data$Position <- as.factor(clu8.data$Position)
clu8.data.split <- train_test_split(clu8.data)
table(clu8.data.split$trainset[,24])
table(clu8.data.split$testset[,24])
new.clu8.trainset <- SmoteClassif(Position ~ ., clu8.data.split$trainset,
                                  C.perc = list(CB=.57,CM=.85,FB=3.2,ST=1.8),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu8.testset <- SmoteClassif(Position ~ ., clu8.data.split$testset,
                                 C.perc = list(CB=.57,CM=.85,FB=3.2,ST=1.8),
                                 k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu8.trainset[,24])
table(new.clu8.testset[,24])
RF_clu8 <- randomForest(Position~., data = new.clu8.trainset,
                        ntree = BNT, mtry = round((sqrt(23)),0),
                        importance = TRUE)
#######
clu9.data = filter(q9.data, Cluster == 9)
clu9.data= clu9.data[,-25]; clu9.data$Position <- as.factor(clu9.data$Position)
clu9.data.split <- train_test_split(clu9.data)
table(clu9.data.split$trainset[,24])
table(clu9.data.split$testset[,24])
new.clu9.trainset <- SmoteClassif(Position ~ ., clu9.data.split$trainset,
                                  C.perc = list(CB=4,CM=.6,FB=.7,ST=0.57),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu9.testset <- SmoteClassif(Position ~ ., clu9.data.split$testset,
                                 C.perc = list(CB=2.5,CM=.6,FB=.7,ST=0.57),
                                 k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu9.trainset[,24])
table(new.clu9.testset[,24])
RF_clu9 <- randomForest(Position~., data = new.clu9.trainset,
                        ntree = BNT, mtry = round((sqrt(23)),0),
                        importance = TRUE)
####
clu11.data = filter(q9.data, Cluster == 11)
clu11.data= clu11.data[,-25]; clu11.data$Position <- as.factor(clu11.data$Position)
clu11.data.split <- train_test_split(clu11.data)
table(clu11.data.split$trainset[,24])
table(clu11.data.split$testset[,24])
new.clu11.trainset <- SmoteClassif(Position ~ ., clu11.data.split$trainset,
                                   C.perc = list(CB=1,CM=.7,FB=2,ST=1.6),
                                   k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu11.testset <- SmoteClassif(Position ~ ., clu11.data.split$testset,
                                  C.perc = list(CB=.8,CM=.7,FB=2,ST=1.6),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu11.trainset[,24])
table(new.clu11.testset[,24])
RF_clu11 <- randomForest(Position~., data = new.clu11.trainset,
                         ntree = BNT, mtry = round((sqrt(23)),0),
                         importance = TRUE)


####
clu12.data = filter(q9.data, Cluster == 12)
clu12.data= clu12.data[,-25]; clu12.data$Position <- as.factor(clu12.data$Position)
clu12.data.split <- train_test_split(clu12.data)
table(clu12.data.split$trainset[,24])
table(clu12.data.split$testset[,24])
new.clu12.trainset <- SmoteClassif(Position ~ ., clu12.data.split$trainset,
                                   C.perc = list(CB=14,CM=.5,FB=.5,ST=.75),
                                   k = 2,repl = FALSE, dist = "Euclidean", p = 2)
new.clu12.testset <- SmoteClassif(Position ~ ., clu12.data.split$testset,
                                  C.perc = list(CB=14,CM=.7,FB=2,ST=1.6),
                                  k = 2,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu12.trainset[,24])
table(new.clu12.testset[,24])
RF_clu12 <- randomForest(Position~., data = new.clu12.trainset,
                         ntree = BNT, mtry = round((sqrt(23)),0),
                         importance = TRUE)
###3
clu13.data = filter(q9.data, Cluster == 13)
clu13.data= clu13.data[,-25]; clu13.data$Position <- as.factor(clu13.data$Position)
clu13.data.split <- train_test_split(clu13.data)
table(clu13.data.split$trainset[,24])
table(clu13.data.split$testset[,24])
new.clu13.trainset <- SmoteClassif(Position ~ ., clu13.data.split$trainset,
                                   C.perc = list(CB=1.3,CM=1,FB=.5,ST=3.7),
                                   k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu13.testset <- SmoteClassif(Position ~ ., clu13.data.split$testset,
                                  C.perc = list(CB=1,CM=1,FB=.5,ST=3.7),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu13.trainset[,24])
table(new.clu13.testset[,24])
RF_clu13 <- randomForest(Position~., data = new.clu13.trainset,
                         ntree = BNT, mtry = round((sqrt(23)),0),
                         importance = TRUE)
#####
clu14.data = filter(q9.data, Cluster == 14)
clu14.data= clu14.data[,-25]; clu14.data$Position <- as.factor(clu14.data$Position)
clu14.data.split <- train_test_split(clu14.data)
table(clu14.data.split$trainset[,24])
table(clu14.data.split$testset[,24])
new.clu14.trainset <- SmoteClassif(Position ~ ., clu14.data.split$trainset,
                                   C.perc = list(CB=3.2,CM=.8,FB=.65,ST=2.1),
                                   k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu14.testset <- SmoteClassif(Position ~ ., clu14.data.split$testset,
                                  C.perc = list(CB=2.1,CM=.8,FB=.65,ST=2.1),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu14.trainset[,24])
table(new.clu14.testset[,24])
RF_clu14 <- randomForest(Position~., data = new.clu14.trainset,
                         ntree = BNT, mtry = round((sqrt(23)),0),
                         importance = TRUE)

###
clu15.data = filter(q9.data, Cluster == 15)
clu15.data= clu15.data[,-25]; clu15.data$Position <- as.factor(clu15.data$Position)
clu15.data.split <- train_test_split(clu15.data)
table(clu15.data.split$trainset[,24])
table(clu15.data.split$testset[,24])
new.clu15.trainset <- SmoteClassif(Position ~ ., clu15.data.split$trainset,
                                   C.perc = list(CB=6,CM=1,FB=1,ST=.62),
                                   k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu15.testset <- SmoteClassif(Position ~ ., clu15.data.split$testset,
                                  C.perc = list(CB=4.5,CM=1,FB=1.2,ST=.62),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu15.trainset[,24])
table(new.clu15.testset[,24])
RF_clu15 <- randomForest(Position~., data = new.clu15.trainset,
                         ntree = BNT, mtry = round((sqrt(23)),0),
                         importance = TRUE)
####
clu17.data = filter(q9.data, Cluster == 17)
clu17.data= clu17.data[,-25]; clu17.data$Position <- as.factor(clu17.data$Position)
clu17.data.split <- train_test_split(clu17.data)
table(clu17.data.split$trainset[,24])
table(clu17.data.split$testset[,24])
new.clu17.trainset <- SmoteClassif(Position ~ ., clu17.data.split$trainset,
                                   C.perc = list(CB=7,CM=.45,FB=.5,ST=.5),
                                   k = 3,repl = FALSE, dist = "Euclidean", p = 2)
new.clu17.testset <- SmoteClassif(Position ~ ., clu17.data.split$testset,
                                  C.perc = list(CB=5,CM=.45,FB=.5,ST=.5),
                                  k = 3,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu17.trainset[,24])
table(new.clu17.testset[,24])
RF_clu17 <- randomForest(Position~., data = new.clu17.trainset,
                         ntree = BNT, mtry = round((sqrt(23)),0),
                         importance = TRUE)

####
clu3.data.split <- train_test_split(clu3.data)
table(clu3.data.split$trainset[,24])
table(clu3.data.split$testset[,24])
library(UBL)
new.clu3.trainset <- SmoteClassif(Position ~ ., clu3.data.split$trainset,
                                  C.perc = list(CB=45,CM=7,FB=7,ST=0.40),
                                  k = 2,repl = FALSE, dist = "Euclidean", p = 2)

new.clu3.testset <- SmoteClassif(Position ~ ., clu3.data.split$testset,
                                  C.perc = list(CB=14,CM=6,FB=7,ST=0.40),
                                  k = 2,repl = FALSE, dist = "Euclidean", p = 2)
table(new.clu3.trainset[,24])
table(new.clu3.testset[,24])

RF_clu3 <- randomForest(Position~., data = new.clu3.trainset,
                        ntree = BNT, mtry = round((sqrt(23)),0),
                        importance = TRUE)
#sum(diag(rf_CL$confusion[,c(1:4)]))/sum(rf_CL$confusion[,c(1:4)])
RF_clu3.test <- predict(RF_clu3,newdata=new.clu3.testset[,-24])
confusion_matrix = table(True = new.clu3.testset[,24], Prediction = RF_clu3.test)
confusion_matrix
print(conf_percentage(confusion_matrix))
mean((RF_clu3.test==new.clu3.testset[,24]))




new = data$testset
new$cluster = new_features.c[rownames(new),]$cluster
new$prediction = NA
for (i in 1:nrow(new)){
  if (new$cluster[i]=='1'){
    new$prediction[i]="CM"
  }
  else if (new$cluster[i]=='2'){
    new$prediction[i]=predict(RF_clu2,newdata = new[i,])
  }
  else if (new$cluster[i]=='3'){
    new$prediction[i]="ST"
  }
  else if (new$cluster[i]=='4'){
    new$prediction[i]="CB"
  }
  else if (new$cluster[i]=='5'){
    new$prediction[i]="CB"
  }
  else if (new$cluster[i]=='6'){
    new$prediction[i]="CB"
  }
  else if (new$cluster[i]=='7'){
    new$prediction[i]=predict(RF_clu7,newdata = new[i,])
  }
  else if (new$cluster[i]=='8'){
    new$prediction[i]=predict(RF_clu8,newdata = new[i,])
  }
  else if (new$cluster[i]=='9'){
    new$prediction[i]=predict(RF_clu9,newdata = new[i,])
  }
  else if (new$cluster[i]=='10'){
    new$prediction[i]="CM"
  }
  else if (new$cluster[i]=='11'){
    new$prediction[i]=predict(RF_clu11,newdata = new[i,])
  }
  else if (new$cluster[i]=='12'){
    new$prediction[i]=predict(RF_clu12,newdata = new[i,])
  }
  else if (new$cluster[i]=='13'){
    new$prediction[i]=predict(RF_clu13,newdata = new[i,])
  }
  else if (new$cluster[i]=='14'){
    new$prediction[i]=predict(RF_clu14,newdata = new[i,])
  }
  else if (new$cluster[i]=='15'){
    new$prediction[i]=predict(RF_clu15,newdata = new[i,])
  }
  else if (new$cluster[i]=='16'){
    new$prediction[i]="CB"
  }
  else if (new$cluster[i]=='17'){
    new$prediction[i]=predict(RF_clu17,newdata = new[i,])
  }
}
TEST_SET_Q10 = new

for(i in 1:nrow(new)){
  if(new$prediction[i]==1){
    TEST_SET_Q10$prediction[i]="CB"
  }else if(new$prediction[i]==2){
    TEST_SET_Q10$prediction[i]="CM"
  }else if(new$prediction[i]==3){
    TEST_SET_Q10$prediction[i]="FB"
  }else if(new$prediction[i]==4){
    TEST_SET_Q10$prediction[i]="ST"
  }else{
    TEST_SET_Q10$prediction = TEST_SET_Q10$prediction
  }
}


TEST_SET_Q10$prediction = as.factor(TEST_SET_Q10$prediction)
conf_matrix10 = table(True = data$testset$Position, Prediction = TEST_SET_Q10$prediction)
sum(diag(conf_matrix10)/sum(conf_matrix10))



### Q11
svmc1 = 'CB'
svmc2 = 'FB'
SVM.data.trainset = filter(data$trainset, Position == svmc1 | Position == svmc2)
SVM.data.testset = filter(data$testset, Position == svmc1 | Position == svmc2)
SVM.data.trainset$Position = droplevels(SVM.data.trainset$Position)
SVM.data.testset$Position = droplevels(SVM.data.testset$Position)

start_time <- Sys.time()
tune.out=tune(svm,
              Position~., data = SVM.data.trainset,
              kernel="linear",
              ranges=list(cost=c(0.001,.01,.1,0.5,1,5,10,100)))
end_time <- Sys.time() # End timer
RF_time = end_time - start_time
print(RF_time)
summary(tune.out)
par(mar=c( 5.1,4.1,4.1,3.1))
plot(tune.out$performances[['cost']],1-tune.out$performances[['error']],
     xlab='cost',ylab='Accuracy',main = '10-fold cross validation accuracy vs Cost',
     col='blue',pch=16)
svm.pred <- predict(tune.out$best.model,SVM.data.testset)
conf_mat = table(true=SVM.data.testset$Position, pred=svm.pred)
conf_percentage(conf_mat)
mean(svm.pred == SVM.data.testset$Position)
plot(tune.out$best.model, SVM.data.testset, LongPassing ~ Movement)


library(e1071)
grid = c(0.001,.01,.1,0.5,1,5,10,100)
svm_test_acc = numeric(8)
count = 0
for (i in grid){
  count = count+1
  svm.obj <- svm(Position ~., data = SVM.data.trainset,
                 kernel='linear',cost=i)
  #mean(predict(svm.obj) == SVM.data.trainset$Position)
  pred.test = predict(svm.obj,SVM.data.testset)
  test_acc = mean(pred.test == SVM.data.testset$Position)
  svm_test_acc[count] = test_acc
}
plot(grid,svm_test_acc)

table(True = SVM.data.testset$Position, prediction = pred.test)
plot(svm.obj, SVM.data.testset, Age ~ Movement)
     #,slice = list(Sepal.Width = 3, Sepal.Length = 4))
plot(SVM.data.testset$Age,SVM.data.testset$SprintSpeed,col=c('red','blue')[SVM.data.testset$Position])
