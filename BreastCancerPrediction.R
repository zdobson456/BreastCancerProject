options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# Read and Check data

dim(brca$x)

## [1] 569 30

length(brca$y)

## [1] 569

head(brca$x)

# Predictors

head(brca$y)

# Outcomes

summary(brca$y)

## B 357 M 212

prop.table(table(brca$y))

## B .627 M .373

corr_mat <- cor(brca$x)

# Highest mean

which.max(colMeans(brca$x))

# Lowest standard deviation

which.min(colSds(brca$x))

# Scaling the Matrix

x_centered <- sweep(brca$x,2,colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

# After scaling standar deviation is 1 for all columns

colSds(x_scaled)

## [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

summary(x_scaled)

## radius_mean     texture_mean   perimeter_mean    area_mean     smoothness_mean compactness_mean concavity_mean  concave_pts_mean
## Min.   :-2.03   Min.   :-2.23   Min.   :-1.98   Min.   :-1.45   Min.   :-3.11   Min.   :-1.61    Min.   :-1.11   Min.   :-1.26   
## 1st Qu.:-0.69   1st Qu.:-0.73   1st Qu.:-0.69   1st Qu.:-0.67   1st Qu.:-0.71   1st Qu.:-0.75    1st Qu.:-0.74   1st Qu.:-0.74   
## Median :-0.21   Median :-0.10   Median :-0.24   Median :-0.29   Median :-0.03   Median :-0.22    Median :-0.34   Median :-0.40   
## Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00    Mean   : 0.00   Mean   : 0.00   
## 3rd Qu.: 0.47   3rd Qu.: 0.58   3rd Qu.: 0.50   3rd Qu.: 0.36   3rd Qu.: 0.64   3rd Qu.: 0.49    3rd Qu.: 0.53   3rd Qu.: 0.65   
## Max.   : 3.97   Max.   : 4.65   Max.   : 3.97   Max.   : 5.25   Max.   : 4.77   Max.   : 4.56    Max.   : 4.24   Max.   : 3.92   
## symmetry_mean   fractal_dim_mean   radius_se       texture_se     perimeter_se      area_se      smoothness_se   compactness_se 
## Min.   :-2.74   Min.   :-1.82    Min.   :-1.06   Min.   :-1.55   Min.   :-1.04   Min.   :-0.74   Min.   :-1.77   Min.   :-1.30  
## 1st Qu.:-0.70   1st Qu.:-0.72    1st Qu.:-0.62   1st Qu.:-0.69   1st Qu.:-0.62   1st Qu.:-0.49   1st Qu.:-0.62   1st Qu.:-0.69  
## Median :-0.07   Median :-0.18    Median :-0.29   Median :-0.20   Median :-0.29   Median :-0.35   Median :-0.22   Median :-0.28  
## Mean   : 0.00   Mean   : 0.00    Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00  
## 3rd Qu.: 0.53   3rd Qu.: 0.47    3rd Qu.: 0.27   3rd Qu.: 0.47   3rd Qu.: 0.24   3rd Qu.: 0.11   3rd Qu.: 0.37   3rd Qu.: 0.39  
## Max.   : 4.48   Max.   : 4.91    Max.   : 8.90   Max.   : 6.65   Max.   : 9.45   Max.   :11.03   Max.   : 8.02   Max.   : 6.14  
## concavity_se   concave_pts_se   symmetry_se    fractal_dim_se   radius_worst   texture_worst   perimeter_worst   area_worst   
## Min.   :-1.06   Min.   :-1.91   Min.   :-1.53   Min.   :-1.10   Min.   :-1.73   Min.   :-2.22   Min.   :-1.69   Min.   :-1.22  
## 1st Qu.:-0.56   1st Qu.:-0.67   1st Qu.:-0.65   1st Qu.:-0.58   1st Qu.:-0.67   1st Qu.:-0.75   1st Qu.:-0.69   1st Qu.:-0.64  
## Median :-0.20   Median :-0.14   Median :-0.22   Median :-0.23   Median :-0.27   Median :-0.04   Median :-0.29   Median :-0.34  
## Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00   Mean   : 0.00  
## 3rd Qu.: 0.34   3rd Qu.: 0.47   3rd Qu.: 0.36   3rd Qu.: 0.29   3rd Qu.: 0.52   3rd Qu.: 0.66   3rd Qu.: 0.54   3rd Qu.: 0.36  
## Max.   :12.06   Max.   : 6.64   Max.   : 7.07   Max.   : 9.84   Max.   : 4.09   Max.   : 3.88   Max.   : 4.28   Max.   : 5.92  
## smoothness_worst compactness_worst concavity_worst concave_pts_worst symmetry_worst  fractal_dim_worst
## Min.   :-2.68    Min.   :-1.44     Min.   :-1.30   Min.   :-1.744    Min.   :-2.16   Min.   :-1.60    
## 1st Qu.:-0.69    1st Qu.:-0.68     1st Qu.:-0.76   1st Qu.:-0.756    1st Qu.:-0.64   1st Qu.:-0.69    
## Median :-0.05    Median :-0.27     Median :-0.22   Median :-0.223    Median :-0.13   Median :-0.22    
## Mean   : 0.00    Mean   : 0.00     Mean   : 0.00   Mean   : 0.000    Mean   : 0.00   Mean   : 0.00    
## 3rd Qu.: 0.60    3rd Qu.: 0.54     3rd Qu.: 0.53   3rd Qu.: 0.712    3rd Qu.: 0.45   3rd Qu.: 0.45    
## Max.   : 3.95    Max.   : 5.11     Max.   : 4.70   Max.   : 2.684    Max.   : 6.04   Max.   : 6.84

# heatmap of the relationship between features using the scaled matrix

d_features <- dist(t)

# standard deviation of the first column

sd(x_scaled[,1])

# median value of the first column

median(x_scaled[,1])

# Distance
# average distance beween the first sample, which is benign, and other benign samples

d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])

# Average distance between first sample and malignant samples

dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)

# heatmap of the relationship between features using the scaled matrix

d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features),labRow=NA,labCol=NA)

# clustering
h <- hclust(d_features)
groups <- cutree(h,k=5)
split(names(groups), groups)

## $`1`
##  [1] "radius_mean"       "perimeter_mean"    "area_mean"        
##  [4] "concavity_mean"    "concave_pts_mean"  "radius_se"        
##  [7] "perimeter_se"      "area_se"           "radius_worst"     
## [10] "perimeter_worst"   "area_worst"        "concave_pts_worst"
## 
## $`2`
## [1] "texture_mean"  "texture_worst"
## 
## $`3`
## [1] "smoothness_mean"   "compactness_mean"  "symmetry_mean"    
## [4] "fractal_dim_mean"  "smoothness_worst"  "compactness_worst"
## [7] "concavity_worst"   "symmetry_worst"    "fractal_dim_worst"
## 
## $`4`
## [1] "texture_se"    "smoothness_se" "symmetry_se"  
## 
## $`5`
## [1] "compactness_se" "concavity_se"   "concave_pts_se" "fractal_dim_se"


# PCA
pca <- prcomp(x_scaled)
summary(pca)

## Importance of components:
##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
## Standard deviation     3.6444 2.3857 1.67867 1.40735 1.28403 1.09880 0.82172
## Proportion of Variance 0.4427 0.1897 0.09393 0.06602 0.05496 0.04025 0.02251
## Cumulative Proportion  0.4427 0.6324 0.72636 0.79239 0.84734 0.88759 0.91010
##                            PC8    PC9    PC10   PC11    PC12    PC13    PC14
## Standard deviation     0.69037 0.6457 0.59219 0.5421 0.51104 0.49128 0.39624
## Proportion of Variance 0.01589 0.0139 0.01169 0.0098 0.00871 0.00805 0.00523
## Cumulative Proportion  0.92598 0.9399 0.95157 0.9614 0.97007 0.97812 0.98335
##                           PC15    PC16    PC17    PC18    PC19    PC20   PC21
## Standard deviation     0.30681 0.28260 0.24372 0.22939 0.22244 0.17652 0.1731
## Proportion of Variance 0.00314 0.00266 0.00198 0.00175 0.00165 0.00104 0.0010
## Cumulative Proportion  0.98649 0.98915 0.99113 0.99288 0.99453 0.99557 0.9966
##                           PC22    PC23   PC24    PC25    PC26    PC27    PC28
## Standard deviation     0.16565 0.15602 0.1344 0.12442 0.09043 0.08307 0.03987
## Proportion of Variance 0.00091 0.00081 0.0006 0.00052 0.00027 0.00023 0.00005
## Cumulative Proportion  0.99749 0.99830 0.9989 0.99942 0.99969 0.99992 0.99997
##                           PC29    PC30
## Standard deviation     0.02736 0.01153
## Proportion of Variance 0.00002 0.00000
## Cumulative Proportion  1.00000 1.00000

# Plotting PCs, we can see the benign tumors tend to have smaller values of PC1 and 
# higher values for malignant tumors
data.frame(pca$x[,1:2],type = brca$y) %>%
  ggplot(aes(PC1,PC2,color=type)) + 
  geom_point()

# Plotting PCs, boxplot.  We can see PC1 is significantly different from others
data.frame(type = brca$y ,pca$x[,1:10]) %>%
  gather(key = "PC",value="value", -type) %>%
  ggplot(aes(PC,value,fill = type)) +
  geom_boxplot()
# PC1 Provides no overlap

geom_point()
## geom_point: na.rm = FALSE
## stat_identity: na.rm = FALSE
## position_identity

# Creating data partition
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(brca$y, time=1, p=0.2,list=FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

# We can see train and test sets have similar proportions
# proportion test
prop.table(table(test_y))

# test_y
# B     M 
# 0.626 0.374

# proportion train
prop.table(table(train_y))

# train_y
# B     M 
# 0.628 0.372

# K-means Clustering
predict_kmeans <- function(x, k) {
  centers <- k$centers 
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances)) 
}

set.seed(3,sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")

# K-means overall accuracy
mean(kmeans_preds == test_y)

## [1] 0.9217391

# Proportion of benign tumors were correctly identified
sensitivity(factor(kmeans_preds), test_y, positive = "B")

## [1] 0.986

# Proportion of malignant tumors are correctly identified
sensitivity(factor(kmeans_preds), test_y, positive = "M")

## [1] 0.814

# Logistic Regression
train_glm <- train(train_x,train_y,method="glm")
glm_preds <- predict(train_glm,test_x)

# Logistic Regression overall accuracy
mean(glm_preds==test_y)

## [1] 0.957

# LDA and QDA models
train_lda <- train(train_x,train_y,method="lda")
lda_preds <- predict(train_lda,test_x)
mean(lda_preds==test_y)

## [1] 0.991

train_qda <- train(train_x,train_y,method="qda")
qda_preds <- predict(train_qda,test_x)
mean(qda_preds==test_y)

## [1] 0.957

# Loess model
set.seed(5, sample.kind = "Rounding")
train_loess <- train(train_x,train_y,method="gamLoess")

loess_preds <- predict(train_loess,test_x)
mean(loess_preds==test_y)

## [1] 0.9826087

# K-nearest neighbors
set.seed(7, sample.kind = "Rounding")
tuning <- data.frame(k=seq(3,21,2))
train_knn <- train(train_x,train_y,method="knn",tuneGrid = tuning)
train_knn$bestTune

##     k
## 10 21

knn_preds <- predict(train_knn,test_x)
mean(knn_preds == test_y)

## [1] 0.948

# Random Forest Model
set.seed(9, sample.kind = "Rounding")
tuning <- data.frame(mtry=c(3,5,7,9))
train_rf <- train(train_x,train_y, method="rf",tuneGrid = tuning, importance = TRUE)
train_rf$bestTune

##   mtry
## 1    3

rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)

## [1] 0.974

varImp(train_rf)

## area_worst is the most important variable in the Random Forest Model

# Ensemble
ensemble <- cbind(glm=glm_preds=="B",lda=lda_preds=="B",qda=qda_preds=="B",loess=loess_preds=="B",
                  rf=rf_preds=="B",knn=knn_preds=="B",kmeans=kmeans_preds=="B")

ensemble_preds <- ifelse(rowMeans(ensemble) >0.5,"B","M")
mean(ensemble_preds==test_y)

## [1] 0.983

models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random fore
st", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)

##                 Model  Accuracy
## 1             K means 0.9217391
## 2 Logistic regression 0.9565217
## 3                 LDA 0.9913043
## 4                 QDA 0.9565217
## 5               Loess 0.9826087
## 6 K nearest neighbors 0.9478261
## 7     Random fore\nst 0.9739130
## 8            Ensemble 0.9826087

## LDA is the best Model to predict Benign or malignant tumors