wd <- "/Users/oordulu/Documents/IE582/HW1/"
setwd(wd)
data_input<-read.csv("hw1_input.csv")
set.seed(3808018)

#Input data is imported. The PCA will be conducted on this data.
#I should scale all variables.

standardized_data=scale(data_input)
head(standardized_data)

colMeans(standardized_data)  # Should be approximately 0
apply(standardized_data, 2, sd)  # Should be approximately 1

# Answer to 3.1.1. Perform PCA. It is possible to reduce complexity of tha data
#with the cost of losing information to a great extent as it is shown in the
#next lines. Many PCs must be included in order to be able to explain, say,
#80% of the variance (8 PCs).
pca_result <- prcomp(standardized_data, scale. = FALSE)  # scale. = FALSE because data is already standardized

# View summary of PCA
summary(pca_result)

#3.1.2: Proportion of variance each PCA covers can be seen in the second row of summary(pca_result)
#Example: It can be seen that the first 2 principal components cover 31.8% of the all variance.
#Then if picked up to 2 PCs, the dimensionality is reduced to 2 from 11 and 
#31.8% of the variance is still explainable after the analyses.

explained_variance <- pca_result$sdev^2
explained_variance_ratio <- explained_variance / sum(explained_variance)

# Scree plot
plot(explained_variance_ratio, type = "b", pch = 19, 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     main = "Scree Plot")

# Eigenvalues (variance explained by each principal component)
pca_result$sdev^2

# Loadings (principal directions)
pca_result$rotation

# Principal component scores (transformed data in new coordinate system)
pca_scores <- pca_result$x

# Absolute loadings for the first PC
loadings <- pca_result$rotation
abs_loadings_pc1 <- abs(loadings[, 1])

# Sort by contribution to PC1
key_variables_pc1 <- sort(abs_loadings_pc1, decreasing = TRUE)
print(key_variables_pc1)

#The importance of input variables can be determined by their absolute value in
#the first PC (the one that explains the most variance). Variables that have 
#large coefficients in this PC can be stated to be critical for determination
#of S11 value. It is seen that width.of.patch, height.of.substrate,
#dielectric.constant.of.substrate are the 3 most important input variables
#according to PCA.

#In general, PCA analysis by itself is not very useful in reducing the
#complexity of the interaction between S11 and the input variables since it
#costs large portion of variance to be unexplainable. However, it is useful in
#terms of understanding which input variables are more important compared to 
#rest of the variables. I have conducted unsupervised PCA since that looked
#more meaningful to me although I searched the literature and PCA involving
#the response variable is also a thing. Thus, the conclusions I can draw
#from PCA is limited to identifying critical inputs, which was already done.

#Now let's start the regression portion of the work.
data_real<-read.csv("hw1_real.csv")
data_imag<-read.csv("hw1_img.csv")

freq=seq(0, 200)

#Calculate magnitudes of s parameter.
squared_matrix1 <- data_real^2
squared_matrix2 <- data_imag^2

sum_of_squares <- squared_matrix1 + squared_matrix2

result_matrix <- sqrt(sum_of_squares)
plot(freq,t(result_matrix[3,]))

# Find the column index of the minimum value in each row which correspond to resonance frequency for each design.
min_columns <- apply(result_matrix, 1, function(row) which.min(row))

#Find resonance frequencies that repeat the most so that the regression is carried out on those.
sorted_counts <- sort(table(min_columns), decreasing = TRUE)
top_5 <- head(sorted_counts, 5)
print(top_5)

#Column numbers 201, 1, 115, 96 and 62 are found to be most repeated minimum entry columns across all designs.
#These column numbers correspond to frequencies 200, 0, 114, 95 and 61. 200 and 0 are minimum not because
#they are frequently the resonance frequency values but they are just the end values, that is why regression
#will not be made for those frequency values. Frequencies 114 and 95 are selected to carry out the regressions.

y_114=result_matrix[,115]
y_95=result_matrix[,96]


#The input variables were already standardized. Response (magnitude for this case)
#variables are also standardized.

y114=scale(y_114)
y95=scale(y_95)

#Regression for magnitudes of s parameters.
#First let's set training and test sets

mydata=cbind(y114,standardized_data)
myobs<- sample(1:385,350);

mydata.tr <- mydata[myobs,];
mydata.te <- mydata[-myobs,]

rownames(mydata.tr)<- 1:350

mydata.tr_dataframe <- as.data.frame(mydata.tr)
mydata.te_dataframe <- as.data.frame(mydata.te)

#In doing regression, first I will get a linear regression model with only the meaningful variables
#(alpha=0.1). Then I will contain nonlinear terms of order 2 for those variables only.

quartz();plot(mydata.tr_dataframe)
#This plot is to see if there are any input variables that are correlated. The
#plot shows no clear correlation.

alt1<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
           height.of.substrate + height.of.solder.resist.layer + 
           radius.of.the.probe + c_pad + c_antipad + c_probe + 
           dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata.tr_dataframe)

summary(alt1)
#Eliminate variable with the most p-value until all remaining have p-values below 0.1.

alt2<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
           height.of.substrate + radius.of.the.probe + c_pad + c_antipad +
           c_probe + dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata.tr_dataframe)

summary(alt2)

alt3<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
           height.of.substrate + radius.of.the.probe + c_pad + c_antipad +
           dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata.tr_dataframe)

summary(alt3)

alt4<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
           height.of.substrate + radius.of.the.probe + c_antipad +
           dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata.tr_dataframe)
summary(alt4)

alt5<-lm(V1~length.of.patch + width.of.patch + height.of.substrate +
           radius.of.the.probe + c_antipad + dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata.tr_dataframe)
summary(alt5)

alt6<-lm(V1~length.of.patch + width.of.patch + height.of.substrate +
           c_antipad + dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata.tr_dataframe)
summary(alt6)

alt7<-lm(V1~length.of.patch + width.of.patch + height.of.substrate +
           dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata.tr_dataframe)
summary(alt7)
plot(alt7,which=1)
plot(alt7,which=2)
acf(alt7$residuals)

#This is the final model with only the linear terms.
#All p-values of variables are below alpha at linear regression model alternative
#7. Residuals vs. Fitted values grapgh shows existence of unexplained behaviour since
#residuals are not independently distributed between 0 and 1. That is why, nonlinear
#terms now will be involved.

altnonlin1<-lm(V1~(length.of.patch + width.of.patch + height.of.substrate +
                 dielectric.constant.of.substrate +
                 dielectric.constant.of.solder.resist.layer)^2 +
                 I(length.of.patch^2) + I(width.of.patch^2) +
                 I(height.of.substrate^2) + I(dielectric.constant.of.substrate^2) +
                 I(dielectric.constant.of.solder.resist.layer^2),
                 mydata.tr_dataframe)
summary(altnonlin1)

#width.of.patch became insignificant.

altnonlin2<-lm(V1~(length.of.patch + height.of.substrate +
                 dielectric.constant.of.substrate +
                 dielectric.constant.of.solder.resist.layer)^2 +
                 I(length.of.patch^2) + I(height.of.substrate^2) +
                 I(dielectric.constant.of.substrate^2) +
                 I(dielectric.constant.of.solder.resist.layer^2),
                 mydata.tr_dataframe)
summary(altnonlin2)

#I can see that only height.of.substrate:dielectric.constant.of.substrate and
#height.of.substrate:dielectric.constant.of.solder.resist.layer interaction
#terms are important.
#Also, squared terms of dielectric.constant.of.substrate and dielectric.constant.of.solder.resist.layer
#are not needed.

altnonlin3<-lm(V1~length.of.patch + height.of.substrate +
                     dielectric.constant.of.substrate +
                     dielectric.constant.of.solder.resist.layer +
                 I(length.of.patch^2) + I(height.of.substrate^2) + 
                 height.of.substrate:dielectric.constant.of.substrate +
                 height.of.substrate:dielectric.constant.of.solder.resist.layer,
               mydata.tr_dataframe)
summary(altnonlin3)

#height.of.substrate:dielectric.constant.of.solder.resist.layer term became insignificant.

altnonlin4<-lm(V1~length.of.patch + height.of.substrate +
                 dielectric.constant.of.substrate +
                 dielectric.constant.of.solder.resist.layer +
                 I(length.of.patch^2) + I(height.of.substrate^2) + 
                 height.of.substrate:dielectric.constant.of.substrate,
               mydata.tr_dataframe)
summary(altnonlin4)
plot(altnonlin4,which=1)
plot(altnonlin4,which=2)
acf(altnonlin4$residuals)

#This is the final model including nonlinear terms.

AIC(alt1,alt2,alt3,alt4,alt5,alt6,alt7,altnonlin1,altnonlin2,altnonlin3,altnonlin4)
#Akeike value is like a goodness of fit value that takes degree of freedom involved
#into consideration. Minimum akeike is desired. Final nonlinear model is to be selected
#looking at this. It is also worth noting that every operation decreased AIC value.
#Looking at this is not sufficient. I should also look at test error.

errorTe_altnonlin4 <-  mydata.te_dataframe$V1 - predict(altnonlin4,new=mydata.te_dataframe) # error Testset
sum(errorTe_altnonlin4^2)/length(errorTe_altnonlin4) #   MSE test set

errorTe_alt7 <-  mydata.te_dataframe$V1 - predict(alt7,new=mydata.te_dataframe) # error Testset
sum(errorTe_alt7^2)/length(errorTe_alt7) #   MSE test set

errorTe_altnonlin3 <-  mydata.te_dataframe$V1 - predict(altnonlin3,new=mydata.te_dataframe) # error Testset
sum(errorTe_altnonlin3^2)/length(errorTe_altnonlin3) #   MSE test set

errorTr_altnonlin4 <-  mydata.tr_dataframe$V1 - predict(altnonlin4,new=mydata.tr_dataframe) # MS error Trainingset
sum(errorTr_altnonlin4^2)/length(errorTr_altnonlin4)  #MSE Training set 

errorTr_alt7 <-  mydata.tr_dataframe$V1 - predict(alt7,new=mydata.tr_dataframe) # error Testset
sum(errorTr_alt7^2)/length(errorTr_alt7) #   MSE test set

errorTr_altnonlin3 <-  mydata.tr_dataframe$V1 - predict(altnonlin3,new=mydata.tr_dataframe) # error Testset
sum(errorTr_altnonlin3^2)/length(errorTr_altnonlin3) #   MSE test set

#It is seen that altnonlin3 has less training error compared to altnonlin4, however,
#the decision should be done regarding the test errors, which is minimum for
#altnonlin4. That is why altnonlin4 is selected although clearly there is still behaviour
#must be incorporated for a model that is reliable.

#Until now, regression was made for s11 performance, which is described as magnitude of the s parameter.
#Let's if we can do regression to predict real and imaginry parts of the s parameters.
y_114_real=data_real[,115]
y_114_imag=data_imag[,115]

y114_real=scale(y_114_real)
y114_imag=scale(y_114_imag)

mydata_real=cbind(y114_real,standardized_data)
mydata_imag=cbind(y114_imag,standardized_data)

mydata_real.tr <- mydata_real[myobs,];
mydata_real.te <- mydata_real[-myobs,]

mydata_imag.tr <- mydata_imag[myobs,];
mydata_imag.te <- mydata_imag[-myobs,]

rownames(mydata_real.tr)<- 1:350
rownames(mydata_imag.tr)<- 1:350

mydata_real.tr_dataframe <- as.data.frame(mydata_real.tr)
mydata_real.te_dataframe <- as.data.frame(mydata_real.te)

mydata_imag.tr_dataframe <- as.data.frame(mydata_imag.tr)
mydata_imag.te_dataframe <- as.data.frame(mydata_imag.te)

alt1_real<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
           height.of.substrate + height.of.solder.resist.layer + 
           radius.of.the.probe + c_pad + c_antipad + c_probe + 
           dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata_real.tr_dataframe)
summary(alt1_real)

alt2_real<-lm(V1~length.of.patch + width.of.patch + 
                height.of.substrate + height.of.solder.resist.layer + 
                radius.of.the.probe + c_pad + c_probe + 
                dielectric.constant.of.substrate + 
                dielectric.constant.of.solder.resist.layer,mydata_real.tr_dataframe)
summary(alt2_real)

alt3_real<-lm(V1~length.of.patch + width.of.patch + 
                height.of.substrate + height.of.solder.resist.layer + 
                radius.of.the.probe + c_probe + 
                dielectric.constant.of.solder.resist.layer,mydata_real.tr_dataframe)
summary(alt3_real)

alt4_real<-lm(V1~length.of.patch + width.of.patch + 
                height.of.substrate + 
                radius.of.the.probe + c_probe,
                mydata_real.tr_dataframe)
summary(alt4_real)
plot(alt4_real,which=1)
plot(alt4_real,which=2)
acf(alt4_real$residuals)

#Start adding interaction and square terms.

altnonlin1_real<-lm(V1~(length.of.patch + width.of.patch + 
                height.of.substrate + 
                radius.of.the.probe + c_probe)^2 + I(length.of.patch^2) + 
                I(width.of.patch^2) + I(height.of.substrate^2) + 
                I(radius.of.the.probe^2) + I(c_probe^2),
              mydata_real.tr_dataframe)
summary(altnonlin1_real)

altnonlin2_real<-lm(V1~(length.of.patch + width.of.patch + 
                          height.of.substrate + 
                          radius.of.the.probe + c_probe)^2 + I(length.of.patch^2) + 
                      I(width.of.patch^2) + I(height.of.substrate^2) + 
                      I(radius.of.the.probe^2) + I(c_probe^2),
                    mydata_real.tr_dataframe)
summary(altnonlin2_real)

#Only length.of.patch:height.of.substrate interaction term is significant and
#none of the squared terms are significant

altnonlin3_real<-lm(V1~length.of.patch + width.of.patch + 
                          height.of.substrate + 
                          radius.of.the.probe + c_probe + length.of.patch:height.of.substrate,
                    mydata_real.tr_dataframe)
summary(altnonlin3_real)

#length.of.patch lost its significance.

altnonlin4_real<-lm(V1~width.of.patch +
                      height.of.substrate + 
                      radius.of.the.probe + c_probe + length.of.patch:height.of.substrate,
                    mydata_real.tr_dataframe)
summary(altnonlin4_real)
plot(altnonlin4_real,which=1)
plot(altnonlin4_real,which=2)
acf(altnonlin4_real$residuals)

AIC(alt1_real,alt2_real,alt3_real,alt4_real,altnonlin1_real,altnonlin2_real,altnonlin3_real,altnonlin4_real)

errorTe_real_altnonlin4_real <-  mydata_real.te_dataframe$V1 - predict(altnonlin4_real,new=mydata_real.te_dataframe) # error Testset
sum(errorTe_real_altnonlin4_real^2)/length(errorTe_real_altnonlin4_real) #   MSE test set

errorTe_real_altnonlin3_real <-  mydata_real.te_dataframe$V1 - predict(altnonlin3_real,new=mydata_real.te_dataframe) # error Testset
sum(errorTe_real_altnonlin3_real^2)/length(errorTe_real_altnonlin3_real) #   MSE test set

#AIC values show nonlin3 model for real parts as the model fitted the most. But the difference
#with the forth nonlinear model is very small. Test error is minimum for nonlin3.
#It must be seen that the models for magnitudes and real parts have included different
#variables.
# Model for real parts can be claimed to do better than model for magnitudes due to lower
#test error for the regression predicting real parts.

#Do the same for imaginary parts
alt1_imag<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
                height.of.substrate + height.of.solder.resist.layer + 
                radius.of.the.probe + c_pad + c_antipad + c_probe + 
                dielectric.constant.of.substrate + 
                dielectric.constant.of.solder.resist.layer,mydata_imag.tr_dataframe)
summary(alt1_imag)

alt2_imag<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
                height.of.substrate + height.of.solder.resist.layer + 
                radius.of.the.probe + c_antipad + c_probe + 
                dielectric.constant.of.substrate
                ,mydata_imag.tr_dataframe)
summary(alt2_imag)

alt3_imag<-lm(V1~length.of.patch + width.of.patch + 
                height.of.substrate + 
                radius.of.the.probe + c_antipad + c_probe + 
                dielectric.constant.of.substrate
              ,mydata_imag.tr_dataframe)
summary(alt3_imag)

alt4_imag<-lm(V1~length.of.patch + width.of.patch + 
                height.of.substrate + 
                radius.of.the.probe + c_probe + 
                dielectric.constant.of.substrate
              ,mydata_imag.tr_dataframe)
summary(alt4_imag)
plot(alt4_imag,which=1)
plot(alt4_imag,which=2)
acf(alt4_imag$residuals)

#Start adding interaction and square terms.

altnonlin1_imag<-lm(V1~(length.of.patch + width.of.patch + 
                      height.of.substrate + 
                      radius.of.the.probe + c_probe + 
                      dielectric.constant.of.substrate)^2 + I(length.of.patch^2) + 
                      I(width.of.patch^2) + I(height.of.substrate^2) + 
                      I(radius.of.the.probe^2) + I(c_probe^2) + I(dielectric.constant.of.substrate^2),
                    mydata_imag.tr_dataframe)
summary(altnonlin1_imag)

#length.of.patch:c_probe and width.of.patch:c_probe interaction terms are significatn.
#None of the squared terms are significant

altnonlin2_imag<-lm(V1~length.of.patch + width.of.patch + 
                          height.of.substrate + 
                          radius.of.the.probe + c_probe + 
                          dielectric.constant.of.substrate + length.of.patch:c_probe +
                          width.of.patch:c_probe, mydata_imag.tr_dataframe)
summary(altnonlin2_imag)
plot(altnonlin2_imag,which=1)
plot(altnonlin2_imag,which=2)
acf(altnonlin2_imag$residuals)

AIC(alt1_imag,alt2_imag,alt3_imag,alt4_imag,altnonlin1_imag,altnonlin2_imag)

errorTe_imag_altnonlin2_imag <-  mydata_imag.te_dataframe$V1 - predict(altnonlin2_imag,new=mydata_imag.te_dataframe) # error Testset
sum(errorTe_imag_altnonlin2_imag^2)/length(errorTe_imag_altnonlin2_imag) #   MSE test set

#Regression regarding the imaginary part of the s parameters performed the worst.


#Now let's repeat everything for the second resonance frequency value which was equal to 95.

mydata2=cbind(y95,standardized_data)

mydata2.tr <- mydata2[myobs,];
mydata2.te <- mydata2[-myobs,]

rownames(mydata2.tr)<- 1:350

mydata2.tr_dataframe <- as.data.frame(mydata2.tr)
mydata2.te_dataframe <- as.data.frame(mydata2.te)

alt1_2<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
           height.of.substrate + height.of.solder.resist.layer + 
           radius.of.the.probe + c_pad + c_antipad + c_probe + 
           dielectric.constant.of.substrate + 
           dielectric.constant.of.solder.resist.layer,mydata2.tr_dataframe)
summary(alt1_2)

alt2_2<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
             height.of.substrate + c_pad + c_antipad + c_probe + 
             dielectric.constant.of.substrate + 
             dielectric.constant.of.solder.resist.layer,mydata2.tr_dataframe)
summary(alt2_2)

alt3_2<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
             height.of.substrate + c_antipad + c_probe + 
             dielectric.constant.of.solder.resist.layer,mydata2.tr_dataframe)
summary(alt3_2)

alt4_2<-lm(V1~length.of.patch + width.of.patch + 
             height.of.substrate + c_antipad + c_probe
             ,mydata2.tr_dataframe)
summary(alt4_2)

alt5_2<-lm(V1~width.of.patch + height.of.substrate + c_antipad, mydata2.tr_dataframe)
summary(alt5_2)

alt6_2<-lm(V1~width.of.patch + height.of.substrate, mydata2.tr_dataframe)
summary(alt6_2)
plot(alt6_2,which=1)
plot(alt6_2,which=2)
acf(alt6_2$residuals)


alt_nonlin1_2<-lm(V1~(width.of.patch + height.of.substrate)^2 +
                      I(width.of.patch^2) + I(height.of.substrate^2),
                  mydata2.tr_dataframe)
summary(alt_nonlin1_2)

#Only squared height.of.substrate term is significant

alt_nonlin2_2<-lm(V1~width.of.patch + height.of.substrate + I(height.of.substrate^2),
                  mydata2.tr_dataframe)
summary(alt_nonlin2_2)
plot(alt_nonlin2_2,which=1)
plot(alt_nonlin2_2,which=2)
acf(alt_nonlin2_2$residuals)

alt_nonlin3_2<-lm(V1~height.of.substrate + I(height.of.substrate^2),
                  mydata2.tr_dataframe)
summary(alt_nonlin3_2)
plot(alt_nonlin3_2,which=1)
plot(alt_nonlin3_2,which=2)
acf(alt_nonlin3_2$residuals)

#Very interestingly, regression for magnitudes at frequency equal to 94 can almost be
#reduced to a nonlinear function og height.of.substrate, only.

AIC(alt1_2, alt2_2, alt3_2, alt4_2, alt5_2, alt6_2, alt_nonlin1_2, alt_nonlin2_2, alt_nonlin3_2)

errorTe_alt_nonlin3_2 <-  mydata2.te_dataframe$V1 - predict(alt_nonlin3_2,new=mydata2.te_dataframe) # error Testset
sum(errorTe_alt_nonlin3_2^2)/length(errorTe_alt_nonlin3_2) #   MSE test set

errorTe_alt_nonlin2_2 <-  mydata2.te_dataframe$V1 - predict(alt_nonlin2_2,new=mydata2.te_dataframe) # error Testset
sum(errorTe_alt_nonlin2_2^2)/length(errorTe_alt_nonlin2_2) #   MSE test set

#Test errors show second nonlinear model to be the best performing, agreeing AIC values.


y_95_real=data_real[,96]
y_95_imag=data_imag[,96]

y95_real=scale(y_95_real)
y95_imag=scale(y_95_imag)

mydata_2_real=cbind(y95_real,standardized_data)
mydata_2_imag=cbind(y95_imag,standardized_data)

mydata_2_real.tr <- mydata_2_real[myobs,];
mydata_2_real.te <- mydata_2_real[-myobs,]

mydata_2_imag.tr <- mydata_2_imag[myobs,];
mydata_2_imag.te <- mydata_2_imag[-myobs,]

rownames(mydata_2_imag.tr)<- 1:350
rownames(mydata_2_real.tr)<- 1:350

mydata_2_real.tr_dataframe <- as.data.frame(mydata_2_real.tr)
mydata_2_real.te_dataframe <- as.data.frame(mydata_2_real.te)

mydata_2_imag.tr_dataframe <- as.data.frame(mydata_2_imag.tr)
mydata_2_imag.te_dataframe <- as.data.frame(mydata_2_imag.te)

alt1_real_2<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
                height.of.substrate + height.of.solder.resist.layer + 
                radius.of.the.probe + c_pad + c_antipad + c_probe + 
                dielectric.constant.of.substrate + 
                dielectric.constant.of.solder.resist.layer,mydata_2_real.tr_dataframe)
summary(alt1_real_2)

alt2_real_2<-lm(V1~length.of.patch + width.of.patch + 
                height.of.substrate + 
                radius.of.the.probe + c_pad + c_probe + 
                dielectric.constant.of.substrate + 
                dielectric.constant.of.solder.resist.layer,mydata_2_real.tr_dataframe)
summary(alt2_real_2)

alt3_real_2<-lm(V1~width.of.patch + 
                  height.of.substrate + 
                  radius.of.the.probe + c_probe + 
                  dielectric.constant.of.solder.resist.layer,mydata_2_real.tr_dataframe)
summary(alt3_real_2)

alt4_real_2<-lm(V1~width.of.patch + 
                  height.of.substrate + 
                  radius.of.the.probe + c_probe
                  ,mydata_2_real.tr_dataframe)
summary(alt4_real_2)
plot(alt4_real,which=1)
plot(alt4_real,which=2)
acf(alt4_real$residuals)

#Start adding interaction and square terms.

altnonlin1_real_2<-lm(V1~(width.of.patch + height.of.substrate + 
                      radius.of.the.probe + c_probe)^2 + I(width.of.patch^2) +
                      I(height.of.substrate^2) + I(radius.of.the.probe^2) +
                      I(c_probe^2), mydata_2_real.tr_dataframe)
summary(altnonlin1_real_2)

#We see no interaction or square terms that are significant

AIC(alt1_real_2,alt2_real_2,alt3_real_2,alt4_real_2,altnonlin1_real_2)

errorTe_alt4_real_2 <-  mydata_2_real.te_dataframe$V1 - predict(alt4_real_2,new=mydata_2_real.te_dataframe) # error Testset
sum(errorTe_alt4_real_2^2)/length(errorTe_alt4_real_2) #   MSE test set

#AIC values show alt_real_2 model for real parts as the model fitted the most.
#It must be seen that the models for magnitudes and real parts have included different
#variables.
# Model for real parts can be claimed to do worse than model for magnitudes due to higher
#test error for the regression predicting real parts.

#Do the same for imaginary parts
alt1_imag_2<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
                height.of.substrate + height.of.solder.resist.layer + 
                radius.of.the.probe + c_pad + c_antipad + c_probe + 
                dielectric.constant.of.substrate + 
                dielectric.constant.of.solder.resist.layer,mydata_2_imag.tr_dataframe)
summary(alt1_imag_2)

alt2_imag_2<-lm(V1~length.of.patch + width.of.patch + height.of.patch + 
                  height.of.substrate + height.of.solder.resist.layer + 
                  radius.of.the.probe + c_antipad + c_probe + 
                  dielectric.constant.of.substrate,mydata_2_imag.tr_dataframe)
summary(alt2_imag_2)

alt3_imag_2<-lm(V1~length.of.patch + width.of.patch + height.of.substrate + 
                  radius.of.the.probe + c_antipad + c_probe + 
                  dielectric.constant.of.substrate,mydata_2_imag.tr_dataframe)
summary(alt3_imag_2)
plot(alt3_imag_2,which=1)
plot(alt3_imag_2,which=2)
acf(alt3_imag_2$residuals)

#Start adding interaction and square terms.

altnonlin1_imag_2<-lm(V1~(length.of.patch + width.of.patch + height.of.substrate + 
                          radius.of.the.probe + c_antipad + c_probe + 
                          dielectric.constant.of.substrate)^2 + I(length.of.patch^2) + 
                      I(width.of.patch^2) + I(height.of.substrate^2) + 
                      I(radius.of.the.probe^2) + I(c_antipad^2) + I(c_probe^2) + I(dielectric.constant.of.substrate^2),
                    mydata_2_imag.tr_dataframe)
summary(altnonlin1_imag_2)

#5 interaction terms are significant.
#None of the squared terms are significant

altnonlin2_imag_2<-lm(V1~length.of.patch + width.of.patch + height.of.substrate + 
                      radius.of.the.probe + c_antipad + c_probe + 
                      dielectric.constant.of.substrate + length.of.patch:radius.of.the.probe +
                      length.of.patch:c_probe + width.of.patch:c_antipad + 
                      height.of.substrate:c_antipad + 
                      radius.of.the.probe:c_antipad, mydata_2_imag.tr_dataframe)
summary(altnonlin2_imag_2)
plot(altnonlin2_imag,which=1)
plot(altnonlin2_imag,which=2)
acf(altnonlin2_imag$residuals)

AIC(alt1_imag_2,alt2_imag_2,alt3_imag_2,altnonlin1_imag_2,altnonlin2_imag_2)

errorTe_imag_altnonlin2_imag_2 <-  mydata_2_imag.te_dataframe$V1 - predict(altnonlin2_imag_2,new=mydata_2_imag.te_dataframe) # error Testset
sum(errorTe_imag_altnonlin2_imag_2^2)/length(errorTe_imag_altnonlin2_imag_2) #   MSE test set

#İmaginary parts regression worked the worst for both resonance frequencies.

summary(altnonlin4)         #Model for magnitudes of s11 at freq=114
summary(altnonlin3_real)    #Model for real parts of s11 at freq=114
summary(altnonlin2_imag)    #Model for imaginary parts of s11 at freq=114

summary(alt_nonlin2_2)      #Model for magnitudes of s11 at freq=95
summary(alt4_real_2)        #Model for real parts of s11 at freq=95
summary(altnonlin2_imag_2)  #Model for imaginary parts of s11 at freq=95

#One key observation is that height of substrate is present in all models that
#are developed.
#Width of patch and length of patch seem to be important in determining magnitudes,
#real parts and imaginary parts of the s11 parameter.
#In all models developed, Residuals vs Fitted grapgh shows some behaviour of
#residuals that must be addressed. This can normally be done by selecting
#a generalized linear model with a link function (from Gamma family, probably)
#However this was not done since regression was the topic for the time being.
#Yet, it should be stated that the current models are not reliable in fully
#understanding the response variable in relation with the input variables.
#It is unlikely that a regression model will be suitable for all frequency values since
#this analysis showed very different models (with different selection of variables)
#for the two studied common resonance frequency values.
#This analysis lacked presence of k-fold cross validation. While comparing 
#different models, AIC values and test errors are studied but in abscence of 
#k-fold cross validation, selection of test data may influenced the value of 
#test errors for different types of models.

#PCA was not used as a predictive tool in this work. PCA was conducted only on 
#input variables.
