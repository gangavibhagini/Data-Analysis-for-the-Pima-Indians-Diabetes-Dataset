#Ganga Vibhagini Gangadharan
#PS ID:1310189

#////// Question 1//////
setwd("~/Desktop")

#Read in the data
a<-read.csv(file ="~/Desktop/data.csv", header = FALSE, sep = "\t")

#next creating a data frame renaming attributes to a-h and class attribute as z
d<-data.frame(a=a[,1],b=a[,2],c=a[,3],d=a[,4],e=a[,5],f=a[,6],g=a[,7],h=a[,8],z=factor(a[,9]))
d[,2:8][d[,2:8]==0]<-NA
columns26<-d[,2:6]
#Calculating the mean values for attributes 2-6
colMeans(columns26, na.rm = TRUE)
#Calculating the Standard deviation for attributes 2-6
apply(d[,2:6], 2, sd, na.rm = TRUE)
View(d)

#////// Question 2//////
#Calculating the covariance matrix for attributes 2-6
cov(columns26, use = "pairwise.complete.obs")
#Calculating the correlation for each of the 10 pairs of the 5 attributes
cor(d[,2:6])

#////// Question 3//////
#scatter plot for attributes 3 and 6
plot(d$c,d$f)
#scatter plot for attributes 2 and 7
plot(d$b,d$g)

#////// Question 4//////
#histograms for attributes 2
hist(d[,2])
#histograms for attributes 3
hist(d[,3])
#histograms for attributes 6
hist(d[,6])
#histograms for the 3 attributes for the instances of class 1 and for the instances of class 0
hist(d[,2][d[,9] == 0])

#////// Question 5//////
#box plots for the 2nd attribute
boxplot(d[,2])
#box plots for the 7th attribute
boxplot(d[,7])
#box plots for the 8th attribute
boxplot(d[,8])
#whole dataset and one each for the instances of the two classes
boxplot(d[,2][d[,9] == 0])

#box plots for the 2nd attribute for the instances of the two classes
boxplot(d$b,
        d$b[d$b != 0 & d$z == 0],
        d$b[d$b != 0 & d$z == 1],
        name=c("Dataset", "Diabtic", "Non Diab"), ylab ="Concentration")

#box plots for the 7th attribute for the instances of the two classes
boxplot(d$g,
        d$g[d$g != 0 & d$z == 0],
        d$g[d$g != 0 & d$z == 1],
        name=c("Dataset", "Diabtic", "Non Diab"), ylab ="Concentration")

#box plots for the 8th attribute for the instances of the two classes
boxplot(d$b,
        d$h[d$h != 0 & d$z == 0],
        d$h[d$h != 0 & d$z == 1],
        name=c("Dataset", "Diabtic", "Non Diab"), ylab ="Concentration")

View(d)

a<-read.csv(file ="~/Desktop/data.csv", header = FALSE, sep = ",")
View(a)

#////// Question 6//////
#Plot of attributes 2,3
plot(d$b - d$c,
     col = cbind("black", "red"), pch = cbind(16,18),
     xlab = "Blood Pressure", ylab = "Plasma concentration", main = "Plasma concentration over Blood pressure" )
     legend(1,50, legend = c("Plasma Concentrations", "Blood Pressure"),
            col = c("black", "red"), pch = cbind(16,18)) 

#Plot of attributes 2,4     
plot(d$b - d$d,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Tricep Thickness", ylab = "Plasma concentration", main = "Plasma concentration over Tricep Thickness" )
     legend(1,50, legend = c("Plasma Concentrations", "Tricep Thickness"),
            col = c("black", "red"), pch = cbind(16,18))  
 
#Plot of attributes 2,5         
plot(d$b - d$e,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Serum Insulin", ylab = "Plasma concentration", main = "Plasma concentration over Serum Insulin" )
     legend(1,50, legend = c("Plasma Concentrations", "Serum Insulin"),
            col = c("black", "red"), pch = cbind(16,18)) 
     
#Plot of attributes 2,6     
plot(d$b - d$f,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Body Mass", ylab = "Plasma concentration", main = "Body mass index" )
     legend(1,50, legend = c("Plasma Concentrations", "Body Mass"),
            col = c("black", "red"), pch = cbind(16,18)) 

#Plot of attributes 3,4    
plot(d$c - d$d,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Diabetes ", ylab = "Plasma concentration", main = "Diabetes pedigree function" )
     legend(1,50, legend = c("Plasma Concentrations", "Diabetes "),
            col = c("black", "red"), pch = cbind(16,18)) 
     
#Plot of attributes 3,5  
plot(d$c - d$e,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "pregnant", ylab = "Plasma concentration", main = "Pregenant" )
     legend(1,50, legend = c("Plasma Concentrations", "pregnant"),
            col = c("black", "red"), pch = cbind(16,18)) 
     
#Plot of attributes 3,6 
plot(d$c - d$f,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Body Mass", ylab = "Plasma concentration", main = "Body mass index" )
     legend(1,50, legend = c("Plasma Concentrations", "Body Mass"),
            col = c("black", "red"), pch = cbind(16,18)) 
     
#Plot of attributes 4,5
plot(d$d - d$e,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Body Mass", ylab = "Plasma concentration", main = "Body mass index" )
     legend(1,50, legend = c("Plasma Concentrations", "Body Mass"),
            col = c("black", "red"), pch = cbind(16,18))
     
#Plot of attributes 4,6 
plot(d$d - d$f,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Body Mass", ylab = "Plasma concentration", main = "Body mass index" )
     legend(1,50, legend = c("Plasma Concentrations", "Body Mass"),
            col = c("black", "red"), pch = cbind(16,18))
     
#Plot of attributes 5,6  
plot(d$e - d$f,
          col = cbind("black", "red"), pch = cbind(16,18),
          xlab = "Body Mass", ylab = "Plasma concentration", main = "Body mass index" )
     legend(1,50, legend = c("Plasma Concentrations", "Body Mass"),
            col = c("black", "red"), pch = cbind(16,18))
     
#Imported to access 3d plots     
install.packages("scatterplot3d")

#3-d scatterplot for attributes 2, 3, 6
scatterplot3d(x = d[,2], y = d[,3], z = d[,6], xlab = "Plasma Glucose Concentration", ylab = "Diastolic Blood Pressure", zlab = "BMI", main = "Attributes 2, 3 and 6")
#3-d scatterplot for attributes 2, 4, 6
scatterplot3d(x = d[,2], y = d[,4], z = d[,6], xlab = "Plasma Glucose Concentration", ylab = "Tricep Skin Fold Thickness", zlab = "BMI", main = "Attributes 2, 4 and 6")

#////// Question 7//////
#Read in the clean data set
a<-read.csv(file ="~/Desktop/cleandata.csv", header = FALSE, sep = "\t")
head(a)

#Making a subset based on class = 0, and class = 1
czero<-d[which(d$z==0),]
cone<-d[which(d$z==1),]

#Star plot for the first 10 instances of class 0 
stars(czero[1:10, c("a", "b", "c", "h")], key.loc = c(0,9), main = "Starplot for class Zero subset")
stars(cone[1:10, c("a", "b", "c", "h")], key.loc = c(0,9), main = "Starplot for class One subset")
#the first 10 instances of class 1 for attributes 1, 2, 3, 8
stars(czero[1:10, c("a", "b", "c", "h")], key.loc = c(0,9), main = "Starplot for class Zero subset")


#////// Question 8//////
# linear model that predicts the class attribute using the 8 z-scored, continuous attributes of the cleaned 
#dataset as independent variables
#Scaling attributes 1-8 in the clean dataset
s   =scale(a[1:8])
df  =data.frame(s, Factor = factor(a[,9]), Z=a[,9])

#Applying the linear model
model1 =lm(Z ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8, data=df)
model2 =lm(Z ~ V1 + V2 + V5 + V6 + V7 + V8, data=df)

#Print summary and reporting the adjusted data
summary(model1)
summary(model2)


#////// Question 9//////

#3 decision tree models with 20 or less nodes using the cleaned dataset
d<-data.frame(a=a[,1],b=a[,2],c=a[,3],d=a[,4],e=a[,5],f=a[,6],g=a[,7],h=a[,8],z=factor(a[,9]))
require("rpart")
model<-rpart(z ~ a + b + c + d + e + f + g + h,
             +              data=d,
             +              method="class", maxdepth=5)
plot(model, uniform=TRUE, branch=0.2, margin=0.2)
model<-rpart(z ~ a + b + c + d + e + f + g + h,data=d,method="class",control=rpart.control(cp=0.01, maxdepth=4))
plot(model, compress=TRUE, uniform=TRUE, margin=0.15)
text(model,use.n=TRUE,all=TRUE,cex=1.1)

install.packages("party")
require("party")
model2<-ctree(z ~ a + b + c + d + e + f + g + h,data=d,controls=ctree_control(maxdepth=3))
plot(model2)
model3<-ctree(z ~ a + b + c + d + e + f + g + h,data=d,controls=ctree_control(maxdepth=7))
plot(model3)
