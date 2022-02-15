
#uploading data
library(openxlsx)
my_data<-read.xlsx("tvprices.xlsx",sheet="tvprices")
head(my_data)


#1
#sampling extraction
set.seed(123) 
sample<-my_data[sample(nrow(my_data), 15), ]
sample


#2
#columns selection
input_data<-sample[,3:7] 
input_data
#Screen: display size in inches
#DCR: dynamic contrast ratio
#LCD Hz: refresh rate
#Nat Res: display resolution
#Price

input_data_bycategory<-aggregate(x=sample[,2:7],by=list(sample$Categoria),mean)
input_data_bycategory[,-1]

input_data_st<-scale(input_data)
input_data_st
d0<-dist(input_data_st)
d0
d<-as.data.frame(as.matrix(d0))
d


Q<-matrix(0,nrow(d),nrow(d))

for(i in 1:nrow(d)){
  for(j in 1:nrow(d)){
    Q[i,j]<--0.5*(d[i,j]^2-sum(d[i,]^2)/nrow(d)-sum(d[,j]^2)/nrow(d)+sum(d^2)/nrow(d)^2) #it is possible to compute the elements of Q from d by utilizing the spectral theorem
  }
}
Q

av<-eigen(Q)
autovalori<-av$values[1:5]
lambda<-diag(autovalori)
lambda
A<-av$vectors[,1:5]
A

coord<-A%*%lambda^0.5
coord

G<-NULL 
for(i in 1:length(autovalori)){ 
  G[i]<-sum(autovalori[1:i]/sum(autovalori))
}
G 


plot(seq(1,5,1),autovalori,type="l",xlab="Number of Components", ylab=expression(lambda[k]),main="Scree Plot for Metric MDS",col="red",lwd="1")
text(autovalori,labels=as.factor(round(autovalori,digits=2)),cex=0.8)


#we choose 2 dimensions

plot(coord[,1],coord[,2],type="n",main="Metric MDS Plot for 15 tv models",xlab="Dim1",ylab="Dim2") 
text(coord,labels=names(d),cex=0.8,col="blue")
abline(v=0,h=0)


#INTERPRETATION

#x-axis is the relationship between screen size and price
x<-(input_data$Screen)/(input_data$Price)
matrix<-cbind(input_data,x)
matrix[order(matrix$x),]

#y-axis is the relationship between refresh rate and price
y<-(input_data$LCD.Hz)/(input_data$Price)
matrix<-cbind(input_data,y)
matrix[order(matrix$y),]

plot(coord[,1],coord[,2],type="n",main="Metric MDS on 15 tv models",xlab="Screen / Price",ylab="LCD.Hz / Price") 
text(coord,labels=names(d),cex=0.7)
abline(v=0,h=0,col=c("red","blue"))


#3
library(dplyr)
library(ca)
library(factoextra)

input_data2<-sample[,c(2,8:14)]
input_data2
input<-aggregate(x=input_data2[,2:8],by=list(input_data2$Categoria),sum)
input
input<-input[,-1]
input
rownames(input)<-c("1","2","3","4")

#sex is a supplemetary information
fit<-ca(input,supcol=c(1,2))
fit
summary(fit)
fviz_eig(fit,main="Scree Plot for Analysis of Corrispondences")
plot(fit,main="Analysis of Corrispondences Plot")

sample[,c(2,7)]

#x axis: age and income
plot(fit,main="Analysis of Corrispondences Plot",xlab="Income & Age")

