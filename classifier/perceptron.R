# TODO: Add comment
# 
# Author: young
###############################################################################

perceptron_train <- function(X,y,maxIter=10000){
	d<-dim(X)
	X<-cbind(rep(1,d[1]),X)
	W<-rep(0,d[2]+1) # init W with 0s
	
	count<-0
	while(count<maxIter){
		H<-sign(X%*%W)
		# sample 1 data point from X, which H disagreed with Y
		indexs<-which(H!=Y)
		if(length(indexs==0)){
			break
		}else{
			i<-sample(indexs,1)
			W<-W+X[i,]*Y[i]
		}
		count<-count+1	
	}
	
	W
}

perceptron_predict <- function(X,W){
	X<-cbind(rep(1,dim(X)[1]),X)
	H<-sign(X%*%W)
}
