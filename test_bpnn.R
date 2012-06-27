# TODO: Add comment
# 
# Author: young
###############################################################################

source('classifier/bpnn.R',local=TRUE)

generate_data <- function(N,d,FUN){
	X<-matrix(runif(N*d,-1,1),nrow=N)
	Y<-FUN(X)
	list(X=X,Y=Y)
}


test_bpnn <- function(){
	N<-1000
	Ntest<-N*4
	
	# a complex target function
	target<-function(x){x-0.25*sin(pi*x)-0.5*x^3+0.5*cos(pi*x^2)+0.6*x^4-0.2*x^5}
	fun<-function(X){sign(X[,2]-(X[,1]-0.25*sin(pi*X[,1])-0.5*X[,1]^3+0.5*cos(pi*X[,1]^2)+0.6*X[,1]^4-0.2*X[,1]^5)) }
	par(bg="grey")
	plot(target,-1,1)
	
	train_data <- generate_data(N,2,fun)
	X<-train_data$X
	Y<-train_data$Y
	
	# add some noise
	indx<-sample.int(N,20)
	Y[indx]<--1*Y[indx]
	
	W<-bpnn_train(X,Y,c(3,6,3),0.05,0.03,5000)
	H<-bpnn_predict(X,W)
	
	test_data<-generate_data(Ntest,2, fun)
	
	Xout<-test_data$X
	Yout<-test_data$Y
	
	Hout<-bpnn_predict(Xout,W)
    
	indP<-which(Hout>0)
	indM<-which(Hout<0)
	points(Xout[indP,],col="blue")
	points(Xout[indM,],col="red")
			
	s<-sprintf("Ein=%.2f, Eout=%.2f", sum(H!=Y)/N, sum(Hout!=Yout)/Ntest)
	print(s)

}


test_bpnn()