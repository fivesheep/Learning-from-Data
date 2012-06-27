# TODO: Add comment
# 
# Author: young
###############################################################################

# HL is a vector contains the number nodes for each hidden layer, sequentially
# eta is the learning rate
# esp is the stop condition
bpnn_train <- function(X,Y,HL,eta=0.1,esp=0.01){
	
	dx<-dim(X)
	N<-dx[1]
	mx<-dx[2]
	# uses single output for this algorithm
	my<-1
	
	AL<-c(mx,HL,my) #All layers
	nl<-length(AL) # number of layers
	
	# Init Weights Randomly
	W<-list(NULL) # No w for the first layer
	for(l in 2:nl){
		i<-AL[l-1]+1;j<-AL[l]
		W[[l]]<-matrix(runif(i*j,-2,2),i,j)
	}
	
	# SGD based iterations to train the networks
	err<-1
	while(err>esp){
		error_count<-0
		# an epoch of training
		for(n in sample.int(N)){
			x<-c(1,X[n,]); y<-Y[n]
			Theta<-list(x=x)
			# feed forward for all Thetas
			for(l in 2:nl){
				Theta[[l]]<-c(1,tanh(t(W[[l]])%*%Theta[[l-1]]))
			}
			
			# backpropagation for W
			Delta<-list()
			#compute the delta for the last layer
			Delta[[nl]]<-c(0,2*(Theta[[nl]][-1]-y)*(1-Theta[[nl]][-1]^2))
			for(l in nl:2){
				Delta[[l-1]]<-c((1-Theta[[l-1]]^2)*(W[[l]]%*%Delta[[l]][-1]))	
			}
			# update weights
			for(l in nl:2){
				W[[l]]<-W[[l]] - eta*Theta[[l-1]]%*%t(Delta[[l]][-1])
			}
			if(sign(Theta[[nl]][-1])!=y){
				error_count<-error_count+1
			}			
		}
		err<-error_count/N
	}
	W
}

bpnn_predict <- function(X, W){
	N<-dim(X)[1]
	Y<-rep(0,N)
	nl<-length(W)
	
	for(n in 1:N){
		theta<-c(1,X[n,])
		for(l in 2:nl){
			theta<-c(1,tanh(t(W[[l]])%*%theta))
		}
		Y[n]<-sign(theta[-1])				
	}
	
	Y
}