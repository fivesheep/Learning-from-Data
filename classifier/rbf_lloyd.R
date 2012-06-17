# The Regular RBF with lloyd's algorithm
# 
# Author: young
###############################################################################


rbf_train <- function(X,Y,K,gamma){
	esp<-1e-10 #a small enough number
	max_iter<-10000
	N<-length(Y)
	
	# init Mu by sampling K random points from X
	Mu<-X[sample(1:N,K),]
	
	Ekm<-Inf
	clusters<-vector('list',K)
	#iterate Emk to make it converage
	for(it in 1:max_iter){
		tmpEkm<-0
		#assign data point to belonging clustersre respectively
		distances<-matrix(0,N,K)
		for(k in 1:K){
			distances[,k] <- t(rowSums((X-rep(1,N)%*%t(Mu[k,]))^2))
		}
		
		belongings<-apply(distances,1,which.min)
		
		for(k in 1:K){
			indx=which(belongings==k,arr.ind=TRUE)
			n<-length(indx) #number of items
			if(n==0){
				# if the kth cluster has no data, assign a new point to it.
				# then drop this iteration
				Mu[k,]<-X[sample(1:N,1),]
				next
			}else if(n==1){
				Mu[k,]<-X[indx,]
			}else{
				Mu[k,]<-colMeans(X[indx,])
			}
			tmpEkm<-tmpEkm+sum((X[indx,]-rep(1,n)%*%t(Mu[k,]))^2)
		}
		
		if (abs(Ekm-tmpEkm)<esp){
			#break when Ekm converaged
			break;
		}else{
			Ekm<-tmpEkm
		}

	}
	
	Theta<-matrix(0,N,K)
	for(k in 1:K){
		Theta[,k]<-exp(-gamma*rowSums((X-rep(1,N)%*%t(Mu[k,]))^2))
	}
	
	W<-solve(t(Theta)%*%Theta)%*%t(Theta)%*%Y
	list(W=W,Mu=Mu,K=K,gamma=gamma)
}

rbf_predict <- function(X,rbf_model){
	W<-rbf_model$W
	Mu<-rbf_model$Mu
	gamma<-rbf_model$gamma
	K<-rbf_model$K
	
	N<-nrow(X)
	Y<-apply(X,1,
			function(x){
				sign(t(W)%*%exp(-gamma*rowSums((rep(1,K)%*%t(x)-Mu)^2)))
			})
}
	
