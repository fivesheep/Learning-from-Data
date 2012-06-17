#  SVM classifiers based on quadprog
# 
# Author: young
###############################################################################
require('quadprog')


polynomial_kernel <- function(x1,x2,order){
	K<-(1+t(x1)%*%x2)^order
}

rbf_kernel <- function(x1,x2,gamma){
	K<-exp(-gamma*rowSums((x1-x2)^2))
}



# soft/hard-margin svm with rbf kernel
# FIXME: the QP function can't get a solution for some data when C is set
rbf_svmtrain <- function(X,Y,C=Inf, gamma=1.5,esp=1e-10){
	N<-length(Y)
	
	Dm<-matrix(0,N,N)
	# TODO: is there a way to avoid these stupid loops???
	for(i in 1:N){
		for(j in 1:N){
			Dm[i,j]<-Y[i]*Y[j]*rbf_kernel(matrix(X[i,],1),matrix(X[j,],1),gamma)
		}
	}
	Dm<-Dm+diag(N)*1e-12 # adding a very small number to the diag, some trick
	dv<-t(rep(1,N))
	meq<-1
	Am<-cbind(matrix(Y,N),diag(N))
	bv<-rep(0,1+N) # the 1 is for the sum(alpha)==0, others for each alpha_i >= 0
	
	if(C!=Inf){
		# an upper bound is given
		Am<-cbind(Am,-1*diag(N))
		bv<-c(cbind(matrix(bv,1),matrix(rep(-C,N),1)))
	}
		
	alpha_org<-solve.QP(Dm,dv,Am,meq=meq,bvec=bv)$solution
	
	indx<-which(alpha_org>esp,arr.ind=TRUE)
	alpha<-alpha_org[indx]
	nSV<-length(indx)
	
	if(nSV==0){
		throw("QP is not able to give a solution for these data points")
		
		#print("No Support Vectors were Found, change the C")
		#write.table(X,file="some_x.txt")
		#write.table(Y,file="some_y.txt")
	}
	
	Xv<-X[indx,]
	Yv<-Y[indx]
	
	# choose one of the support vector to compute b. for safety reason,
	# select the one with max alpha
	idv<-which.max(alpha)
	b<-Yv[idv]-sum(alpha*Yv*rbf_kernel(Xv,matrix(1,nSV,1)%*%Xv[idv,],gamma))
	
	list(alpha=alpha, b=b, nSV=nSV, Xv=Xv, Yv=Yv, gamma=gamma)
}

rbf_svmpredict <- function(X,model){
	N<-dim(X)[1]
	H<-matrix(0,N,1)
	
	alpha<-model$alpha
	b<-model$b
	Yv<-model$Yv
	Xv<-model$Xv
	nSV<-model$nSV
	gamma<-model$gamma
	
	H<-matrix(0,N,1)
	for(i in 1:N){
		H[i]<-sign(sum(alpha*Yv*rbf_kernel(Xv,matrix(1,nSV,1)%*%X[i,],gamma))+b)
	}
	
	H
}

# TODO: test is needed for the polynomial ones
poly_svmtrain <- function(X,Y,C=Inf,order=2,esp=1e-10){
	N<-length(Y)
	
	Dm<-matrix(0,N,N)
	# TODO: is there a way to avoid these stupid loops???
	for(i in 1:N){
		for(j in 1:N){
			Dm[i,j]<-Y[i]*Y[j]*polynomial_kernel(matrix(X[i,],1),matrix(X[j,],1),gamma)
		}
	}
	Dm<-Dm+diag(N)*1e-12 # adding a very small number to the diag, some trick
	dv<-t(rep(1,N))
	meq<-1
	Am<-cbind(matrix(Y,N),diag(N))
	bv<-rep(0,1+N) # the 1 is for the sum(alpha)==0, others for each alpha_i >= 0
	
	if(C!=Inf){
		# an upper bound is given
		Am<-cbind(Am,-1*diag(N))
		bv<-c(cbind(matrix(bv,1),matrix(rep(-C,N),1)))
	}
	
	alpha_org<-solve.QP(Dm,dv,Am,meq=meq,bvec=bv)$solution
	
	indx<-which(alpha_org>esp,arr.ind=TRUE)
	alpha<-alpha_org[indx]
	nSV<-length(indx)
	
	if(nSV==0){
		throw("QP is not able to give a solution for these data points")
	}
	
	Xv<-X[indx,]
	Yv<-Y[indx]
	
	W<-t(Xv)%*%(alpha.*Yv)
	b<-Yv - t(W) %*% Xv
	
	list(alpha=alpha, W=W, b=b, nSV=nSV, Xv=Xv, Yv=Yv, order=order)
}

poly_svmpredict <- function(X,model){
	H<-sign(t(W)%*%X+b)
}
