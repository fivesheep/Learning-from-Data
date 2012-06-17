# TODO: Add comment
# 
# Author: young
###############################################################################

lr_train <- function(X,Y){
	W<-solve(t(X)%*%X)%*%t(X)%*%Y
}

lr_predict <- function(X,W){
	X<-cbind(rep(1,dim(X)[1]),X)
	H<-sign(X%*%W)
}

