#X,Y,HL,MaxIter
eta<-1

X<-matrix(c(1,2),1)
Y<-c(1)
HL<-c(3,2)



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


print(W)

for(n in 1:N){
	x<-c(1,X[n,]); y<-Y[n]
	Theta<-list(x=x)
	S<-list(NULL)
	# feed forward for all Thetas
	for(l in 2:nl){
		S[[l]]<-t(W[[l]])%*%Theta[[l-1]]
		Theta[[l]]<-c(1,tanh(S[[l]]))
	}
	
	Delta<-list()
	Delta[[nl]]<-c(0, (Theta[[nl]][-1]-y)^2)
	
	# backpropagation for W
	for(l in nl:2){
		Delta[[l-1]]<-(1-Theta[[l-1]]^2)*(W[[l]]%*%Delta[[l]][-1])
		
	}
	
	# update weights
	for(l in nl:2){
		W[[l]]<-W[[l]] - eta*Theta[[l-1]]*(Delta[[l]][-1])
#		d<-dim(W[[l]])
#		I<-d[1]
#		J<-d[2]
		
#		for(i in 1:I){
#			for(j in 1:J){
#				print(W[[l]][i,j])
#				W[[l]][i,j]<-W[[l]][i,j] - eta * Theta[[l-1]][i]*Delta[[l]][-1][j]
#				print(W[[l]][i,j])
#			}
#		}
	}
}

print("===========")
print(W)