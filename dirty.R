eta<-0.05

X<-matrix(c(1,1,1,2,2,2,3,3,3,2,3,0,1,-1,3,4,9,1),9)
Y<-c(1,1,-1,-1,-1,1,1,1,-1)
HL<-c(4)
MaxIter<-1000

########

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
	W[[l]]<-matrix(runif(i*j,-1,1),i,j)
}
W0<-W

#######
W[[2]]<-matrix(c(-1.8204964,-0.8417196,-2.5193646,1.7519472,0.8286638,2.5595198,1.2584806,1.7138094,2.3646012,-0.5612430,-0.9287457,-0.16553627),3)
W[[3]]<-matrix(c(0.62131584,0.52831317,-0.38108404 ,0.02190942,0.96908760),5)

#######


for(it in 1:MaxIter){	
	for(n in 1:N){
		x<-c(1,X[n,]); y<-Y[n]
		Theta<-list(x=x)
		#print("thetas")
		# feed forward for all Thetas
		for(l in 2:nl){
			Theta[[l]]<-c(1,tanh(t(W[[l]])%*%Theta[[l-1]])) #Good verified!! the following are same
#			tmp<-rep(0,AL[l])
#			w<-W[[l]]
#			xx<-Theta[[l-1]]
#			for(j in 1:AL[l]){
#				for(i in 1:length(xx)){
#					tmp[j]<-tmp[j]+w[i,j]*xx[i]
#				}
#				tmp[j]<-tanh(tmp[j])
#			}
#			tmp<-c(1,tmp)
#			Theta[[l]]<-tmp
		}
		
		#Theta[[nl]]<-sign(Theta[[nl]])

		Delta<-list()
		#Delta[[nl]]<-c(0, (Theta[[nl]][-1]-y)^2)
		Delta[[nl]]<-c(0,2*(Theta[[nl]][-1]-y)*(1-Theta[[nl]][-1]^2))
		# backpropagation for W
		for(l in nl:2){
			Delta[[l-1]]<-c((1-Theta[[l-1]]^2)*(W[[l]]%*%Delta[[l]][-1]))
		}
		
		# update weights
		for(l in nl:2){
			W[[l]]<-W[[l]] - eta*Theta[[l-1]]%*%t(Delta[[l]][-1])
		}
	}
	#print(W)
}

N<-dim(X)[1]
H<-rep(0,N)
nl<-length(W)

for(n in 1:N){
	theta<-c(1,X[n,])
	for(l in 2:nl){
		theta<-c(1,tanh(t(W[[l]])%*%theta))
	}
	H[n]<-sign(theta[-1])				
}

print(W)
print(sum(H!=Y))


#[[2]]
#[,1]      [,2]     [,3]      [,4]
#[1,] -1.8777786 1.8093070 1.315721 -1.501125
#[2,] -0.8990599 0.8860787 1.771063 -1.879376
#[3,] -2.6343164 2.6745988 2.479543 -2.163145
#
#[[3]]
#[,1]
#[1,]  -999.3787
#[2,]  1000.5282
#[3,] -1000.3810
#[4,]  -999.9780
#[5,]  1000.9197
