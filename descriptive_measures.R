#function: Descriptive Measures

descriptive_measures <- function(x=x,measures=measures) {

	library(plyr)
#matrix for statistical descriptive measures:
tic <- proc.time()
		
		funcvec <- matrix(0,1,length(measures))
			
		funcvec[measures=="Language"] 	<- language
		funcvec[measures=="Family"]		<- family

		N	<- sum(x)		#total number of tokens 
	 	V	<- length(x)	#number of distinct types in the set
		
		funcvec[measures=="Tokens(N)"]	<- N		#total number of tokens 
	 	funcvec[measures=="Types(V)"]	<- V	#number of distinct types in the set
				
		#DESCRIPTIVE STATISTICS

		funcvec[measures=="Mean"]		<- mean(x)
		funcvec[measures=="Std Dev"]	<- sd(x)
		funcvec[measures=="CV"]			<- sd(x)/mean(x)
		funcvec[measures=="Range"]		<- max(x) - min(x)
		funcvec[measures=="Median"] 	<- median(x)
		funcvec[measures=="Skewness"] 	<- skewness(x)
		funcvec[measures=="Kurtosis"]	<- kurtosis(x)

		#LEXICAL MEASURES - functions of vocabulary and sample size

		funcvec[measures=="TTR"] 		<- V/N #Type token ratio
		funcvec[measures=="RTTR"]		<- V/sqrt(N) #Guiraud's Root TTR
		funcvec[measures=="CTTR"] 		<- V/sqrt(N/2) # Carroll's Corrected TTR
		funcvec[measures=="HerdanC"] 	<- log(V)/log(N)	#Herdan's Law
		funcvec[measures=="Rubetk"] 	<- log(V)/log(log(N)) #Rubet's k
		funcvec[measures=="Somer"] 		<- log(log(V))/log(log(N)) #Somer Index 
		funcvec[measures=="Maas"] 		<- (log(N)-log(V))/(log(N))^2 #Maas Index
		funcvec[measures=="DugastU"] 	<- (log(N))^2/(log(N)-log(V)) # Dugast's U
		funcvec[measures=="TuldavaLN"] 	<- (1-V^2)/(V^2*log(N)) #Tuldava's LN
		funcvec[measures=="BrunetW"]	<- N^(V^(-0.172)) #Brunet's W


		#LEXICAL MEASURES - elements of frequency spectrum

		#FREQUENCY SPECTRUM V(m,N):
	 	counts <- count(x)
	 	m <- counts[,1]
	 	VmN <- counts[,2]
		frspc <-spc(VmN,m)
		hapx <- VmN[1] #number of hapax legomena
		disx <- VmN[2] #number of dis legomena

		funcvec[measures=="HapX"]	<- hapx
		funcvec[measures=="HapV"]	<- hapx/V  #proportion of hapax legomena wrt types
		funcvec[measures=="HapN"]	<- hapx/N  #proportion of hapax legomena wrt tokens
		funcvec[measures=="DisX"]	<- disx
		funcvec[measures=="DisV"] 	<- disx/V #proportion of dis legomena
		funcvec[measures=="DisN"] 	<- disx/N #proportion of dis legomena wrt tokens

		funcvec[measures=="HonoreH"] <- 100*(log(N)/(1-(hapx/V))) #Honore's H
		funcvec[measures=="SichelS"] <- (disx/V) #Sichel's S
		funcvec[measures=="MicheaM"]<- 1/(disx/V) #Michea's M
		funcvec[measures=="Entropy"] <- entropy.ChaoShen(x)#entropy




		funcvec[measures=="YuleK"] <- 10000*(sum(m^2*VmN)-N)/N^2 #Yule's K
		funcvec[measures=="SimpsonD"] <- sum(VmN*(m/N)*((m-1)/(N-1))) #Simpson's D
		funcvec[measures=="Vmod"] <- sqrt(sum(VmN*(m/N)^2)-1/V) #Modification of Yule's K

		#HYPERGEOMETRIC DISTRIBUTION OF DIVERSITY:
		k <- 42
		funcvec[measures=="HDD"] <- sum(1-exp(lgamma(N-x+1)+lgamma(N-k+1)-lgamma(N-x-k+1)-lgamma(N+1)))

		if (length(measures[measures=="ZMalpha"])==1 || length(measures[measures=="ZMB"])==1 || length(measures[measures=="fZMalpha"])==1 ||
			length(measures[measures=="fZMB"])==1 || length(measures[measures=="fZMA"])==1 || length(measures[measures=="GIGPgamma"])==1 ||
			length(measures[measures=="GIGPB"])==1 || length(measures[measures=="GIGPC"])==1 || length(measures[measures=="PowerlawAlpha"])==1){	
		# #PARAMETER ESTIMATES:
		zm <- lnre(type = "zm", spc = frspc, exact = F, method = "SANN")$param
		fzm <- lnre(type = "fzm", spc = frspc, exact = F, method = "SANN")$param
		gig <- lnre(type = "gigp", spc = frspc, exact = F, method = "SANN")$param

		#Fit powerlaw:
		mx_pl <- displ$new(x)
		mx_pl$setXmin(1)
		poweralpha <- estimate_pars(mx_pl)$pars

		funcvec[measures=="ZMalpha"]		<- zm$alpha
		funcvec[measures=="ZMB"]			<- zm$B
		funcvec[measures=="fZMalpha"]		<- fzm$alpha
		funcvec[measures=="fZMB"]			<- fzm$B
		funcvec[measures=="fZMA"] 			<- fzm$A 
		funcvec[measures=="GIGPgamma"]		<- gig$gamma
		funcvec[measures=="GIGPB"] 			<- gig$B
		funcvec[measures=="GIGPC"]			<- gig$C
		funcvec[measures=="PowerlawAlpha"]	<- poweralpha

		}


		detach("package:plyr")

	return (funcvec)


}
	

					
