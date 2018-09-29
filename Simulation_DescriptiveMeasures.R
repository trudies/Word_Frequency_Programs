
library (poweRlaw)
library(moments)
library(entropy)
library(zipfR)

options(stringsAsFactors=FALSE) 

#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

#call function for descriptive measures
source(paste0(getwd(),"/Programs/descriptive_measures.R"))
#identify measures included in final data
# measures <- c( "Language","Family", "Tokens(N)", "Types(V)", "Std Dev", "CV", "Range","Median","Skewness", 
# 			"Kurtosis", "TTR", "RTTR", "HerdanC", "Rubetk", "Somer", "DugastU", "TuldavaLN",
# 			"BrunetW", "HapV", "HonoreH", "SichelS", "HapN", "DisN", "Entropy", "YuleK",
# 			"HDD", "ZMalpha", "ZMB", "fZMalpha", "fZMB", "fZMA", "GIGPgamma", "GIGPB", "GIGPC","PowerlawAlpha")
measures <- c( "Language","Family", "Tokens(N)", "Types(V)", "Std Dev", "TTR", 
               "HapV", "SichelS", "HapN", "DisN", "Entropy", "YuleK",
               "HDD", "PowerlawAlpha")				
		
#to decode language codes, language names, language family
decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

#Base simulations on dimentions of empirical data
files <- list.files(path = paste0(getwd(),"/Results/Diversity_Measures/Empirical/Languages"))

numsim <- 50 #number of simulated sets for each language

#create empty matrix for simulated sets:
simmat <- matrix(0,numsim,length(measures))
colnames(simmat) <- measures

#create numsim simulated sets FOR EACH LANGUAGE:
for (k in 1:length(files)){

tic <- proc.time()

	code <- substr(files[k],1,3)
	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])

	empirical <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Empirical/Languages/",files[k]))

	tokens <- as.numeric(empirical$x[3])
	types <- as.numeric(empirical$x[4])

	poweralpha <- as.numeric(empirical$x[14]) #powerlawalpha estimated already

	for (i in 1:numsim){
		#simulation of values from a powerlaw distribution:
		#Generate PowerLaw distribution:
		m_pl <- displ$new()
		m_pl$setXmin(1)
		m_pl$setPars(poweralpha)

		expected_pl <- sort(dist_rand(m_pl,types),decreasing =T)
		prob <- expected_pl/sum(expected_pl)

		#Generate powerlaw distribution with the same number of tokens, types (but no values of 0):
		pl_sample <-rmultinom(1, tokens-types, prob)+1
		x <- sort(pl_sample, decreasing = T)


		simmat[i,] <- descriptive_measures(x,measures)
	}
		
write.table(simmat, file=paste0("Results/Diversity_Measures/Simulations/",code,"_Sim.txt"))
}
toc <- proc.time()

