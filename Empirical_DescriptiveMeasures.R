library (poweRlaw)
library (car)
library(moments)
library(entropy)
library(zipfR)


#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

options(stringsAsFactors=FALSE) 

tic <- proc.time()

#call function for descriptive measures
source(paste0(getwd(),"/Programs/descriptive_measures.R"))
#identify measures included in final data
measures <- c( "Language","Family", "Tokens(N)", "Types(V)", "Std Dev", "CV", "Range","Median","Skewness", 
			"Kurtosis", "TTR", "RTTR", "HerdanC", "Rubetk", "Somer", "DugastU", "TuldavaLN",
			"BrunetW", "HapV", "HonoreH", "SichelS", "HapN", "DisN", "Entropy", "YuleK",
			"HDD", "ZMalpha", "ZMB", "fZMalpha", "fZMB", "fZMA", "GIGPgamma", "GIGPB", "GIGPC","PowerlawAlpha")
				
		
#to decode language codes, language names, language family
decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

#all frequency files (generated word lists)
files <- list.files(path = paste0(getwd(),"/Datasets/Word_Lists/Generated/"),pattern = "_freqs")

#empty matrix for descriptive measures:
filemat <- matrix(0,length(files),length(measures))
colnames(filemat) <- measures
rownames(filemat) <- files

for (k in 1:length(files)){

	code <- substr(files[k],1,3)
	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])


	freq <- read.table (paste0(getwd(),"/Datasets/Word_Lists/Generated/",files[k]))
	x <- freq$n


	#DESCRIPTIVE MEASURES
	filemat[k,] <- descriptive_measures(x,measures)

	#write measures individually per language:
	write.table (filemat[k,],file=paste0(getwd(),"/Results/Diversity_Measures/Empirical/Languages/", code,"_Div_Meas.txt"))
				

}

write.table (filemat,file=paste0(getwd(),"/Results/Diversity_Measures/Empirical/Div_Meas_All.txt"))


toc <- proc.time()

			