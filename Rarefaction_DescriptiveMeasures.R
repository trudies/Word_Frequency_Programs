
library (poweRlaw)
library (car)
library(moments)
library(entropy)
library(zipfR)
library(tm)
library(readtext)
library (tidytext)
library (dplyr)


#set working directory
#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

#call diversity measure function
source(paste0(getwd(),"/Programs/descriptive_measures.R"))

#identify measures to calculate
measures <- c("Language","Family","Tokens(N)", "Types(V)", "Std Dev", "CV", "Range","Median","Skewness", 
		"Kurtosis", "TTR", "RTTR", "HerdanC", "Rubetk", "Somer", "DugastU", "TuldavaLN",
		"BrunetW", "HapV", "HonoreH", "SichelS", "HapN", "DisN", "Entropy", "YuleK",
		"HDD", "ZMalpha", "ZMB", "fZMalpha", "fZMB", "fZMA", "GIGPgamma", "GIGPB", "GIGPC","PowerlawAlpha")


#decode file: language code, language name, language family:
decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

#all sentence files in folder (original sentences, sine rarefaction relies on re-sampling from original data)
files <- list.files(path = paste0(getwd(),"/Datasets/Sentences"),pattern = "sentences")

for (k in 1: length(files)){
	
	text <- gsub("\\b\\d+\\b", "", as.character(readtext(paste0(getwd(),"/Datasets/Sentences/",files[k]))))
	code <- substr(files[k],1,3)
	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])

	#wordlist for total empirical data set:
	text_df <- data_frame(line = 1, text = text)
	tidy <- text_df %>% unnest_tokens(word, text)
	freq <- tidy %>% count(word, sort = TRUE)
	x <- freq$n
	N <- sum(freq$n)
	V <- length(freq$n)


	nobs <- 5 #how many different sampling points
	incs <- round(N/nobs)#increments so that we have nobs different sampled Ns
	nsm <- 5 #number of samples for each rarefaction datapoint

	sequence <- rbind(as.matrix(seq(10000,N,incs)),N)#include total number of tokens (empirical data)
	nsmmat <- matrix(0,nsm,length(measures)) #empty matrix for each rarefaction point: to obtain the mean value.
	filemat <- matrix(0,length(sequence),length(measures))#empty matrix for this specific language k
				colnames(filemat) <- measures
			
	#sampling of sn number of tokens:
	i <-  1
		for (sn in sequence){
		
		X <- tidy #from the original sentence data set

		if (sn==N){#if sn=N, use complete wordlist from empirical data set
			sampleN <- X
			freqs <- sampleN %>% count(word, sort = T)
			x <- freqs$n
			Ns <- sum(freqs$n)
			Vs <- length(freqs$n)

			filemat[i,] <- descriptive_measures(x,measures)

		} else{#if sn !=N, we sample number of sn words out of original data, nsm number of times.		
			
			for (j in 1: nsm){
				sampleN <- X[sample(nrow(X),size=sn,replace=FALSE),]	
				freqs <- sampleN %>% count(word, sort = T)
				x <- freqs$n
				Ns <- sum(freqs$n)
				Vs <- length(freqs$n)
				nsmmat[j,] <- descriptive_measures(x,measures)#calculate descriptive measures for this sample
		

				filemat[i,1:2] <- nsmmat[j,1:2] #language, family from descriptive measure function
				}

			allm <- matrix(as.numeric(nsmmat[,3:35]), nrow=nsm, ncol=33) # MEAN of nsm samples' value from descriptive measure function

			filemat[i,3:35] <- colMeans(allm)
			}	
		 
		i <- i+1
	}

	write.table (filemat,file=paste0(getwd(),"/Results/Diversity_Measures/Rarefaction/", code,"_SamplesN",nobs,".txt"))
 
}

