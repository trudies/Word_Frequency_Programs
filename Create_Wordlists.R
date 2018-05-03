
library(tm)
library(readtext)
library (tidytext)
library (dplyr)


#set working directory
#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

#decode file: language code, language name, language family:
decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

#all sentence files in folder 
files <- list.files(path = paste0(getwd(),"/Datasets/Sentences"),pattern = "sentences")

#create word lists for all languages with package tidytext
for (k in 1:length(files)){
	#k=1
	text <- gsub("\\b\\d+\\b", "", as.character(readtext(paste0(getwd(),"/Datasets/Sentences/",files[k]))))
	code <- substr(files[k],1,3)

	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])


	text_df <- data_frame(line = 1, text = text)
	tidy <- text_df %>% unnest_tokens(word, text)
	freq <- tidy %>% count(word, sort = TRUE)
	x <- freq$n
	N <- sum(freq$n)
	V <- length(freq$n)


write.table (freq,file=paste0(getwd(),"/Datasets/Word_Lists/Generated_code/", code,"_freqs.txt"))

}