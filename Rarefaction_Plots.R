

library (ggplot2)


#set working directory
#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

files <- list.files(paste0(getwd(),"/Results/Diversity_Measures/Rarefaction"))

#first file read into matrix
allmat <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Rarefaction/",files[1]))

for (k in 2:length(files)){#read all other files into same matrix

filemat <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Rarefaction/",files[k]))
allmat <- rbind(allmat,filemat)

}

datf <- data.frame(allmat)#matrix as a dataframe

dats<- subset(datf,Tokens.N. > 1000)#only plot points where N is larger than 1000
meas <- colnames(datf)

pdf(file = paste0(getwd(),"Results/Plots/Rarefaction/InfluenceLine.pdf"))
layout(matrix(1:9, nrow=3 , byrow=T))
	
for (i in 4:35){
d <- ggplot(dats, aes_string(x=meas[3], y=meas[i])) +  geom_line(aes(colour = Language))
print(d)
}

dev.off()


