
library (poweRlaw)


#set working directory:
#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

#all frequeny files in folder:
files <- list.files(path = paste0(getwd(),"/Datasets/Word_Lists/Generated/"),pattern = "_freqs")
		
#to decode language codes, language names, language family
decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

#power law distributions for all frequency files:
for (k in 1:length(files)){
	
	code <- substr(files[k],1,3)
	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])


	freqs <- read.table (paste0(getwd(),"/Datasets/Word_Lists/Generated/",files[k]))
	x <- freqs$n

	#POWERLAW DISTRIBUTION investigation:
	pdf (file = paste0(getwd(),"/Results/Plots/Power_Law_Fit/",language,".pdf"))

		#WE SET X-MIN TO 1 FOR ALL OF THE DISTRIBUTION OBJECTS: (in order to compare distributions)
		#Fit powerlaw:
		mx_pl <- displ$new(x)
		#Estimate x min and parameters: (x-min will be used for all other distributions)
		mx_pl$setXmin(1)
		poweralpha <- estimate_pars(mx_pl)$pars
		#update object
		mx_pl$setPars(poweralpha)



		#Fit log normal distribution:
		mx_ln <- dislnorm$new(x)
		mx_ln$setXmin(mx_pl$getXmin())
		est <- estimate_pars(mx_ln)
		mx_ln$setPars(est)


		#Fit poison:
		mx_pois <- dispois$new(x)
		mx_pois$setXmin(mx_pl$getXmin())
		est <- estimate_pars(mx_pois)
		mx_pois$setPars(est)


		#Fit exponential:
		mx_exp <- disexp$new(x)
		mx_exp$setXmin(mx_pl$getXmin())
		est <- estimate_pars(mx_exp)
		mx_exp$setPars(est)


		plot(mx_ln, ylab = "S(n)",xlab = "n")
		lines(mx_pl, col=2)
		lines(mx_ln, col=3)
		lines(mx_pois, col=4)
		lines(mx_exp, col = 5)
		title(main = paste0(language))
		legend("topright", legend=c("Power_Law", "Log-Normal", "Poisson", "Exponential"),
		       col=c(2, 3, 4,5), lty=1, cex=0.8)
	dev.off()		

}

