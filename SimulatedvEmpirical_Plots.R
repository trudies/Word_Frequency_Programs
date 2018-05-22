
setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

files <- list.files(path = paste0(getwd(),"/Results/Diversity_Measures/Simulations/"),pattern = "_Sim")


percs <- matrix(0,length(files), length((5:35)))

diffmeas <- matrix(0,length(files), length((5:35)))
for (k in 1:length(files)){

	code <- substr(files[k],1,3)
	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])

	simulated <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Simulations/",files[k]))
	empirical <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Empirical/Languages/",code, "_Div_Meas.txt"))

    #histogram:
# 	pdf (file = paste0(getwd(), "/Results/Plots/Sim_v_Emp/Histograms/", language, "_SimvEmp_hist.pdf"))
# 	layout(matrix(1:6, nrow=2 , byrow=T))
# 
# 	for (i in 5:35) {
# 
# 	  empvalue <- as.numeric(as.character(empirical$x[i]))
# 
# 
#  h <- hist(simulated[,i], freq = F, main=colnames(simulated)[i], breaks = 100,
#            xlim = c(min(min(simulated[,i]),empvalue), max(max(simulated[,i]),empvalue)),
#            xlab = colnames(simulated)[i])
#  #plot(h, main=colnames(simulated)[i], xlab = simulated[,i])
#  abline(v=empvalue, col = "red")
# 
#  	}
#  	dev.off()
 	
 	


    #Kernel density plots
# 	pdf (file = paste0(getwd(), "/Results/Plots/Sim_v_Emp/Density/", language, "_SimvEmp_dens.pdf"))
# 	layout(matrix(1:6, nrow=2 , byrow=T))
# 	
# 	for (i in 5:35) {
# 	  
# 	  empvalue <- as.numeric(as.character(empirical$x[i]))
# 	  d <- density (simulated[,i])
# 	  plot(d, type="n", main=colnames(simulated)[i], xlim = c(min(min(simulated[,i]),empvalue), max(max(simulated[,i]),empvalue)))
# 	  polygon(d, col="red", border="gray")
# 	  abline(v=empvalue)
# 	  
# 	}
# 	dev.off()
	
	
}
