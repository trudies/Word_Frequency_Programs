
#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

files <- list.files(path = paste0(getwd(),"/Results/Diversity_Measures/Simulations/"),pattern = "Sim_")

for (k in 1:length(files)){

	code <- substr(files[k],1,3)
	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])

	simulated <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Simulations/",files[k]))
	empirical <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Empirical/Languages/",code, "_Div_Meas.txt"))


	pdf (file = paste0(getwd(), "/Results/Plots/Sim_v_Emp/SimvEmp_",language,".pdf"))
	layout(matrix(1:6, nrow=2 , byrow=T))

	for (i in 5:35) {

		d <- density (simulated[,i])
		plot(d, type="n", main=colnames(simulated)[i])
		polygon(d, col="red", border="gray")
		abline(v=as.numeric(empirical$x[i]))

	}
	dev.off()
}





