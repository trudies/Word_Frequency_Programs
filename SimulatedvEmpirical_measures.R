
setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/Project")

decode <- read.csv(paste0(getwd(),"/Programs/Decode.csv"), header = TRUE, sep = ";", quote = "", allowEscapes = TRUE)

files <- list.files(path = paste0(getwd(),"/Results/Diversity_Measures/Simulations/"),pattern = "_Sim")

#create diffmeas (our variation of a z-score) and percentiles empty matrices
diffmeas <- matrix(0,length(files), length((4:14)))
percs <- matrix(0,length(files), length((4:14)))

for (k in 1:length(files)){


	code <- substr(files[k],1,3)
	language <- as.character(decode[as.character(decode[,1]) == code,2])
	family <- as.character(decode[as.character(decode[,1]) == code,3])

	simulated <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Simulations/",files[k]))
	empirical <- read.table(paste0(getwd(),"/Results/Diversity_Measures/Empirical/Languages/",code, "_Div_Meas.txt"))

		#our measure for difference (Tokens, and all measures except types)
    	for (i in c(3,5:14)) {
    	  
    	  empvalue <- as.numeric(as.character(empirical$x[i]))
    	  sims <- simulated[,i]
    	  d <- mean(sims)-empvalue
    	  
        	  if (d<=0){
        	    newrange <- sims[sims>=mean(sims)]
        	  } else{
        	    newrange <- sims[sims<=mean(sims)]
        	  }
    	 #complete diffmeas matrix with values of our measurement 
        diffmeas[k,1]  <- as.numeric(as.character(empirical$x[3]))
    	  diffmeas[k,i-3] <- d/sum((newrange-mean(sims))^2)
    	  
    	 #compleate percs matrix with values of percentiles 
    	 f <- ecdf(simulated[,i])
    	 percs[k,1]  <- as.numeric(as.character(empirical$x[3]))
    	 percs[k,i-3] <- f(empvalue) 
    	  
    	}
	
#vector of language names to name rows.
	if (k==1){
	  rowlangs <- language}else{
	    rowlangs <- rbind(rowlangs,language)
	  }
	
} 
	
	colnames(diffmeas)<-colnames(simulated[,c(3,5:14)])
	rownames(diffmeas)<- rowlangs
	
	colnames(percs)<-colnames(simulated[,c(3,5:14)])
	rownames(percs)<- rowlangs
	
	
	#exclude measures based solely on token-type ratios
	percs <- percs[, colSums(is.na(diffmeas)) != nrow(diffmeas)]
	diffmeas <- diffmeas[, colSums(is.na(diffmeas)) != nrow(diffmeas)]

	#plotting:
	
	#diffmeas
	pdf (file = paste0(getwd(), "/Results/Plots/Sim_v_Emp/Scores_log_N.pdf"))
	layout(matrix(1:6, nrow=2 , byrow=T))

	for (i in 2:dim(diffmeas)[2]) {
    y <- sign(diffmeas[,i])*log(abs(diffmeas[,i])+1)
	  h <- plot(diffmeas[,1],y,
	            main=colnames(diffmeas)[i],
	            ylab = "Similarity_score",
	            xlab = paste0("log_",colnames(diffmeas)[1]),
	            log='x')
	}
	dev.off()

#percentiles
pdf (file = paste0(getwd(), "/Results/Plots/Sim_v_Emp/percentiles_log_N.pdf"))
	layout(matrix(1:6, nrow=2 , byrow=T))

	for (i in 2:dim(percs)[2]) {
	  
	  y <- sign(percs[,i])*log(abs(percs[,i])+1)
	  h <- plot(y,
	            main=colnames(percs)[i],
	            ylab = "Similarity_score",
	            xlab = paste0("log_",colnames(percs)[1]), 
	            log='x')

# 	 h <- hist(diffmeas[,i], freq = F,
#            main=colnames(diffmeas)[i],
#            xlab = colnames(diffmeas)[i])

	}
	dev.off()

	
	#to determine which of the languages lie far below the rest for each of the measures
	diffmeasy<-diffmeas
	
	for (i in 2:dim(diffmeas)[1]) {
	  for (j in 2:dim(diffmeas)[2]){
	  diffmeasy[i,j] <- sign(diffmeas[i,j])*log(abs(diffmeas[i,j])+1)
	  }}
	
	for (j in 1:dim(diffmeas)[2]){
	  
	  langie <- which(diffmeasy[,j]==min(diffmeasy[,j]))
	  print(colnames(diffmeas)[j])
	  #print(diffmeas[1])
	  print(langie)
	  
	}