
#setwd("/Users/trudiestrauss/Dropbox/Research/PhD/Word_Frequency/")

#project folder
dir.create(paste0(getwd(),"/Project")) #-> this should be the working directory.

	#subfolder1: Datasets
	dir.create(paste0(getwd(),"/Project/Datasets"))
		dir.create(paste0(getwd(),"/Project/Datasets/Sentences"))
		dir.create(paste0(getwd(),"/Project/Datasets/Word_Lists"))
			dir.create(paste0(getwd(),"/Project/Datasets/Word_Lists/Generated"))

	#subfolder2: Programs (code in github repository)
	dir.create(paste0(getwd(),"/Project/Programs"))

	#subfolder3: Results
	dir.create(paste0(getwd(),"/Project/Results"))
		dir.create(paste0(getwd(),"/Project/Results/Diversity_Measures"))
			dir.create(paste0(getwd(),"/Project/Results/Diversity_Measures/Empirical"))
				dir.create(paste0(getwd(),"/Project/Results/Diversity_Measures/Empirical/Languages"))
			dir.create(paste0(getwd(),"/Project/Results/Diversity_Measures/Rarefaction"))
			dir.create(paste0(getwd(),"/Project/Results/Diversity_Measures/Simulated"))
		dir.create(paste0(getwd(),"/Project/Results/Plots"))
			dir.create(paste0(getwd(),"/Project/Results/Plots/Power_Law_Fit"))
			dir.create(paste0(getwd(),"/Project/Results/Plots/Rarefaction"))
			dir.create(paste0(getwd(),"/Project/Results/Plots/Sim_v_Emp"))



