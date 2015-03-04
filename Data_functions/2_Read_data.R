#This file calls the data needed to be analyzed

#First setwd to datapath
setwd(datapath)

#Read in data
Data_OFO <- read.delim("Data_OFO1.txt")
attach(Data_OFO)

#Make selection of variables used in model
#For the first analysis I use t_fusp_t_new (follow-up in years from inclusion),einddrt_new (status yes/no pregnancy end follow-up)
#and duursubf (duration of subfertility/childwish before entering, this is left truncation time)
Data_OFO_a <- as.data.frame(cbind(sleuteli,lok.OFO,t_fusp_t_new,einddrtt_new,duursubf))

#detach and remove original data
detach(Data_OFO)
rm(Data_OFO)

sub.data <- subset(Data_OFO_a, (duursubf != 1.0000))

#set working directory back to OFO folder
setwd(funcpath)
?subset
