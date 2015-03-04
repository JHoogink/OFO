#Specify path to find data and functions
datapath <- "F://Data"
funcpath <- "D:/Dropbox/UU M&S/Thesis/OFO/Data_functions"

#Specify working directory
wd <- "D:/Dropbox/UU M&S/Thesis/OFO"


#Call data
setwd(funcpath)
source('2_Read_data.R')
#Call likelihood function 
source('3_Likelihoodfunctions.R')

setwd(wd)



