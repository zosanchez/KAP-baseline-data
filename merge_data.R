read.csv("C:\\Users\\zoeea\\OneDrive\\Documents\\Immune\\Data\\original_data")
data_catch<- read.csv("Data/data_catchments_complete.csv", header=TRUE)
#Merge cleaned immune survey data with data_catchments_complete
merge(data, data_catch, by="UNICODE")
data<- merge(data, data_catch, by="UNICODE")