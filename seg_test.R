library(jiebaR)
library(lsa)
source(file="myTextmatrix.R")
source(file="indexCosine.R")
STOPWORDS =read.csv(file = "D:\\RWorkspaces\\Index Transformation\\STOPWORDS.csv",header=FALSE,fileEncoding="utf-8")
STOPWORDS <- na.omit(as.vector(STOPWORDS[,2]))

#1 Parameter and file definition
inputTable = read.csv(file = "Data\\index_input.csv",
                      header=FALSE,fileEncoding="utf-8")
file.create("Data\\debug_info.csv")
yearbookIndex = na.omit(as.vector(inputTable[,1]))
databaseIndex = na.omit(as.vector(inputTable[,2]))
MAPPING_RESULT_COUNT = 8

mappingResulMatrix = matrix(,MAPPING_RESULT_COUNT, 2)
mappingResulMatrix[,1] = 0
mappingResulMatrix[,2] = ""
outputTable = matrix(,1,MAPPING_RESULT_COUNT+1)

refreshMappringResultMatrix <- function(index1, index2, currentResult, mappingMatrix){
  for(iterator in 1 : MAPPING_RESULT_COUNT){
   if(currentResult > mappingResulMatrix[iterator,1]){
      mappingMatrix[iterator,1] <- currentResult
      mappingMatrix[iterator,2] <- index2 
      break
    }
  }
  return (mappingMatrix)
}

#2 Compute cosines for every pair indexes
#  Save the top 3 yearbook indexes
for(index1 in databaseIndex){

  for(index2 in yearbookIndex){
    currentResult = indexCosine(index1,index2)

    mappingResulMatrix <- refreshMappringResultMatrix(index1, index2, currentResult,mappingResulMatrix)
    
  }
#  Sys.sleep(0.1)
 
  outputTable <- rbind(outputTable, append(index1, mappingResulMatrix[,2]))
  print(index1)
  print(mappingResulMatrix)
  mappingResulMatrix[,1] = 0
  mappingResulMatrix[,2] = ""
}

#3output the reult
write.csv(outputTable, file = "Data\\output.csv", 
          row.names = F, quote = F)

