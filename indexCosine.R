### -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
###   Index Cosine
###
###   Compute the cosine value between two token vectors 
### -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
###
###Dependencies
###library(jiebaR)
###library(lsa)
###source("myTextmatrix.R")
###


hasSameToken <- function(vector1,vector2){
  for(token1 in vector1){
    for(token2 in vector2){
      if (token1 == token2)
        return (TRUE)
    }
  }
  return (FALSE)
}

eraseStopWords <- function(x, stopwords=STOPWORDS){
  newVector <- x[!x %in% stopwords]
  return (newVector)
}

preProcess <- function(yearbookIndex){
  processedIndex <- sub(pattern = "[^_]*_", replacement="", yearbookIndex)
  return (processedIndex)
}

indexCosine <- function(index1, index2, preprocess_flag = TRUE){
  if(index1 == '' || index2 == ''){
    return (0)
  }
  #0 Pre-process
  if(preprocess_flag){
    index2 <- preProcess(index2)
  }
  
  
  #1 Cut indexes
  seg_index1 <- qseg[index1]
  seg_index2 <- qseg[index2]
  
  seg_index1 <- eraseStopWords(seg_index1)
  seg_index2 <- eraseStopWords(seg_index2)
  
  if(length(seg_index1) == 0 || length(seg_index2) == 0 || !hasSameToken(seg_index1, seg_index2)){
    return (0)
  }
  
  #2  create temp files
  td = "TempWorkFile"
  dir.create(td)
  write( seg_index1, file=paste(td, "D1", sep="/"))
  write( seg_index2, file=paste(td, "D2", sep="/"))
  
  if(!file.exists("TempWorkFile\\D1") || !file.exists("TempWorkFile\\D2")){
    return (0)
  }else{
    #3 read files into a document-term matrix
    myMatrix = mytextmatrix(td, minWordLength=1)

    #4 compute cosine
    result = cosine(myMatrix[,1], myMatrix[,2])
  
#    unlink(td, recursive=TRUE)
    return (result)  
  }
}


