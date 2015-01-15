index1="全市国民经济主要指标_人口_年末总人口"
index2="年末常驻人口"
seg_index1 = qseg[index1]
seg_index2 = qseg[index2]

seg_index1 <- eraseStopWords(seg_index1,STOPWORDS)
temp <- eraseStopWords(seg_index2)

ddd=c("个数","总数")
temp <- seg_index2[!seg_index2 %in% ddd ]
seg_index2 <- temp






indexCosine("全市国民经济主要指标_人口_年末总人口", "年末常驻人口")
if(index1 == '' || index2 == ''){
  print("sss")
}
#1 Cut indexes
seg_index1 <- qseg[index1]
seg_index2 <- qseg[index2]

seg_index1 <- eraseStopWords(seg_index1)
seg_index2 <- eraseStopWords(seg_index2)

if(length(seg_index1) == 0 || length(seg_index2) == 0 || !hasSameToken(seg_index1, seg_index2)){
  print("sss")
}

#2  create temp files
td = "D:\\RWorkspaces\\Index Transformation\\DATA"
dir.create(td)
write( seg_index1, file=paste(td, "D1", sep="/"))
write( seg_index2, file=paste(td, "D2", sep="/"))


#3 read files into a document-term matrix
myMatrix = mytextmatrix(td, minWordLength=1)

#4 compute cosine
result = cosine(myMatrix[,1], myMatrix[,2])


unlink(td, recursive=TRUE)
----
text="Hellow, Adam! Hi, Adam! How are you, Adam."
sub(pattern = "Adam", replacement = "world", text)
