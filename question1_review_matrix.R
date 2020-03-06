setwd("E:\\MCM\\processed_data")
microwave.head.length <- read.table("a\\microwave.head.length.txt",header = F)
microwave.head_unique_words <- read.table("a\\microwave.head_unique_words.txt",as.is = T)
microwave.head_unique_words <- microwave.head_unique_words[,2]
filelist = list.files(path = "a\\",pattern = "^[0-9].*.txt")
#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read.table(paste0("a\\",x), header=F)) 
#assuming the same header/columns for all files
#datafr = do.call("rbind", datali(st) 
microwave.head.length <- microwave.head.length[1:10,]
reviwe_mat <- matrix(0,nrow = length(microwave.head.length),ncol = length(microwave.head_unique_words))
for (i in 1:10) {
  ind <- match(as.character(unlist(datalist[[i]][2])),microwave.head_unique_words)
  reviwe_mat[i,ind] <- unlist(datalist[[i]][1])/microwave.head.length[i]

}
