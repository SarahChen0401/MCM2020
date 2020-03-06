setwd("E:\\MCM\\processed_data")
microwave.head.length <- read.table("microwave\\microwave.head.length.txt",header = F,as.is=T)
microwave.head.length <- as.matrix(unlist(microwave.head.length))
microwave.head_unique_words <- read.table("microwave\\microwave.head_unique_words.txt",as.is = T,header = F)
microwave.head_unique_words <- as.character(unlist(microwave.head_unique_words))
microwave.count.row <- read.table("microwave\\microwave.head.count_row.txt",header = F,as.is = T)
microwave.count.row <- microwave.count.row +1
filelist = list.files(path = "microwave\\",pattern = "microwave.head.[0-9].*.word_count.txt")
#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read.table(paste0("microwave\\",x), header=F,fill =T )) 
#assuming the same header/columns for all files
#datafr = do.call("rbind", datali(st) 
reviwe_head <- matrix(0,nrow = dim(microwave.head.length)[1],ncol = length(microwave.head_unique_words))
colnames(reviwe_head) <- microwave.head_unique_words
for (i in 1:length(filelist)) {
  ind <- match(as.character(unlist(datalist[[i]][2])),microwave.head_unique_words)
  non_na <- !is.na(ind)
  reviwe_head[i,na.omit(ind)] <- as.data.frame(datalist[[i]])[non_na,1]/microwave.head.length[i,1]
}

idf <- log10(length(filelist)/microwave.count.row)
TD_IDF_microwave_head <- t(apply(as.matrix(reviwe_head), 1, function(x) as.matrix(x*idf)))
colnames(TD_IDF_microwave_head) <- colnames(reviwe_head)



microwave.head.length <- read.table("hair_dryer\\hair_dryer.head.length.txt",header = F,as.is=T)
microwave.head.length <- as.matrix(unlist(microwave.head.length))
microwave.head_unique_words <- read.table("hair_dryer\\hair_dryer.head_unique_words.txt",as.is = T,header = F)
microwave.head_unique_words <- as.character(unlist(microwave.head_unique_words))
microwave.count.row <- read.table("hair_dryer\\hair_dryer.head.count_row.txt",header = F,as.is = T)
microwave.count.row <- microwave.count.row +1
filelist = list.files(path = "hair_dryer\\",pattern = "hair_dryer.head.[0-9].*.word_count.txt")
#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read.table(paste0("hair_dryer\\",x), header=F,fill =T )) 
#assuming the same header/columns for all files
#datafr = do.call("rbind", datali(st) 
reviwe_head <- matrix(0,nrow = dim(microwave.head.length)[1],ncol = length(microwave.head_unique_words))
colnames(reviwe_head) <- microwave.head_unique_words
#for (i in 1:length(filelist)) {
for (i in 1:dim(microwave.head.length)[1]) { 
 ind <- match(as.character(unlist(datalist[[i]][2])),microwave.head_unique_words)
  non_na <- !is.na(ind)
  reviwe_head[i,na.omit(ind)] <- as.data.frame(datalist[[i]])[non_na,1]/microwave.head.length[i,1]
}

idf <- log10(length(filelist)/microwave.count.row)
TD_IDF_microwave_head <- t(apply(as.matrix(reviwe_head), 1, function(x) as.matrix(x*idf)))
colnames(TD_IDF_microwave_head) <- colnames(reviwe_head)



review_mat <- function(product,reviewPart){
  microwave.head.length <- read.table(paste0(product,"\\",product,".",reviewPart,".length.txt"),header = F,as.is=T)
  microwave.head.length <- as.matrix(unlist(microwave.head.length))
  microwave.head_unique_words <- read.table(paste0(product,"\\",product,".",reviewPart,"_unique_words.txt"),as.is = T)
  microwave.head_unique_words <- as.character(unlist(microwave.head_unique_words))
  microwave.count.row <- read.table(paste0(product,"\\",product,".",reviewPart,".count_row.txt"),header = F,as.is = T)
  microwave.count.row <- microwave.count.row +1
  filelist = list.files(path = paste0(product,"\\"),pattern = paste0(product,".",reviewPart,".[0-9].*.word_count.txt"))
  #assuming tab separated values with a header    
  datalist = lapply(filelist, function(x)read.table(paste0(product,"\\",x), header=F,fill =T )) 
  #assuming the same header/columns for all files
  #datafr = do.call("rbind", datali(st) 
  
  reviwe_head <- matrix(0,nrow = dim(microwave.head.length)[1],ncol = length(microwave.head_unique_words))
  colnames(reviwe_head) <- microwave.head_unique_words
  for (i in 1:length(filelist)) {
    ind <- match(as.character(unlist(datalist[[i]][2])),microwave.head_unique_words)
    non_na <- !is.na(ind)
    reviwe_head[i,na.omit(ind)] <- as.data.frame(datalist[[i]])[non_na,1]/microwave.head.length[i,1]
  }
  
  idf <- log10(length(filelist)/microwave.count.row)
  TD_IDF_microwave_head <- t(apply(as.matrix(reviwe_head), 1, function(x) as.matrix(x*idf)))
  colnames(TD_IDF_microwave_head) <- colnames(reviwe_head)
  return(TD_IDF_microwave_head)
  
}


microwaveHead <- review_mat("microwave","head")
microwaveBody <- review_mat("microwave","body")
hair_dryerHead <- review_mat("hair_dryer","head")
hair_dryerBody <- review_mat("hair_dryer","body")
pacifierHead <- review_mat("pacifier","head")
pacifierBody <- review_mat("pacifier","body")

write.table(microwaveHead,file = "review_mat\\microwaveHead.txt", col.names = T,row.names = F,sep = "\t",quote = F)
write.table(microwaveBody,file = "review_mat\\microwaveBody.txt", col.names = T,row.names = F,sep = "\t",quote = F)

#save(microwaveHead,microwaveBody,hair_dryerHead,hair_dryerBody,pacifierHead,pacifierBody, file = "review_mat.Rdata")
save(microwaveHead,microwaveBody, file = "review_mat.Rdata")
