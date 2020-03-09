#####question1:convert reviews to matrix#######
setwd("E:\\MCM\\processed_data")
microwave.head.length <- read.table("microwave\\microwave.head.length.txt",header = F,as.is=T)
microwave.head.length <- as.matrix(unlist(microwave.head.length)) ##review's length
microwave.head_unique_words <- read.table("microwave\\microwave.head_unique_words.txt",as.is = T,header = F)
microwave.head_unique_words <- as.character(unlist(microwave.head_unique_words))#unique words
microwave.count.row <- read.table("microwave\\microwave.head.count_row.txt",header = F,as.is = T)
microwave.count.row <- as.data.frame(microwave.count.row +1) ##the number of occourance of unique words
microwave.count.row$word <-  microwave.head_unique_words
filelist = list.files(path = "microwave\\",pattern = "microwave.head.[0-9].*.word_count.txt")#the number of each word in one review
#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read.table(paste0("microwave\\",x), header=F,fill =T,as.is=T)) 
#assuming the same header/columns for all files
#datafr = do.call("rbind", datali(st) 
TF <- matrix(0,nrow = dim(microwave.head.length)[1],ncol = length(microwave.head_unique_words))
colnames(TF) <- microwave.head_unique_words
library(stringr)
for (i in 1:length(filelist)) {
  data <- matrix(unlist(datalist[[i]]),ncol = 2, byrow = F)#convert list to matrix
  name <- filelist[i]
  number <- as.numeric(str_extract( name,'[0-9]+'))#extract the file ture number
  ind <- match(as.character(data[,2]),microwave.head_unique_words)
  non_na <- !is.na(ind)
  TF[number,na.omit(ind)]  <- as.numeric(data[non_na,1])/microwave.head.length[number,1]
}

IDF <- log10(length(filelist)/microwave.count.row[,1])##IDF matrix (number of reviews/number of unique words' occourance in all reviews)
#TF_IDF_microwave_head <- t(apply(as.matrix(TF), 1, function(x) as.matrix(x*IDF)))##TF-IDF matrix
#colnames(TF_IDF_microwave_head) <- colnames(TF)
TF1 <- TF[1:round(dim(TF)[1]/2),]
TF2 <- TF[round(dim(TF)[1]/2)+1:dim(TF)[1],]
#rm(TF)
TF_IDF_microwave_head1 <- t(apply(as.matrix(TF1), 1, function(x) as.matrix(x*IDF)))
TF_IDF_microwave_head2 <- t(apply(as.matrix(TF2), 1, function(x) as.matrix(x*IDF)))
TF_IDF_microwave_head <- rbind(TF_IDF_microwave_head1,TF_IDF_microwave_head2)
colnames(TF_IDF_microwave_head) <- microwave.head_unique_words
rm(TF1,TF2,TF_IDF_microwave_head1,TF_IDF_microwave_head2)

setwd("E:\\MCM\\processed_data")
review_mat <- function(product,reviewPart){
  microwave.head.length <- read.table(paste0(product,"\\",product,".",reviewPart,".length.txt"),header = F,as.is=T)
  microwave.head.length <- as.matrix(unlist(microwave.head.length))
  microwave.head_unique_words <- read.table(paste0(product,"\\",product,".",reviewPart,"_unique_words.txt"),as.is = T,blank.lines.skip=F,header=F)
  microwave.head_unique_words <- as.character(unlist(microwave.head_unique_words[,1]))
  microwave.count.row <- read.table(paste0(product,"\\",product,".",reviewPart,".count_row.txt"),header = F,as.is = T)
  microwave.count.row <- as.data.frame(microwave.count.row +1) ##the number of occourance of unique words
  microwave.count.row$word <-  microwave.head_unique_words
  filelist = list.files(path = paste0(product,"\\"),pattern = paste0(product,".",reviewPart,".[0-9].*.word_count.txt"))
  #assuming tab separated values with a header    
  datalist = lapply(filelist, function(x)read.table(paste0(product,"\\",x), header=F,fill =T ,as.is=T)) 
  #assuming the same header/columns for all files
  #datafr = do.call("rbind", datali(st) 
  TF <- matrix(0,nrow = dim(microwave.head.length)[1],ncol = length(microwave.head_unique_words))
  colnames(TF) <- microwave.head_unique_words
  library(stringr)
  for (i in 1:length(filelist)) {
    data <- matrix(unlist(datalist[[i]]),ncol = 2, byrow = F)#convert list to matrix
    name <- filelist[i]
    number <- as.numeric(str_extract( name,'[0-9]+'))#extract the file ture number
    ind <- match(as.character(data[,2]),microwave.head_unique_words)
    non_na <- !is.na(ind)
    TF[number,na.omit(ind)]  <- as.numeric(data[non_na,1])/microwave.head.length[number,1]
  }
  
  IDF <- log10(length(filelist)/microwave.count.row[,1])##IDF matrix (number of reviews/number of unique words' occourance in all reviews)
  #TF_IDF_microwave_head <- t(apply(as.matrix(TF), 1, function(x) as.matrix(x*IDF)))##TF-IDF matrix
  TF1 <- TF[1:round(dim(TF)[1]/2),]
  TF2 <- TF[(round(dim(TF)[1]/2)+1):dim(TF)[1],]
  rm(TF)
  TF_IDF_microwave_head1 <- t(apply(as.matrix(TF1), 1, function(x) as.matrix(x*IDF)))
  TF_IDF_microwave_head2 <- t(apply(as.matrix(TF2), 1, function(x) as.matrix(x*IDF)))
  TF_IDF_microwave_head <- rbind(TF_IDF_microwave_head1,TF_IDF_microwave_head2)
  colnames(TF_IDF_microwave_head) <- microwave.head_unique_words
  rm(TF1,TF2,TF_IDF_microwave_head1,TF_IDF_microwave_head2)
  return(TF_IDF_microwave_head)
}


microwaveHead <- review_mat("microwave","head")
microwaveBody <- review_mat("microwave","body")
hair_dryerHead <- review_mat("hair_dryer","head")
hair_dryerBody <- review_mat("hair_dryer","body")
pacifierHead <- review_mat("pacifier","head")
pacifierBody <- review_mat("pacifier","body")

write.table(microwaveHead,file = "review_mat\\microwaveHead.txt", col.names = T,row.names = F,sep = "\t",quote = F)
write.table(microwaveBody,file = "review_mat\\microwaveBody.txt", col.names = T,row.names = F,sep = "\t",quote = F)
write.table(hair_dryerHead,file = "review_mat\\hair_dryerHead.txt", col.names = T,row.names = F,sep = "\t",quote = F)
write.table(hair_dryerBody,file = "review_mat\\hair_dryerBody.txt", col.names = T,row.names = F,sep = "\t",quote = F)
write.table(pacifierHead,file = "review_mat\\pacifierHead.txt", col.names = T,row.names = F,sep = "\t",quote = F)
write.table(pacifierBody,file = "review_mat\\pacifierBody.txt", col.names = T,row.names = F,sep = "\t",quote = F)

save(microwaveHead,microwaveBody,hair_dryerHead,hair_dryerBody,pacifierHead,pacifierBody, file = "review_mat\\review_mat.Rdata")
