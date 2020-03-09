##II.e. calculate the chi-square test on the relationship between "positive/negtive words" and "star_rating" 
##confused matrix
chisq<-matrix(0,ncol = 2,nrow = 3)
colnames(chisq) <- c("X-squared","p_value")
rownames(chisq) <- c("pacifier","hair_dryer","microwave")

pac <- matrix(c(15280,551,
                1571,	747),ncol = 2,byrow = T)
colnames(pac) <- c("postive",	"negive")
rownames(pac) <- c("1","0")
chisq[1] <-chisq.test(pac)$p.value

hair<- matrix(c(8229,	820,
                1268,	982),ncol = 2,byrow = T)
colnames(hair) <- c("postive",	"negive")
rownames(hair) <- c("1","0")
chisq[2] <-chisq.test(hair)$p.value

mic <- matrix(c(1327,	81,
                353,	342),ncol = 2,byrow = T)
colnames(mic) <- c("postive",	"negive")
rownames(mic) <- c("1","0")
chisq[3] <-chisq.test(pac)$p.value
i=1
for (i in 1:3) {
  a <- list(pac,hair,mic)
  chisq[i,1] <-chisq.test(a[[i]])$statistic
  chisq[i,2] <-chisq.test(a[[i]])$p.value
}

chisq
#/calculate the chi-square test on the relationship between "positive/negtive words" and "star_rating" 


#calculate the pearson correlation & t test  on the relationship between "positive/negtive words" and "star_rating" 
word <- read.table("E:\\MCM\\processed_data\\2.txt",fill = T,header = F)
ratio <- word[,c(5,6,11,12)]
pac <- ratio[3:11,]
hair <- ratio[13:21,]
mic <- ratio[23:35,]
pearson<-matrix(0,ncol = 6,nrow = 3)
colnames(pearson) <- c("correlation_pos","cor.p_pos","ttest.p_pos","correlation_neg","cor.p_neg","ttest.p_neg")
rownames(pearson) <- c("pacifier","hair_dryer","microwave")

#pearson&ttest
i=1
for (i in 1:3) {
  a <- list(pac,hair,mic)
  pearson[i,1] <- cor.test(as.numeric(a[[i]]$V5),as.numeric(a[[i]]$V6),method = "pearson")$estimate
  pearson[i,2] <- p.adjust(cor.test(as.numeric(a[[i]]$V5),as.numeric(a[[i]]$V6),method = "pearson")$p.value,"fdr")
  pearson[i,3] <- t.test(as.numeric(a[[i]]$V5),as.numeric(a[[i]]$V6))$p.value
  pearson[i,4] <- cor.test(as.numeric(a[[i]]$V11),as.numeric(a[[i]]$V12),method = "pearson")$estimate
  pearson[i,5] <- p.adjust(cor.test(as.numeric(a[[i]]$V11),as.numeric(a[[i]]$V12),method = "pearson")$p.value,"fdr")
  pearson[i,6] <- t.test(as.numeric(a[[i]]$V11),as.numeric(a[[i]]$V12))$p.value
}
pearson <- t(round(pearson,4))
