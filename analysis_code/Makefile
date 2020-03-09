# 2020_MCM_Problem_C
# 2020.Mar.6
#
## (1) 
unique_words:
	make _unique_words A=microwave
	make _unique_words A=hair_dryer
	make _unique_words A=pacifier
_unique_words:
	#cat $A.txt | awk -F "\t" '{print $$13}' | grep -o -E '\w+' | sort -u -f > $A.head_unique_words.txt
	#cat $A.txt | awk -F "\t" '{print $$14}' | grep -o -E '\w+' | sort -u -f > $A.body_unique_words.txt
	#print review head and body into a file
	cat $A.txt | awk -F "\t" '{print $$13}' > $A/$A.head.txt
	cat $A.txt | awk -F "\t" '{print $$14}' > $A/$A.body.txt
	#/
	#calculate the unique words in head and body 
	cat $A.head.txt  | sed -e 's/[^[:alpha:]]/ /g' | tr '\n' " " |  tr -s " " | tr " " '\n'| tr 'A-Z' 'a-z' | sort | uniq -c  | awk '{print $$2}' > $A/$A.head_unique_words.txt  
	cat $A.body.txt| sed -e 's/[^[:alpha:]]/ /g' | tr '\n' " " |  tr -s " " | tr " " '\n'| tr 'A-Z' 'a-z' | sort | uniq -c | awk '{print $$2}' > $A/$A.body_unique_words.txt
	#/
	#calculate the review body and head length
	cat $A.head.txt | awk '{ print NF, $$0 }' | awk '{print $$1}' > $A/$A.head.length.txt
	cat $A.body.txt | awk '{ print NF, $$0 }' | awk '{print $$1}' > $A/$A.body.length.txt
	#/

## (2)

count_row:
	#calculate the number of unique word occourance  in all the review head and body
	bash count_row.sh microwave head
	bash count_row.sh microwave body
	bash count_row.sh hair_dryer head
	bash count_row.sh hair_dryer body
	bash count_row.sh pacifier head
	bash count_row.sh pacifier body
	#/
## (3)

count_features:
	#calculate every words occurance in every review head and body 
	bash count_features.sh microwave head
	bash count_features.sh microwave body
	bash count_features.sh hair_dryer head
	bash count_features.sh hair_dryer body
	bash count_features.sh pacifier head
	bash count_features.sh pacifier body




