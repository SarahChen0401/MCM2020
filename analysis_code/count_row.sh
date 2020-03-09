
while read p;
do
	 #echo "$p" | grep -w "$p" microwave.head.txt | wc -l >> count.row.txt 
	#rm -f $1/$1.$2.count_row.txt
	#calculate the unique words occurance in review head and body
	 echo "$p" | grep -w "$p" $1/$1.$2.txt | wc -l >> $1/$1.$2.count_row.txt
#done <  microwave.head_unique_words.txt 
done < $1/$1.$2_unique_words.txt	
