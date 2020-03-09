i=0;
while read p; 
do
	i=$((i+1))
	#echo "$p"
	#echo "$i"
	#calculate the every word occurance in one review head and body 
	echo  "$p" | sed -e 's/[^[:alpha:]]/ /g' | tr '\n' " " |  tr -s " " | tr " " '\n'| tr 'A-Z' 'a-z' | sort | uniq -c >  $1/$1.$2.$i.word_count.txt
#done < a.txt
done < $1/$1.$2.txt

 




