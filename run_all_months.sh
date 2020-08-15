#!/bin/bash 
START=$(date +%s%3N);
# to calculate less months change the range 1 12
# Example  1 3 for january to march
for i in `seq 1 12`;
do
./most.out parameters.dat $i
done

END=$(date +%s%3N);
echo $((END-START)) | awk '{print "most run duration: " $1/1000 " seconds."}'
