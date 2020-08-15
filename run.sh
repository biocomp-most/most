START=$(date +%s%3N);
./most.out parameters.dat $1
END=$(date +%s%3N);
echo $((END-START)) | awk '{print "most run duration: " $1/1000 " seconds."}'
