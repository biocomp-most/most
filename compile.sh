START=$(date +%s%3N);
echo "MOST compilation started"
CURRENTDIR="$(pwd)"

# See : https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-developer-guide-and-reference/top/compiler-reference/compiler-options/alphabetical-list-of-compiler-options.html
# -g = debug for idb --> https://software.intel.com/content/www/us/en/develop/articles/using-intel-debugger-for-linux-on-the-command-line.html
# -traceback = to localize execution errors at run time

ifort -O3 -fpe0 -implicitnone -traceback -o most.out  -module $CURRENTDIR/modules/ \
   -I $CURRENTDIR/cdflib/   @sourcesall.dat  $CURRENTDIR/cdflib/cdflib.a 

END=$(date +%s%3N);
echo $((END-START)) | awk '{print "----- Compilation ended after " $1/1000 " seconds. -----"}'
