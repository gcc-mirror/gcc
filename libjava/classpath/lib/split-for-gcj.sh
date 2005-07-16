#! /bin/sh

# Split in multiple parts for gcj.  This uses a somewhat hacky
# procedure for finding the package of a given file.
echo "Splitting for gcj"
rm -f Makefile.deps > /dev/null 2>&1
test -d lists || mkdir lists
for dir in java javax gnu org; do
   for file in `cat classes | fgrep /$dir/`; do
      pkg=`echo "$file " | sed -n -e "s,^.*/\($dir/.*\)/[^/]*$,\1,p"`
      list=lists/`echo $pkg | sed -e 's,/,-,g'`
      echo "$file" >> ${list}.list.1
      f2=`echo "$file" | sed -n -e "s,^.*/\($dir/.*\)$,\1,p"`
      f2=${f2%.java}.class
      echo "$f2: ${list}.stamp" >> Makefile.deps
      echo "${list}.list: $file" >> Makefile.deps
   done
done

# Only update a .list file if it changed.
for file in lists/*.list.1; do
   real=${file%.1}
   if ! cmp -s $real $file; then
      mv $file $real
   else
      rm $file
   fi
done
