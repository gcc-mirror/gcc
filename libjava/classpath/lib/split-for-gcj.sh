#! /bin/sh

# This script is used when compiling Classpath with gcj.  The idea is
# to compile one package at a time, and only recompile packages when
# actually required.

# We build java->class by package so we need to know what .java files
# correspond to what package.

# We have a .stamp file for each package; this is the makefile target.
# We also have a .list file for each package, which lists all the
# input files in that package.

# gen-classlist.sh makes a list of all the .java files we are going to compile.

# This script generates Makefile.deps, which looks like this:
# 
# java/awt/AWTUtilities.class: lists/java-awt.stamp
# lists/java-awt.list: /home/aph/gcc/gcc/libjava/classpath/gnu/java/awt/AWTUtilities.java
# java/awt/BitMaskExtent.class: lists/java-awt.stamp
# lists/java-awt.list: /home/aph/gcc/gcc/libjava/classpath/gnu/java/awt/BitMaskExtent.java
# java/awt/BitwiseXORComposite.class: lists/java-awt.stamp
# lists/java-awt.list: /home/aph/gcc/gcc/libjava/classpath/gnu/java/awt/BitwiseXORComposite.java

# This uses a somewhat hacky procedure for finding the package of a
# given file.

echo "Splitting for gcj"
rm -f Makefile.dtmp > /dev/null 2>&1
test -d lists || mkdir lists
for dir in java javax gnu org; do
   fgrep /$dir/ classes | while read file; do
      pkg=`echo "$file " | sed -n -e "s,^.*/\($dir/.*\)/[^/]*$,\1,p"`
      list=lists/`echo $pkg | sed -e 's,/,-,g' | cut -f1-3 -d-`
      echo "$file" >> ${list}.list.1
      f2=`echo "$file" | sed -n -e "s,^.*/\($dir/.*\)$,\1,p"`
      f2=`echo "$f2" | sed -e 's/.java$//'`.class
      echo "$f2: ${list}.stamp" >> Makefile.dtmp
      echo "${list}.list: $file" >> Makefile.dtmp
   done
done

# Only update a .list file if it changed.
for file in lists/*.list.1; do
   real=`echo "$file" | sed -e 's/.1$//'`
   if cmp -s $real $file; then
      rm $file
   else
      mv $file $real
   fi
done

# If we were run we must update Makefile.deps.
mv Makefile.dtmp Makefile.deps
