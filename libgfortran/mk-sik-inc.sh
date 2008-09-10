#!/bin/sh

compile="$1"
kinds=""
possible_kinds="1 2 4 8 16"
c=0

for k in $possible_kinds; do
  echo "  integer (kind=$k) :: x" > tmp$$.f90
  echo "  x = 1_$k" >> tmp$$.f90
  echo "  end" >> tmp$$.f90
  if $compile -S tmp$$.f90 > /dev/null 2>&1; then
    kinds="$kinds $k"
    c=`expr $c + 1`
  fi
  rm -f tmp$$.*
done

echo "  integer, parameter :: c = $c"
echo "  type (int_info), parameter :: int_infos(c) = (/ &"

i=0
for k in $kinds; do
  # echo -n is not portable
  str="    int_info ($k, range(0_$k))"
  i=`expr $i + 1`
  if [ $i -lt $c ]; then
    echo "$str, &"
  else
    echo "$str /)"
  fi
done

exit 0
