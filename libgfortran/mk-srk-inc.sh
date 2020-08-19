#!/bin/sh

if test "$#" -ne 2; then
  echo "Usage $0 real_kinds compile"
  exit 1
fi

# Possible kinds must be listed in ascending order
possible_real_kinds="$1"
compile="$2"

kinds=""
c=0

for k in $possible_real_kinds; do
  echo "  real (kind=$k) :: x" > tmp$$.f90
  echo "  x = 1.0_$k" >> tmp$$.f90
  echo "  end" >> tmp$$.f90
  if $compile -S tmp$$.f90 > /dev/null 2>&1; then
    kinds="$kinds $k"
    c=`expr $c + 1`
  fi
  rm -f tmp$$.*
done

echo "  integer, parameter :: c = $c"
echo "  type (real_info), parameter :: real_infos(c) = (/ &"

i=0
for k in $kinds; do
  # echo -n is not portable
  str="    real_info ($k, precision(0.0_$k), range(0.0_$k), radix(0.0_$k))"
  i=`expr $i + 1`
  if [ $i -lt $c ]; then
    echo "$str, &"
  else
    echo "$str /)"
  fi
done

exit 0
