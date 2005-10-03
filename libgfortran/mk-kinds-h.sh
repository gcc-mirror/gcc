#!/bin/sh

compile="$1"

# Possible types must be listed in ascending order
possible_integer_kinds="1 2 4 8 16"
possible_real_kinds="4 8 10 16"


largest=""
for k in $possible_integer_kinds; do
  echo "  integer (kind=$k) :: i" > tmp$$.f90
  echo "  end" >> tmp$$.f90
  if $compile -c tmp$$.f90 > /dev/null 2>&1; then
    s=`expr 8 \* $k`
    largest="$k"

    if [ $s -eq 128 ]; then
      prefix="__"
    else
      prefix=""
    fi

    echo "typedef ${prefix}int${s}_t GFC_INTEGER_${k};"
    echo "typedef ${prefix}uint${s}_t GFC_UINTEGER_${k};"
    echo "typedef GFC_INTEGER_${k} GFC_LOGICAL_${k};"
    echo "#define HAVE_GFC_LOGICAL_${k}"
    echo "#define HAVE_GFC_INTEGER_${k}"
  fi
  rm -f tmp$$.*
done

echo "#define GFC_INTEGER_LARGEST GFC_INTEGER_${largest}"
echo "#define GFC_UINTEGER_LARGEST GFC_UINTEGER_${largest}"
echo ""


largest_ctype=""
for k in $possible_real_kinds; do
  echo "  real (kind=$k) :: x" > tmp$$.f90
  echo "  end" >> tmp$$.f90
  if $compile -c tmp$$.f90 > /dev/null 2>&1; then
    case $k in
      4) ctype="float" ;;
      8) ctype="double" ;;
      10) ctype="long double" ;;
      16) ctype="long double" ;;
      *) echo "$0: Unknown type" >&2 ; exit 1 ;;
    esac
    largest_ctype="$ctype"
    echo "typedef ${ctype} GFC_REAL_${k};"
    echo "typedef complex ${ctype} GFC_COMPLEX_${k};"
    echo "#define HAVE_GFC_REAL_${k}"
    echo "#define HAVE_GFC_COMPLEX_${k}"
  fi
  rm -f tmp$$.*
done

case $largest_ctype in
  float) echo "#define GFC_REAL_LARGEST_FORMAT \"\"" ;;
  double) echo "#define GFC_REAL_LARGEST_FORMAT \"l\"" ;;
  "long double") echo "#define GFC_REAL_LARGEST_FORMAT \"L\"" ;;
  *) echo "$0: Unknown type" >&2 ; exit 1 ;;
esac
echo "#define GFC_REAL_LARGEST $largest_ctype"

exit 0
