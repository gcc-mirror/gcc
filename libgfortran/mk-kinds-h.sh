#!/bin/sh
LC_ALL=C
export LC_ALL

compile="$1"

# Possible types must be listed in ascending order
possible_integer_kinds="1 2 4 8 16"
possible_real_kinds="4 8 10 16"


largest=""
smallest=""
for k in $possible_integer_kinds; do
  echo "  integer (kind=$k) :: i" > tmp$$.f90
  echo "  i = 1_$k" >> tmp$$.f90
  echo "  end" >> tmp$$.f90
  if $compile -S tmp$$.f90 > /dev/null 2>&1; then
    s=`expr 8 \* $k`
    largest="$k"

    if [ $s -eq 128 ]; then
      prefix="__"
    else
      prefix=""
    fi

    if [ "$smallest" = "" ]; then
	smallest="$k"
    fi

    echo "typedef ${prefix}int${s}_t GFC_INTEGER_${k};"
    echo "typedef ${prefix}uint${s}_t GFC_UINTEGER_${k};"
    echo "typedef GFC_INTEGER_${k} GFC_LOGICAL_${k};"
    echo "#define HAVE_GFC_LOGICAL_${k}"
    echo "#define HAVE_GFC_INTEGER_${k}"
    echo ""
  fi
  rm -f tmp$$.*
done

echo "#define GFC_INTEGER_LARGEST GFC_INTEGER_${largest}"
echo "#define GFC_UINTEGER_LARGEST GFC_UINTEGER_${largest}"
echo "#define GFC_DEFAULT_CHAR ${smallest}"
echo ""


# Get the kind value for long double, so we may disambiguate it
# from __float128.
echo "use iso_c_binding; print *, c_long_double ; end" > tmq$$.f90
long_double_kind=`$compile -S -fdump-parse-tree tmq$$.f90 | grep TRANSFER \
			| sed 's/ *TRANSFER *//'`
rm -f tmq$$.*


for k in $possible_real_kinds; do
  echo "  real (kind=$k) :: x" > tmp$$.f90
  echo "  x = 1.0_$k" >> tmp$$.f90
  echo "  end" >> tmp$$.f90
  if $compile -S tmp$$.f90 > /dev/null 2>&1; then
    case $k in
      4) ctype="float" ; cplxtype="complex float" ; suffix="f" ;;
      8) ctype="double" ; cplxtype="complex double" ; suffix="" ;;
      10) ctype="long double" ; cplxtype="complex long double" ; suffix="l" ;;
      16) if [ $long_double_kind -eq 10 ]; then
	    ctype="__float128"
	    cplxtype="_Complex float __attribute__((mode(TC)))"
	    suffix="q"
	  else
	    ctype="long double"
	    cplxtype="complex long double"
	    suffix="l"
	  fi ;;
      *) echo "$0: Unknown type" >&2 ; exit 1 ;;
    esac

    # Check for the value of HUGE
    echo "print *, huge(0._$k) ; end" > tmq$$.f90
    huge=`$compile -S -fdump-parse-tree tmq$$.f90 | grep TRANSFER \
		| sed 's/ *TRANSFER *//' | sed 's/_.*//'`
    rm -f tmq$$.*

    # Check for the value of DIGITS
    echo "print *, digits(0._$k) ; end" > tmq$$.f90
    digits=`$compile -S -fdump-parse-tree tmq$$.f90 | grep TRANSFER \
		| sed 's/ *TRANSFER *//'`
    rm -f tmq$$.*

    # Check for the value of RADIX
    echo "print *, radix(0._$k) ; end" > tmq$$.f90
    radix=`$compile -S -fdump-parse-tree tmq$$.f90 | grep TRANSFER \
		| sed 's/ *TRANSFER *//'`
    rm -f tmq$$.*

    # Output the information we've gathered
    echo "typedef ${ctype} GFC_REAL_${k};"
    echo "typedef ${cplxtype} GFC_COMPLEX_${k};"
    echo "#define HAVE_GFC_REAL_${k}"
    echo "#define HAVE_GFC_COMPLEX_${k}"
    echo "#define GFC_REAL_${k}_HUGE ${huge}${suffix}"
    echo "#define GFC_REAL_${k}_LITERAL_SUFFIX ${suffix}"
    if [ "x$suffix" = "x" ]; then
      echo "#define GFC_REAL_${k}_LITERAL(X) (X)"
    else
      echo "#define GFC_REAL_${k}_LITERAL(X) (X ## ${suffix})"
    fi
    echo "#define GFC_REAL_${k}_DIGITS ${digits}"
    echo "#define GFC_REAL_${k}_RADIX ${radix}"
    echo ""
  fi
  rm -f tmp$$.*
done


# After this, we include a header that can override some of the 
# autodetected settings.
echo '#include "kinds-override.h"'

exit 0
