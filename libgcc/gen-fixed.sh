#!/bin/sh

# Worker script for libgcc/Makefile.in
# Generate lists of fixed-point labels, funcs, modes, from, to.
# Usage:
#    gen-fixed.sh arith labels
#    gen-fixed.sh arith funcs
#    gen-fixed.sh arith modes
#    gen-fixed.sh conv labels
#    gen-fixed.sh conv funcs
#    gen-fixed.sh conv from
#    gen-fixed.sh conv to

fixed_sfract_modes="QQ HQ SQ DQ TQ HA SA DA TA"
fixed_ufract_modes="UQQ UHQ USQ UDQ UTQ UHA USA UDA UTA"
fixed_fract_modes="$fixed_sfract_modes $fixed_ufract_modes"

fixed_signed_modes="QI HI SI DI TI SF DF"
fixed_unsigned_modes="UQI UHI USI UDI UTI"

fixed_func_names="_add _sub _neg _mul _mulhelper _divhelper _ashl _ashlhelper _cmp _saturate1 _saturate2"
fixed_sfunc_names="_ssadd _sssub _ssneg _ssmul _ssdiv _div _ssashl _ashr"
fixed_ufunc_names="_usadd _ussub _usneg _usmul _usdiv _udiv _usashl _lshr"

# emit the function information
# $1 = output type selector
# $2 = base function name
# $3 = from mode
# $4 = to mode
emit ()
{
    if [ "$3" != "$4" ]; then
	case "$1" in
	  labels)
            echo $2 ;;
	  from | modes)
	    echo $3 ;;
	  to)
	    echo $4 ;;
	  funcs)
	    echo $2$3$4 ;;
	esac
    fi
}

case "$1" in
  arith)
    for n in $fixed_func_names; do
	for m in $fixed_fract_modes; do
	    emit $2 $n $m
	done
    done

    for n in $fixed_sfunc_names; do
	for m in $fixed_sfract_modes; do
	    emit $2 $n $m
	done
    done

    for n in $fixed_ufunc_names; do
	for m in $fixed_ufract_modes; do
	    emit $2 $n $m
	done
    done
    ;;

  conv)
    for f in $fixed_fract_modes; do
	for t in $fixed_fract_modes $fixed_signed_modes; do
	    emit $2 _fract $f $t
	done
    done

    for f in $fixed_signed_modes; do
	for t in $fixed_fract_modes; do
	    emit $2 _fract $f $t
	done
    done

    for f in $fixed_fract_modes $fixed_signed_modes; do
	for t in $fixed_fract_modes; do
	    emit $2 _satfract $f $t
	done
    done

    for f in $fixed_fract_modes; do
	for t in $fixed_unsigned_modes; do
	    emit $2 _fractuns $f $t
	done
    done

    for f in $fixed_unsigned_modes; do
	for t in $fixed_fract_modes; do
	    emit $2 _fractuns $f $t
	done
    done

    for f in $fixed_unsigned_modes; do
	for t in $fixed_fract_modes; do
	    emit $2 _satfractuns $f $t
	done
    done
    ;;

esac
