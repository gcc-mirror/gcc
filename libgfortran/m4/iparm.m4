dnl Support macro file for intrinsic functions.
dnl Works out all the function types from the filename.
dnl This file is part of the GNU Fortran 95 Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
dnl M4 macro file to get type names from filenames
define(get_typename2, `GFC_$1_$2')dnl
define(get_typename, `get_typename2(ifelse($1,i,INTEGER,ifelse($1,r,REAL,ifelse($1,l,LOGICAL,ifelse($1,c,COMPLEX,ifelse($1,m,UINTEGER,ifelse($1,s,UINTEGER,unknown)))))),`$2')')dnl
define(get_arraytype, `gfc_array_$1$2')dnl
define(define_type, `dnl
ifelse(regexp($2,`^[0-9]'),-1,`dnl
define($1_letter, substr($2, 0, 1))dnl
define($1_kind, substr($2, 1))dnl
',`dnl
define($1_letter,i)dnl
define($1_kind,$2)dnl
')dnl
define($1_code,$1_letter`'$1_kind)dnl
define($1,get_arraytype($1_letter,$1_kind))dnl
define($1_name, get_typename($1_letter, $1_kind))')dnl
dnl
define_type(atype, regexp(file, `_\(.?[0-9]*\)\.c$', `\1'))dnl
define(rtype_tmp, regexp(file, `_\(.?[0-9]*\)_[^_]*\.c$', `\1'))dnl
ifelse(rtype_tmp,,`dnl
define_type(rtype, atype_code)dnl
define(rtype_qual,`')dnl
',`dnl
define_type(rtype, rtype_tmp)dnl
define(rtype_qual,`_'rtype_kind)dnl
')dnl
define(atype_max, atype_name`_HUGE')dnl
define(atype_min,ifelse(index(atype_name,`GFC_UINTEGER'),0,`0',ifelse(regexp(file, `_\(.\)[0-9]*\.c$', `\1'),`i',`(-'atype_max`-1)',`-'atype_max)))dnl
define(atype_inf, atype_name`_INFINITY')dnl
define(atype_nan, atype_name`_QUIET_NAN')dnl
define(name, regexp(regexp(file, `[^/]*$', `\&'), `^\([^_]*\)_', `\1'))dnl
define(`upcase', `translit(`$*', `a-z', `A-Z')')dnl
define(`u_name',`regexp(upcase(name),`\([A-Z]*\)',`\1')')dnl
define(rtype_ccode,ifelse(rtype_letter,`i',rtype_kind,rtype_code))dnl
define(initval,ifelse(index(name,`maxval'),0,0,index(name,`minval'),0,255))dnl
define(back_arg,ifelse(index(name,`maxloc'),0,``, GFC_LOGICAL_4 back'',dnl
index(name,`minloc'),0,``, GFC_LOGICAL_4 back''))dnl
