include(head.m4)
define(atype_code,regexp(file,`_\([ircl][0-9]+\).[fF]90',`\1'))dnl
define(atype_letter,substr(atype_code, 0, 1))dnl
define(atype_kind,substr(atype_code, 1))dnl
define(get_typename2, `$1 (kind=$2)')dnl
define(get_typename, `get_typename2(ifelse($1,i,integer,ifelse($1,r,real,ifelse($1,l,logical,ifelse($1,c,complex,unknown)))),`$2')')dnl
dnl
dnl KIND=17 is special: This is just a naming convention
dnl in the library, for Fortran source code KIND=16 is needed
dnl
define(name_kind, ifelse(atype_kind,17,16,atype_kind))dnl
define(atype_name, get_typename(atype_letter,name_kind))dnl
define(name, regexp(regexp(file, `[^/]*$', `\&'), `^_\([^_]*\)_', `\1'))dnl
define(rtype_name,get_typename(ifelse(name,abs,ifelse(atype_letter,c,r,atype_letter),ifelse(name,aimag,ifelse(atype_letter,c,r,atype_letter),atype_letter)),name_kind))dnl
define(function_name,dnl
ifelse(name,conjg,`_gfortran_specific__conjg_'atype_kind,`_gfortran_specific__'name`_'atype_code))dnl

define(type,dnl
ifelse(atype_letter,l,LOGICAL,dnl
ifelse(atype_letter,i,INTEGER,dnl
ifelse(atype_letter,r,REAL,dnl
ifelse(atype_letter,c,COMPLEX,UNKNOW)))))dnl
define(Q,ifelse(atype_kind,4,F,
ifelse(atype_kind,8,`',
ifelse(atype_kind,10,L,
ifelse(atype_kind,16,L,
ifelse(atype_kind,17,L,
`_'atype_kind))))))dnl

dnl A few specifics require a function other than their name, or
dnl nothing. The list is currently:
dnl    - integer and logical specifics require no libm function
dnl    - AINT requires the trunc() family functions
dnl    - ANINT requires round()
dnl    - AIMAG, CONJG, DIM, SIGN require no libm function
define(needed,dnl
ifelse(atype_letter,i,`none',dnl
ifelse(atype_letter,l,`none',dnl
ifelse(name,aint,trunc,dnl
ifelse(name,anint,round,dnl
ifelse(name,aimag,none,dnl
ifelse(name,conjg,none,dnl
ifelse(name,dim,none,dnl
ifelse(name,sign,none,dnl
ifelse(name,abs,fabs,name))))))))))dnl
define(prefix,ifelse(atype_letter,c,C,`'))dnl

dnl Special case for fabs, for which the corresponding complex function
dnl is not cfabs but cabs.
define(NEEDED,translit(ifelse(prefix`'needed,`Cfabs',`abs',needed),`a-z',`A-Z'))dnl

#include "config.h"
#include "kinds.inc"
#include "c99_protos.inc"

`#if defined (HAVE_GFC_'type`_'atype_kind`)'
ifelse(NEEDED,NONE,`',`#ifdef HAVE_'prefix`'NEEDED`'Q)

elemental function function_name (parm)
   atype_name, intent (in) :: parm
   rtype_name :: function_name

   function_name = name (parm)
end function

ifelse(NEEDED,NONE,`',`#endif')
#endif
