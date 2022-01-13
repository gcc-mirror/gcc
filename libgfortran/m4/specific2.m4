include(head.m4)
define(atype_code,regexp(file,`_\([ircl][0-9]+\).[fF]90',`\1'))dnl
define(atype_letter,substr(atype_code, 0, 1))dnl
define(atype_kind,substr(atype_code, 1))dnl
define(get_typename2, `$1 (kind=$2)')dnl
define(get_typename, `get_typename2(ifelse($1,i,integer,ifelse($1,r,real,ifelse($1,l,logical,ifelse($1,c,complex,unknown)))),`$2')')dnl
define(name_kind, ifelse(atype_kind,17,16,atype_kind))dnl
define(atype_name, get_typename(atype_letter,name_kind))dnl
define(name, regexp(regexp(file, `[^/]*$', `\&'), `^_\([^_]*\)_', `\1'))dnl
define(function_name,`_gfortran_specific__'name`_'atype_code)dnl

define(Q,dnl
ifelse(atype_kind,4,F,dnl
ifelse(atype_kind,8,`',dnl
ifelse(atype_kind,10,L,dnl
ifelse(atype_kind,16,L,dnl
ifelse(atype_kind,17,L,dnl
`_'atype_kind))))))dnl

#include "config.h"
#include "kinds.inc"
#include "c99_protos.inc"

`#if defined (HAVE_GFC_'ifelse(atype_letter,l,LOGICAL,ifelse(atype_letter,i,INTEGER,ifelse(atype_letter,r,REAL,ifelse(atype_letter,c,COMPLEX,UNKNOW))))`_'atype_kind`)'

ifelse(name,atan2,`#ifdef HAVE_ATAN2'Q,)

elemental function function_name (p1, p2)
   atype_name, intent (in) :: p1, p2
   atype_name :: function_name

   function_name = name (p1, p2)
end function

ifelse(name,atan2,`#endif',)

#endif
