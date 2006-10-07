include(head.m4)dnl
dnl
dnl This file contains the specific functions that are not handled in the
dnl m4/specific.m4 file.

#include "config.h"
#include "kinds.inc"

dnl This is from GNU m4 examples file foreach.m4:
divert(-1)
# foreach(x, (item_1, item_2, ..., item_n), stmt)
define(`foreach', `pushdef(`$1', `')_foreach(`$1', `$2',
`$3')popdef(`$1')')
define(`_arg1', `$1')
define(`_foreach',
        `ifelse(`$2', `()', ,
                `define(`$1', _arg1$2)$3`'_foreach(`$1', (shift$2),
`$3')')')
# traceon(`define', `foreach', `_foreach', `ifelse')
divert

dnl   NINT specifics
foreach(`ikind', `(4, 8, 16)', `foreach(`rkind', `(4, 8, 10, 16)', `
`#if defined (HAVE_GFC_REAL_'rkind`) && defined (HAVE_GFC_INTEGER_'ikind`)'
elemental function specific__nint_`'ikind`_'rkind (parm)
   real (kind=rkind) , intent (in) :: parm
   integer (kind=ikind) :: specific__nint_`'ikind`_'rkind
   specific__nint_`'ikind`_'rkind = nint (parm)
end function
#endif
')')

dnl   CHAR specifics
foreach(`ckind', `(1)', `foreach(`ikind', `(4, 8, 16)', `
`#if defined (HAVE_GFC_INTEGER_'ikind`)'
elemental function specific__char_`'ckind`_i'ikind (parm)
   integer (kind=ikind) , intent (in) :: parm
   character (kind=ckind,len=1) :: specific__char_`'ckind`_i'ikind
   specific__char_`'ckind`_i'ikind` = char (parm, kind='ckind`)'
end function
#endif
')')

dnl   LEN specifics
foreach(`ckind', `(1)', `foreach(`ikind', `(4, 8, 16)', `
`#if defined (HAVE_GFC_INTEGER_'ikind`)'
elemental function specific__len_`'ckind`_i'ikind (parm)
   character (kind=ckind,len=*) , intent (in) :: parm
   integer (kind=ikind) :: specific__len_`'ckind`_i'ikind
   specific__len_`'ckind`_i'ikind` = len (parm)'
end function
#endif
')')

dnl   INDEX specifics
foreach(`ckind', `(1)', `foreach(`ikind', `(4, 8, 16)', `
`#if defined (HAVE_GFC_INTEGER_'ikind`)'
elemental function specific__index_`'ckind`_i'ikind (parm1, parm2)
   character (kind=ckind,len=*) , intent (in) :: parm1, parm2
   integer (kind=ikind) :: specific__index_`'ckind`_i'ikind
   specific__index_`'ckind`_i'ikind` = index (parm1, parm2)'
end function
#endif
')')
