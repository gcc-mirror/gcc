define(get_typename2, `GFC_$1_$2')dnl
define(get_typename, `get_typename2(ifelse($1,i,INTEGER,ifelse($1,r,REAL,ifelse($1,l,LOGICAL,ifelse($1,c,COMPLEX,unknown)))),`$2')')dnl
define(get_arraytype, `gfc_array_$1$2')dnl
define(name, regexp(regexp(file, `[^/]*$', `\&'), `^\([^_]*\)_', `\1'))dnl
