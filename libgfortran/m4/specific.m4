include(head.m4)
define(atype_code,regexp(file,`_\([ircl][0-9]+\).f90',`\1'))dnl
define(atype_letter,substr(atype_code, 0, 1))dnl
define(atype_kind,substr(atype_code, 1))dnl
define(get_typename2, `$1 (kind=$2)')dnl
define(get_typename, `get_typename2(ifelse($1,i,integer,ifelse($1,r,real,ifelse($1,l,logical,ifelse($1,c,complex,unknown)))),`$2')')dnl
define(atype_name, get_typename(atype_letter,atype_kind))dnl
define(name, regexp(regexp(file, `[^/]*$', `\&'), `^_\([^_]*\)_', `\1'))dnl
define(function_name,`specific__'name`_'atype_code)dnl

elemental function function_name (parm)
   atype_name, intent (in) :: parm
   atype_name :: function_name

   function_name = name (parm)
end function
