include(head.m4)
define(type_code,regexp(file,`_\([ircl][0-9]+\).f90',`\1'))dnl
define(type_letter,substr(type_code, 0, 1))dnl
define(type_kind,substr(type_code, 1))dnl
define(get_typename2, `$1 (kind=$2)')dnl
define(get_typename, `get_typename2(ifelse($1,i,integer,ifelse($1,r,real,ifelse($1,l,logical,ifelse($1,c,complex,unknown)))),`$2')')dnl
define(type_name, get_typename(type_letter,type_kind))dnl
define(name, regexp(regexp(file, `[^/]*$', `\&'), `^_\([^_]*\)_', `\1'))dnl
define(function_name,`specific__'name`_'type_code)dnl

elemental function function_name (p1, p2)
   type_name, intent (in) :: p1, p2
   type_name :: function_name

   function_name = name (p1, p2)
end function
