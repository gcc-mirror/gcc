dnl Support macro file for intrinsic functions.
dnl Works out all the function types from the filename.
dnl This file is part of the GNU Fortran 95 Runtime Library (libgfortran)
dnl Distributed under the GNU LGPL.  See COPYING for details.
dnl M4 macro file to get type names from filenames
include(`types.m4')
define(type_letter, regexp(file, `_\([irlc]\)[^_]*$', \1))dnl
define(type_kind, regexp(file, `_[irlc]\([0-9]*\)[^_]*$', \1))dnl
define(rtype_kind, regexp(file, `_\([0-9]*\)_[irlc][0-9]*[^_]*$', `\1'))dnl
define(type_code, type_letter`'type_kind)dnl
define(type_name, get_typename(type_letter,type_kind))dnl
define(atype, get_arraytype(type_letter,type_kind))dnl
ifelse(rtype_kind,,
`define(rtype_letter,type_letter)dnl
define(rtype_name, type_name)dnl
define(rtype_code, type_code)dnl
define(rtype, atype)dnl
define(rtype_qual,`')dnl
',
`define(rtype_letter,i)dnl
define(rtype_name, get_typename(rtype_letter,rtype_kind))dnl
define(rtype, get_arraytype(rtype_letter,rtype_kind))dnl
define(rtype_qual,`_'rtype_kind)dnl
')dnl
define(type_max, type_name`_HUGE')dnl
define(type_min, `-'type_max)dnl
