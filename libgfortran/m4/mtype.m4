dnl Get type kind from filename.
define(kind,regexp(file, `_.\([0-9]+\).c$', `\1'))dnl
define(complex_type, `GFC_COMPLEX_'kind)dnl
define(real_type, `GFC_REAL_'kind)dnl
define(q,ifelse(kind,4,f,ifelse(kind,8,`',`_'kind)))dnl
