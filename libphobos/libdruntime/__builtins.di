/* This D file is implicitly imported by all ImportC source files.
 * It provides definitions for C compiler builtin functions and declarations.
 * The purpose is to make it unnecessary to hardwire them into the compiler.
 * As the leading double underscore suggests, this is for internal use only.
 *
 * Copyright: Copyright Digital Mars 2022
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 * Source: $(DRUNTIMESRC __builtins.d)
 */


module __builtins;

/* gcc relies on internal __builtin_xxxx functions and templates to
 * accomplish <stdarg.h>. D does the same thing with templates in core.stdc.stdarg.
 * Here, we redirect the gcc builtin declarations to the equivalent
 * ones in core.stdc.stdarg, thereby avoiding having to hardware them
 * into the D compiler.
 */

import core.stdc.stdarg;

alias va_list = core.stdc.stdarg.va_list;

version (Posix)
{
    version (X86_64)
        alias __va_list_tag = core.stdc.stdarg.__va_list_tag;
}

alias __builtin_va_start = core.stdc.stdarg.va_start;

alias __builtin_va_end = core.stdc.stdarg.va_end;

alias __builtin_va_copy = core.stdc.stdarg.va_copy;

/* dmd's ImportC rewrites __builtin_va_arg into an instantiation of va_arg
 */
alias va_arg = core.stdc.stdarg.va_arg;
