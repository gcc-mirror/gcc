/**
 * Varargs implementation for the GNU compilers (Gnu D Compiler)
 * Used by core.stdc.stdarg and core.vararg.
 *
 * Reference: https://github.com/ARM-software/abi-aa/blob/master/aapcs64/aapcs64.rst#appendix-variable-argument-lists
 *
 * Copyright: Copyright D Language Foundation 2025
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Various
 * Source: $(DRUNTIMESRC core/internal/vararg/gnu.d)
 */

module core.internal.vararg.gnu;

version (GNU):

import gcc.builtins;

@nogc:
nothrow:

/**
 * The argument pointer type.
 */
alias va_list = __gnuc_va_list;
alias __gnuc_va_list = __builtin_va_list;

/**
 * Initialize ap.
 * parmn should be the last named parameter.
 */
void va_start(T)(out va_list ap, ref T parmn);

/**
 * Retrieve and return the next value that is of type T.
 */
T va_arg(T)(ref va_list ap); // intrinsic

/**
 * Retrieve and store in parmn the next value that is of type T.
 */
void va_arg(T)(ref va_list ap, ref T parmn); // intrinsic

/**
 * End use of ap.
 */
alias va_end = __builtin_va_end;

/**
 * Make a copy of ap.
 */
alias va_copy = __builtin_va_copy;
