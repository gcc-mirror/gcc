/**********************************************
 * This module implements common builtins for the D frontend.
 *
 * Copyright: Copyright © 2019, The D Language Foundation
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Walter Bright
 * Source:    $(DRUNTIMESRC core/builtins.d)
 */

module core.builtins;

version (GNU)
    public import gcc.builtins;

version (LDC)
    public import ldc.intrinsics;

/// Writes `s` to `stderr` during CTFE (does nothing at runtime).
void __ctfeWrite(scope const(char)[] s) @nogc @safe pure nothrow {}
