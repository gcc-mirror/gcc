/**
 * The vararg module is intended to facilitate vararg manipulation in D.
 * It should be interface compatible with the C module "stdarg," and the
 * two modules may share a common implementation if possible (as is done
 * here).
 * Copyright: Copyright Digital Mars 2000 - 2009.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Walter Bright, Hauke Duden
 * Source:    $(DRUNTIMESRC core/_vararg.d)
 */

/*          Copyright Digital Mars 2000 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.vararg;

public import core.stdc.stdarg;
