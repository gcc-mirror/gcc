/**
 * Contains object comparator functions called by generated code.
 *
 * Copyright: Copyright Digital Mars 2002 - 2010.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 */

/*          Copyright Digital Mars 2000 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module rt.obj;

extern (C):

/********************************
 * Compiler helper for operator == for class objects.
 */

int _d_obj_eq(Object o1, Object o2)
{
    return o1 is o2 || (o1 && o1.opEquals(o2));
}


/********************************
 * Compiler helper for operator <, <=, >, >= for class objects.
 */

int _d_obj_cmp(Object o1, Object o2)
{
    return o1.opCmp(o2);
}
