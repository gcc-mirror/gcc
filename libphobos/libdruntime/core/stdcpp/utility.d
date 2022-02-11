/**
 * D header file for interaction with Microsoft C++ <utility>
 *
 * Copyright: Copyright (c) 2018 D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/utility.d)
 */

module core.stdcpp.utility;

import core.stdcpp.xutility : StdNamespace;

extern(C++, (StdNamespace)):
@nogc:

/**
* D language counterpart to C++ std::pair.
*
* C++ reference: $(LINK2 https://en.cppreference.com/w/cpp/utility/pair)
*/
struct pair(T1, T2)
{
    ///
    alias first_type = T1;
    ///
    alias second_type = T2;

    ///
    T1 first;
    ///
    T2 second;

    // FreeBSD has pair as non-POD so add a contructor
    version (FreeBSD)
    {
        this(T1 t1, T2 t2) inout
        {
            first  = t1;
            second = t2;
        }
        this(ref return scope inout pair!(T1, T2) src) inout
        {
            first  = src.first;
            second = src.second;
        }
    }
}
