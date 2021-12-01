/**
 * D header file for interaction with C++ std::type_traits.
 *
 * Copyright: Copyright Digital Mars 2018.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/type_traits.d)
 */

module core.stdcpp.type_traits;

extern(C++, "std"):

///
struct integral_constant(T, T Val)
{
    ///
    enum T value = Val;
    ///
    alias value_type = T;
    ///
    alias type = typeof(this);
}

///
alias bool_constant(bool b) = integral_constant!(bool, b);

// Useful for dealing with enable_if constraints.
///
alias true_type  = bool_constant!true;
///
alias false_type = bool_constant!false;

///
struct is_empty(T)
{
    static if (is(T == struct))
        private enum __is_empty = T.tupleof.length == 0;
    else
        private enum __is_empty = false;

    ///
    enum bool value = __is_empty;
    ///
    alias value_type = bool;
    ///
    alias type = integral_constant!(bool, value);
}
