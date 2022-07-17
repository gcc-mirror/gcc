// { dg-options "-O1 -g0" }
// { dg-do compile }
// { dg-final { scan-assembler-not "12basic_stringIcSt11char_traitsIcESaIcEE7compare" } }

#include <string>
bool eq() { return std::string("blah") == "literal"; }
bool lt() { return std::string("blah") < "literal"; }
