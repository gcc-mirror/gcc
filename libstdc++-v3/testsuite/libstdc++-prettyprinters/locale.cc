// { dg-do run }
// { dg-options "-g -O0" }
// { dg-require-namedlocale "fr_FR.ISO8859-15" }
// { dg-require-namedlocale "de_DE.ISO8859-15" }
// { dg-require-namedlocale "en_US.ISO8859-1" }

#include <locale>
#include <testsuite_hooks.h> // for ISO_8859 macro

int main()
{
  std::locale l1 = std::locale::classic();
// { dg-final { note-test l1 {std::locale = "C"} } }

  std::locale l2(ISO_8859(15,fr_FR));
// { dg-final { regexp-test l2 {std::locale = "fr_FR.ISO8859-15(@euro)?"} } }

  std::locale l3(l2, ISO_8859(15,de_DE), std::locale::time);
// { dg-final { regexp-test l3 {std::locale = "fr_FR.ISO8859-15(@euro)?" with "LC_TIME=de_DE.ISO8859-15(@euro)?"} } }

  std::locale l4(l3, ISO_8859(1,en_US), std::locale::monetary);
// We don't know which order the categories will occur in the string,
// so test three times, checking for the required substring each time:
// { dg-final { regexp-test l4 {std::locale = "(.*;)?LC_CTYPE=fr_FR.ISO8859-15(@euro)?(;.*)?"} } }
  std::locale l5 = l4;
// { dg-final { regexp-test l5 {std::locale = "(.*;)?LC_TIME=de_DE.ISO8859-15(@euro)?(;.*)?"} } }
  std::locale l6 = l5;
// { dg-final { regexp-test l6 {std::locale = "(.*;)?LC_MONETARY=en_US.ISO8859-1(;.*)?"} } }

  std::locale l7(l1, &std::use_facet<std::ctype<char> >(l1));
// { dg-final { regexp-test l7 {std::locale = "\*"} } }

  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
