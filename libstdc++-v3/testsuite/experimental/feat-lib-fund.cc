// { dg-options "-std=gnu++14" }
// { dg-do compile }

#include <experimental/optional>
#include <experimental/string_view>

#if !__has_include(<experimental/optional>)
#  error "<experimental/optional>"
#endif

//#if !__has_include(<experimental/net>)
//#  error "<experimental/net>"
//#endif

#if !__has_include(<experimental/any>)
#  error "<experimental/any>"
#endif

//#if !__has_include(<experimental/memory_resource>)
//#  error "<experimental/memory_resource>"
//#endif

#if !__has_include(<experimental/string_view>)
#  error "<experimental/string_view>"
#endif
