// { dg-do preprocess { target c++14 } }

#if !__has_include(<experimental/tuple>)
#  error "<experimental/tuple>"
#endif

#if !__has_include(<experimental/type_traits>)
#  error "<experimental/type_traits>"
#endif

#if !__has_include(<experimental/ratio>)
#  error "<experimental/ratio>"
#endif

#if !__has_include(<experimental/chrono>)
#  error "<experimental/chrono>"
#endif

#if !__has_include(<experimental/system_error>)
#  error "<experimental/system_error>"
#endif

#if !__has_include(<experimental/functional>)
#  error "<experimental/functional>"
#endif

#if !__has_include(<experimental/optional>)
#  error "<experimental/optional>"
#endif

#if !__has_include(<experimental/any>)
#  error "<experimental/any>"
#endif

#if !__has_include(<experimental/string_view>)
#  error "<experimental/string_view>"
#endif

//#if !__has_include(<experimental/memory>)
//#  error "<experimental/memory>"
//#endif

//#if !__has_include(<experimental/memory_resource>)
//#  error "<experimental/memory_resource>"
//#endif

//#if !__has_include(<experimental/future>)
//#  error "<experimental/future>"
//#endif

#if !__has_include(<experimental/algorithm>)
#  error "<experimental/algorithm>"
#endif

//#if !__has_include(<experimental/net>)
//#  error "<experimental/net>"
//#endif
