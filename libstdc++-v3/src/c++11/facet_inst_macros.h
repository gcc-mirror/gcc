#ifndef _FACET_INST_MACROS_H
#define _FACET_INST_MACROS_H

// These macros are used for explicit instantiation definitions in src/c++11/

#define INSTANTIATE_USE_FACET(...)			    \
  template const __VA_ARGS__*				    \
    __try_use_facet< __VA_ARGS__ >(const locale&) noexcept; \
  template const __VA_ARGS__&				    \
    use_facet<__VA_ARGS__>(const locale&)

#define INSTANTIATE_FACET_ACCESSORS(...)		    \
  INSTANTIATE_USE_FACET(__VA_ARGS__);			    \
  template bool						    \
    has_facet<__VA_ARGS__>(const locale&) noexcept

#endif // _FACET_INST_MACROS_H
