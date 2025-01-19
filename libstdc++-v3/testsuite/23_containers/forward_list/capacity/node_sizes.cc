// { dg-do compile { target c++11 } }

#include <forward_list>

#if _GLIBCXX_FWDLIST_USE_ALLOC_PTR

#ifdef _GLIBCXX_DEBUG
namespace C = std::_GLIBCXX_STD_C;
#else
namespace C = std;
#endif

// We use double here because for ADJUST_FIELD_ALIGN targets (like i386)
// its alignment differs when used as a data member or as a complete object.
static_assert(sizeof(C::_Fwd_list_node<double>)
	      == sizeof(C::__fwdlist::_Node<double*>),
	      "node types have same size");
static_assert(alignof(C::_Fwd_list_node<double>)
	      == alignof(C::__fwdlist::_Node<double*>),
	      "node types have same alignment");
static_assert(__alignof(C::_Fwd_list_node<double>)
	      == __alignof(C::__fwdlist::_Node<double*>),
	      "node types have same preferred alignment");
#endif
