// This is only in a header so we can use the system_header pragma,
// to suppress the warning caused by using a reserved init_priority.
#pragma GCC system_header

[[gnu::init_priority(98)]]
constinit tzdb_list tzdb_list::_Node::_S_the_list(nullptr);

[[gnu::init_priority(98)]]
constinit tzdb_list::_Node::head_ptr tzdb_list::_Node::_S_head_owner{nullptr};

#if USE_ATOMIC_LIST_HEAD
[[gnu::init_priority(98)]]
constinit atomic<tzdb_list::_Node*> tzdb_list::_Node::_S_head_cache{nullptr};
#endif
