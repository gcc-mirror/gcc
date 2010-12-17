struct objc_protocol_list {
  struct objc_protocol_list *next;
  size_t count;
  Protocol *list[1];
};
