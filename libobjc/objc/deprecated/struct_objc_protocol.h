typedef struct objc_protocol {
  struct objc_class* class_pointer;
  char *protocol_name;
  struct objc_protocol_list *protocol_list;
  struct objc_method_description_list *instance_methods, *class_methods; 
} Protocol;
