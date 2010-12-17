typedef struct objc_method_list {
  struct objc_method_list*  method_next;    /* This variable is used to link 
                                               a method list to another.  It 
                                               is a singly linked list. */
  int            method_count;              /* Number of methods defined in 
                                               this structure. */
  Method method_list[1];                    /* Variable length 
                                               structure. */
} MethodList, *MethodList_t;
