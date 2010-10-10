/*
** The compiler generates one (or more) of these structures for a class that
** has methods defined in its specification. 
** 
** The implementation of a class can be broken into separate pieces in a file
** and categories can break them across modules. To handle this problem is a
** singly linked list of methods. 
*/
struct objc_method {
  SEL         method_name;                  /* This variable is the method's 
                                               name.  It is a char*. 
                                               The unique integer passed to 
                                               objc_msg_send is a char* too.  
                                               It is compared against 
                                               method_name using strcmp. */
  const char* method_types;                 /* Description of the method's
                                               parameter list.  Useful for
                                               debuggers. */
  IMP         method_imp;                   /* Address of the method in the 
                                               executable. */
};

