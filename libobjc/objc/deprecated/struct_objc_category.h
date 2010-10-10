/*
** The compiler generates one of these structures for each category.  A class
** may have many categories and contain both instance and factory methods.  
*/
struct objc_category {
  const char*   category_name;                /* Name of the category.  Name
                                                contained in the () of the
                                                category definition. */
  const char*   class_name;                   /* Name of the class to which
                                                the category belongs. */
  MethodList_t  instance_methods;             /* Linked list of instance
                                                methods defined in the 
                                                category. NULL indicates no
                                                instance methods defined. */
  MethodList_t  class_methods;                /* Linked list of factory 
                                                methods defined in the
                                                category.  NULL indicates no
                                                class methods defined. */
  struct objc_protocol_list *protocols;	      /* List of Protocols 
					         conformed to */
};
