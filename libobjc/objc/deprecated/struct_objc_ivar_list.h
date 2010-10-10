typedef struct objc_ivar_list {
  int   ivar_count;                             /* Number of structures (Ivar) 
                                                  contained in the list.  One
                                                  structure per instance 
                                                  variable defined in the
                                                  class. */
  struct objc_ivar ivar_list[1];               /* Variable length 
                                                  structure. */
} IvarList, *IvarList_t;

