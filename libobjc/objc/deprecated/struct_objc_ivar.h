/*
** The compiler generates one of these structures for a class that has
** instance variables defined in its specification. 
*/
typedef struct objc_ivar {
    const char* ivar_name;                      /* Name of the instance
                                                  variable as entered in the
                                                  class definition. */
    const char* ivar_type;                      /* Description of the Ivar's
                                                  type.  Useful for 
                                                  debuggers. */
    int        ivar_offset;                    /* Byte offset from the base 
                                                  address of the instance 
                                                  structure to the variable. */
} *Ivar_t;
