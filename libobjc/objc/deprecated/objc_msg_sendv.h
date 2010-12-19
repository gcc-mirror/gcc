/* The following types and functions are provided only for
   backwards-compatibility and should not be used in new code.  They
   were deprecated in GCC 4.6 and will be removed in the next
   release.  */
typedef void* retval_t;		/* return value */
typedef void(*apply_t)(void);	/* function pointer */
typedef union arglist {
  char *arg_ptr;
  char arg_regs[sizeof (char*)];
} *arglist_t;		        /* argument frame */

objc_EXPORT retval_t objc_msg_sendv(id, SEL, arglist_t);
