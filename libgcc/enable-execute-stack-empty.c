/* Dummy implementation of __enable_execute_stack.  */

extern void __enable_execute_stack (void *);

/* Attempt to turn on execute permission for the stack.  */

void
__enable_execute_stack (void *addr __attribute__((__unused__)))
{
}
