/* This API is incredibly limited and unsophisticated.  objc_error()
   generally causes the program to abort, so it should only be used
   when something so dramatic happens that it could be pointless to
   continue.  Only two cases I can think of: failure to allocate new
   memory or failure to load an Objective-C module.
*/
/* Error handling
  
   Call objc_error() or objc_verror() to record an error; this error
   routine will generally exit the program but not necessarily if the
   user has installed his own error handler.
  
   Call objc_set_error_handler to assign your own function for
   handling errors.  The function should return YES if it is ok
   to continue execution, or return NO or just abort if the
   program should be stopped.  The default error handler is just to
   print a message on stderr.
  
   The error handler function should be of type objc_error_handler
   The first parameter is an object instance of relevance.
   The second parameter is an error code.
   The third parameter is a format string in the printf style.
   The fourth parameter is a variable list of arguments.  */
void objc_error(id object, int code, const char* fmt, ...);
void objc_verror(id object, int code, const char* fmt, va_list ap);
typedef BOOL (*objc_error_handler)(id, int code, const char *fmt, va_list ap);
objc_error_handler objc_set_error_handler(objc_error_handler func);

/* Error codes
   These are used by the runtime library, and your
   error handling may use them to determine if the error is
   hard or soft thus whether execution can continue or abort.  */
#define OBJC_ERR_UNKNOWN 0             /* Generic error */

#define OBJC_ERR_OBJC_VERSION 1        /* Incorrect runtime version */
#define OBJC_ERR_GCC_VERSION 2         /* Incorrect compiler version */
#define OBJC_ERR_MODULE_SIZE 3         /* Bad module size */
#define OBJC_ERR_PROTOCOL_VERSION 4    /* Incorrect protocol version */

#define OBJC_ERR_MEMORY 10             /* Out of memory */

#define OBJC_ERR_RECURSE_ROOT 20       /* Attempt to archive the root
					  object more than once. */
#define OBJC_ERR_BAD_DATA 21           /* Didn't read expected data */
#define OBJC_ERR_BAD_KEY 22            /* Bad key for object */
#define OBJC_ERR_BAD_CLASS 23          /* Unknown class */
#define OBJC_ERR_BAD_TYPE 24           /* Bad type specification */
#define OBJC_ERR_NO_READ 25            /* Cannot read stream */
#define OBJC_ERR_NO_WRITE 26           /* Cannot write stream */
#define OBJC_ERR_STREAM_VERSION 27     /* Incorrect stream version */
#define OBJC_ERR_BAD_OPCODE 28         /* Bad opcode */

#define OBJC_ERR_UNIMPLEMENTED 30      /* Method is not implemented */

#define OBJC_ERR_BAD_STATE 40          /* Bad thread state */

