#include <ansidecl.h>

#ifdef __STDC__
#include <stddef.h>
#define clock_t unsigned long
#define DEF(NAME, RETURN_TYPE, ARGLIST, ARGS) extern RETURN_TYPE NAME (ARGS);
#define DEFFUNC(NAME, RETURN_TYPE, ARGLIST, ARGS) extern RETURN_TYPE NAME (ARGS);
#else
#define void int
#define size_t unsigned long
#define clock_t unsigned long
#define DEF(NAME, RETURN_TYPE, ARGLIST, ARGS) extern RETURN_TYPE NAME ();
#define DEFFUNC(NAME, RETURN_TYPE, ARGLIST, ARGS) extern RETURN_TYPE NAME ();
#endif

#define DEFVAR(NAME,DECL,USE) extern DECL;

#define NOTHING /*nothing*/

#include "alloca-conf.h"
#include "functions.def"

/* Always use our: getopt.o getopt1.o obstack.o spaces.o */

int
main (argc, argv)
     int argc; char **argv;
{

/* Create a dummy function call for each DEF-defined function. */

#undef DEF
#undef DEFVAR
#undef DEFFUNC
#undef AND
#define AND = 0;
/* ARGS expands into a set of declaration.  NAME ARG_LIST expands
   info a function call that uses those variables as actual parameters.
   If the function has been DEF'ed correctly, we can pass the right
   number and types of parameters, which is nice.  (E.g. gcc may
   otherwise complain about the wrong number of parameters to certain
   builtins.) */
#define DEF(NAME, RETURN_TYPE, ARG_LIST, ARGS) { ARGS; NAME ARG_LIST; }
#define DEFVAR(NAME, DECL, USE) { USE; }
#define DEFFUNC(NAME, RETURN_TYPE, ARG_LIST, ARGS) { ARGS; NAME ARG_LIST; }
#include "functions.def"

  return (0);
}
