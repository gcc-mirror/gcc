/*
** The compiler generates one of these structures for each module that
** composes the executable (eg main.m).  
** 
** This data structure is the root of the definition tree for the module.  
** 
** A collect program runs between ld stages and creates a ObjC ctor array. 
** That array holds a pointer to each module structure of the executable. 
*/
typedef struct objc_module {
  unsigned long version; /* Version of the Module data structure.  */
  unsigned long size;    /* sizeof(Module) according to the compiler -
			    only used to sanity check that it matches
			    sizeof(Module) according to the
			    runtime.  */
  const char* name;      /* Name of the file used to compile the
			    module - not set by modern compilers for
			    security reasons.  */
  Symtab_t    symtab;    /* Pointer to the Symtab of the module.  The
			    Symtab holds an array of pointers to the
			    classes and categories defined in the
			    module. */
} Module, *Module_t;

