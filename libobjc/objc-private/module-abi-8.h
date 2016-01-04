/* Definitions of Module Structures used by ABI version 8
   Copyright (C) 1993-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef __objc_private_module_abi_8_INCLUDE_GNU
#define __objc_private_module_abi_8_INCLUDE_GNU

/* For every class which happens to have statically allocated instances in
   this module, one OBJC_STATIC_INSTANCES is allocated by the compiler.
   INSTANCES is NULL terminated and points to all statically allocated
   instances of this class.  */
struct objc_static_instances
{
  char *class_name;
#ifdef __cplusplus
  id instances[1];
#else
  id instances[0];
#endif
};

/* Whereas a Module (defined further down) is the root (typically) of a file,
   a Symtab is the root of the class and category definitions within the
   module.  
   
   A Symtab contains a variable length array of pointers to classes and
   categories  defined in the module.   */
struct objc_symtab
{
  unsigned long sel_ref_cnt;  /* Unused (always set to 0). */
  struct objc_selector *refs; /* The table of selectors referenced in
                                 this module.  This is terminated by a
                                 selector with NULL sel_id and NULL
                                 sel_types.  Note that we use the type
                                 'struct objc_selector *' and not
                                 'SEL' (which is 'const struct
                                 objc_selector *') because the sel_id
                                 of these selectors is patched up by
                                 the runtime when the module is
                                 loaded.  */
  unsigned short cls_def_cnt; /* Number of classes compiled (defined)
                                 in the module. */
  unsigned short cat_def_cnt; /* Number of categories compiled
                                 (defined) in the module. */
  void      *defs[1];         /* Variable array of pointers.
				 cls_def_cnt of type Class followed by
				 cat_def_cnt of type Category_t,
				 followed by a NULL terminated array
				 of objc_static_instances. */
};

/* The compiler generates one of these structures for each module that
   composes the executable (eg main.m).
 
   This data structure is the root of the definition tree for the
   module.
 
   A collect program runs between ld stages and creates a ObjC ctor
   array.  That array holds a pointer to each module structure of the
   executable.  */
struct objc_module
{
  unsigned long version;      /* Version of the Module data
				 structure.  */
  unsigned long size;         /* sizeof(Module) according to the
				 compiler - only used to sanity check
				 that it matches sizeof(Module)
				 according to the runtime.  */
  const char* name;           /* Name of the file used to compile the
				 module - not set by modern compilers
				 for security reasons.  */
  struct objc_symtab *symtab; /* Pointer to the Symtab of the module.
				 The Symtab holds an array of pointers
				 to the classes and categories defined
				 in the module. */
};

/* The compiler generates one of these structures for a class that has
   instance variables defined in its specification.  */
struct objc_ivar
{
  const char* ivar_name;  /* Name of the instance variable as entered
			     in the class definition. */
  const char* ivar_type;  /* Description of the Ivar's type.  Useful
			     for debuggers. */
  int        ivar_offset; /* Byte offset from the base address of the
			     instance structure to the variable. */
};

struct objc_ivar_list
{
  int   ivar_count;              /* Number of structures (Ivar)
				    contained in the list.  One
				    structure per instance variable
				    defined in the class. */
  struct objc_ivar ivar_list[1]; /* Variable length structure. */
};

/* The compiler generates one (or more) of these structures for a
   class that has methods defined in its specification.
 
   The implementation of a class can be broken into separate pieces in
   a file and categories can break them across modules. To handle this
   problem is a singly linked list of methods.  */
struct objc_method
{
  SEL         method_name;  /* This variable is the method's name.
			       The compiler puts a char* here, and
			       it's replaced by a real SEL at runtime
			       when the method is registered.  */
  const char* method_types; /* Description of the method's parameter
			       list.  Used when registering the
			       selector with the runtime.  When that
			       happens, method_name will contain the
			       method's parameter list.  */
  IMP         method_imp;   /* Address of the method in the
			       executable. */
};

struct objc_method_list
{
  struct objc_method_list*  method_next; /* This variable is used to
					    link a method list to
					    another.  It is a singly
					    linked list. */
  int            method_count;            /* Number of methods defined
					     in this structure. */
  struct objc_method method_list[1];      /* Variable length
					     structure. */
};

/* Note that a 'struct objc_method_description' as embedded inside a
   Protocol uses the same trick as a 'struct objc_method': the
   method_name is a 'char *' according to the compiler, who puts the
   method name as a string in there.  At runtime, the selectors need
   to be registered, and the method_name then becomes a SEL.  */
struct objc_method_description_list
{
  int count;
  struct objc_method_description list[1];
};

struct objc_protocol {
  struct objc_class* class_pointer;
  char *protocol_name;
  struct objc_protocol_list *protocol_list;
  struct objc_method_description_list *instance_methods, *class_methods; 
};


struct objc_protocol_list
{
  struct objc_protocol_list *next;
  size_t count;
  struct objc_protocol *list[1];
};

/*
  The compiler generates one of these structures for each class.  

  This structure is the definition for classes.

  This structure is generated by the compiler in the executable and
  used by the run-time during normal messaging operations.  Therefore
  some members change type. The compiler generates "char* const" and
  places a string in the following member variables: super_class.
*/
struct objc_class {
  struct objc_class*  class_pointer;    /* Pointer to the class's meta
					   class. */
  struct objc_class*  super_class;      /* Pointer to the super
					   class. NULL for class
					   Object. */
  const char*         name;             /* Name of the class. */
  long                version;          /* Unknown. */
  unsigned long       info;             /* Bit mask.  See class masks
					   defined below. */
  long                instance_size;    /* Size in bytes of the class.
					   The sum of the class
					   definition and all super
					   class definitions. */
#ifdef _WIN64
  /* We pad the structure manually to prevent warning when -Wpadded is
     used.  The compiler automatically pads the structures that it
     generates, so this manually padded structure still matches the
     one generated by the compiler, but if we don't pad manually,
     -Wpadded detects that padding is being added and generates
     annoying warnings.  This hack is necessary as on LLP64 targets
     sizeof (long) isn't equal to sizeof (void *).  */
  long pad;
#endif
  struct objc_ivar_list* ivars;         /* Pointer to a structure that
					   describes the instance
					   variables in the class
					   definition.  NULL indicates
					   no instance variables.
					   Does not include super
					   class variables. */
  struct objc_method_list*  methods;    /* Linked list of instance
					   methods defined for the
					   class. */
  struct sarray *    dtable;            /* Pointer to instance method
					   dispatch table. */  
  struct objc_class* subclass_list;     /* Subclasses */
  struct objc_class* sibling_class;

  struct objc_protocol_list *protocols; /* Protocols conformed to */
  void* gc_object_type;
};

/* This is used to assure consistent access to the info field of 
   classes.  */
#ifndef HOST_BITS_PER_LONG
# define HOST_BITS_PER_LONG  (sizeof(long)*8)
#endif 

#define __CLS_INFO(cls) ((cls)->info)
#define __CLS_ISINFO(cls, mask) ((__CLS_INFO(cls)&mask)==mask)
#define __CLS_SETINFO(cls, mask) (__CLS_INFO(cls) |= mask)
#define __CLS_SETNOTINFO(cls, mask) (__CLS_INFO(cls) &= ~mask)

/* The structure is of type MetaClass */
#define _CLS_META 0x2L
#define CLS_ISMETA(cls) ((cls)&&__CLS_ISINFO(cls, _CLS_META))

/* The structure is of type Class */
#define _CLS_CLASS 0x1L
#define CLS_ISCLASS(cls) ((cls)&&__CLS_ISINFO(cls, _CLS_CLASS))

/* The class is initialized within the runtime.  This means that it
   has had correct super and sublinks assigned.  */
#define _CLS_RESOLV 0x8L
#define CLS_ISRESOLV(cls) __CLS_ISINFO(cls, _CLS_RESOLV)
#define CLS_SETRESOLV(cls) __CLS_SETINFO(cls, _CLS_RESOLV)

/* The class has been send a +initialize message or a such is not 
   defined for this class.  */
#define _CLS_INITIALIZED 0x04L
#define CLS_ISINITIALIZED(cls) __CLS_ISINFO(cls, _CLS_INITIALIZED)
#define CLS_SETINITIALIZED(cls) __CLS_SETINFO(cls, _CLS_INITIALIZED)

/* The class is being constructed; it has been allocated using
   objc_allocateClassPair(), but has not been registered yet by using
   objc_registerClassPair().  This means it is possible to freely add
   instance variables to the class, but it can't be used for anything
   yet.  */
#define _CLS_IN_CONSTRUCTION 0x10L
#define CLS_IS_IN_CONSTRUCTION(cls) __CLS_ISINFO(cls, _CLS_IN_CONSTRUCTION)
#define CLS_SET_IN_CONSTRUCTION(cls) __CLS_SETINFO(cls, _CLS_IN_CONSTRUCTION)
#define CLS_SET_NOT_IN_CONSTRUCTION(cls) __CLS_SETNOTINFO(cls, _CLS_IN_CONSTRUCTION)

/* The class number of this class.  This must be the same for both the
   class and its meta class object.  */
#define CLS_GETNUMBER(cls) (__CLS_INFO(cls) >> (HOST_BITS_PER_LONG/2))
#define CLS_SETNUMBER(cls, num) \
  ({ (cls)->info <<= (HOST_BITS_PER_LONG/2); \
     (cls)->info >>= (HOST_BITS_PER_LONG/2); \
     __CLS_SETINFO(cls, (((unsigned long)num) << (HOST_BITS_PER_LONG/2))); })

/* The compiler generates one of these structures for each category.
   A class may have many categories and contain both instance and
   factory methods.  */
struct objc_category
{
  const char*   category_name;                /* Name of the category.
						 Name contained in the
						 () of the category
						 definition.  */
  const char*   class_name;                   /* Name of the class to
						 which the category
						 belongs.  */
  struct objc_method_list  *instance_methods; /* Linked list of
						 instance methods
						 defined in the
						 category. NULL
						 indicates no instance
						 methods defined.  */
  struct objc_method_list *class_methods;     /* Linked list of
						 factory methods
						 defined in the
						 category.  NULL
						 indicates no class
						 methods defined.  */
  struct objc_protocol_list *protocols;	      /* List of Protocols
					         conformed to.  */
};

#endif /* __objc_private_module_abi_8_INCLUDE_GNU */
