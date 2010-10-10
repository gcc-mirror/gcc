/* Definitions of Module Structures used by ABI version 8
   Copyright (C) 1993, 1995, 1996, 1997, 2001, 2002, 2003, 2004, 2005,
   2007, 2009, 2010 Free Software Foundation, Inc.

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
  unsigned long sel_ref_cnt;  /* Unknown. */
  SEL        refs;            /* Unknown. */
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
  SEL         method_name;  /* This variable is the method's name.  It
			       is a char*.  The unique integer passed
			       to objc_msg_send is a char* too.  It is
			       compared against method_name using
			       strcmp. */
  const char* method_types; /* Description of the method's parameter
			       list.  Useful for debuggers. */
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

struct objc_protocol_list
{
  struct objc_protocol_list *next;
  size_t count;
  Protocol *list[1];
};

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
