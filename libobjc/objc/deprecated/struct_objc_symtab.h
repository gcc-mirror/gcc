/* Whereas a Module (defined further down) is the root (typically) of a file,
   a Symtab is the root of the class and category definitions within the
   module.  
   
   A Symtab contains a variable length array of pointers to classes and
   categories  defined in the module.   */
typedef struct objc_symtab {
  unsigned long sel_ref_cnt;  /* Unused (always set to 0). */
  SEL      refs;              /* The table of selectors referenced in
                                 this module.  This is terminated by a
                                 selector with NULL sel_id and NULL
                                 sel_types.  */
  unsigned short cls_def_cnt;                   /* Number of classes compiled
                                                  (defined) in the module. */
  unsigned short cat_def_cnt;                   /* Number of categories 
                                                  compiled (defined) in the 
                                                  module. */

  void      *defs[1];                           /* Variable array of pointers.
                                                  cls_def_cnt of type Class 
                                                  followed by cat_def_cnt of
                                                  type Category_t, followed
						  by a NULL terminated array
						  of objc_static_instances. */
} Symtab,   *Symtab_t;

