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

