/* These functions are deprecated and currently ignored.  */
/*
** Hook functions for allocating, copying and disposing of instances
*/
objc_EXPORT id (*_objc_object_alloc)(Class _class);
objc_EXPORT id (*_objc_object_copy)(id object);
objc_EXPORT id (*_objc_object_dispose)(id object);

