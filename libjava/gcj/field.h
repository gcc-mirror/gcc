// field.h - Header file for fieldID instances.  -*- c++ -*-

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_FIELD_H__
#define __GCJ_FIELD_H__

#include <java/lang/Class.h>
#include <java/lang/reflect/Field.h>

#define _Jv_FIELD_UNRESOLVED_FLAG	0x8000
#define	_Jv_FIELD_CONSTANT_VALUE	0x4000

struct _Jv_Field
{
#ifndef COMPACT_FIELDS
  struct _Jv_Utf8Const*	name;
#endif

  /* The type of the field, if isResolved().
     If !isResolved():  The fields's signature as a (Utf8Const*). */
  jclass		type;

  _Jv_ushort		flags;

#ifdef COMPACT_FIELDS
  jshort		nameIndex;  /* offset in class's name table */
#else
  _Jv_ushort		bsize;  /* not really needed ... */
#endif

  union {
    jint		boffset;  /* offset in bytes for instance field */
    void*		addr;  /* address of static field */
  } u;

#ifdef __cplusplus
  jboolean isResolved ()
  { return ! (flags & _Jv_FIELD_UNRESOLVED_FLAG); }

  public:

  int getOffset () { return u.boffset; }

  jobject getObjectField (jobject obj)
  { return *(jobject *)((char *)obj + getOffset ()); }

  jfieldID getNextField () { return this + 1; }

  jboolean isRef () 
    { 
      if (!isResolved ()) 
	{
	  char first = ((_Jv_Utf8Const*)type)->data[0]; 
	  return first == '[' || first == 'L';
	}
      else
	{
	  return ! type->isPrimitive ();
	}
    }

  jclass getClass ()
  {
    // We can't use JvAssert here because it is not in a public
    // header.
    // JvAssert (isResolved ());
    return type;
  }

  // FIXME - may need to mask off internal flags.
  int getModifiers() { return flags; }

#ifdef COMPACT_FIELDS
  _Jv_Utf8Const * getNameUtf8Const (jclass cls)
  { return clas->fieldNames + nameIndex; }
#else
  _Jv_Utf8Const * getNameUtf8Const (jclass) { return name; }
#endif
#endif
};

#ifdef __cplusplus

inline jbyte
_Jv_GetStaticByteField (jclass, _Jv_Field* field)
{
  return * (jbyte *) field->u.addr;
}

inline jshort
_Jv_GetStaticShortField (jclass, _Jv_Field* field)
{
  return * (jshort *) field->u.addr;
}

inline jint
_Jv_GetStaticIntField (jclass, _Jv_Field* field)
{
  return * (jint *) field->u.addr;
}

inline jlong
_Jv_GetStaticLongField (jclass, _Jv_Field* field)
{
  return * (jlong *) field->u.addr;
}

inline jobject
_Jv_GetObjectField (jobject obj, _Jv_Field* field)
{
  return field->getObjectField (obj);
}

inline jbyte
_Jv_GetByteField (jobject obj, _Jv_Field* field)
{
  return * (jbyte *) ((char*) obj + field->getOffset ());
}

inline jshort
_Jv_GetShortField (jobject obj, _Jv_Field* field)
{
  return * (jshort *) ((char*) obj + field->getOffset ());
}
inline jint
_Jv_GetIntField (jobject obj, _Jv_Field* field)
{
  return * (jint *) ((char*) obj + field->getOffset ());
}
inline jlong
_Jv_GetLongField (jobject obj, _Jv_Field* field)
{
  return * (jlong *) ((char*) obj + field->getOffset ());
}

extern inline jfieldID 
_Jv_FromReflectedField (java::lang::reflect::Field *field)
{ 
  return (jfieldID) ((char *) field->declaringClass->fields + field->offset); 
} 


#ifdef __GCJ_CNI_H__
extern inline jfieldID
JvGetFirstInstanceField (jclass klass)
{
  return &(klass->fields[klass->static_field_count]);
}

extern inline jint
JvNumInstanceFields (jclass klass)
{
  return klass->field_count - klass->static_field_count;
}

extern inline jfieldID
JvGetFirstStaticField (jclass klass)
{
  return &(klass->fields[0]);
}

extern inline jint
JvNumStaticFields (jclass klass)
{
  return klass->static_field_count;
}

extern inline jboolean
JvFieldIsRef (jfieldID field)
{
  return field->isRef ();
}

extern inline jobject
JvGetObjectField (jobject obj, _Jv_Field* field)
{
  return _Jv_GetObjectField (obj, field);
}
#endif /* defined (__GCJ_CNI_H__) */

#endif /* __cplusplus */

#endif /* __GCJ_FIELD_H */
