// java-cpool.h - Constant pool parsing header.  -*- c++ -*-

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_CPOOL_H__
#define __JAVA_CPOOL_H__

#include <javaprims.h>

// we rename these, to avoid polluting the name space
#define JV_CONSTANT_Undefined (0L)
#define JV_CONSTANT_Utf8 (1L)
#define JV_CONSTANT_Unicode (2L)
#define JV_CONSTANT_Integer (3L)
#define JV_CONSTANT_Float (4L)
#define JV_CONSTANT_Long (5L)
#define JV_CONSTANT_Double (6L)
#define JV_CONSTANT_Class (7L)
#define JV_CONSTANT_String (8L)
#define JV_CONSTANT_Fieldref (9L)
#define JV_CONSTANT_Methodref (10L)
#define JV_CONSTANT_InterfaceMethodref (11L)
#define JV_CONSTANT_NameAndType (12L)
#define JV_CONSTANT_ResolvedFlag (16L)
#define JV_CONSTANT_ResolvedString (16L | 8L)
#define JV_CONSTANT_ResolvedClass  (16L | 7L)

/* We use the following two operations uniformly for all put/get operations
 * in the runtime system (constant pool & stack), to assure that we keep
 * everything in the same format.  The idea is, that these should be inlined
 * away, into just a simple store (for small data types, and a pair of stores
 * if double or long has alignment greater than void *.  On an 64-bit
 * architecture, all operations should be simple stores; on a 32-bit
 * architecture it depends on the alignment requirement for the specific
 * type.  */

template <class T>
static inline void _Jv_put (void *dst, T value)
{
#if 0
  if (sizeof (T) == 8 && __alignof__ (T) > __alignof__ (void*))
    {
      jint *v_dst  = (jint*)(dst);
      jint *v_src  = (jint*)&value;

      v_dst[0] = v_src[0];
      v_dst[1] = v_src[1];
    }
  else 
#endif
    {
      *((T*) (dst)) = value;
    }
}

template <class T>
static inline T _Jv_get (void *src)
{
#if 0
  if (sizeof (T) == 8 && __alignof__ (T) > __alignof__ (void*))
    {
      T     value;
      jint *v_dst  = (jint*)&value;
      jint *v_src  = (jint*)src;

      v_dst[0] = v_src[0];
      v_dst[1] = v_src[1];

      return value;
    }
  else 
#endif
    {
      return *((T*) (src));
    }
}

/** needed to keep the CONSTANT_XXXRef & CONSTANT_NameAndType entries */
extern inline void 
_Jv_storeIndexes (void **data,
		 _Jv_ushort index0,
		 _Jv_ushort index1)
{
  // accomodate 64bit machines...
  if (sizeof (void*) == (2 * sizeof (jint)))
    {
      ((jint*)data)[0] = index0;
      ((jint*)data)[1] = index0;
    }
  else
    {
      _Jv_put<jint>(data, ((jint)index0 << 16) | (jint)index1);
    }
}

extern inline void 
_Jv_loadIndexes (const void **data,
		 _Jv_ushort& index0,
		 _Jv_ushort& index1)
{
  if (sizeof (void*) == (2*sizeof (jint)))
    {
      index0 = ((jint*)data)[0];
      index0 = ((jint*)data)[1];
    }
  else
    {
      jint udata = _Jv_get<jint>(data);
      
      _Jv_uint uindex0 = ((udata >> 16) & 0xffff);
      _Jv_uint uindex1 = udata & 0xffff;
      
      index0 = uindex0;
      index1 = uindex1;
    }
}

extern inline void
_Jv_storeFloat (void **data, jfloat f)
{
  _Jv_put<jfloat>(data, f);
}

extern inline jfloat
_Jv_loadFloat (void **data)
{
  return _Jv_get<jfloat>(data);
}

extern inline void
_Jv_storeInt (void **data, jint i)
{
  _Jv_put<jint>(data, i);
}

extern inline jint
_Jv_loadInt (void **data)
{
  return _Jv_get<jint>(data);
}

extern inline void
_Jv_storeLong (void **data, jlong l)
{
  return _Jv_put<jlong>(data, l);
}

extern inline jlong
_Jv_loadLong (void **data)
{
  return _Jv_get<jlong>(data);
}

extern inline void
_Jv_storeDouble (void **data, jdouble d)
{
  _Jv_put<jdouble>(data, d);
}

extern inline jdouble
_Jv_loadDouble (void **data)
{
  return _Jv_get<jdouble> (data);
}


#endif /* __JAVA_CPOOL_H__ */
