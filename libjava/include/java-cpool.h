// java-cpool.h - Constant pool parsing header.  -*- c++ -*-

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_CPOOL_H__
#define __JAVA_CPOOL_H__

#include <gcj/javaprims.h>

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

extern inline void 
_Jv_storeIndexes (_Jv_word *data,
		 _Jv_ushort index0,
		 _Jv_ushort index1)
{
  data->i = (((jint)index0) << 16) | (jint) index1;
}

extern inline void 
_Jv_loadIndexes (const _Jv_word *data,
		 _Jv_ushort& index0,
		 _Jv_ushort& index1)
{
  jint udata = data->i;
      
  _Jv_uint uindex0 = ((udata >> 16) & 0xffff);
  _Jv_uint uindex1 = udata & 0xffff;
      
  index0 = uindex0;
  index1 = uindex1;
}

extern inline void
_Jv_storeFloat (_Jv_word *data, jfloat f)
{
  data->f = f;
}

extern inline jfloat
_Jv_loadFloat (_Jv_word *data)
{
  return data->f;
}

extern inline void
_Jv_storeInt (_Jv_word *data, jint i)
{
  data->i = i;
}

extern inline jint
_Jv_loadInt (_Jv_word *data)
{
  return data->i;
}

extern inline void
_Jv_storeLong (_Jv_word *data, jlong l)
{
  _Jv_word2 tmp;
  tmp.l = l;
  data[0].ia[0] = tmp.ia[0];
  data[1].ia[0] = tmp.ia[1];
}

extern inline jlong
_Jv_loadLong (_Jv_word *data)
{
  _Jv_word2 tmp;
  tmp.ia[0] = data[0].ia[0];
  tmp.ia[1] = data[1].ia[0];
  return tmp.l;
}

extern inline void
_Jv_storeDouble (_Jv_word *data, jdouble d)
{
  _Jv_word2 tmp;
  tmp.d = d;
  data[0].ia[0] = tmp.ia[0];
  data[1].ia[0] = tmp.ia[1];
}

extern inline jdouble
_Jv_loadDouble (_Jv_word *data)
{
  _Jv_word2 tmp;
  tmp.ia[0] = data[0].ia[0];
  tmp.ia[1] = data[1].ia[0];
  return tmp.d;
}


#endif /* __JAVA_CPOOL_H__ */
