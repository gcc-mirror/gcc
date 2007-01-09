/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
/**
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 * @date Tue 08 Aug 2006 */
/* Implemented for our sole pool, the heap, and our sole memory
 * manager/garbage collector, Boehm GC.
 * Status:  Believed complete and correct.
 */

#include <config.h>

#include <gcj/cni.h>
#include <java/lang/String.h>
#include <java/lang/management/VMManagementFactory.h>

JArray< ::java::lang::String *> *
java::lang::management::VMManagementFactory::getMemoryPoolNames ()
{
  return (JArray<jstring>*)
    JvNewObjectArray(1, &java::lang::String::class$, JvNewStringLatin1("Heap"));
}


JArray< ::java::lang::String *> *
java::lang::management::VMManagementFactory::getMemoryManagerNames ()
{
  return (JArray<jstring>*)
    JvNewObjectArray(0, &java::lang::String::class$, NULL);
}


JArray< ::java::lang::String *> *
java::lang::management::VMManagementFactory::getGarbageCollectorNames ()
{
  return (JArray<jstring>*) 
    JvNewObjectArray(1, &java::lang::String::class$, JvNewStringLatin1("BoehmGC"));
}
