/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj;

/** A type used to indicate special data used by native code. Unlike the 
    <code>RawData</code> type, fields declared as <code>RawDataManaged</code> 
    will be "marked" by the memory manager and considered for garbage 
    collection.  
    
    Native data which is allocated using CNI's <code>JvAllocBytes()</code> 
    function and stored in a <code>RawDataManaged</code> will be automatically 
    freed when the Java object it is associated with becomes unreachable.  */

public final class RawDataManaged
{
   private RawDataManaged() { }
}
