/* GCInfo.java -- Support for creating heap dumps.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package gnu.gcj.util;

public class GCInfo
{
  private GCInfo()
  {
  }

  /**
   * @throws SecurityException if there is a SecurityManager installed
   * and UtilPermission("dumpHeap") is not granted.
   */
  private static void checkPermission()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new UtilPermission("dumpHeap"));
  }
  

  /**
   * Dump a description of the heap state.
   *
   * @param namePrefix The filename prefix for the dump files.
   *
   * @throws SecurityException if there is a SecurityManager installed
   * and UtilPermission("dumpHeap") is not granted.
   */
  public static synchronized void dump(String name)
  {
    checkPermission();
    dump0(name);
  }
  
  private static native void dump0(String name);


  /**
   * Create a heap dump.
   *
   * @param namePrefix The filename prefix for the dump files.
   *
   * @throws SecurityException if there is a SecurityManager installed
   * and UtilPermission("dumpHeap") is not granted.
   */
  public static synchronized void enumerate(String namePrefix)
  {
    checkPermission();
    enumerate0(namePrefix);
  }
  
  private static native void enumerate0(String namePrefix);

  /**
   * Cause a heap dump if out-of-memory condition occurs.
   *
   * @param namePrefix The filename prefix for the dump files.  If
   * null no dumps are created.
   *
   * @throws SecurityException if there is a SecurityManager installed
   * and UtilPermission("dumpHeap") is not granted.
   */
  public static synchronized void setOOMDump(String namePrefix)
  {
    checkPermission();
    setOOMDump0(namePrefix);
  }
  
  private static native void setOOMDump0(String namePrefix);
}
