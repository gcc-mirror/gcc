/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang.reflect;

/** 
 * @author Per Bothner <bothner@cygnus.com> 
 * @date September 27, 1998. 
 */ 
/* Written using "Java Class Libraries", 2nd edition.
 * Status:  Believed complete and correct.
 */

public interface Member
{
  public static final int PUBLIC = 0;
  public static final int DECLARED = 1;
  public Class getDeclaringClass ();
  public int getModifiers ();
  public String getName();
}
