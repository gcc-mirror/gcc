// Compiler.java - Control byte->machine code compiler.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 23, 1998.  
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 */

public final class Compiler
{
  public static Object command (Object arg)
    {
      // Our implementation defines this to a no-op.
      return null;
    }

  public static boolean compileClass (Class oneClass)
    {
      // Never succeed.
      return false;
    }

  public static boolean compileClasses (String classNames)
    {
      // Note the incredibly lame interface.  Always fail.
      return false;
    }

  public static void disable ()
    {
    }

  public static void enable ()
    {
    }

  // Don't allow new `Compiler's to be made.
  private Compiler ()
    {
    }
}
