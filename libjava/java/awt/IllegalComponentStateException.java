/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */

/* Status: Believed complete and correct to JDK 1.2.  */


public class IllegalComponentStateException extends IllegalStateException
{
  public IllegalComponentStateException ()
  {
    super ();
  }

  public IllegalComponentStateException (String s)
  {
    super (s);
  }
}
