/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.geom;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date July 15, 2000
 */

public class NoninvertibleTransformException extends Exception
{
  public NoninvertibleTransformException (String s)
  {
    super (s);
  }
}
