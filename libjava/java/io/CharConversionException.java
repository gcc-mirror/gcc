// CharConversionException.java - Problem converting char to byte encoding.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 25, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to 1.1.
 */

public class CharConversionException extends IOException
{
  public CharConversionException ()
  {
    super ();
  }

  public CharConversionException (String s)
  {
    super (s);
  }
}
