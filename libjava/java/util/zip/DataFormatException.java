// DataFormatException.java

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.util.zip;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification.
 * Believed complete and correct.
 */

public class DataFormatException extends Exception
{
  public DataFormatException ()
  {
    super();
  }

  public DataFormatException (String msg)
  {
    super(msg);
  }
}
