// SocketException.java - Base class for networking exceptions

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

/**
 * @author Per Bothner
 * @date January 6, 1999.
 */

/** Written using on-line Java Platform 1.2 API Specification.
  * Believed complete and correct.
  */

public class SocketException extends java.io.IOException
{
  public SocketException ()
  {
    super();
  }

  public SocketException (String s)
  {
    super(s);
  }
}
