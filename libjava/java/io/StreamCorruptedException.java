/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date February 7, 2000.
 */

/* Written using on-line Java Platform 1.2 API Specification.
 * Status:  Believed complete and correct.
 */

public class StreamCorruptedException extends ObjectStreamException
{
  public StreamCorruptedException()
  {
    super();
  }

  public StreamCorruptedException(String msg)
  {
    super(msg);
  }
}
