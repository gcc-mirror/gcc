// ZipException.java

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.util.zip;

/**
 * @author Per Bothner
 * @date January 9, 1999.
 */

/* Written using on-line Java Platform 1.2 API Specification.
 * Believed complete and correct.
 */

public class ZipException extends java.io.IOException
{
  public ZipException ()
  {
    super();
  }

  public ZipException (String msg)
  {
    super(msg);
  }
}
