/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.transaction;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date May 25, 2001
 */

public class SystemException extends java.lang.Exception
{
  public int errorCode;

  public SystemException ()
  {
    super();
  }

  public SystemException (String msg)
  {
    super(msg);
  }

  public SystemException(int errcode)
  {
    super ();
    this.errorCode = errcode;
  }
}
