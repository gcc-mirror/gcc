/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.security;
 
public class NoSuchAlgorithmException extends Exception
{
  public NoSuchAlgorithmException()
  {
    super();
  }

  public NoSuchAlgorithmException(String msg)
  {
    super(msg);
  }
}
