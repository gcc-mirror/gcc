/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 17, 1998.  
 */

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

public class UnsupportedEncodingException extends IOException
{
  public UnsupportedEncodingException ()
  {
    super();
  }

  public UnsupportedEncodingException (String msg)
  {
    super(msg);
  }
}
