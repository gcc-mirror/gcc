/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 25, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

public class ParseException extends Exception
{
  private int errorOffset;

  public ParseException (String msg, int errorOffset)
  {
    super(msg);
    this.errorOffset = errorOffset;
  }

  public int getErrorOffset ()
  {
    return errorOffset;
  }
}
