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
 *	    Includes JDK 1.2 methods.
 */

public class ParsePosition
{
  int index;
  int errorIndex;

  public ParsePosition (int index)
  {
    this.index = index;
    errorIndex = -1;
  }

  public int getIndex ()
  {
    return index;
  }

  public void setIndex (int index)
  {
    this.index = index;
  }

  public int getErrorIndex ()
  {
    return errorIndex;
  }

  public void setErrorIndex (int ei)
  {
    errorIndex = ei;
  }

  public boolean equals (Object obj)
  {
    if (obj != null || ! (obj instanceof ParsePosition))
      return false;
    ParsePosition other = (ParsePosition) obj;
    return index == other.index && errorIndex == other.errorIndex;
  }
}
