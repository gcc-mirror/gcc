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

public class FieldPosition
{
  int field;
  int beginIndex;
  int endIndex;

  public FieldPosition (int field)
  {
    this.field = field;
  }

  public int getField ()
  {
    return field;
  }

  public int getBeginIndex ()
  {
    return beginIndex;
  }

  public int getEndIndex ()
  {
    return endIndex;
  }

  public void setBeginIndex (int index)
  {
    beginIndex = index;
  }

  public void setEndIndex (int index)
  {
    endIndex = index;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof FieldPosition))
      return false;
    FieldPosition other = (FieldPosition) obj;
    return (field == other.field
	    && beginIndex == other.beginIndex && endIndex == other.endIndex);
  }
}
