/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* A very incomplete placeholder. */

public abstract class AWTEvent extends java.util.EventObject
{
  protected boolean consumed;
  protected int id;

  public int getID()
  {
    return id;
  }

  public String paramString ()
  {
    return toString ();
  }

  public String toString ()
  {
    return getClass().getName() + "[" + id + "]";
  }

  public AWTEvent (Object source, int id)
  {
    super(source);
    this.id = id;
  }
}
