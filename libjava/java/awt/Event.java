/* Copyright (C) 1999  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* A very incomplete placeholder. */

public class Event
{
  public Event evt;
  public Object arg;
  public int id;
  public Object target;

  public Event (Object target, int id, Object arg)
  {
    this.id = id;
    this.target = target;
    this.arg = arg;
  }
}
