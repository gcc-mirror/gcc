// Object.java - The root of all evil.

/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 30, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * plus gcj compiler sources (to determine object layout)
 * Status:  Complete to version 1.1
 */

public class Object
{
  // This must come first.  See _JvObjectPrefix in Object.h.
  protected void finalize () throws Throwable
  {
  }

  public final native Class getClass ();
  public native int hashCode ();
  public final native void notify ();
  public final native void notifyAll ();
  public final native void wait (long timeout, int nanos)
    throws InterruptedException;

  public boolean equals (Object obj)
  {
    return this == obj;
  }

  public Object ()
  {
  }

  public String toString ()
  {
    return getClass().getName() + '@' + Integer.toHexString(hashCode());
  }

  public final void wait () throws InterruptedException
  {
    wait (0, 0);
  }

  public final void wait (long timeout) throws InterruptedException
  {
    wait (timeout, 0);
  }

  protected native Object clone () throws CloneNotSupportedException;

  // This initializes the sync_info member.  It is here for
  // completeness (some day we'll be able to auto-generate Object.h).
  private final native void sync_init ();

  // Note that we don't mention the sync_info field here.  If we do,
  // jc1 will not work correctly.
}
