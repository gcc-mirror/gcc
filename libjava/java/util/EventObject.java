// EventObject.java - Represent events fired by objects.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.util;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date December 12, 1998
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Believed complete, but not fully correct.
 */

public class EventObject implements java.io.Serializable
{
  public EventObject (Object source)
    {
      this.source = source;
    }

  public Object getSource ()
    {
      return source;
    }

  public String toString ()
    {
      // FIXME.
      return getSource().toString();
    }

  // Source of the event.
  protected transient Object source;
}
