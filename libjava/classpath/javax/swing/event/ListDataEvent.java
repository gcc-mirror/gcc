/* ListDataEvent.java --
   Copyright (C) 2002, 2006, Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package javax.swing.event;

import java.util.EventObject;

/**
 * An event that contains information about a modification to the content of
 * a list.
 * 
 * @author Andrew Selkirk
 * @author Ronald Veldema
 */
public class ListDataEvent extends EventObject
{
  private static final long serialVersionUID = 2510353260071004774L;
  
  /** An event type indicating that the list content has been modified. */
  public static final int CONTENTS_CHANGED = 0;
  
  /** An event type indicating that an interval has been added to the list. */
  public static final int INTERVAL_ADDED = 1;
  
  /** 
   * An event type indicating that an interval has been removed from the 
   * list. 
   */
  public static final int INTERVAL_REMOVED = 2;

  private int type;
  private int index0;
  private int index1;
	
  /**
   * Creates a <code>ListDataEvent</code> object.
   * 
   * @param source  the source of the event (<code>null</code> not permitted).
   * @param type  the type of the event (should be one of 
   *     {@link #CONTENTS_CHANGED}, {@link #INTERVAL_ADDED} or 
   *     {@link #INTERVAL_REMOVED}, although this is not enforced).
   * @param index0  the index for one end of the modified range of list 
   *     elements.
   * @param index1  the index for the other end of the modified range of list 
   *     elements.
   */
  public ListDataEvent(Object source, int type, int index0, int index1)
  {
    super(source);
    this.type = type;
    this.index0 = Math.min(index0, index1);
    this.index1 = Math.max(index0, index1);
  }
	
  /**
   * Returns the index of the first item in the range of modified list items.
   * 
   * @return The index of the first item in the range of modified list items.
   */
  public int getIndex0()
  {
    return index0;
  }

  /**
   * Returns the index of the last item in the range of modified list items.
   * 
   * @return The index of the last item in the range of modified list items.
   */
  public int getIndex1()
  {
    return index1;
  }

  /**
   * Returns a code representing the type of this event, which is usually one
   * of {@link #CONTENTS_CHANGED}, {@link #INTERVAL_ADDED} or 
   * {@link #INTERVAL_REMOVED}.
   * 
   * @return The event type.
   */
  public int getType()
  {
    return type;
  }
  
  /**
   * Returns a string representing the state of this event.
   * 
   * @return A string.
   */
  public String toString()
  {
    return getClass().getName() + "[type=" + type + ",index0=" + index0 
        + ",index1=" + index1 + "]";
  }
}
