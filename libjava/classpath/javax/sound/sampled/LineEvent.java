/* 
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.sound.sampled;

import java.util.EventObject;

// FIXME: attempts to serialize this should fail

/**
 * This class holds information about a state change of a Line.
 * @since 1.3
 */
public class LineEvent extends EventObject
{
  /**
   * This class represents the kinds of state changes that can occur
   * to a Line.  The standard states are availabe as static instances.
   * @since 1.3
   */
  public static class Type
  {
    /** An event of this type is posted when a Line closes.  */
    public static final Type CLOSE = new Type("close");

    /** An event of this type is posted when a Line opens.  */
    public static final Type OPEN = new Type("open");

    /** An event of this type is posted when a Line starts.  */
    public static final Type START = new Type("start");

    /** An event of this type is posted when a Line stops.  */
    public static final Type STOP = new Type("stop");

    private String name;

    /**
     * Create a new type with the indicated name.
     * @param name the name
     */
    protected Type(String name)
    {
      this.name = name;
    }

    public final boolean equals(Object o)
    {
      return super.equals(o);
    }

    public final int hashCode()
    {
      return super.hashCode();
    }

    /**
     * Return the name of this Type.
     */
    public String toString()
    {
      return name;
    }
  }

  private Type type;
  private long framePosition;
  private Line line;

  /**
   * Create a new LineEvent with the indicated line, type, and frame position.
   * @param line the line
   * @param type the type of the event
   * @param pos the frame position
   */
  public LineEvent(Line line, Type type, long pos)
  {
    super(line);
    this.line = line;
    this.type = type;
    this.framePosition = pos;
  }

  /**
   * Return the frame position associated with this event.
   */
  public long getFramePosition()
  {
    return framePosition;
  }

  /**
   * Return the Line associated with this event.
   */
  public Line getLine()
  {
    return line;
  }

  /**
   * Return the Type associated with this event.
   */
  public Type getType()
  {
    return type;
  }

  /**
   * Return a description of this event.
   */
  public String toString()
  {
    return ("type=" + type + "; framePosition=" + framePosition
	    + "line=" + line);
  }
}
