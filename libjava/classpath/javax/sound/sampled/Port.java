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

/**
 * A Port is a Line which represents an audio device, for instance
 * a microphone.
 *
 * @since 1.3
 */
public interface Port extends Line
{
  /**
   * This describes a single port.
   * @since 1.3
   */
  class Info extends Line.Info
  {
    // FIXME names?

    /** A CD player.  */
    public static final Info COMPACT_DISC = new Info(Port.class,
                                                     "Compact Disc",
                                                     true);

    /** Headphones.  */
    public static final Info HEADPHONE = new Info(Port.class, "Headphone",
                                                  false);

    /** Generic input line.  */
    public static final Info LINE_IN = new Info(Port.class, "Line in",
                                                true);

    /** Generic output line.  */
    public static final Info LINE_OUT = new Info(Port.class, "Line out",
                                                 false);

    /** A microphone.  */
    public static final Info MICROPHONE = new Info(Port.class, "Microphone",
                                                   true);

    /** A speaker.  */
    public static final Info SPEAKER = new Info(Port.class, "Speaker",
                                                false);

    private String name;
    private boolean isSource;

    /**
     * Create a new Info object, given the line's class, the name,
     * and an argument indicating whether this is an input or an output.
     * @param klass the class of the line
     * @param name the name of the line
     * @param isSource true if this is an input source
     */
    public Info(Class<?> klass, String name, boolean isSource)
    {
      super(klass);
      this.name = name;
      this.isSource = isSource;
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
     * Return the name of this object.
     */
    public String getName()
    {
      return name;
    }

    /**
     * Return true if this describes an input line.
     */
    public boolean isSource()
    {
      return isSource;
    }

    public boolean matches(Line.Info other)
    {
      return super.matches(other) && equals(other);
    }

    public final String toString()
    {
      return super.toString() + "; name=" + name + "; isSource=" + isSource;
    }
  }
}
