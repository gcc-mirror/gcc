/* Control over an attribute of a line
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
 * A control provides the ability to affect some attribute of a line,
 * for instance its volume.
 * @since 1.3
 */
public class Control
{
  /**
   * This describes a single control.
   * @since 1.3
   */
  public static class Type
  {
    private String name;

    /**
     * Create a new Type given its name.
     * @param name the name of the type
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

  /**
   * Create a new Control given its Type.
   * @param type the type
   */
  protected Control(Type type)
  {
    this.type = type;
  }

  /**
   * Return the Type of this Control.
   */
  public Type getType()
  {
    return type;
  }

  /**
   * Return a String descrsibing this control.  In particular the
   * value will include the name of the associated Type.
   */
  public String toString()
  {
    return type.toString();
  }
}
