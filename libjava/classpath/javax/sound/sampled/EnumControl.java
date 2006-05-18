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
 * An EnumControl is a Control which can take one of a specified set of
 * values. 
 * @since 1.3
 */
public abstract class EnumControl extends Control
{
  /**
   * This Type describes an EnumControl.
   * @since 1.3
   */
  public static class Type extends Control.Type
  {
    /** This describes an enum control used for reverb.  */
    public static final Type REVERB = new Type("Reverb");

    /**
     * Create a new Type given its name.
     * @param name the name of the type
     */
    protected Type(String name)
    {
      super(name);
    }
  }

  private Object[] values;
  private Object value;

  /**
   * Create a new enumerated control given its Type, the range of valid
   * values, and its initial value.
   * @param type the type
   * @param values the valid values
   * @param val the initial value
   */
  protected EnumControl(Type type, Object[] values, Object val)
  {
    super(type);
    // FIXME: error checking: values.length>0, val in values... ?
    // FIXME: clone here?
    this.values = values;
    this.value = val;
  }

  /**
   * Return the current value of this control.
   */
  public Object getValue()
  {
    return value;
  }

  /**
   * Return the valid values for this control.
   */
  public Object[] getValues()
  {
    // FIXME: clone here?
    return values;
  }

  /**
   * Set the value of this control.  If the indicated value is not among
   * the valid values, this method will throw an IllegalArgumentException.
   * @param value the new value
   * @throws IllegalArgumentException if the new value is invalid
   */
  public void setValue(Object value)
  {
    for (int i = 0; i < values.length; ++i)
      {
	if (! values[i].equals(value))
	  throw new IllegalArgumentException("value not supported");
      }
    this.value = value;
  }

  /**
   * Return a string describing this control.
   */
  public String toString()
  {
    return super.toString() + ": " + value;
  }
}
