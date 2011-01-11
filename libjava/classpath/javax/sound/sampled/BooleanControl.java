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
 * A BooleanControl is a Control which has two states.
 * @since 1.3
 */
public abstract class BooleanControl extends Control
{
  /**
   * A Type specialized to represent a boolean control.
   * @since 1.3
   */
  public static class Type extends Control.Type
  {
    // FIXME: correct constructions?

    /**
     * A control for applying reverb.
     */
    public final static Type APPLY_REVERB = new Type("Apply reverb");

    /**
     * A control for muting.
     */
    public final static Type MUTE = new Type("Mute");

    /**
     * Create a new Type given its name.
     * @param name the name of the type
     */
    protected Type(String name)
    {
      super(name);
    }
  }

  private boolean value;
  private String trueLabel;
  private String falseLabel;

  /**
   * Create a new boolean control, with the indicated Type and initial
   * value.  The description strings will default to "true" and "false".
   * @param type the type
   * @param init the initial value
   */
  protected BooleanControl(Type type, boolean init)
  {
    super(type);
    this.value = init;
    this.trueLabel = "true";
    this.falseLabel = "false";
  }

  /**
   * Create a new boolean control, with the indicated Type, initial
   * value, and labels.
   * @param type the type
   * @param init the initial value
   * @param trueLabel the label for the true state
   * @param falseLabel the label for the false state
   */
  protected BooleanControl(Type type, boolean init, String trueLabel,
                           String falseLabel)
  {
    super(type);
    this.value = init;
    this.trueLabel = trueLabel;
    this.falseLabel = falseLabel;
  }

  /**
   * Return the label corresponding to the indicated state.
   * @param state the state
   * @return the true label or the false label, as appropriate
   */
  public String getStateLabel(boolean state)
  {
    return state ? trueLabel : falseLabel;
  }

  /**
   * Return the current value of thhe control.
   */
  public boolean getValue()
  {
    return value;
  }

  /**
   * Set the value of the control as indicated.
   * @param value the new value
   */
  public void setValue(boolean value)
  {
    this.value = value;
  }

  /**
   * Return a string describing this control.
   */
  public String toString()
  {
    return super.toString() + ": " + getStateLabel(value);
  }
}
