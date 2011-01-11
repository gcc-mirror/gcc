/* Floating point control
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

/** @since 1.3 */
public abstract class FloatControl extends Control
{
  /**
   * An instance of this class describes a particular floating point control.
   * @since 1.3
     */
  public static class Type extends Control.Type
  {
    /** Auxiliary return gain.  */
    public static final Type AUX_RETURN = new Type("AUX return");

    /** Auxiliary send gain.  */
    public static final Type AUX_SEND = new Type("AUX send");

    /** Balance.  */
    public static final Type BALANCE = new Type("Balance");

    /** Master gain control.  */
    public static final Type MASTER_GAIN = new Type("Master gain");

    /** Control for panning.  */
    public static final Type PAN = new Type("Pan");

    /** Post-reverb gain.  */
    public static final Type REVERB_RETURN = new Type("Reverb return");

    /** Pre-reverb gain.  */
    public static final Type REVERB_SEND = new Type("Reverb send");

    /** Control the sample rate.  */
    public static final Type SAMPLE_RATE = new Type("Sample rate");

    /** Volume control.  */
    public static final Type VOLUME = new Type("Volume");

    /**
     * Create a new type given its name.
     * @param name the name of the type
     */
    protected Type(String name)
    {
      super(name);
    }
  }

  private float minimum;
  private float maximum;
  private float precision;
  private int updatePeriod;
  private float value;
  private String units;
  private String minLabel;
  private String maxLabel;
  private String midLabel;

  /**
   * Create a new FloatControl given its type and various parameters.
   * The minimum, maximum, and midpoint labels will all be the empty string.
   *
   * @param type the type
   * @param min the minimum valuee
   * @param max the maximum value
   * @param prec the precision
   * @param update the update period
   * @param init the initial value
   * @param units the description of the units
   */
  protected FloatControl(Type type, float min, float max, float prec,
                         int update, float init, String units)
  {
    super(type);
    this.minimum = min;
    this.maximum = max;
    this.precision = prec;
    this.updatePeriod = update;
    this.value = init;
    this.units = units;
    this.minLabel = "";
    this.maxLabel = "";
    this.midLabel = "";
  }

  /**
   * Create a new FloatControl given its type and various parameters.
   *
   * @param type the type
   * @param min the minimum valuee
   * @param max the maximum value
   * @param prec the precision
   * @param update the update period
   * @param init the initial value
   * @param units the description of the units
   * @param minLabel the label for the minimum value
   * @param midLabel the label for the midpoint
   * @param maxLabel the label for the maximum value
   */
  protected FloatControl(Type type, float min, float max, float prec,
                         int update, float init, String units,
                         String minLabel, String midLabel, String maxLabel)
  {
    super(type);
    this.minimum = min;
    this.maximum = max;
    this.precision = prec;
    this.updatePeriod = update;
    this.value = init;
    this.units = units;
    this.minLabel = minLabel;
    this.maxLabel = maxLabel;
    this.midLabel = midLabel;
  }

  /**
   * Return the maximum value of this control.
   */
  public float getMaximum()
  {
    return maximum;
  }

  /**
   * Return the label for the minimum value of this control.
   */
  public String getMaxLabel()
  {
    return maxLabel;
  }

  /**
   * Return the label for the midpoint of this control.
   */
  public String getMidLabel()
  {
    return midLabel;
  }

  /**
   * Return the minimum value of this control.
   */
  public float getMinimum()
  {
    return minimum;
  }

  /**
   * Return the label for the minimum value of this control.
   */
  public String getMinLabel()
  {
    return minLabel;
  }

  /**
   * Return the precision of this control.
   */
  public float getPrecision()
  {
    return precision;
  }

  /**
   * Return the name of the units for this control.
   */
  public String getUnits()
  {
    return units;
  }

  /**
   * Return the update period of this control.
   */
  public int getUpdatePeriod()
  {
    return updatePeriod;
  }

  /**
   * Return the current value of this control.
   */
  public float getValue()
  {
    return value;
  }

  /**
   * Set the new value of this control.
   * @param value the new value
   * @throws IllegalArgumentException if the new value is greater than the
   * maximum or less than the minimum.
   */
  public void setValue(float value)
  {
    if (value < minimum || value > maximum)
      throw new IllegalArgumentException("value out of range");
    this.value = value;
  }

  /**
   * This tells the control to start at the starting value
   * and to shift its value incrementally to the final value
   * over the given time interval, specified in microseconds.
   * The default implementation does not do this, but instead
   * simply sets the value to the final value immediately.
   *
   * @param from the starting value
   * @param to the final value
   * @param ms the number of microseconds
   */
  public void shift(float from, float to, int ms)
  {
    if (from < minimum || from > maximum
        || to < minimum || to > maximum
        || ms < 0)
      throw new IllegalArgumentException("argument out of range");
    // The default just sets the value to TO.
    this.value = to;
  }

  /**
   * Return a string describing this control.
   */
  public String toString()
  {
    return super.toString() + ": " + value;
  }
}
