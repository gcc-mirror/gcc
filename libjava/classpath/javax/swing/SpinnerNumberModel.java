/* SpinnerNumberModel.java --
   Copyright (C) 2002, 2004, 2006 Free Software Foundation, Inc.

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

package javax.swing;

import java.io.Serializable;

import javax.swing.event.ChangeEvent;

/**
 * A model used by the {@link JSpinner} component.
 *
 * @author Ka-Hing Cheung
 * @since 1.4
 */
public class SpinnerNumberModel extends AbstractSpinnerModel
  implements Serializable
{
  /**
   * For compatability with Sun's JDK
   */
  private static final long serialVersionUID = 7279176385485777821L;

  /** The current value. */
  private Number value;

  /** The minimum value (or <code>null</code>). */
  private Comparable minimum;

  /** The maximum value (or <code>null</code>). */
  private Comparable maximum;

  /** The step size. */
  private Number stepSize;

  /**
   * Creates a <code>SpinnerNumberModel</code> with initial value 0, step 1,
   * and no maximum nor minimum.
   */
  public SpinnerNumberModel()
  {
    this(new Integer(0), null, null, new Integer(1));
  }

  /**
   * Creates a <code>SpinnerNumberModel</code> with double precision.
   *
   * @param value the initial value
   * @param minimum the minimum value
   * @param maximum the maximum value
   * @param stepSize the step size
   * @throws IllegalArgumentException if minimum &lt;= value &lt;= maximum does 
   *         not hold.
   */
  public SpinnerNumberModel(double value, double minimum, double maximum,
                            double stepSize)
  {
    this(new Double(value), new Double(minimum), new Double(maximum),
         new Double(stepSize));
  }

  /**
   * Creates a <code>SpinnerNumberModel</code> with integer precision.
   *
   * @param value the initial value
   * @param minimum the minimum value
   * @param maximum the maximum value
   * @param stepSize the step size
   * @throws IllegalArgumentException if minimum &lt;= value &lt;= maximum does 
   *         not hold.
   */
  public SpinnerNumberModel(int value, int minimum, int maximum, int stepSize)
  {
    this(new Integer(value), new Integer(minimum), new Integer(maximum),
         new Integer(stepSize));
  }

  /**
   * Creates a <code>SpinnerNumberModel</code> with the given attributes.  The
   * caller should ensure that both <code>minimum</code> and 
   * <code>maximum</code> are serializable.
   *
   * @param value the initial value (<code>null</code> not permitted).
   * @param minimum the minimum value (<code>null</code> permitted).
   * @param maximum the maximum value (<code>null</code> permitted).
   * @param stepSize the step size  (<code>null</code> not permitted).
   *
   * @throws IllegalArgumentException if minimum &lt;= value &lt;= maximum
   *         does not hold
   * @throws IllegalArgumentException if <code>value</code> is 
   *         <code>null</code>.
   * @throws IllegalArgumentException if <code>stepSize</code> is 
   *         <code>null</code>.
   */
  public SpinnerNumberModel(Number value, Comparable minimum,
                            Comparable maximum, Number stepSize)
  {
    if (stepSize == null)
      throw new IllegalArgumentException("stepSize may not be null");
    if (value == null)
      throw new IllegalArgumentException("value may not be null");
    if (minimum != null)
      {
        if (minimum.compareTo(value) > 0)
          throw new IllegalArgumentException("minimum is not <= value");
      }
    if (maximum != null)
      {
        if (maximum.compareTo(value) < 0)
          throw new IllegalArgumentException("maximum is not >= value");
      }

    this.value = value;
    this.stepSize = stepSize;
    this.minimum = minimum;
    this.maximum = maximum;
  }

  /**
   * Sets the current value and, if the new value is different to the old
   * value, sends a {@link ChangeEvent} to all registered listeners.
   *
   * @param value the new value (<code>null</code> not permitted, must be an
   *              instance of <code>Number</code>).
   *
   * @throws IllegalArgumentException if <code>value</code> is not an instance
   *         of <code>Number</code>.
   */
  public void setValue(Object value)
  {
    if (! (value instanceof Number))
      throw new IllegalArgumentException("value must be a Number");

    if (!this.value.equals(value)) 
      {
        this.value = (Number) value;
        fireStateChanged();
      }
  }

  /**
   * Returns the current value, which for this class is always an instance of
   * {@link Number}.
   *
   * @return The current value.
   * 
   * @see #getNumber()
   */
  public Object getValue()
  {
    return value;
  }

  /**
   * Returns the next value, or <code>null</code> if adding the step size to
   * the current value results in a value greater than the maximum value.  
   * The current value is not changed.
   *
   * @return The next value, or <code>null</code> if the current value is the 
   *         maximum value represented by this model.
   */
  public Object getNextValue()
  {
    Number num;

    if (value instanceof Double)
      num = new Double(value.doubleValue() + stepSize.doubleValue());
    else if (value instanceof Float)
      num = new Double(value.floatValue() + stepSize.floatValue());
    else if (value instanceof Long)
      num = new Long(value.longValue() + stepSize.longValue());
    else if (value instanceof Integer)
      num = new Integer(value.intValue() + stepSize.intValue());
    else if (value instanceof Short)
      num = new Short((short) (value.shortValue() + stepSize.shortValue()));
    else
      num = new Byte((byte) (value.byteValue() + stepSize.byteValue()));
    
    // check upper bound if set
    if ((maximum != null) && maximum.compareTo(num) < 0)
      num = null;
    
    return num;
  }

  /**
   * Returns the previous value, or <code>null</code> if subtracting the
   * step size from the current value results in a value less than the minimum
   * value.  The current value is not changed.
   *
   * @return The previous value, or <code>null</code> if the current value
   *         is the minimum value represented by this model.
   */
  public Object getPreviousValue()
  {
    Number num;

    if (value instanceof Double)
      num = new Double(value.doubleValue() - stepSize.doubleValue());
    else if (value instanceof Float)
      num = new Double(value.floatValue() - stepSize.floatValue());
    else if (value instanceof Long)
      num = new Long(value.longValue() - stepSize.longValue());
    else if (value instanceof Integer)
      num = new Integer(value.intValue() - stepSize.intValue());
    else if (value instanceof Short)
      num = new Short((short) (value.shortValue() - stepSize.shortValue()));
    else
      num = new Byte((byte) (value.byteValue() - stepSize.byteValue()));
    
    // check lower bound if set
    if ((minimum != null) && minimum.compareTo(num) > 0)
      num = null;

    return num;
  }

  /**
   * Returns the current value.
   *
   * @return The current value.
   */
  public Number getNumber()
  {
    return value;
  }

  /**
   * Returns the minimum value, or <code>null</code> if there is no minimum.
   * 
   * @return The minimum value.
   * 
   * @see #setMinimum(Comparable)
   */
  public Comparable getMinimum()
  {
    return minimum;
  }

  /**
   * Sets the minimum value and, if the new value is different to the old
   * value, sends a {@link ChangeEvent} to all registered listeners.  A 
   * <code>null</code> value is interpreted as "no minimum value".  No check
   * is made to ensure that the new minimum is less than or equal to the 
   * current value, the caller is responsible for ensuring that this 
   * relationship holds.  In addition, the caller should ensure that
   * <code>newMinimum</code> is {@link Serializable}.
   * 
   * @param newMinimum  the new minimum value (<code>null</code> permitted).
   * 
   * @see #getMinimum()
   */
  public void setMinimum(Comparable newMinimum)
  {
    if (minimum != null ? !minimum.equals(newMinimum) : newMinimum != null)
      {
        minimum = newMinimum;
        fireStateChanged();
      }
  }

  /**
   * Returns the maximum value, or <code>null</code> if there is no maximum.
   * 
   * @return The maximum value.
   * 
   * @see #getMinimum()
   * @see #setMaximum(Comparable)
   */
  public Comparable getMaximum()
  {
    return maximum;
  }

  /**
   * Sets the maximum value and, if the new value is different to the old
   * value, sends a {@link ChangeEvent} to all registered listeners.  A 
   * <code>null</code> value is interpreted as "no maximum value".  No check
   * is made to ensure that the new maximum is greater than or equal to the 
   * current value, the caller is responsible for ensuring that this 
   * relationship holds. In addition, the caller should ensure that
   * <code>newMaximum</code> is {@link Serializable}.
   * 
   * @param newMaximum  the new maximum (<code>null</code> permitted).
   * 
   * @see #getMaximum()
   */
  public void setMaximum(Comparable newMaximum)
  {
    if (maximum != null ? !maximum.equals(newMaximum) : newMaximum != null)
      {
        maximum = newMaximum;
        fireStateChanged();
      }
  }

  /**
   * Returns the step size.
   * 
   * @return The step size (never <code>null</code>).
   */
  public Number getStepSize()
  {
    return stepSize;
  }

  /**
   * Sets the step size and, if the new step size is different to the old
   * step size, sends a {@link ChangeEvent} to all registered listeners.
   * 
   * @param newStepSize  the new step size (<code>null</code> not permitted).
   * 
   * @throws IllegalArgumentException if <code>newStepSize</code> is 
   *         <code>null</code>.
   */
  public void setStepSize(Number newStepSize)
  {
    if (newStepSize == null)
      throw new IllegalArgumentException();

    if (!stepSize.equals(newStepSize))
      {
        stepSize = newStepSize;
        fireStateChanged();
      }
  }
}
