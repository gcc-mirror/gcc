/* SpinnerNumberModel.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

/**
 * SpinnerNumberModel
 *
 * @author Ka-Hing Cheung
 * @version 1.0
 */
public class SpinnerNumberModel extends AbstractSpinnerModel
  implements Serializable
{
  /**
   * For compatability with Sun's JDK
   */
  private static final long serialVersionUID = 7279176385485777821L;

  /** DOCUMENT ME! */
  private Number value;

  /** DOCUMENT ME! */
  private Comparable minimum;

  /** DOCUMENT ME! */
  private Comparable maximum;

  /** DOCUMENT ME! */
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
   * Creates a <code>SpinnerNumberModel</code> with double precision
   *
   * @param value the initial value
   * @param minimum the minimum value
   * @param maximum the maximum value
   * @param stepSize the step size
   * @throws IllegalArgumentException if minimum &lt;= value &lt;= maximum does not
   *                                  hold
   */
  public SpinnerNumberModel(double value, double minimum, double maximum,
                            double stepSize)
  {
    this(new Double(value), new Double(minimum), new Double(maximum),
         new Double(stepSize));
  }

  /**
   * Creates a <code>SpinnerNumberModel</code> with integer precision
   *
   * @param value the initial value
   * @param minimum the minimum value
   * @param maximum the maximum value
   * @param stepSize the step size
   * @throws IllegalArgumentException if minimum &lt;= value &lt;= maximum does not
   *                                  hold
   */
  public SpinnerNumberModel(int value, int minimum, int maximum, int stepSize)
  {
    this(new Integer(value), new Integer(minimum), new Integer(maximum),
         new Integer(stepSize));
  }

  /**
   * Creates a <code>SpinnerNumberModel</code> with <code>Number</code>s and
   * <code>Comparable</code>s.
   *
   * @param value the initial value
   * @param minimum the minimum value, if null there's no minimum
   * @param maximum the maximum value, if null there's no maximum
   * @param stepSize the step size
   *
   * @throws IllegalArgumentException if minimum &lt;= value &lt;= maximum
   *         does not hold
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
    else
      minimum = new Comparable()
	  {
	    public int compareTo(Object obj)
	    {
	      return -1;
	    }
	  };


    if (maximum != null)
      {
	if (maximum.compareTo(value) < 0)
	  throw new IllegalArgumentException("maximum is not >= value");
      }
    else
      maximum = new Comparable()
	  {
	    public int compareTo(Object obj)
	    {
	      return 1;
	    }
	  };


    this.value = value;
    this.stepSize = stepSize;
    this.minimum = minimum;
    this.maximum = maximum;
  }

  /**
   * Sets the new value and fire a change event
   *
   * @param value the new value
   *
   * @throws IllegalArgumentException if minimum &lt;= value &lt;= maximum
   *         does not hold
   */
  public void setValue(Object value)
  {
    if (! (value instanceof Number))
      throw new IllegalArgumentException("value must be a Number");

    this.value = (Number) value;
    fireStateChanged();
  }

  /**
   * Gets the current value
   *
   * @return the current value
   */
  public Object getValue()
  {
    return value;
  }

  /**
   * Gets the next value without changing the current value, or null if the
   * current value is maximum.
   *
   * @return the next value
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

    return maximum.compareTo(num) >= 0 ? num : null;
  }

  /**
   * Gets the previous value without changing the current value, or null if
   * the current value is minimum.
   *
   * @return the previous value
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

    return minimum.compareTo(num) <= 0 ? num : null;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Number getNumber()
  {
    return value;
  }

  public Comparable getMinimum()
  {
    return minimum;
  }

  public void setMinimum(Comparable newMinimum)
  {
    if (minimum != newMinimum)
      {
	minimum = newMinimum;
	fireStateChanged();
      }
  }

  public Comparable getMaximum()
  {
    return maximum;
  }

  public void setMaximum(Comparable newMaximum)
  {
    if (maximum != newMaximum)
      {
	maximum = newMaximum;
	fireStateChanged();
      }
  }

  public Number getStepSize()
  {
    return stepSize;
  }

  public void setStepSize(Number newStepSize)
  {
    if (newStepSize == null)
      throw new IllegalArgumentException();

    if (stepSize != newStepSize)
      {
	stepSize = newStepSize;
	fireStateChanged();
      }
  }
}
