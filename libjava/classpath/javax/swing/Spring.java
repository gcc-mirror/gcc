/* Spring.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Dimension;

/**
 * Calculates the space between component edges, that are layed out by
 * {@link SpringLayout}.
 * <p>
 * A Spring defines a minimum, preferred and maximum distance for each edge
 * (north, east, south, west) of a component.
 * </p>
 * However, springs are not static, their actual values are computed at
 * runtime. That means, if a Spring C is defined as the sum of Spring A and
 * Spring B, then the values (min, pref and max) are not calculated at
 * creation of Spring C, but instead always when {@link #getValue} is
 * called. So, when Spring A or Spring B changes, this is reflected in
 * Spring C.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
public abstract class Spring
{

  /** Indicates a not-set value. **/
  public static final int UNSET = Integer.MIN_VALUE;

  /**
   * Creates a new Spring object. This constructor is used by the static
   * methods which create Springs.
   */
  protected Spring()
  {
    // Nothing to do here.
  }

  /**
   * Creates a Spring which min, pref and max values are all the same.
   * These kind of Springs are 'struts'.
   *
   * @param val the constant for min, pref and max values.
   * @return a Spring object with constant values for min, pref and max.
   */
  public static Spring constant(int val)
  {
    return new SimpleSpring(val, val, val);
  }

  /** Creates a Spring which min, pref and max values are constants.
   * @param min the constant for the minimum value.
   * @param pref the constant for the preferred value.
   * @param max the constant for the maximum value.
   * @return a Spring object with constant values for min, pref and max.
   */
  public static Spring constant(int min, int pref, int max)
  {
    return new SimpleSpring(min, pref, max);
  }

  /**
   * Returns the maximum value of the Spring.
   *
   * @return the maximum value.
   */
  public abstract int getMaximumValue();

  /**
   * Returns the minimum value of this Spring.
   *
   * @return the minimum value.
   */
  public abstract int getMinimumValue();

  /**
   * Return the preferred value of this Spring.
   *
   * @return the preferred value.
   */
  public abstract int getPreferredValue();

  /**
   * Return the actual value of this Spring.
   *
   * @return the actual value of this Spring.
   */
  public abstract int getValue();

  /**
   * Creates and returns a Spring, which always has the maximum values
   * min = max(min_s1, min_s2), pref = max(pref_s1, pref_s2), max =
   * max(max_s1, max_s2).
   *
   * @param s1 the first summand of the max Spring.
   * @param s2 the second summand of the max Spring.
   * @return a Spring which is max(s1, s2).
   */
  public static Spring max(Spring s1, Spring s2)
  {
    return new MaxSpring(s1, s2);
  }

  /**
   * Creates and returns a Spring, which is always the negation of s.
   * min = -min_s, pref = -pref_s, max = -max_pref.
   *
   * @param s the Spring to be negated.
   * @return the negative of <code>s</code>.
   */
  public static Spring minus(Spring s)
  {
    return new MinusSpring(s);
  }

  /**
   * Sets the actual value. If <code>value</code> is out of the (min, max)
   * bounds, then the value is adjusted, so that is inside these bounds.
   *
   * @param value the value to be set.
   */
  public abstract void setValue(int value);

  private int getShrinkRange()
  {
    return (getPreferredValue() - getMinimumValue());
  }

  private int getExpandRange()
  {
    return (getMaximumValue() - getPreferredValue());
  }

  double getStrain()
  {
    int v = getValue();
    int p = getPreferredValue();
    int r = (v < p) ? getShrinkRange() : getExpandRange();
    if (r == 0)
      r = 1;
    return (double)(v - p) / r;
  }

  void setStrain(double strain)
  {
    int r = (strain < 0) ? getShrinkRange() : getExpandRange();
    int v = (getPreferredValue() + (int)(strain * r));
    setValue(v);
  }

  /**
   * Creates and returns a Spring, which is always the sum of s1 and s2.
   * min_sum = min_s1 + min_s2, pref_sum = pref_s1 + pref_s2, max_sum =
   * max_s1 + max_s2.
   *
   * @param s1 the 1st summand of the sum Spring.
   * @param s2 the 2nd summand of the sum Spring.
   * @return a sum which is <code>s1 + s2</code>.
   */
  public static Spring sum(Spring s1, Spring s2)
  {
    return new AddSpring(s1, s2);
  }

  /**
   * Return a new Spring which computes its values by scaling
   * the values of another spring by a constant factor.  If the
   * factor is negative, the minimum and maximum values of
   * the argument spring will be interchanged.
   * @param spring the spring to track
   * @param factor the factor by which to scale
   * @return a new multiplicative Spring
   * @since 1.5
   */
  public static Spring scale(final Spring spring, final float factor)
  {
    if (spring == null)
      throw new NullPointerException("spring argument is null");
    return new Spring()
    {
      public int getMaximumValue()
      {
        return (int) ((factor < 0 ? spring.getMinimumValue()
                            : spring.getMaximumValue())
                      * factor);
      }

      public int getMinimumValue()
      {
        return (int) ((factor < 0 ? spring.getMaximumValue()
                                  : spring.getMinimumValue())
                            * factor);
      }

      public int getPreferredValue()
      {
        return (int) (spring.getPreferredValue() * factor);
      }

      public int getValue()
      {
        return (int) (spring.getValue() * factor);
      }

      public void setValue(int value)
      {
        spring.setValue((int) (value / factor));
      }
    };
  }

  /**
   * Return a new Spring which takes its values from the specified
   * Component.  In particular, the maximum value is taken from
   * the maximumSize, the minimum value is taken from the minimumSize,
   * the preferred value is taken from the preferredSize, and the
   * value is taken from the component's current size.  These values
   * change as the component changes size.
   * @param component the component
   * @return a new Spring which tracks the component's width
   * @since 1.5
   */
  public static Spring width(final Component component)
  {
    return new Spring()
    {
      public int getMaximumValue()
      {
        return component.getMaximumSize().width;
      }

      public int getMinimumValue()
      {
        return component.getMinimumSize().width;
      }

      public int getPreferredValue()
      {
        return component.getPreferredSize().width;
      }

      public int getValue()
      {
        return component.getSize().width;
      }

      public void setValue(int value)
      {
        Dimension d = component.getSize();
        component.setSize(value, d.height);
      }
    };
  }

  /**
   * Return a new Spring which takes its values from the specified
   * Component.  In particular, the maximum value is taken from
   * the maximumSize, the minimum value is taken from the minimumSize,
   * the preferred value is taken from the preferredSize, and the
   * value is taken from the component's current size.  These values
   * change as the component changes size.
   * @param component the component
   * @return a new Spring which tracks the component's height
   * @since 1.5
   */
  public static Spring height(final Component component)
  {
    return new Spring()
    {
      public int getMaximumValue()
      {
        return component.getMaximumSize().height;
      }

      public int getMinimumValue()
      {
        return component.getMinimumSize().height;
      }

      public int getPreferredValue()
      {
        return component.getPreferredSize().height;
      }

      public int getValue()
      {
        return component.getSize().height;
      }

      public void setValue(int value)
      {
        Dimension d = component.getSize();
        component.setSize(d.width, value);
      }
    };
  }

  /**
   * A simple Spring, that holds constant values for min, pref and max.
   *
   * @author Roman Kennke (roman@ontographics.com)
   */
  private static final class SimpleSpring extends Spring
  {

    /** The constant value for min. */
    private final int min;

    /** The constant value for pref. */
    private final int pref;

    /** The constant value for max. */
    private final int max;

    /** The actual value of the spring. */
    private int value;

    public String toString()
    {
      return "SimpleSpring of " + value;
    }

    /**
     * Creates a new SimpleSpring object.
     *
     * @param newMin the constant minimum value.
     * @param newPref the constant preferred value.
     * @param newMax the constant maximum value.
     */
    public SimpleSpring(int newMin, int newPref, int newMax)
    {
      min = newMin;
      pref = newPref;
      max = newMax;
      value = newPref;
    }

    /**
     * Returns the maximum value of this Spring.
     *
     * @return the maximum value.
     */
    public int getMaximumValue()
    {
      return max;
    }

    /**
     * Returns the minimum value of this Spring.
     *
     * @return the minimum value.
     */
    public int getMinimumValue()
    {
      return min;
    }

    /**
     * Returns the preferred value of this Spring.
     *
     * @return the preferred value.
     */
    public int getPreferredValue()
    {
      return pref;
    }

    /**
     * Return the actual current value of this Spring.
     *
     * @return the current value.
     */
    public int getValue()
    {
      if (value == Spring.UNSET)
          return pref;
      return value;
    }

    /**
     * Sets the current value.
     *
     * @param val the value to be set.
     */
    public void setValue(int val)
    {
      value = val;
    }
  }


  /**
   * A Spring, that is the sum of two other Springs.
   *
   * @author Roman Kennke (roman@ontographics.com)
   */
  private static final class AddSpring extends Spring
  {

    /** The springs, that are the 'operands' of this Spring. */
    private final Spring s1;
    private final Spring s2;

    /** The current value for this Spring. */
    private int value;

    public String toString()
    {
      return "AddSpring of " + s1 + " and " + s2;
    }

    /**
     * Creates a new AddSpring object.
     *
     * @param s1 the first operand.
     * @param s2 the second operand.
     */
    protected AddSpring(Spring s1, Spring s2)
    {
      super();
      this.s1 = s1;
      this.s2 = s2;
      value = Spring.UNSET;
    }

    /**
     * Returns the maximum value of this Spring.
     *
     * @return the maximum value.
     */
    public int getMaximumValue()
    {
      int max1 = s1.getMaximumValue();
      int max2 = s2.getMaximumValue();
      return max1 + max2;
    }

    /**
     * Return the minimum value of this Spring.
     *
     * @return the minimum value.
     */
    public int getMinimumValue()
    {
      int min1 = s1.getMinimumValue();
      int min2 = s2.getMinimumValue();
      return min1 + min2;
    }

    /**
     * Returns the preferred value of this Spring.
     *
     * @return the preferred value.
     */
    public int getPreferredValue()
    {
      int pref1 = s1.getPreferredValue();
      int pref2 = s2.getPreferredValue();
      return pref1 + pref2;
    }

    /**
     * Returns the actual current value of this Spring.
     *
     * @return the current value of this Spring.
     */
    public int getValue()
    {
      if (value == Spring.UNSET)
        {
          int val1 = s1.getValue();
          int val2 = s2.getValue();
          value = val1 + val2;
        }
      return value;
    }

    /**
     * Sets the current value.
     *
     * @param val the value to be set.
     */
    public void setValue(int val)
    {
      if (val == Spring.UNSET)
      {
        if (value != Spring.UNSET)
        {
          s1.setValue(Spring.UNSET);
          s2.setValue(Spring.UNSET);
        }
        value = Spring.UNSET;
        return;
      }

      value = val;

      //Spead the value over the two components
      double fStrain = getStrain();
      s1.setStrain(fStrain);
      int remainder = val - s1.getValue();
      s2.setValue(remainder);
    }

  }


  /**
   * A Spring that is calculated as the negation of another Spring.
   *
   * @author Roman Kennke (roman@ontographics.com)
   */
  private static final class MinusSpring extends Spring
  {

    /** The Spring from which to calculate the negation. */
    private final Spring s;

    public String toString()
    {
      return "MinusSpring of " + s;
    }

    /**
     * Creates a new MinusSpring object.
     * @param s the Spring from which to calculate the negation.
     */
    protected MinusSpring(Spring s)
    {
      super();
      this.s = s;
    }

    /** Returns the maximum value of this Spring.
     *
     * @return the maximum value.
     */
    public int getMaximumValue()
    {
      return -s.getMinimumValue();
    }

    /**
     * Returns the minimum value of this Spring.
     *
     * @return the minimum value.
     */
    public int getMinimumValue()
    {
      return -s.getMaximumValue();
    }

    /**
     * Returns the preferred value of this Spring.
     *
     * @return the preferred value.
     */
    public int getPreferredValue()
    {
      return -s.getPreferredValue();
    }

    /**
     * Returns the current value of this Spring.
     *
     * @return the current value.
     */
    public int getValue()
    {
      return -s.getValue();
    }

    /**
     * Sets the current value.
     *
     * @param val the value to be set.
     */
    public void setValue(int val)
    {
      if (val == Spring.UNSET)
        s.setValue(Spring.UNSET);
      else
        s.setValue(-val);
    }
  }


  /**
   * A Spring, that is calculated as the maximum of two Springs.
   *
   * @author Roman Kennke (roman@ontographics.com)
   */
  private static final class MaxSpring extends Spring
  {

    /** The two other Springs from which to calculate the maximum. */
    private final Spring s1;
    private final Spring s2;

    public String toString()
    {
      return "MaxSpring of " + s1 + " and " + s2;
    }

    /** The current value of this Spring. */
    private int value;

    /**
     * Creates a new MaxSpring object.
     *
     * @param s1 the 1st operand.
     * @param s2 the 2nd operand.
     */
    protected MaxSpring(Spring s1, Spring s2)
    {
      super();
      this.s1 = s1;
      this.s2 = s2;
      value = Spring.UNSET;
    }


    /**
     * Returns the maximum value of this Spring.
     *
     * @return the maximum value.
     */
    public int getMaximumValue()
    {
      int max1 = s1.getMaximumValue();
      int max2 = s2.getMaximumValue();
      return Math.max(max1, max2);
    }

    /**
     * Returns the minimum value of this Spring.
     *
     * @return the minimum value.
     */
    public int getMinimumValue()
    {
      int min1 = s1.getMinimumValue();
      int min2 = s2.getMinimumValue();
      return Math.max(min1, min2);
    }

    /**
     * Returns the preferred value of this Spring.
     *
     * @return the preferred value.
     */
    public int getPreferredValue()
    {
      int pref1 = s1.getPreferredValue();
      int pref2 = s2.getPreferredValue();
      return Math.max(pref1, pref2);
    }

    /**
     * Returns the actual value of this Spring.
     *
     * @return the current value.
     */
    public int getValue()
    {
      if (value == Spring.UNSET)
      {
          int val1 = s1.getValue();
          int val2 = s2.getValue();
          value = Math.max(val1, val2);
      }
      return value;
    }

    /**
     * Sets the current value.
     *
     * @param val the value to be set.
     */
    public void setValue(int val)
    {
      if (val == Spring.UNSET)
      {
        if (value != Spring.UNSET)
        {
          s1.setValue(Spring.UNSET);
          s2.setValue(Spring.UNSET);
        }
        value = Spring.UNSET;
        return;
      }

      value = val;

      int p1 = s1.getPreferredValue();
      int p2 = s2.getPreferredValue();

      if (p1 < p2)
      {
        s1.setValue(Math.min(val, p1));
        s2.setValue(val);
      }
      else
      {
        s1.setValue(val);
        s2.setValue(Math.min(val, p2));
      }
    }
  }
}
