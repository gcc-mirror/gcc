/* Length.java -- Converts CSS length values
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.swing.text.html.css;

/**
 * Converts CSS length values to usable length values.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class Length
{

  /**
   * The original value.
   */
  private String value;

  /**
   * The converted value.
   */
  protected float floatValue;

  /**
   * Indicates when the value is a percentage value.
   */
  private boolean isPercentage;

  /**
   * Indicates a length value that is relative to the font size (em).
   */
  private boolean isFontEMRelative;

  /**
   * Indicates a length value that is relative to the font size (ex).
   */
  private boolean isFontEXRelative;

  /**
   * The EM base size.
   */
  private float emBase;

  /**
   * The EX base size.
   */
  private float exBase;

  /**
   * Creates a new length converter instance.
   *
   * @param val the CSS value
   */
  public Length(String val)
  {
    isFontEMRelative = false;
    isFontEXRelative = false;
    isPercentage = false;
    value = val;
    int i = value.indexOf("px");
    int percent = value.indexOf("%");
    int em = value.indexOf("em");
    int ex = value.indexOf("ex");
    try
      {
        floatValue = 0.0F;
        if (i != -1)
          {
            String sub = value.substring(0, i);
            floatValue = Float.parseFloat(sub);
          }
        else if (percent != -1)
          {
            isPercentage = true;
            String sub = value.substring(0, percent);
            floatValue = Float.parseFloat(sub) / 100;
          }
        else if (em != -1)
          {
            isFontEMRelative = true;
            String sub = value.substring(0, em);
            floatValue = Float.parseFloat(sub);
          }
        else if (ex != -1)
          {
            isFontEXRelative = true;
            String sub = value.substring(0, ex);
            floatValue = Float.parseFloat(sub);
          }
        else
          {
            floatValue = Float.parseFloat(value);
          }
      }
    catch (NumberFormatException exc)
      {
        // Don't let such small problems interrupt CSS parsing.
        System.err.println("couldn't parse: " + val);
      }
  }

  /**
   * Returns the value converted to pixels.
   *
   * @return the value converted to pixels
   */
  public float getValue()
  {
    return floatValue;
  }

  /**
   * Returns the absolute span for the case when this length value is
   * a relative value.
   *
   * @param base the base span
   *
   * @return the absolute span
   */
  public float getValue(float base)
  {
    float span = floatValue;
    if (isPercentage)
      span *= base;
    else if (isFontEMRelative)
      span *= emBase;
    else if (isFontEXRelative)
      span *= exBase;
    return span;
  }

  /**
   * Sets the font relative EM base.
   *
   * @param base the font relative EM base
   */
  public void setEMBase(float base)
  {
    emBase = base;
  }

  /**
   * Sets the font relative EX base.
   *
   * @param base the font relative EX base
   */
  public void setEXBase(float base)
  {
    exBase = base;
  }

  /**
   * Sets the font relative base values.
   *
   * @param emBase the EM base
   * @param exBase the EX base
   */
  public void setFontBases(float emBase, float exBase)
  {
    setEMBase(emBase);
    setEXBase(exBase);
  }

  /**
   * Returns true when this length value is an em font relative value. In
   * order to get correct results, you need the exBase property set up
   * correctly.
   *
   * @return true when this length value is an ex font relative value
   */
  public boolean isFontEMRelative()
  {
    return isFontEMRelative;
  }

  /**
   * Returns true when this length value is an ex font relative value. In
   * order to get correct results, you need the emBase property set up
   * correctly.
   *
   * @return true when this length value is an ex font relative value
   */
  public boolean isFontEXRelative()
  {
    return isFontEXRelative;
  }

  /**
   * Returns <code>true</code> when the length value is a percentage
   * value, <code>false</code> otherwise.
   *
   * @return <code>true</code> when the length value is a percentage
   *         value, <code>false</code> otherwise
   */
  public boolean isPercentage()
  {
    return isPercentage;
  }

  /**
   * Checks if the specified value makes up a valid length value.
   *
   * @param value the value to check
   *
   * @return <code>true</code> if the value is a valid length
   */
  public static boolean isValid(String value)
  {
    boolean isValid = true;
    int px = value.indexOf("px");
    int em = value.indexOf("em");
    int ex = value.indexOf("ex");
    int pc = value.indexOf('%');
    try
      {
        if (px != -1)
          {
            Integer.parseInt(value.substring(0, px));
          }
        else if (em != -1)
          {
            Integer.parseInt(value.substring(0, em));
          }
        else if (ex != -1)
          {
            Integer.parseInt(value.substring(0, ex));
          }
        else if (pc != -1)
          {
            Integer.parseInt(value.substring(0, ex));
          }
        else
          {
            Integer.parseInt(value);
          }
      }
    catch (NumberFormatException nfe)
      {
        isValid = false;
      }
    return isValid;
  }

  public String toString()
  {
    return value;
  }
}
