/* ResolutionSyntax.java --
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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

package javax.print.attribute;

import java.io.Serializable;

/**
 * <code>ResolutionSyntax</code> is the abstract base class of all attribute
 * classes which provide a resolution as value (e.g. printer resolution).
 * <p>
 * A <code>ResolutionSyntax</code> instance consists of two integer values
 * describing the resolution in feed and cross feed direction. The units of
 * the given values is determined by two defined constants:
 * <ul>
 * <li>DPCM - dots per centimeter</li>
 * <li>DPI - dots per inch</li>
 * </ul>
 * </p>
 * <p>
 * A resolutions attribute is constructed by two values for the resolution and
 * one of the two constants defining the actual units of the given values.
 * </p>
 * <p>
 * There are different methods provided to return the resolution values in
 * either of the both units and to compare if a resolution is less than or
 * equal to a given other resolution attribute.
 * </p>
 * <p>
 * <b>Internal storage:</b><br>
 * The resolutions are stored internally as dots per 100 inches (dphi). The
 * values of the provided constants for dots per inch (value 100) and dots
 * per centimeter (value 254) are used as conversion factors to the internal
 * storage units. To get the internal dphi values a multiplication of a given
 * resolution value with its units constant value is needed. Retrieving the
 * resolution for specific units is done by dividing the internal stored
 * value through the units constant value. Clients are therefore able to
 * provide their own resolution units by supplying other conversion factors.
 * Subclasses of <code>ResolutionSyntax</code> have access to the internal
 * resolution values through the protected methods
 * {@link #getCrossFeedResolutionDphi()} and {@link #getFeedResolutionDphi()}.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class ResolutionSyntax
  implements Cloneable, Serializable
{
  private static final long serialVersionUID = 2706743076526672017L;

  /**
   * Constant for units of dots per centimeter.
   */
  public static final int DPCM = 254;

  /**
   * Constant for units of dots per inch
   */
  public static final int DPI = 100;

  private int crossFeedResolution;
  private int feedResolution;

  /**
   * Creates a <code>ResolutionSyntax</code> object with the given arguments.
   *
   * @param crossFeedResolution the cross feed resolution
   * @param feedResolution the feed resolution
   * @param units the unit to use (e.g. {@link #DPCM} or {@link #DPI})
   *
   * @exception IllegalArgumentException if preconditions fail
   */
  public ResolutionSyntax(int crossFeedResolution, int feedResolution,
                          int units)
  {
    if (crossFeedResolution < 1
        || feedResolution < 1
        || units < 1)
      throw new IllegalArgumentException("no argument may be less than 1");

    this.crossFeedResolution = crossFeedResolution * units;
    this.feedResolution = feedResolution * units;
  }

  /**
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal,
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if(! (obj instanceof ResolutionSyntax))
      return false;

    ResolutionSyntax tmp = (ResolutionSyntax) obj;

    return (crossFeedResolution == tmp.getCrossFeedResolutionDphi()
            && feedResolution == tmp.getFeedResolutionDphi());
  }

  /**
   * Returns the cross feed resolution for the given units.
   *
   * @param units the unit to use (e.g. {@link #DPCM} or {@link #DPI})
   * @return The resolution for the given units.
   *
   * @exception IllegalArgumentException if units &lt; 1
   */
  public int getCrossFeedResolution(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return crossFeedResolution / units;
  }

  /**
   * Returns the raw cross feed resolution in dots per 100 inches.
   *
   * @return The raw resolution.
   */
  protected int getCrossFeedResolutionDphi()
  {
    return crossFeedResolution;
  }

  /**
   * Returns the feed resolution for the given units.
   *
   * @param units the unit to use (e.g. {@link #DPCM} or {@link #DPI})
   * @return The resolution for the given units.
   *
   * @exception IllegalArgumentException if units &lt; 1
   */
  public int getFeedResolution(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return feedResolution / units;
  }

  /**
   * Returns the raw feed resolution in dots per 100 inches.
   *
   * @return The raw resolution.
   */
  protected int getFeedResolutionDphi()
  {
    return feedResolution;
  }

  /**
   * Returns the resolution as two field array. Index 0 is the cross feed
   * resolution, index 1 the feed resolution.
   *
   * @param units the units to use
   *
   * @return The array with the resolutions.
   */
  public int[] getResolution(int units)
  {
    int[] resolution = new int[2];
    resolution[0] = getCrossFeedResolution(units);
    resolution[1] = getFeedResolution(units);
    return resolution;
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return The hashcode.
   */
  public int hashCode()
  {
    return crossFeedResolution + feedResolution;
  }

  /**
   * Checks if the given resolution attribute is a lower or equal
   * to this resolution object.
   *
   * @param other the resolution to check against
   *
   * @return <code>true</code> if other resolution attribute describes
   * a lower or equal resolution, <code>false</code> otherwise.
   */
  public boolean lessThanOrEquals(ResolutionSyntax other)
  {
    if (other == null)
      throw new NullPointerException("other may not be null");

    return (crossFeedResolution <= other.getCrossFeedResolutionDphi()
            && feedResolution <= other.getFeedResolutionDphi());
  }

  /**
   * Returns the string representation for this object.
   * <p>
   * The returned string is in the form "CxF dphi" with C standing
   * for the cross feed and F for the feed direction resolution.
   * Units used are dots per 100 inches (dphi).
   * </p>
   * @return The string representation.
   */
  public String toString()
  {
    return toString(1, "dphi");
  }

  /**
   * Returns the string representation for this object.
   * <p>
   * The returned string is in the form "CxF U" with C standing
   * for the cross feed and F for the feed direction resolution.
   * U denotes the units name if one is supplied.
   * </p>
   *
   * @param units the units to use
   * @param unitsName the name of the units. If <code>null</code>
   * it is ommitted from the string representation.
   *
   * @return The string representation.
   */
  public String toString(int units, String unitsName)
  {
    if (unitsName == null)
      return getCrossFeedResolution(units) + "x" + getFeedResolution(units);

    return ("" + getCrossFeedResolution(units)
            + "x" + getFeedResolution(units)
            + " " + unitsName);
  }
}
