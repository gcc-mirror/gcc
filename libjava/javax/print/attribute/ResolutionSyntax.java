/* ResolutionSyntax.java -- 
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
 * @author Michael Koch
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
   * @param units the unit to use
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
   * Tests of obj is equal to this object.
   *
   * @param obj the object to test
   *
   * @return true if both objects are equal, false otherwise.
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
   * Returns the cross feed resolution in units.
   *
   * @return the resolution
   *
   * @exception IllegalArgumentException if units < 1
   */
  public int getCrossFeedResolution(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return (crossFeedResolution + units) / units;
  }

  /**
   * Returns the raw cross feed resolution in units.
   *
   * @return the raw resolution
   */
  protected int getCrossFeedResolutionDphi()
  {
    return crossFeedResolution;
  }

  /**
   * Returns the feed resolution in units.
   *
   * @return the resolution
   *
   * @exception IllegalArgumentException if units < 1
   */
  public int getFeedResolution(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return (crossFeedResolution + units) / units;
  }

  /**
   * Returns the raw feed resolution in units.
   *
   * @return the raw resolution
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
   * @return the array with the resolutions
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
   * @return the hashcode
   */
  public int hashCode()
  {
    return crossFeedResolution + feedResolution;
  }

  /**
   * Checks of other is a lower or equal resolution.
   *
   * @param other the resolution to check against
   *
   * @return true if other describes a lower or equal resolution
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
   *
   * @return the string representation
   */
  public String toString()
  {
    return toString(1, "dphi");
  }

  /**
   * Returns the string representation for this object.
   *
   * @param units the units to use
   * @param unitsName the name of the units
   *
   * @return the string representation
   */
  public String toString(int units, String unitsName)
  {
    return ("" + getCrossFeedResolution(units)
            + "x" + getFeedResolution(units)
            + " " + unitsName);
  }
}
