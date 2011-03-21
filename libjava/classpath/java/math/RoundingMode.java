/* RoundingMode.java -- An Enum to replace BigDecimal rounding constants.
   Copyright (C) 1999, 2000, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.math;

/**
 * An enum to specify rounding behaviour for numerical operations that may
 * discard precision.
 * @author Anthony Balkissoon abalkiss at redhat dot com
 *
 */
public enum RoundingMode
{
  UP, DOWN, CEILING, FLOOR, HALF_UP, HALF_DOWN, HALF_EVEN, UNNECESSARY;

  /**
   * For compatability with Sun's JDK
   */
  private static final long serialVersionUID = 432302042773881265L;

  /**
   * Returns the RoundingMode object corresponding to the legacy rounding modes
   * in BigDecimal.
   * @param rm the legacy rounding mode
   * @return the corresponding RoundingMode
   */
  public static RoundingMode valueOf(int rm)
  {
    switch (rm)
      {
      case BigDecimal.ROUND_CEILING:
        return CEILING;
      case BigDecimal.ROUND_FLOOR:
        return FLOOR;
      case BigDecimal.ROUND_DOWN:
        return DOWN;
      case BigDecimal.ROUND_UP:
        return UP;
      case BigDecimal.ROUND_HALF_UP:
        return HALF_UP;
      case BigDecimal.ROUND_HALF_DOWN:
        return HALF_DOWN;
      case BigDecimal.ROUND_HALF_EVEN:
        return HALF_EVEN;
      case BigDecimal.ROUND_UNNECESSARY:
        return UNNECESSARY;
      default:
        throw new
          IllegalArgumentException("invalid argument: " + rm +
                                   ".  Argument should be one of the " +
                                   "rounding modes defined in BigDecimal.");
      }
  }
}
