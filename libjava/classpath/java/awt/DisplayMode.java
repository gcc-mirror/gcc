/* DisplayMode.java -- a description of display mode configurations
   Copyright (C) 2002, 2003, 2005  Free Software Foundation, Inc.

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


package java.awt;

/**
 * This encapsulates information about the display mode for a graphics
 * device configuration. They are device dependent, and may not always be
 * available.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see GraphicsDevice
 * @since 1.4
 * @status updated to 1.4
 */
public final class DisplayMode
{
  /**
   * Value of the bit depth if multiple depths are supported.
   *
   * @see #getBitDepth()
   */
  public static final int BIT_DEPTH_MULTI = -1;

  /**
   * Value of an unknown refresh rate.
   *
   * @see #getRefreshRate()
   */
  public static final int REFRESH_RATE_UNKNOWN = 0;

  /** The width. */
  private final int width;

  /** The height. */
  private final int height;

  /** The bit depth. */
  private final int bitDepth;

  /** The refresh rate. */
  private final int refreshRate;

  /**
   * Create a mode with the given parameters.
   *
   * @param width the width
   * @param height the height
   * @param bitDepth the bitDepth
   * @param refreshRate the refreshRate
   * @see #BIT_DEPTH_MULTI
   * @see #REFRESH_RATE_UNKNOWN
   */
  public DisplayMode(int width, int height, int bitDepth, int refreshRate)
  {
    this.width = width;
    this.height = height;
    this.bitDepth = bitDepth;
    this.refreshRate = refreshRate;
  }

  /**
   * Returns the height, in pixels.
   *
   * @return the height
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * Returns the width, in pixels.
   *
   * @return the width
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * Returns the bit depth, in bits per pixel. This may be BIT_DEPTH_MULTI.
   *
   * @return the bit depth
   * @see #BIT_DEPTH_MULTI
   */
  public int getBitDepth()
  {
    return bitDepth;
  }

  /**
   * Returns the refresh rate, in hertz. This may be REFRESH_RATE_UNKNOWN.
   *
   * @return the refresh rate
   * @see #REFRESH_RATE_UNKNOWN
   */
  public int getRefreshRate()
  {
    return refreshRate;
  }

  /**
   * Test for equality. This returns true for two modes with identical
   * parameters.
   *
   * @param dm The display mode to compare to
   *
   * @return true if it is equal
   */
  public boolean equals (DisplayMode dm)
  {
    return (width == dm.width
	    && height == dm.height
	    && bitDepth == dm.bitDepth
	    && refreshRate == dm.refreshRate);
  }

  /**
   * Returns a hash code for the display mode.
   *
   * @return the hash code
   */
  public int hashCode()
  {
    return width + height + bitDepth + refreshRate;
  }
} // class DisplayMode
