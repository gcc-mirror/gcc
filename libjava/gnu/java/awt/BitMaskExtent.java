/* Copyright (C) 2000, 2002  Free Software Foundation

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package gnu.java.awt;

/** 
 * Simple transparent utility class that can be used to perform bit
 * mask extent calculations.
 */
public final class BitMaskExtent
{
  /** The number of the least significant bit of the bit mask extent. */
  public byte leastSignificantBit;

  /** The number of bits in the bit mask extent. */
  public byte bitWidth;
  
  /**
   * Set the bit mask. This will calculate and set the leastSignificantBit
   * and bitWidth fields.
   *
   * @see #leastSignificantBit
   * @see #bitWidth
   */
  public void setMask(long mask)
  {
    leastSignificantBit = 0;
    bitWidth = 0;
    if (mask == 0) return;
    long shiftMask = mask;
    for (; (shiftMask&1) == 0; shiftMask >>>=1) leastSignificantBit++;
    for (; (shiftMask&1) != 0; shiftMask >>>=1) bitWidth++;
    
    if (shiftMask != 0)
      throw new IllegalArgumentException("mask must be continuous");
  }
  
  /** 
   * Calculate the bit mask based on the values of the
   * leastSignificantBit and bitWidth fields.
   */
  public long toMask()
  {
    return ((1<<bitWidth)-1) << leastSignificantBit;
  }  
}
