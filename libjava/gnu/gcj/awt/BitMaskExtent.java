/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.awt;

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
