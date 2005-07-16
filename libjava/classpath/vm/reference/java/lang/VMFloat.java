/* VMFloat.java -- VM Specific Float methods
   Copyright (C) 2003 Free Software Foundation

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

package java.lang;

import gnu.classpath.Configuration;

/*
 * This class is a reference version, mainly for compiling a class library
 * jar.  It is likely that VM implementers replace this with their own
 * version that can communicate effectively with the VM.
 */

/**
 * Code relocated from java.lang.Float by 
 * @author Dave Grove <groved@us.ibm.com>
 */
final class VMFloat
{

  /**
   * Load native routines necessary for this class.
   */
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javalang");
      }
  }

  /**
   * Convert the float to the IEEE 754 floating-point "single format" bit
   * layout. Bit 31 (the most significant) is the sign bit, bits 30-23
   * (masked by 0x7f800000) represent the exponent, and bits 22-0
   * (masked by 0x007fffff) are the mantissa. This function collapses all
   * versions of NaN to 0x7fc00000. The result of this function can be used
   * as the argument to <code>Float.intBitsToFloat(int)</code> to obtain the
   * original <code>float</code> value.
   *
   * @param value the <code>float</code> to convert
   * @return the bits of the <code>float</code>
   * @see #intBitsToFloat(int)
   */
  static native int floatToIntBits(float value);

  /**
   * Convert the float to the IEEE 754 floating-point "single format" bit
   * layout. Bit 31 (the most significant) is the sign bit, bits 30-23
   * (masked by 0x7f800000) represent the exponent, and bits 22-0
   * (masked by 0x007fffff) are the mantissa. This function leaves NaN alone,
   * rather than collapsing to a canonical value. The result of this function
   * can be used as the argument to <code>Float.intBitsToFloat(int)</code> to
   * obtain the original <code>float</code> value.
   *
   * @param value the <code>float</code> to convert
   * @return the bits of the <code>float</code>
   * @see #intBitsToFloat(int)
   */
  static native int floatToRawIntBits(float value);

  /**
   * Convert the argument in IEEE 754 floating-point "single format" bit
   * layout to the corresponding float. Bit 31 (the most significant) is the
   * sign bit, bits 30-23 (masked by 0x7f800000) represent the exponent, and
   * bits 22-0 (masked by 0x007fffff) are the mantissa. This function leaves
   * NaN alone, so that you can recover the bit pattern with
   * <code>Float.floatToRawIntBits(float)</code>.
   *
   * @param bits the bits to convert
   * @return the <code>float</code> represented by the bits
   * @see #floatToIntBits(float)
   * @see #floatToRawIntBits(float)
   */
  static native float intBitsToFloat(int bits);

} // class VMFloat
