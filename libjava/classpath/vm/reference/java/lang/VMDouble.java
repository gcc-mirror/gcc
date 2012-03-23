/* VMDouble.java -- VM Specific Double methods
   Copyright (C) 2003, 2005, 2010  Free Software Foundation, Inc.

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
 * Code relocated from java.lang.Double by
 * @author Dave Grove (groved@us.ibm.com)
 */
final class VMDouble
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

    initIDs();
  }

  private VMDouble() {} // Prohibits instantiation.

  /**
   * Convert the double to the IEEE 754 floating-point "double format" bit
   * layout. Bit 63 (the most significant) is the sign bit, bits 62-52
   * (masked by 0x7ff0000000000000L) represent the exponent, and bits 51-0
   * (masked by 0x000fffffffffffffL) are the mantissa. This function
   * leaves NaN alone, rather than collapsing to a canonical value. The
   * result of this function can be used as the argument to
   * <code>Double.longBitsToDouble(long)</code> to obtain the original
   * <code>double</code> value.
   *
   * @param value the <code>double</code> to convert
   * @return the bits of the <code>double</code>
   * @see #longBitsToDouble(long)
   */
  static native long doubleToRawLongBits(double value);

  /**
   * Convert the argument in IEEE 754 floating-point "double format" bit
   * layout to the corresponding float. Bit 63 (the most significant) is the
   * sign bit, bits 62-52 (masked by 0x7ff0000000000000L) represent the
   * exponent, and bits 51-0 (masked by 0x000fffffffffffffL) are the mantissa.
   * This function leaves NaN alone, so that you can recover the bit pattern
   * with <code>Double.doubleToRawLongBits(double)</code>.
   *
   * @param bits the bits to convert
   * @return the <code>double</code> represented by the bits
   * @see #doubleToLongBits(double)
   * @see #doubleToRawLongBits(double)
   */
  static native double longBitsToDouble(long bits);

  /**
   * Helper method to convert to string.
   *
   * @param d the double to convert
   * @param isFloat true if the conversion is requested by Float (results in
   *        fewer digits)
   */
  static native String toString(double d, boolean isFloat);

  /**
   * Initialize JNI cache.  This method is called only by the
   * static initializer when using JNI.
   */
  private static native void initIDs();

  /**
   * Parse the specified String as a double.
   * @throws NumberFormatException if str cannot be parsed
   * @throws NullPointerException if str is null
   */
  static native double parseDouble(String str);
}
