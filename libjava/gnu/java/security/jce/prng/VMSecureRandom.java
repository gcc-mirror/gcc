/* VMSecureRandom.java -- random seed generator.
   Copyright (C) 2006, 2009  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.java.security.jce.prng;

/**
 * VM-specific methods for generating real (or almost real) random
 * seeds. VM implementors should write a version of this class that
 * reads random bytes from some system source.
 */
final class VMSecureRandom
{

  /**
   * <p>
   * Generate a random seed. Implementations are free to generate
   * fewer random bytes than are requested, and leave the remaining
   * bytes of the destination buffer as zeros. Implementations SHOULD,
   * however, make a best-effort attempt to satisfy the request.
   * </p>
   * <p>
   * The GCJ implementation uses a native method to read bytes from
   * a system random source (e.g. /dev/random).
   * </p>
   *
   * @param buffer The destination buffer.
   * @param offset The offset in the buffer to start putting bytes.
   * @param length The number of random bytes to generate.
   * @return the number of bytes generated.
   */
  static int generateSeed(byte[] buffer, int offset, int length)
  {
    if (length < 0)
      throw new IllegalArgumentException("length must be nonnegative");
    if (offset < 0 || offset + length > buffer.length)
      throw new IndexOutOfBoundsException();

    return natGenerateSeed(buffer, offset, length);
  }

  static native int natGenerateSeed(byte[] buffer, int offset, int length);
 
}