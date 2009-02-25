/* VMSecureRandom.java -- random seed generator.
   Copyright (C) 2006  Free Software Foundation, Inc.

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
 *
 * <p>The default implementation of this class runs eight threads that
 * increment counters in a tight loop, and XORs each counter to
 * produce one byte of seed data. This is not very efficient, and is
 * not guaranteed to be random (the thread scheduler is probably
 * deterministic, after all). If possible, VM implementors should
 * reimplement this class so it obtains a random seed from a system
 * facility, such as a system entropy gathering device or hardware
 * random number generator.
 */
final class VMSecureRandom
{

  /**
   * Generate a random seed. Implementations are free to generate
   * fewer random bytes than are requested, and leave the remaining
   * bytes of the destination buffer as zeros. Implementations SHOULD,
   * however, make a best-effort attempt to satisfy the request.
   *
   * @param buffer The destination buffer.
   * @param offset The offset in the buffer to start putting bytes.
   * @param length The number of random bytes to generate.
   */
  static int generateSeed(byte[] buffer, int offset, int length)
  {
    if (length < 0)
      throw new IllegalArgumentException("length must be nonnegative");
    if (offset < 0 || offset + length > buffer.length)
      throw new IndexOutOfBoundsException();

    Spinner[] spinners = new Spinner[8];
    int n = 0x1;
    for (int i = 0; i < spinners.length; i++)
      {
        spinners[i] = new Spinner((byte) n);
        Thread t = new Thread(spinners[i]);
        t.start();
        n <<= 1;
      }

    // Wait until at least one spinner has started.
    while (!(spinners[0].running || spinners[1].running || spinners[2].running
             || spinners[3].running || spinners[4].running || spinners[5].running
             || spinners[6].running || spinners[7].running))
      {
        Thread.yield();
      }

    for (int i = offset; i < length; i++)
      {
        buffer[i] = (byte) (spinners[0].value ^ spinners[1].value ^ spinners[2].value
                            ^ spinners[3].value ^ spinners[4].value ^ spinners[5].value
                            ^ spinners[6].value ^ spinners[7].value);
        Thread.yield();
      }

    for (int i = 0; i < spinners.length; i++)
      spinners[i].stop();

    return length;
  }

  static class Spinner implements Runnable
  {
    volatile byte value;
    volatile boolean running;

    Spinner(final byte initial)
    {
      value = initial;
    }

    public void run()
    {
      running = true;
      while (running)
        value++;
    }

    private void stop()
    {
      running = false;
    }
  }
}