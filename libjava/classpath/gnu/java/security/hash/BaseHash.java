/* BaseHash.java --
   Copyright (C) 2001, 2002, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.hash;

/**
 * A base abstract class to facilitate hash implementations.
 */
public abstract class BaseHash
    implements IMessageDigest
{
  /** The canonical name prefix of the hash. */
  protected String name;

  /** The hash (output) size in bytes. */
  protected int hashSize;

  /** The hash (inner) block size in bytes. */
  protected int blockSize;

  /** Number of bytes processed so far. */
  protected long count;

  /** Temporary input buffer. */
  protected byte[] buffer;

  /**
   * Trivial constructor for use by concrete subclasses.
   *
   * @param name the canonical name prefix of this instance.
   * @param hashSize the block size of the output in bytes.
   * @param blockSize the block size of the internal transform.
   */
  protected BaseHash(String name, int hashSize, int blockSize)
  {
    super();

    this.name = name;
    this.hashSize = hashSize;
    this.blockSize = blockSize;
    this.buffer = new byte[blockSize];

    resetContext();
  }

  public String name()
  {
    return name;
  }

  public int hashSize()
  {
    return hashSize;
  }

  public int blockSize()
  {
    return blockSize;
  }

  public void update(byte b)
  {
    // compute number of bytes still unhashed; ie. present in buffer
    int i = (int) (count % blockSize);
    count++;
    buffer[i] = b;
    if (i == (blockSize - 1))
      transform(buffer, 0);
  }

  public void update(byte[] b)
  {
    update(b, 0, b.length);
  }

  public void update(byte[] b, int offset, int len)
  {
    int n = (int) (count % blockSize);
    count += len;
    int partLen = blockSize - n;
    int i = 0;

    if (len >= partLen)
      {
        System.arraycopy(b, offset, buffer, n, partLen);
        transform(buffer, 0);
        for (i = partLen; i + blockSize - 1 < len; i += blockSize)
          transform(b, offset + i);

        n = 0;
      }

    if (i < len)
      System.arraycopy(b, offset + i, buffer, n, len - i);
  }

  public byte[] digest()
  {
    byte[] tail = padBuffer(); // pad remaining bytes in buffer
    update(tail, 0, tail.length); // last transform of a message
    byte[] result = getResult(); // make a result out of context

    reset(); // reset this instance for future re-use

    return result;
  }

  public void reset()
  { // reset this instance for future re-use
    count = 0L;
    for (int i = 0; i < blockSize;)
      buffer[i++] = 0;

    resetContext();
  }

  public abstract Object clone();

  public abstract boolean selfTest();

  /**
   * Returns the byte array to use as padding before completing a hash
   * operation.
   *
   * @return the bytes to pad the remaining bytes in the buffer before
   *         completing a hash operation.
   */
  protected abstract byte[] padBuffer();

  /**
   * Constructs the result from the contents of the current context.
   *
   * @return the output of the completed hash operation.
   */
  protected abstract byte[] getResult();

  /** Resets the instance for future re-use. */
  protected abstract void resetContext();

  /**
   * The block digest transformation per se.
   *
   * @param in the <i>blockSize</i> long block, as an array of bytes to digest.
   * @param offset the index where the data to digest is located within the
   *          input buffer.
   */
  protected abstract void transform(byte[] in, int offset);
}
