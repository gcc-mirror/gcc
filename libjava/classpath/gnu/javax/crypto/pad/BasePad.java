/* BasePad.java --
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.pad;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Configuration;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * An abstract class to facilitate implementing padding algorithms.
 */
public abstract class BasePad
    implements IPad
{
  private static final Logger log = Logger.getLogger(BasePad.class.getName());
  /** The canonical name prefix of the padding algorithm. */
  protected String name;
  /** The block size, in bytes, for this instance. */
  protected int blockSize;

  /** Trivial constructor for use by concrete subclasses. */
  protected BasePad(final String name)
  {
    super();

    this.name = name;
    blockSize = -1;
  }

  public String name()
  {
    final CPStringBuilder sb = new CPStringBuilder(name);
    if (blockSize != -1)
      sb.append('-').append(String.valueOf(8 * blockSize));
    return sb.toString();
  }

  public void init(final int bs) throws IllegalStateException
  {
    if (blockSize != -1)
      throw new IllegalStateException();
    blockSize = bs;
    setup();
  }

  /**
   * Initialises the algorithm with designated attributes. Names, valid and/or
   * recognisable by all concrete implementations are described in {@link IPad}
   * class documentation. Other algorithm-specific attributes MUST be documented
   * in the implementation class of that padding algorithm.
   * <p>
   * For compatibility reasons, this method is not declared <i>abstract</i>.
   * Furthermore, and unless overridden, the default implementation will throw
   * an {@link UnsupportedOperationException}. Concrete padding algorithms MUST
   * override this method if they wish to offer an initialisation method that
   * allows for other than the padding block size parameter to be specified.
   *
   * @param attributes a set of name-value pairs that describes the desired
   *          future behaviour of this instance.
   * @exception IllegalStateException if the instance is already initialised.
   * @exception IllegalArgumentException if the block size value is invalid.
   */
  public void init(Map attributes) throws IllegalStateException
  {
    throw new UnsupportedOperationException();
  }

  public void reset()
  {
    blockSize = -1;
  }

  /**
   * A default implementation of a correctness test that exercises the padder
   * implementation, using block sizes varying from 2 to 256 bytes.
   *
   * @return <code>true</code> if the concrete implementation correctly unpads
   *         what it pads for all tested block sizes. Returns <code>false</code>
   *         if the test fails for any block size.
   */
  public boolean selfTest()
  {
    final byte[] in = new byte[1024];
    for (int bs = 2; bs < 256; bs++)
      if (! test1BlockSize(bs, in))
        return false;
    return true;
  }

  /**
   * The basic symmetric test for a padder given a specific block size.
   * <p>
   * The code ensures that the implementation is capable of unpadding what it
   * pads.
   *
   * @param size the block size to test.
   * @param buffer a work buffer. It is exposed as an argument for this method
   *          to reduce un-necessary object allocations.
   * @return <code>true</code> if the test passes; <code>false</code>
   *         otherwise.
   */
  protected boolean test1BlockSize(int size, byte[] buffer)
  {
    byte[] padBytes;
    final int offset = 5;
    final int limit = buffer.length;
    this.init(size);
    for (int i = 0; i < limit - offset - blockSize; i++)
      {
        padBytes = pad(buffer, offset, i);
        if (((i + padBytes.length) % blockSize) != 0)
          {
            if (Configuration.DEBUG)
              log.log(Level.SEVERE,
                      "Length of padded text MUST be a multiple of "
                      + blockSize, new RuntimeException(name()));
            return false;
          }
        System.arraycopy(padBytes, 0, buffer, offset + i, padBytes.length);
        try
          {
            if (padBytes.length != unpad(buffer, offset, i + padBytes.length))
              {
                if (Configuration.DEBUG)
                  log.log(Level.SEVERE,
                          "IPad [" + name() + "] failed symmetric operation",
                          new RuntimeException(name()));
                return false;
              }
          }
        catch (WrongPaddingException x)
          {
            if (Configuration.DEBUG)
              log.throwing(this.getClass().getName(), "test1BlockSize", x);
            return false;
          }
      }
    this.reset();
    return true;
  }

  /**
   * If any additional checks or resource setup must be done by the subclass,
   * then this is the hook for it. This method will be called before the
   * {@link #init(int)} method returns.
   */
  public abstract void setup();

  public abstract byte[] pad(byte[] in, int off, int len);

  public abstract int unpad(byte[] in, int off, int len)
      throws WrongPaddingException;
}
