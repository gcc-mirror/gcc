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

/**
 * <p>An abstract class to facilitate implementing padding algorithms.</p>
 */
public abstract class BasePad implements IPad
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /** The canonical name prefix of the padding algorithm. */
  protected String name;

  /** The block size, in bytes, for this instance. */
  protected int blockSize;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /** Trivial constructor for use by concrete subclasses. */
  protected BasePad(final String name)
  {
    super();

    this.name = name;
    blockSize = -1;
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------

  // IPad interface implementation -------------------------------------------

  public String name()
  {
    final StringBuffer sb = new StringBuffer(name);
    if (blockSize != -1)
      {
        sb.append('-').append(String.valueOf(8 * blockSize));
      }
    return sb.toString();
  }

  public void init(final int bs) throws IllegalStateException
  {
    if (blockSize != -1)
      {
        throw new IllegalStateException();
      }
    blockSize = bs;
    setup();
  }

  public void reset()
  {
    blockSize = -1;
  }

  public boolean selfTest()
  {
    byte[] padBytes;
    final int offset = 5;
    final int limit = 1024;
    final byte[] in = new byte[limit];
    for (int bs = 2; bs < 256; bs++)
      {
        this.init(bs);
        for (int i = 0; i < limit - offset - blockSize; i++)
          {
            padBytes = pad(in, offset, i);
            if (((i + padBytes.length) % blockSize) != 0)
              {
                new RuntimeException(name()).printStackTrace(System.err);
                return false;
              }

            System.arraycopy(padBytes, 0, in, offset + i, padBytes.length);
            try
              {
                if (padBytes.length != unpad(in, offset, i + padBytes.length))
                  {
                    new RuntimeException(name()).printStackTrace(System.err);
                    return false;
                  }
              }
            catch (WrongPaddingException x)
              {
                x.printStackTrace(System.err);
                return false;
              }
          }
        this.reset();
      }

    return true;
  }

  // abstract methods to implement by subclasses -----------------------------

  /**
   * <p>If any additional checks or resource setup must be done by the
   * subclass, then this is the hook for it. This method will be called before
   * the {@link #init(int)} method returns.</p>
   */
  public abstract void setup();

  public abstract byte[] pad(byte[] in, int off, int len);

  public abstract int unpad(byte[] in, int off, int len)
      throws WrongPaddingException;
}