/* MessageDigestSpi.java --- The message digest service provider interface.
   Copyright (C) 1999, 2005  Free Software Foundation, Inc.

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

package java.security;

import java.nio.ByteBuffer;

/**
   This is the Service Provider Interface (SPI) for MessageDigest
   class in java.security. It provides the back end functionality
   for the MessageDigest class so that it can compute message
   hashes. The default hashes are SHA-1 and MD5. A message hash
   takes data of arbitrary length and produces a unique number
   representing it.

   Cryptography service providers who want to implement their
   own message digest hashes need only to subclass this class.

   The implementation of a Cloneable interface is left to up to
   the programmer of a subclass.

   @version 0.0

   @author Mark Benvenuto (ivymccough@worldnet.att.net)
 */
public abstract class MessageDigestSpi
{
  /**
     Default constructor of the MessageDigestSpi class
   */
  public MessageDigestSpi()
  {
  }

  /**
     Returns the length of the digest.  It may be overridden by the
     provider to return the length of the digest.  Default is to
     return 0.  It is concrete for backwards compatibility with JDK1.1
     message digest classes.

     @return Length of Digest in Bytes

     @since 1.2
   */
  protected int engineGetDigestLength()
  {
    return 0;
  }

  /**
     Updates the digest with the specified byte.

     @param input the byte to update digest with
   */
  protected abstract void engineUpdate(byte input);


  /**
     Updates the digest with the specified bytes starting with the
     offset and proceeding for the specified length.

     @param input the byte array to update digest with
     @param offset the offset of the byte to start with
     @param len the number of the bytes to update with
   */
  protected abstract void engineUpdate(byte[]input, int offset, int len);

  /**
   * Updates this digest with the remaining bytes of a byte buffer.
   * 
   * @param input The input buffer.
   * @since 1.5
   */
  protected void engineUpdate (ByteBuffer input)
  {
    byte[] buf = new byte[1024];
    while (input.hasRemaining())
      {
        int n = Math.min(input.remaining(), buf.length);
        input.get (buf, 0, n);
        engineUpdate (buf, 0, n);
      }
  }
  
  /**
     Computes the final digest of the stored bytes and returns
     them. It performs any necessary padding. The message digest
     should reset sensitive data after performing the digest.

     @return An array of bytes containing the digest
   */
  protected abstract byte[] engineDigest();

  /**
     Computes the final digest of the stored bytes and returns
     them. It performs any necessary padding. The message digest
     should reset sensitive data after performing the digest.  This
     method is left concrete for backwards compatibility with JDK1.1
     message digest classes.

     @param buf An array of bytes to store the digest
     @param offset An offset to start storing the digest at
     @param len The length of the buffer
     @return Returns the length of the buffer

     @since 1.2
   */
  protected int engineDigest(byte[]buf, int offset, int len)
    throws DigestException
  {
    if (engineGetDigestLength() > len)
      throw new DigestException("Buffer is too small.");

    byte[] tmp = engineDigest();
    if (tmp.length > len)
      throw new DigestException("Buffer is too small");

    System.arraycopy(tmp, 0, buf, offset, tmp.length);
    return tmp.length;
  }

  /**
     Resets the digest engine. Reinitializes internal variables
     and clears sensitive data.
   */
  protected abstract void engineReset();

  /**
     Returns a clone of this class.

     If cloning is not supported, then by default the class throws a
     CloneNotSupportedException.  The MessageDigestSpi provider
     implementation has to overload this class in order to be
     cloneable.
   */
  public Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }
}
