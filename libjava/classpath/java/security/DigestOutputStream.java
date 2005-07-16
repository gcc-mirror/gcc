/* DigestOutputStream.java --- An output stream tied to a message digest
   Copyright (C) 1999, 2004, 2005  Free Software Foundation, Inc.

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

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * DigestOutputStream is a class that ties an OutputStream with a
 * MessageDigest. The Message Digest is used by the class to update it
 * self as bytes are written to the OutputStream.
 *
 * The updating to the digest depends on the on flag which is set to
 * true by default that tells the class to update the data in the
 * message digest.
 *
 * @version 0.0
 * @author Mark Benvenuto (ivymccough@worldnet.att.net)
 */
public class DigestOutputStream extends FilterOutputStream
{
  /**
   * The message digest for the DigestOutputStream
   */
  protected MessageDigest digest;

  //Manages the on flag
  private boolean state = true;

  /**
   * Constructs a new DigestOutputStream.  It associates a
   * MessageDigest with the stream to compute the stream as data is
   * written.
   *
   * @param stream An OutputStream to associate this stream with
   * @param digest A MessageDigest to hash the stream with
   */
  public DigestOutputStream(OutputStream stream, MessageDigest digest)
  {
    super(stream);
    this.digest = digest;
  }

  /**
   * Returns the MessageDigest associated with this DigestOutputStream
   *
   * @return The MessageDigest used to hash this stream
   */
  public MessageDigest getMessageDigest()
  {
    return digest;
  }

  /**
   * Sets the current MessageDigest to current parameter
   *
   * @param digest A MessageDigest to associate with this stream
   */
  public void setMessageDigest(MessageDigest digest)
  {
    this.digest = digest;
  }


  /**
   * Updates the hash if the on flag is true and then writes a byte to
   * the underlying output stream.
   *
   * @param b A byte to write to the output stream
   *
   * @exception IOException if the underlying output stream 
   * cannot write the byte, this is thrown.
   */
  public void write(int b) throws IOException
  {
    if (state)
      digest.update((byte) b);

    out.write(b);
  }

  /**
   * Updates the hash if the on flag is true and then writes the bytes
   * to the underlying output stream.
   *
   * @param b Bytes to write to the output stream
   * @param off Offset to start to start at in array
   * @param len Length of data to write
   *
   * @exception IOException if the underlying output stream 
   * cannot write the bytes, this is thrown.
   */
  public void write(byte[]b, int off, int len) throws IOException
  {
    if (state)
      digest.update(b, off, len);

    out.write(b, off, len);
  }

  /**
   * Sets the flag specifying if this DigestOutputStream updates the
   * digest in the write() methods. The default is on;
   *
   * @param on True means it digests stream, false means it does not
   */
  public void on(boolean on)
  {
    state = on;
  }

  /**
   * Converts the output stream and underlying message digest to a string.
   *
   * @return A string representing the output stream and message digest.
   */
  public String toString()
  {
    return "[Digest Output Stream] " + digest.toString();
  }
}
