/* SecurityParameters.java -- SSL security parameters.
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


package gnu.javax.net.ssl.provider;

import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;
import gnu.java.security.util.ByteArray;
import gnu.java.security.util.ByteBufferOutputStream;

import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;

import java.util.Arrays;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.Mac;
import javax.crypto.ShortBufferException;

import javax.net.ssl.SSLException;

public class InputSecurityParameters
{
  private static final SystemLogger logger = SystemLogger.SYSTEM;
  private final Cipher cipher;
  private final Mac mac;
  private final Inflater inflater;
  private SessionImpl session;
  private final CipherSuite suite;
  private long sequence;

  public InputSecurityParameters (final Cipher cipher, final Mac mac,
                                  final Inflater inflater,
                                  final SessionImpl session,
                                  final CipherSuite suite)
  {
    this.cipher = cipher;
    this.mac = mac;
    this.inflater = inflater;
    this.session = session;
    this.suite = suite;
    sequence = 0;
  }

  /**
   * Decrypt a record, storing the decrypted fragment into the given array
   * of byte buffers.
   * 
   * @param record The input record.
   * @param output The output buffers.
   * @param offset The offset of the first buffer to use.
   * @param length The number of buffers to use.
   * @return The number of bytes put in the output buffers.
   * @throws DataFormatException If decompression fails.
   * @throws IllegalBlockSizeException If the current cipher is a block cipher,
   *  and the input fragment is not a multiple of the block size.
   * @throws MacException If verifying the MAC fails.
   * @throws SSLException ???
   * @throws ShortBufferException 
   */
  public int decrypt(Record record, ByteBuffer[] output, int offset, int length)
    throws DataFormatException, IllegalBlockSizeException,
           MacException, SSLException, ShortBufferException
  {
    return decrypt(record, output, offset, length, null);
  }
  
  /**
   * Decrypt a record, storing the decrypted fragment into the given growable
   * buffer.
   * 
   * @param record The input record.
   * @param outputStream The output buffer.
   * @return The number of bytes put into the output buffer.
   * @throws DataFormatException
   * @throws IllegalBlockSizeException
   * @throws MacException
   * @throws SSLException
   * @throws ShortBufferException
   */
  public int decrypt(Record record, ByteBufferOutputStream outputStream)
    throws DataFormatException, IllegalBlockSizeException,
           MacException, SSLException, ShortBufferException
  {
    return decrypt(record, null, 0, 0, outputStream);
  }
  
  private int decrypt(Record record, ByteBuffer[] output, int offset, int length,
                      ByteBufferOutputStream outputStream)
    throws DataFormatException, IllegalBlockSizeException,
           MacException, SSLException, ShortBufferException
  {
    boolean badPadding = false;
    ByteBuffer fragment;
    if (cipher != null)
      {
        ByteBuffer input = record.fragment();
        fragment = ByteBuffer.allocate(input.remaining());
        cipher.update(input, fragment);
      }
    else
      fragment = record.fragment();

    if (Debug.DEBUG_DECRYPTION)
      logger.logv(Component.SSL_RECORD_LAYER, "decrypted fragment:\n{0}",
                  Util.hexDump((ByteBuffer) fragment.duplicate().position(0), " >> "));
    
    int fragmentLength = record.length();
    int maclen = 0;
    if (mac != null)
      maclen = mac.getMacLength();
    fragmentLength -= maclen;

    int padlen = 0;
    int padRemoveLen = 0;
    if (!suite.isStreamCipher ())
      {
        padlen = fragment.get(record.length() - 1) & 0xFF;
        padRemoveLen = padlen + 1;
        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER, "padlen:{0}", padlen);

        if (record.version() == ProtocolVersion.SSL_3)
          {
            // In SSLv3, the padding length must not be larger than
            // the cipher's block size.
            if (padlen > cipher.getBlockSize ())
              badPadding = true;
          }
        else if (record.version().compareTo(ProtocolVersion.TLS_1) >= 0)
          {
            // In TLSv1 and later, the padding must be `padlen' copies of the
            // value `padlen'.
            byte[] pad = new byte[padlen];
            ((ByteBuffer) fragment.duplicate().position(record.length() - padlen - 1)).get(pad);
            for (int i = 0; i < pad.length; i++)
              if ((pad[i] & 0xFF) != padlen)
                badPadding = true;
            if (Debug.DEBUG)
              logger.logv(Component.SSL_RECORD_LAYER, "TLSv1.x padding\n{0}",
                          new ByteArray(pad));
          }
        
        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER, "padding bad? {0}",
                      badPadding);
        if (!badPadding)
          fragmentLength = fragmentLength - padRemoveLen;
      }
    
    int ivlen = 0;
    if (session.version.compareTo(ProtocolVersion.TLS_1_1) >= 0
        && !suite.isStreamCipher())
      ivlen = cipher.getBlockSize();

    // Compute and check the MAC.
    if (mac != null)
      {
        mac.update((byte) (sequence >>> 56));
        mac.update((byte) (sequence >>> 48));
        mac.update((byte) (sequence >>> 40));
        mac.update((byte) (sequence >>> 32));
        mac.update((byte) (sequence >>> 24));
        mac.update((byte) (sequence >>> 16));
        mac.update((byte) (sequence >>>  8));
        mac.update((byte)  sequence);
        mac.update((byte) record.getContentType().getValue());
        ProtocolVersion version = record.version();
        if (version != ProtocolVersion.SSL_3)
          {
            mac.update((byte) version.major());
            mac.update((byte) version.minor());
          }
        mac.update((byte) ((fragmentLength - ivlen) >>> 8));
        mac.update((byte)  (fragmentLength - ivlen));
        ByteBuffer content =
          (ByteBuffer) fragment.duplicate().position(ivlen).limit(fragmentLength);
        mac.update(content);
        byte[] mac1 = mac.doFinal ();
        byte[] mac2 = new byte[maclen];
        mac.reset();
        ((ByteBuffer) fragment.duplicate().position(fragmentLength)).get(mac2);
        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER, "mac1:{0} mac2:{1}",
                      Util.toHexString(mac1, ':'), Util.toHexString(mac2, ':'));
        if (!Arrays.equals (mac1, mac2))
          badPadding = true;
      }

    // We always say "bad MAC" and not "bad padding," because saying
    // the latter will leak information to an attacker.
    if (badPadding)
      throw new MacException ();

    // Inflate the compressed bytes.
    int produced = 0;
    if (inflater != null)
      {
        ByteBufferOutputStream out = new ByteBufferOutputStream(fragmentLength);
        byte[] inbuffer = new byte[1024];
        byte[] outbuffer = new byte[1024];
        boolean done = false;
        if (record.version().compareTo(ProtocolVersion.TLS_1_1) >= 0
            && !suite.isStreamCipher())
          fragment.position (cipher.getBlockSize());
        else
          fragment.position(0);
        fragment.limit(fragmentLength);
        
        while (!done)
          {
            int l;
            if (inflater.needsInput())
              {
                l = Math.min(inbuffer.length, fragment.remaining());
                fragment.get(inbuffer, 0, l);
                inflater.setInput(inbuffer);
              }

            l = inflater.inflate(outbuffer);
            out.write(outbuffer, 0, l);
            done = !fragment.hasRemaining() && inflater.finished();
          }
        
        ByteBuffer outbuf = out.buffer();
        if (outputStream != null)
          {
            byte[] buf = new byte[1024];
            while (outbuf.hasRemaining())
              {
                int l = Math.min(outbuf.remaining(), buf.length);
                outbuf.get(buf, 0, l);
                outputStream.write(buf, 0, l);
                produced += l;
              }
          }
        else
          {
            int i = offset;
            while (outbuf.hasRemaining() && i < offset + length)
              {
                int l = Math.min(output[i].remaining(), outbuf.remaining());
                ByteBuffer b = (ByteBuffer)
                  outbuf.duplicate().limit(outbuf.position() + l);
                output[i++].put(b);
                outbuf.position(outbuf.position() + l);
                produced += l;
              }
            if (outbuf.hasRemaining())
              throw new BufferOverflowException();
          }
      }
    else
      {
        ByteBuffer outbuf = (ByteBuffer)
          fragment.duplicate().position(0).limit(record.length() - maclen - padRemoveLen);
        if (record.version().compareTo(ProtocolVersion.TLS_1_1) >= 0
            && !suite.isStreamCipher())
          outbuf.position(cipher.getBlockSize());
        if (outputStream != null)
          {
            byte[] buf = new byte[1024];
            while (outbuf.hasRemaining())
              {
                int l = Math.min(outbuf.remaining(), buf.length);
                outbuf.get(buf, 0, l);
                outputStream.write(buf, 0, l);
                produced += l;
              }
          }
        else
          {
            int i = offset;
            while (outbuf.hasRemaining() && i < offset + length)
              {
                int l = Math.min(output[i].remaining(), outbuf.remaining());
                ByteBuffer b = (ByteBuffer) outbuf.duplicate().limit(outbuf.position() + l);
                output[i++].put(b);
                outbuf.position(outbuf.position() + l);
                produced += l;
              }
            if (outbuf.hasRemaining())
              throw new BufferOverflowException();
          }
      }

    sequence++;
    
    return produced;
  }
  
  CipherSuite cipherSuite ()
  {
    return suite;
  }
}
