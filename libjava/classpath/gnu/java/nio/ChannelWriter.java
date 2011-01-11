/* ChannelWriter.java -- nio / writer bridge
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.nio;

import java.io.IOException;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

/**
 * A Writer implementation that works by wrapping an NIO channel.
 */
public class ChannelWriter
    extends Writer
{
  private static final int DEFAULT_BUFFER_CAP = 8192;

  /**
   * The output channel.
   */
  private WritableByteChannel byteChannel;

  /**
   * The encoder to use.
   */
  private CharsetEncoder enc;

  /**
   * The byte buffer.  Translated characters are stored here on their way out.
   */
  private ByteBuffer byteBuffer;

  /**
   * The character buffer.  Characters are stored here on their way into
   * the encoder.
   */
  private CharBuffer charBuffer;

  private void writeBuffer() throws IOException
  {
    byteBuffer.flip();
    byteChannel.write(byteBuffer);
  }

  /**
   * Create a new instance, given the output byte channel, the encoder
   * to use, and the minimum buffer capacity.
   */
  public ChannelWriter(WritableByteChannel ch, CharsetEncoder enc,
                       int minBufferCap)
  {
    this.byteChannel = ch;
    this.enc = enc;
    if (minBufferCap == -1)
      minBufferCap = DEFAULT_BUFFER_CAP;
    this.byteBuffer
      = ByteBuffer.allocate((int) (minBufferCap * enc.maxBytesPerChar()));
    this.charBuffer = CharBuffer.allocate(minBufferCap);
    this.charBuffer.clear();
  }

  /* (non-Javadoc)
   * @see java.io.Writer#flush()
   */
  public void flush() throws IOException
  {
    // Presumably if we have characters in our buffer, it is
    // due to an underflow.  So we don't bother trying to flush
    // that here.
  }

  /* (non-Javadoc)
   * @see java.io.Writer#close()
   */
  public void close() throws IOException
  {
    synchronized (lock)
      {
        if (enc == null)
          throw new IOException("writer already closed");

        byteBuffer.clear();
        charBuffer.flip();
        CoderResult res = enc.encode(charBuffer, byteBuffer, true);
        if (res.isError() || res.isMalformed() || res.isUnmappable())
          res.throwException();
        writeBuffer();

        byteBuffer.clear();
        res = enc.flush(byteBuffer);
        if (res.isError() || res.isMalformed() || res.isUnmappable())
          res.throwException();
        writeBuffer();
        enc = null;
      }
  }

  /* (non-Javadoc)
   * @see java.io.Writer#write(char[], int, int)
   */
  public void write(char[] buf, int offset, int len) throws IOException
  {
    synchronized (lock)
      {
        if (enc == null)
          throw new IOException("writer already closed");
        int lastLen = -1;
        while (len > 0)
          {
            // Copy data into our character buffer.
            int allowed = Math.min(charBuffer.remaining(), len);
            charBuffer.put(buf, offset, allowed);
            // Update for the next pass through the loop.
            offset += allowed;
            len -= allowed;
            charBuffer.flip();
            // If we didn't make any progress, we want to clean up
            // and save our state for the next write().
            if (len == lastLen)
              {
                if (len <= charBuffer.remaining())
                  {
                    charBuffer.put(buf, offset, len);
                    charBuffer.flip();
                  }
                else
                  {
                    CharBuffer ncb = CharBuffer.allocate(charBuffer.length()
                                                         + len);
                    ncb.put(charBuffer);
                    ncb.put(buf, offset, len);
                    charBuffer = ncb;
                  }
                break;
              }
            lastLen = len;

            // Convert.
            byteBuffer.clear();
            CoderResult res = enc.encode(charBuffer, byteBuffer, false);
            // Compact here, as we want to leave the buffer in the
            // right state for any future put()s.
            charBuffer.compact();
            if (res.isError() || res.isMalformed() || res.isUnmappable())
              res.throwException();
            // Write the byte buffer to the output channel.
            writeBuffer();
          }
      }
  }
}
