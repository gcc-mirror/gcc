/* ChannelReader.java -- 
 Copyright (C) 2005 Free Software Foundation, Inc.

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
 Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 02111-1307 USA.

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
import java.io.Reader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;

/**
 * A Reader implementation that works using a ReadableByteChannel and a
 * CharsetDecoder.
 * 
 * <p>
 * This is a bridge between NIO <-> IO character decoding.
 * </p>
 * 
 * @author Robert Schuster
 */
public class ChannelReader extends Reader
{

  private static final int DEFAULT_BUFFER_CAP = 8192;

  private ReadableByteChannel channel;

  private CharsetDecoder decoder;

  private ByteBuffer byteBuffer;

  private CharBuffer charBuffer;

  public ChannelReader(ReadableByteChannel channel, CharsetDecoder decoder,
                       int minBufferCap)
  {
    this.channel = channel;
    this.decoder = decoder;

    // JDK reports errors, so we do the same.
    decoder.onMalformedInput(CodingErrorAction.REPORT);
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT);
    decoder.reset();

    int size = (minBufferCap == -1) ? DEFAULT_BUFFER_CAP : minBufferCap;

    // Allocates the buffers and prepares them for reading, because that is the
    // first operation being done on them.
    byteBuffer = ByteBuffer.allocate(size);
    byteBuffer.flip();
    charBuffer = CharBuffer.allocate((int) (size * decoder.averageCharsPerByte()));
  }

  public int read(char[] buf, int offset, int count) throws IOException
  {
    // I declared channel being null meaning that the reader is closed.
    if (!channel.isOpen())
      throw new IOException("Reader was already closed.");

    // I declared decoder being null meaning that there is no more data to read
    // and convert.
    if (decoder == null)
      return -1;

    // Stores the amount of character being read. It -1 so that if no conversion
    // occured the caller will see this as an 'end of file'.
    int sum = -1;

    // Copies any characters which may be left from the last invocation into the
    // destination array.
    if (charBuffer.remaining() > 0)
      {
        sum = Math.min(count, charBuffer.remaining());
        charBuffer.get(buf, offset, sum);

        // Updates the control variables according to the latest copy operation.
        offset += sum;
        count -= sum;
      }

    // Copies the character which have not been put in the destination array to
    // the beginning. If data is actually copied count will be 0. If no data is
    // copied count is >0 and we can now convert some more characters.
    charBuffer.compact();

    int converted = 0;
    boolean last = false;

    while (count != 0)
      {
        // Tries to convert some bytes (Which will intentionally fail in the
        // first place because we have not read any bytes yet.)
        CoderResult result = decoder.decode(byteBuffer, charBuffer, last);
        if (result.isMalformed() || result.isUnmappable())
          {
            // JDK throws exception when bytes are malformed for sure.
            // FIXME: Unsure what happens when a character is simply
            // unmappable.
            result.throwException();
          }

        // Marks that we should end this loop regardless whether the caller
        // wants more chars or not, when this was the last conversion.
        if (last)
          {
            decoder = null;
          }
        else if (result.isUnderflow())
          {
            // We need more bytes to do the conversion.

            // Copies the not yet converted bytes to the beginning making it
            // being able to receive more bytes.
            byteBuffer.compact();

            // Reads in another bunch of bytes for being converted.
            if (channel.read(byteBuffer) == -1)
              {
                // If there is no more data available in the channel we mark
                // that state for the final character conversion run which is
                // done in the next loop iteration.
                last = true;
              }

            // Prepares the byteBuffer for the next character conversion run.
            byteBuffer.flip();
          }

        // Prepares the charBuffer for being drained.
        charBuffer.flip();

        converted = Math.min(count, charBuffer.remaining());
        charBuffer.get(buf, offset, converted);

        // Copies characters which have not yet being copied into the char-Array
        // to the beginning making it possible to read them later (If data is
        // really copied here, then the caller has received enough characters so
        // far.).
        charBuffer.compact();

        // Updates the control variables according to the latest copy operation.
        offset += converted;
        count -= converted;

        // Updates the amount of transferred characters.
        sum += converted;

        if (decoder == null)
          {
            break;
          }

        // Now that more characters have been transfered we let the loop decide
        // what to do next.
      }

    // Makes the charBuffer ready for reading on the next invocation.
    charBuffer.flip();

    return sum;
  }

  public void close() throws IOException
  {
    channel.close();

    // Makes sure all intermediate data is released by the decoder.
    if (decoder != null)
      decoder.reset();
  }

}
