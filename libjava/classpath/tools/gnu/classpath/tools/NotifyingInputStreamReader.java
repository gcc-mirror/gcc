/* gnu.classpath.tools.NotifyingInputStreamReader
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.classpath.tools;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;

import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 *  Similar to {@link java.io.InputStreamReader}, but can give
 *  notification when malformed input is encountered.
 *
 *  <p> Users of this class can register interest in the event of
 *  malformed input by calling {@link
 *  #addMalformedInputListener}. Each time a run of malformed input
 *  data is encountered, all listener objects are notified. For
 *  instance, this allows the calling code to inform the user about
 *  problems in the input stream. </p>
 *
 *  <p> <strong>Background:</strong> The default
 *  <code>InputStreamReader</code> implementation will ignore
 *  malformed input, silently replacing it with the default
 *  replacement string (usually the question mark). Alternatively, you
 *  can configure a <code>CharsetDecoder</code> for the default
 *  <char>InputStreamReader</code> implementation, instructing it to
 *  ignore malformed input without replacing it; to replace it with a
 *  different string; or to throw an exception when malformed input is
 *  encountered. However, you cannot configure an
 *  <code>InputStreamReader</code> to gracefully handle malformed data
 *  but notify the client code about such
 *  problems. <code>NotifyingInputStreamReader</code> fills this
 *  gap. </p>
 *
 *  @author Julian Scheid
 */
public class NotifyingInputStreamReader
   extends Reader
{
   /**
    *  The default size (in bytes) for byteBuf, i.e. the size of data
    *  chunks read from the underlying input stream.
    */
   private static final int DEFAULT_INPUT_BUFFER_SIZE = 64;

   /**
    *  The default size (in chars) for charBuf. This should be large
    *  enough to hold a decoded chunk of input data for the Charset
    *  with the minimum number of bytes per character. Since the
    *  minimum number of bytes per character for usual Charsets is
    *  one, this number should be identical to
    *  DEFAULT_INPUT_BUFFER_SIZE.
    */
   private static final int DEFAULT_OUTPUT_BUFFER_SIZE = 64;

   /**
    *  The underlying InputStream.
    */
   private InputStream in;

   /**
    *  The CharsetDecoder used to decode the underlying stream.
    */
   private CharsetDecoder decoder;

   /**
    *  Holds bytes already read from the underlying stream, but not
    *  yet decoded.
    */
   private ByteBuffer byteBuffer;

   /**
    *  Holds characters already decoded, but not yet fetched via
    *  read().
    */
   private CharBuffer charBuffer;

   /**
    *  This is the primitive byte array wrapped in byteBuffer for
    *  passing to to InputStream.read().
    */
   private byte[] readBuffer;

   /**
    *  Keeps track of the current line number (1-based).
    */
   private int lineNumber = 1;

   /**
    *  Keeps track of the current column number (0-based).
    */
   private int columnNumber = 0;

   /**
    *  Becomes true as soon as EOF has been reached in the underlying
    *  InputStream. At this point, byteBuffer contains the last chunk
    *  of data from the underlying InputStream.
    */
   private boolean allInputConsumed = false;

   /**
    *  Becomes true as soon as the decoder has been supplied with the
    *  last chunk of data from the InputStream after EOF has been
    *  reached. At this point, the last chunk of data has been drained
    *  from the byteBuffer.
    */
   private boolean decodingFinished = false;

   /**
    *  Becomes true as soon as the decoder has been flushed. At this
    *  point, the last chunk of character data has been written to the
    *  charBuffer.
    */
   private boolean flushed = false;

   /**
    *  Stores all registered MalformedInputListeners.
    */
   private Set listeners = new LinkedHashSet();

   /**
    *  Initializes a new instance for reading from the given
    *  InputStream using the default encoding. The default encoding is
    *  currently determined by looking at the system property
    *  <code>file.encoding</code>. If this property isn't set,
    *  <code>ISO-8859-1</code> is used as a fallback.
    *
    *  <p>This method should use {@link Charset.defaultCharset()}
    *  instead, but this isn't implemented in Classpath at the
    *  moment.</p>
    *
    *  @param in the <code>InputStream</code> to read from.
    */
   public NotifyingInputStreamReader(InputStream in)
   {
      this(in, System.getProperty("file.encoding", "ISO-8859-1"));
   }

   /**
    *  Initializes a new instance for reading from the given
    *  InputStream using the specified charset.
    *
    *  @param in the <code>InputStream</code> to read from.
    *  @param charsetName the canonical name or an alias of a
    *  <code>Charset</code>.
    *
    *  @throws IllegalCharsetNameException if there is no
    *  <code>Charset</code> with the given canonical name or alias.
    *
    *  @throws UnsupportedCharsetException if there is no support for
    *  the specified <code>Charset</code> in the runtime environment.
    */
   public NotifyingInputStreamReader(InputStream in, String charsetName)
      throws IllegalCharsetNameException, UnsupportedCharsetException
   {
      this(in, Charset.forName(charsetName));
   }

   /**
    *  Initializes a new instance for reading from the given
    *  InputStream using the specified charset.
    *
    *  @param in the <code>InputStream</code> to read from.
    *  @param charset the <code>Charset</code> to use for decoding
    *  characters.
    */
   public NotifyingInputStreamReader(InputStream in, Charset charset)
   {
      this(in, charset.newDecoder());
   }

   /**
    *  Initializes a new instance for reading from the given
    *  InputStream using the specified charset decoder.
    *
    *  <strong>Note:</strong> the
    *  <code>NotifyingInputStreamReader</code> will not exhibit the
    *  advertised behaviour if you changed the action to take on
    *  malformed input in the specified
    *  <code>CharsetDecoder</code>. In other words, you should not
    *  call {@link CharsetDecoder.onMalformedInput(CodingErrorAction)}
    *  on the specified decoder before or while this reader is being
    *  used unless you know what you're doing.
    *
    *  @param in the <code>InputStream</code> to read from.
    *  @param charset the <code>CharsetDecoder</code> to use for
    *  decoding characters.
    */
   public NotifyingInputStreamReader(InputStream in, CharsetDecoder decoder)
   {
      this.in = in;
      this.decoder = decoder;
      this.charBuffer = CharBuffer.wrap(new char[DEFAULT_INPUT_BUFFER_SIZE]);
      this.charBuffer.position(charBuffer.limit());
      this.readBuffer = new byte[DEFAULT_OUTPUT_BUFFER_SIZE];
      this.byteBuffer = ByteBuffer.wrap(readBuffer);
      this.byteBuffer.position(byteBuffer.limit());
   }

   public void close()
      throws IOException
   {
      in.close();
   }

   /**
    *  Fill charBuffer with new character data. This method returns if
    *  either the charBuffer has been filled completely with decoded
    *  character data, or if EOF is reached in the underlying
    *  InputStream. When this method returns, charBuffer is flipped
    *  and ready to be read from.
    */
   private void fillCharBuf()
      throws IOException
   {
      charBuffer.clear();
   outer:
      while (!flushed) {
         CoderResult coderResult;
         int charBufferPositionBefore = charBuffer.position();
         if (!decodingFinished) {
            coderResult = decoder.decode(byteBuffer, charBuffer, allInputConsumed);
            if (allInputConsumed) {
               decodingFinished = true;
            }
         }
         else {
            coderResult = decoder.flush(charBuffer);
            flushed = coderResult.isUnderflow();
         }

         int charBufferPositionAfter = charBuffer.position();
         for (int i=charBufferPositionBefore; i<charBufferPositionAfter; ++i) {
            if (10 == charBuffer.get(i)) {
               ++ lineNumber;
               columnNumber = 0;
            }
            else {
               ++ columnNumber;
            }
         }
         if (coderResult.isOverflow()) {
            break;
         }
         else if (coderResult.isUnderflow()) {
            if (!allInputConsumed) {
               int nRemainingBytes = 0;
               if (byteBuffer.position() > 0) {
                  nRemainingBytes = Math.max(0, byteBuffer.limit() - byteBuffer.position());
               }
               if (nRemainingBytes > 0) {
                  byteBuffer.get(readBuffer, 0, nRemainingBytes);
               }
               byteBuffer.rewind();
               int nread = in.read(readBuffer, nRemainingBytes,
                                   readBuffer.length - nRemainingBytes);
               if (nread < 0) {
                  allInputConsumed = true;
               }
               byteBuffer.limit(nRemainingBytes + Math.max(0, nread));
            }
            else {
               break;
            }
         }
         else if (coderResult.isMalformed()) {
            fireMalformedInputEncountered(coderResult.length());
            String replacement = decoder.replacement();
            for (int i=0; i<coderResult.length(); ++i) {
               if (charBuffer.remaining() > replacement.length()) {
                  charBuffer.put(replacement);
                  byteBuffer.position(byteBuffer.position() + 1);
                  columnNumber ++;
               }
               else {
                  break outer;
               }
            }
         }
         else if (coderResult.isUnmappable()) {
            // This should not happen, since unmappable input bytes
            // trigger a "malformed" event instead.
            coderResult.throwException();
         }
         else {
            // This should only happen if run in a future environment
            // where additional events apart from underflow, overflow,
            // malformed and unmappable can be generated.
            coderResult.throwException();
         }
      }
      charBuffer.flip();
   }

   /**
    *  Fire a MalformedInputEvent, notifying all registered listeners.
    */
   private void fireMalformedInputEncountered(int length)
   {
      MalformedInputEvent event
         = new MalformedInputEvent(this, lineNumber, columnNumber, length);
      Iterator it = listeners.iterator();
      while (it.hasNext()) {
         MalformedInputListener listener
            = (MalformedInputListener)it.next();
         listener.malformedInputEncountered(event);
      }
   }

   public int read(char[] cbuf, int offset, int length)
      throws IOException
   {
      if (flushed) {
         return -1;
      }
      else {
         int nread = 0;
         while (nread < length && !flushed) {
            while (charBuffer.hasRemaining() && nread < length) {
               int copyLen = Math.min(length - nread,
                                      charBuffer.remaining());
               charBuffer.get(cbuf, offset + nread, copyLen);
               nread += copyLen;
            }
            if (nread < length) {
               fillCharBuf();
            }
         }
         return nread;
      }
   }

   public int read()
      throws IOException
   {
      while (!flushed) {
         if (charBuffer.hasRemaining()) {
            return charBuffer.get();
         }
         else {
            fillCharBuf();
         }
      }
      return -1;
   }

   /**
    *  Returns whether this reader is ready. The reader is ready if
    *  there is data in the internal buffer, or if additional data can
    *  be read from the underlying InputStream.
    */
   public boolean ready()
   {
      return charBuffer.hasRemaining() || !flushed;
   }

   /**
    *  Register a <code>MalformedInputListener</code> which should be
    *  notified when malformed input is encountered.
    */
   public void addMalformedInputListener(MalformedInputListener listener)
   {
      this.listeners.add(listener);
   }

   /**
    *  Unregister a previously registered
    *  <code>MalformedInputListener</code> if it should no longer be
    *  notified when malformed input is encountered.
    */
   public void removeMalformedInputListener(MalformedInputListener listener)
   {
      this.listeners.remove(listener);
   }

}
