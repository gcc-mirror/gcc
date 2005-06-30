/* Inflater.java - Decompress a data stream
   Copyright (C) 1999, 2000, 2001, 2003  Free Software Foundation, Inc.

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

package java.util.zip;

import gnu.gcj.RawData;

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

/**
 * Inflater is used to decompress data that has been compressed according 
 * to the "deflate" standard described in rfc1950.
 *
 * The usage is as following.  First you have to set some input with
 * <code>setInput()</code>, then inflate() it.  If inflate doesn't
 * inflate any bytes there may be three reasons:
 * <ul>
 * <li>needsInput() returns true because the input buffer is empty.
 * You have to provide more input with <code>setInput()</code>.  
 * NOTE: needsInput() also returns true when, the stream is finished.
 * </li>
 * <li>needsDictionary() returns true, you have to provide a preset 
 *     dictionary with <code>setDictionary()</code>.</li>
 * <li>finished() returns true, the inflater has finished.</li>
 * </ul>
 * Once the first output byte is produced, a dictionary will not be
 * needed at a later stage.
 *
 * @author John Leuner, Jochen Hoenicke
 * @author Tom Tromey
 * @date May 17, 1999
 * @since JDK 1.1
 */
public class Inflater
{
  // The zlib stream.
  private RawData zstream;

  // True if finished.
  private boolean is_finished;

  // True if dictionary needed.
  private boolean dict_needed;

  /**
   * Creates a new inflater.
   */
  public Inflater ()
  {
    this (false);
  }

  /**
   * Creates a new inflater.
   * @param nowrap true if no header and checksum field appears in the
   * stream.  This is used for GZIPed input.  For compatibility with
   * Sun JDK you should provide one byte of input more than needed in
   * this case.
   */
  public Inflater (boolean noHeader)
  {
    init (noHeader);
  }

  /**
   * Finalizes this object.
   */
  protected void finalize ()
  {
    end ();
  }

  /**
   * Frees all objects allocated by the inflater.  There's no reason
   * to call this, since you can just rely on garbage collection (even
   * for the Sun implementation).  Exists only for compatibility
   * with Sun's JDK, where the compressor allocates native memory.
   * If you call any method (even reset) afterwards the behaviour is
   * <i>undefined</i>.  
   * @deprecated Just clear all references to inflater instead.
   */
  public native void end ();

  /**
   * Returns true, if the inflater has finished.  This means, that no
   * input is needed and no output can be produced.
   */
  public synchronized boolean finished ()
  {
    return is_finished;
  }

  /**
   * Gets the adler checksum.  This is either the checksum of all
   * uncompressed bytes returned by inflate(), or if needsDictionary()
   * returns true (and thus no output was yet produced) this is the
   * adler checksum of the expected dictionary.
   * @returns the adler checksum.
   */
  public native int getAdler ();
  
  /**
   * Gets the number of unprocessed input.  Useful, if the end of the
   * stream is reached and you want to further process the bytes after
   * the deflate stream.  
   * @return the number of bytes of the input which were not processed.
   */
  public native int getRemaining ();
  
  /**
   * Gets the total number of processed compressed input bytes.
   * @return the total number of bytes of processed input bytes.
   */
  public native int getTotalIn ();

  /**
   * Gets the total number of output bytes returned by inflate().
   * @return the total number of output bytes.
   */
  public native int getTotalOut ();

  /**
   * Inflates the compressed stream to the output buffer.  If this
   * returns 0, you should check, whether needsDictionary(),
   * needsInput() or finished() returns true, to determine why no 
   * further output is produced.
   * @param buffer the output buffer.
   * @return the number of bytes written to the buffer, 0 if no further
   * output can be produced.  
   * @exception DataFormatException if deflated stream is invalid.
   * @exception IllegalArgumentException if buf has length 0.
   */
  public int inflate (byte[] buf) throws DataFormatException
  {
    return inflate (buf, 0, buf.length);
  }

  /**
   * Inflates the compressed stream to the output buffer.  If this
   * returns 0, you should check, whether needsDictionary(),
   * needsInput() or finished() returns true, to determine why no 
   * further output is produced.
   * @param buffer the output buffer.
   * @param off the offset into buffer where the output should start.
   * @param len the maximum length of the output.
   * @return the number of bytes written to the buffer, 0 if no further
   * output can be produced.  
   * @exception DataFormatException if deflated stream is invalid.
   * @exception IndexOutOfBoundsException if the off and/or len are wrong.
   */
  public native int inflate (byte[] buf, int off, int len)
    throws DataFormatException;

  private native void init (boolean noHeader);

  /**
   * Returns true, if a preset dictionary is needed to inflate the input.
   */
  public synchronized boolean needsDictionary ()
  {
    return dict_needed;
  }

  /**
   * Returns true, if the input buffer is empty.
   * You should then call setInput(). <br>
   *
   * <em>NOTE</em>: This method also returns true when the stream is finished.
   */
  public synchronized boolean needsInput ()
  {
    return getRemaining () == 0;
  }

  /**
   * Resets the inflater so that a new stream can be decompressed.  All
   * pending input and output will be discarded.
   */
  public native void reset ();

  /**
   * Sets the preset dictionary.  This should only be called, if
   * needsDictionary() returns true and it should set the same
   * dictionary, that was used for deflating.  The getAdler()
   * function returns the checksum of the dictionary needed.
   * @param buffer the dictionary.
   * @exception IllegalStateException if no dictionary is needed.
   * @exception IllegalArgumentException if the dictionary checksum is
   * wrong.  
   */
  public void setDictionary (byte[] buf)
  {
    setDictionary (buf, 0, buf.length);
  }

  /**
   * Sets the preset dictionary.  This should only be called, if
   * needsDictionary() returns true and it should set the same
   * dictionary, that was used for deflating.  The getAdler()
   * function returns the checksum of the dictionary needed.
   * @param buffer the dictionary.
   * @param off the offset into buffer where the dictionary starts.
   * @param len the length of the dictionary.
   * @exception IllegalStateException if no dictionary is needed.
   * @exception IllegalArgumentException if the dictionary checksum is
   * wrong.  
   * @exception IndexOutOfBoundsException if the off and/or len are wrong.
   */
  public native void setDictionary (byte[] buf, int off, int len);

  /**
   * Sets the input.  This should only be called, if needsInput()
   * returns true.
   * @param buffer the input.
   * @exception IllegalStateException if no input is needed.
   */
  public void setInput (byte[] buf) 
  {
    setInput (buf, 0, buf.length);
  }

  /**
   * Sets the input.  This should only be called, if needsInput()
   * returns true.
   * @param buffer the input.
   * @param off the offset into buffer where the input starts.
   * @param len the length of the input.  
   * @exception IllegalStateException if no input is needed.
   * @exception IndexOutOfBoundsException if the off and/or len are wrong.
   */
  public native void setInput (byte[] buf, int off, int len);
}
