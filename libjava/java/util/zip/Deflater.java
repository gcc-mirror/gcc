/* Deflater.java - Compress a data stream
   Copyright (C) 1999, 2000, 2001, 2004, 2006 Free Software Foundation, Inc.

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

/**
 * This is the Deflater class.  The deflater class compresses input
 * with the deflate algorithm described in RFC 1951.  It has several
 * compression levels and three different strategies described below.
 * 
 * This class is <i>not</i> thread safe.  This is inherent in the API, due
 * to the split of deflate and setInput.
 * 
 * @author Jochen Hoenicke
 * @author Tom Tromey
 */
public class Deflater
{
  /**
   * The best and slowest compression level.  This tries to find very
   * long and distant string repetitions.  
   */
  public static final int BEST_COMPRESSION = 9;
  /**
   * The worst but fastest compression level.  
   */
  public static final int BEST_SPEED = 1;
  /**
   * The default compression level.
   */
  public static final int DEFAULT_COMPRESSION = -1;
  /**
   * This level won't compress at all but output uncompressed blocks.
   */
  public static final int NO_COMPRESSION = 0;

  /**
   * The default strategy.
   */
  public static final int DEFAULT_STRATEGY = 0;
  /**
   * This strategy will only allow longer string repetitions.  It is
   * useful for random data with a small character set.
   */
  public static final int FILTERED = 1;

  /** 
   * This strategy will not look for string repetitions at all.  It
   * only encodes with Huffman trees (which means, that more common
   * characters get a smaller encoding.  
   */
  public static final int HUFFMAN_ONLY = 2;

  /**
   * The compression method.  This is the only method supported so far.
   * There is no need to use this constant at all.
   */
  public static final int DEFLATED = 8;

  /** Compression level. */
  private int level;

  /** Compression strategy. */
  private int strategy;

  /** The zlib stream. */
  private RawData zstream;

  /** True if finished. */
  private boolean is_finished;

  /** `Flush' flag to pass to next call to deflate. */
  private int flush_flag;

  /**
   * Creates a new deflater with default compression level.
   */
  public Deflater()
  {
    this(DEFAULT_COMPRESSION, false);
  }

  /**
   * Creates a new deflater with given compression level.
   * @param lvl the compression level, a value between NO_COMPRESSION
   * and BEST_COMPRESSION, or DEFAULT_COMPRESSION.  
   * @exception IllegalArgumentException if lvl is out of range.
   */
  public Deflater(int lvl)
  {
    this(lvl, false);
  }

  /**
   * Creates a new deflater with given compression level.
   * @param lvl the compression level, a value between NO_COMPRESSION
   * and BEST_COMPRESSION.  
   * @param nowrap true, iff we should suppress the deflate header at the
   * beginning and the adler checksum at the end of the output.  This is
   * useful for the GZIP format.
   * @exception IllegalArgumentException if lvl is out of range.
   */
  public Deflater(int lvl, boolean noHeader)
  {
    this.strategy = DEFAULT_STRATEGY;
    init(lvl, noHeader);
    setLevel(lvl);
  }

  private native void init(int level, boolean noHeader);
  
  private native void update();

  /** 
   * Resets the deflater.  The deflater acts afterwards as if it was
   * just created with the same compression level and strategy as it
   * had before.  
   */
  public native void reset();
  
  /**
   * Frees all objects allocated by the compressor.  There's no
   * reason to call this, since you can just rely on garbage
   * collection.  Exists only for compatibility against Sun's JDK,
   * where the compressor allocates native memory.
   * If you call any method (even reset) afterwards the behaviour is
   * <i>undefined</i>.  
   * @deprecated Just clear all references to deflater instead.
   */
  public native void end();

  /** 
   * Gets the current adler checksum of the data that was processed so
   * far.
   */
  public native int getAdler();

  /** 
   * Gets the number of input bytes processed so far.
   */
  @Deprecated
  public int getTotalIn()
  {
    return (int) getBytesRead();
  }

  /** 
   * Gets the number of input bytes processed so far.
   * @since 1.5
   */
  public native long getBytesRead();

  /** 
   * Gets the number of output bytes so far.
   */
  @Deprecated
  public int getTotalOut()
  {
    return (int) getBytesWritten();
  }

  /** 
   * Gets the number of output bytes so far.
   * @since 1.5
   */
  public native long getBytesWritten();

  /** 
   * Finalizes this object.
   */
  protected void finalize()
  {
    end();
  }

  /** 
   * Finishes the deflater with the current input block.  It is an error
   * to give more input after this method was called.  This method must
   * be called to force all bytes to be flushed.
   */
  public native void finish();

  /** 
   * Returns true iff the stream was finished and no more output bytes
   * are available.
   */
  public synchronized boolean finished()
  {
    return is_finished;
  }

  /**
   * Returns true, if the input buffer is empty.
   * You should then call setInput(). <br>
   *
   * <em>NOTE</em>: This method can also return true when the stream
   * was finished.  
   */
  public native boolean needsInput();

  /**
   * Sets the data which should be compressed next.  This should be only
   * called when needsInput indicates that more input is needed.
   * If you call setInput when needsInput() returns false, the
   * previous input that is still pending will be thrown away.
   * The given byte array should not be changed, before needsInput() returns
   * true again.
   * This call is equivalent to <code>setInput(input, 0, input.length)</code>.
   * @param input the buffer containing the input data.
   * @exception IllegalStateException if the buffer was finished() or ended().
   */
  public void setInput(byte[] input)
  {
    setInput(input, 0, input.length);
  }

  /**
   * Sets the data which should be compressed next.  This should be
   * only called when needsInput indicates that more input is needed.
   * The given byte array should not be changed, before needsInput() returns
   * true again.
   * @param input the buffer containing the input data.
   * @param off the start of the data.
   * @param len the length of the data.  
   * @exception IllegalStateException if the buffer was finished() or ended()
   * or if previous input is still pending.
   */
  public native void setInput(byte[] input, int off, int len);

  /** 
   * Sets the compression level.  There is no guarantee of the exact
   * position of the change, but if you call this when needsInput is
   * true the change of compression level will occur somewhere near
   * before the end of the so far given input.  
   * @param lvl the new compression level.
   */
  public synchronized void setLevel(int lvl)
  {
    if (lvl != -1 && (lvl < 0 || lvl > 9))
      throw new IllegalArgumentException();
    level = (lvl == -1) ? 6 : lvl;
    update();
  }

  /** 
   * Sets the compression strategy. Strategy is one of
   * DEFAULT_STRATEGY, HUFFMAN_ONLY and FILTERED.  For the exact
   * position where the strategy is changed, the same as for
   * setLevel() applies.
   * @param stgy the new compression strategy.
   */
  public synchronized void setStrategy(int stgy)
  {
    if (stgy != DEFAULT_STRATEGY && stgy != FILTERED
	&& stgy != HUFFMAN_ONLY)
      throw new IllegalArgumentException();
    strategy = stgy;
    update();
  }

  /**
   * Deflates the current input block to the given array.  It returns 
   * the number of bytes compressed, or 0 if either 
   * needsInput() or finished() returns true or length is zero.
   * @param output the buffer where to write the compressed data.
   */
  public int deflate(byte[] output)
  {
    return deflate(output, 0, output.length);
  }

  /**
   * Deflates the current input block to the given array.  It returns 
   * the number of bytes compressed, or 0 if either 
   * needsInput() or finished() returns true or length is zero.
   * @param output the buffer where to write the compressed data.
   * @param offset the offset into the output array.
   * @param length the maximum number of bytes that may be written.
   * @exception IllegalStateException if end() was called.
   * @exception IndexOutOfBoundsException if offset and/or length
   * don't match the array length.  
   */
  public native int deflate(byte[] output, int off, int len);

  /**
   * Sets the dictionary which should be used in the deflate process.
   * This call is equivalent to <code>setDictionary(dict, 0,
   * dict.length)</code>.  
   * @param dict the dictionary.  
   * @exception IllegalStateException if setInput () or deflate ()
   * were already called or another dictionary was already set.  
   */
  public void setDictionary(byte[] dict)
  {
    setDictionary(dict, 0, dict.length);
  }

  /**
   * Sets the dictionary which should be used in the deflate process.
   * The dictionary should be a byte array containing strings that are
   * likely to occur in the data which should be compressed.  The
   * dictionary is not stored in the compressed output, only a
   * checksum.  To decompress the output you need to supply the same
   * dictionary again.
   * @param dict the dictionary.
   * @param offset an offset into the dictionary.
   * @param length the length of the dictionary.
   * @exception IllegalStateException if setInput () or deflate () were
   * already called or another dictionary was already set.
   */
  public native void setDictionary(byte[] buf, int off, int len);

  // Classpath's compression library supports flushing, but we
  // don't.  So this is a no-op here.
  void flush()
  {
  }
}
