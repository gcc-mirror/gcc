/* Deflater.java - Compress a data stream
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

package java.util.zip;

import gnu.gcj.RawData;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
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

  public int deflate (byte[] buf)
  {
    return deflate (buf, 0, buf.length);
  }

  public native int deflate (byte[] buf, int off, int len);
  private native void init (int level, boolean noHeader);
  private native void update ();

  public Deflater ()
  {
    this (DEFAULT_COMPRESSION, false);
  }

  public Deflater (int lvl)
  {
    this (lvl, false);
  }

  public Deflater (int lvl, boolean noHeader)
  {
    this.strategy = DEFAULT_STRATEGY;
    init (lvl, noHeader);
    setLevel (lvl);
  }

  public native void end ();

  protected void finalize ()
  {
    end ();
  }

  public native void finish ();

  public synchronized boolean finished ()
  {
    return is_finished;
  }

  public native int getAdler ();
  public native int getTotalIn ();
  public native int getTotalOut ();
  public native boolean needsInput ();
  public native void reset ();

  public void setDictionary (byte[] buf)
  {
    setDictionary (buf, 0, buf.length);
  }

  public native void setDictionary (byte[] buf, int off, int len);

  public void setInput (byte[] buf)
  {
    setInput (buf, 0, buf.length);
  }

  public native void setInput (byte[] buf, int off, int len);

  public synchronized void setLevel (int lvl)
  {
    if (lvl != -1 && (lvl < 0 || lvl > 9))
      throw new IllegalArgumentException ();
    level = (lvl == -1) ? 6 : lvl;
    update ();
  }

  public synchronized void setStrategy (int stgy)
  {
    if (stgy != DEFAULT_STRATEGY && stgy != FILTERED
	&& stgy != HUFFMAN_ONLY)
      throw new IllegalArgumentException ();
    strategy = stgy;
    update ();
  }

  // Compression level.
  private int level;

  // Compression strategy.
  private int strategy;

  // The zlib stream.
  private RawData zstream;

  // True if finished.
  private boolean is_finished;

  // `Flush' flag to pass to next call to deflate.
  private int flush_flag;
}
