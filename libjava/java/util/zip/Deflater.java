// Deflater.java - Compress a data stream.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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
  public static final int BEST_COMPRESSION = 9;
  public static final int BEST_SPEED = 1;
  public static final int DEFAULT_COMPRESSION = -1;
  public static final int NO_COMPRESSION = 0;

  public static final int DEFAULT_STRATEGY = 0;
  public static final int FILTERED = 1;
  public static final int HUFFMAN_ONLY = 2;

  public static final int DEFLATED = 8;

  public int deflate (byte[] buf)
  {
    return deflate (buf, 0, buf.length);
  }

  public native int deflate (byte[] buf, int off, int len);
  public native void init (int level, boolean noHeader);
  public native void update ();

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
