// Inflater.java - Decompress a data stream.

/* Copyright (C) 1999  Free Software Foundation

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

public class Inflater
{
  public native void end ();

  protected void finalize ()
  {
    end ();
  }

  public synchronized boolean finished ()
  {
    return is_finished;
  }

  public native int getAdler ();
  public native int getRemaining ();
  public native int getTotalIn ();
  public native int getTotalOut ();

  public int inflate (byte[] buf) throws DataFormatException
  {
    return inflate (buf, 0, buf.length);
  }

  public native int inflate (byte[] buf, int off, int len)
    throws DataFormatException;

  private native void init (boolean noHeader);

  public Inflater ()
  {
    this (false);
  }

  public Inflater (boolean noHeader)
  {
    init (noHeader);
  }

  public synchronized boolean needsDictionary ()
  {
    return dict_needed;
  }

  public synchronized boolean needsInput ()
  {
    return getRemaining () == 0;
  }

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

  // The zlib stream.
  private RawData zstream;

  // True if finished.
  private boolean is_finished;

  // True if dictionary needed.
  private boolean dict_needed;
}
