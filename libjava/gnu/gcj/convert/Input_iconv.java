// Input_iconv.java -- Java side of iconv() reader.

/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;
import gnu.gcj.RawData;
import java.io.UnsupportedEncodingException;

/**
 * Convert bytes in some iconv-supported encoding to Unicode.
 * @author Tom Tromey <tromey@redhat.com>
 * @date January 30, 2000
 */

public class Input_iconv extends BytesToUnicode
{
  public Input_iconv (String encoding) throws UnsupportedEncodingException
  {
    this.encoding = encoding;
    this.handle = null;
    init (encoding);
  }

  public String getName() { return encoding; }

  public native void finalize ();
  private native void init (String encoding)
    throws UnsupportedEncodingException;
  public native int read (char[] outbuffer, int outpos, int count);
  public native void done ();

  // The encoding we're using.
  private String encoding;

  // The iconv handle.
  private RawData handle;
}
