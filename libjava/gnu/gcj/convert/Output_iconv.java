// Output_iconv.java -- Java side of iconv() writer.

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;
import gnu.gcj.RawData;
import java.io.UnsupportedEncodingException;

/**
 * Convert Unicode to bytes in some iconv-supported encoding.
 * @author Tom Tromey <tromey@redhat.com>
 * @date January 30, 2000
 */

public class Output_iconv extends UnicodeToBytes
{
  public Output_iconv (String encoding) throws UnsupportedEncodingException
  {
    this.encoding = encoding;
    this.handle = null;
    init (encoding);
  }

  public String getName() { return encoding; }

  public native void finalize ();
  private native void init (String encoding)
    throws UnsupportedEncodingException;
  public native int write (char[] inbuffer, int inpos, int count);

  // The encoding we're using.
  private String encoding;

  // The iconv handle.
  private RawData handle;
}
