/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert Unicode to SJIS (Shift JIS, used on Japanese MS-Windows).
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 1999.
 */

public class Output_SJIS extends UnicodeToBytes
{
  public String getName() { return "SJIS"; }

  public native int write (char[] inbuffer, int inpos, int inlength);

  public native int write (String str, int inpos, int inlength, char[] work);

  int pending = -1;
}
