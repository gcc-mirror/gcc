/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert SJIS (Shift JIS, used on Japanese MS-Windows) to Unicode.
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 1999.
 */

public class Input_SJIS extends BytesToUnicode
{
  public String getName() { return "SJIS"; }

  public native int read (char[] outbuffer, int outpos, int count);

  int first_byte;
}
