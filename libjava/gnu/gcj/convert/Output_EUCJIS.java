/* Copyright (C) 1999, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

/**
 * Convert Unicode to EUCJIS (Extended Unix Code for Japan).
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 1999.
 */

public class Output_EUCJIS extends UnicodeToBytes
{
  public String getName() { return "EUCJIS"; }

  public native int write (char[] inbuffer, int inpos, int inlength);

  public native int write (String str, int inpos, int inlength, char[] work);

  public boolean havePendingBytes()
  {
    return pending1 >= 0;
  }

  int pending1 = -1;
  int pending2;
}
