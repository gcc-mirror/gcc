/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

public class Input_EUCJIS extends BytesToUnicode
{
  public String getName() { return "EUCJIS"; }

  int codeset = 0;
  int first_byte;

  public native int read (char[] outbuffer, int outpos, int outlength);
}
