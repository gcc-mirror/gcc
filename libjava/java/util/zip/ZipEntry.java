/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;

/**
 * @author Per Bothner
 * @date January 6, 1999.
 */

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public class ZipEntry
{
  // These values were determined using a simple test program.
  public static final int STORED = 0;
  public static final int DEFLATED = 8;

  String comment;
  long compressedSize = -1;
  long crc = -1;
  byte[] extra;
  int method = -1;
  String name;
  long size = -1;
  long time = -1;

  ZipEntry next;

  public ZipEntry (String name)
  {
    this.name = name;
  }

  public String getComment () { return comment; }

  public long getCompressedSize () { return compressedSize; }

  public long getCrc () { return crc; }

  public byte[] getExtra() { return extra; }

  public int getMethod () { return method; }

  public String getName () { return name; }

  public long getSize () { return size; }

  public long getTime () { return time; }

  public boolean isDirectory ()
  {
    if (name != null)
      {
	int nlen = name.length();
	if (nlen > 0 && name.charAt(nlen-1) == '/')
	  return true;
      }
    return false;
  }

  public void setComment (String comment) { this.comment = comment; }

  public void setCrc (long crc) { this.crc = crc; }

  public void setExtra (byte[] extra) { this.extra = extra; }

  public void setMethod(int method) { this.method = method; }

  public void setSize (long size) { this.size = size; }

  public void setTime (long time) { this.time = time; }

  public String toString () { return name; }
}
