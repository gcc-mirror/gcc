/* Copyright (C) 1999  Red Hat, Inc.

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

public class ZipEntry implements ZipConstants
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
  long relativeOffset = -1;

  ZipEntry next;

  public ZipEntry (String name)
  {
    if (name == null)
      throw new NullPointerException ();
    if (name.length() > 65535)
      throw new IllegalArgumentException ();
    this.name = name;
  }

  public ZipEntry (ZipEntry ent)
  {
    comment = ent.comment;
    compressedSize = ent.compressedSize;
    crc = ent.crc;
    extra = ent.extra;
    method = ent.method;
    size = ent.size;
    time = ent.time;
    relativeOffset = ent.relativeOffset;
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

  public void setComment (String comment)
  {
    if (comment != null && comment.length() > 65535)
      throw new IllegalArgumentException ();
    this.comment = comment;
  }

  public void setCrc (long crc) 
  {
    if (crc < 0 || crc > 0xffffffffL)
      throw new IllegalArgumentException ();
    this.crc = crc;
  }

  public void setExtra (byte[] extra)
  {
    if (extra != null && extra.length > 65535)
      throw new IllegalArgumentException ();
    this.extra = extra;
  }

  public void setMethod (int method)
  {
    if (method != DEFLATED && method != STORED)
      throw new IllegalArgumentException ();
    this.method = method;
  }

  public void setSize (long size)
  {
    if (size < 0 || size > 0xffffffffL)
      throw new IllegalArgumentException ();
    this.size = size;
  }

  public void setTime (long time)
  {
    this.time = time;
  }

  private final static short[] daysToMonthStart = {
    //Jan Feb Mar    Apr      May         Jun         Jul
    0,    31, 31+28, 2*31+28, 2*31+28+30, 3*31+28+30, 3*31+28+2*30,
    // Aug        Sep           Oct           Nov           Dec
    4*31+28+2*30, 5*31+28+2*30, 5*31+28+3*30, 6*31+28+3*30, 6*31+28+4*30};

  /** Convert a DOS-style type value to milliseconds since 1970. */
  static long timeFromDOS (int date, int time)
  {
    int sec = 2 * (time & 0x1f);
    int min = (time >> 5) & 0x3f;
    int hrs = (time >> 11) & 0x1f;
    int day = date & 0x1f;
    int mon = ((date >> 5) & 0xf) - 1;
    int year = ((date >> 9) & 0x7f) + 10;  /* Since 1970. */

    // Guard against invalid or missing date causing IndexOutOfBoundsException.
    if (mon < 0 || mon > 11)
      return -1;

    long mtime = (((hrs * 60) + min) * 60 + sec) * 1000;

    // Leap year calculations are rather trivial in this case ...
    int days = 365 * year + ((year+1)>>2);
    days += daysToMonthStart[mon];
    if ((year & 3) == 0 && mon > 1)
      days++;
    days += day;
    return (days * 24*60*60L + ((hrs * 60) + min) * 60 + sec) * 1000L;
  }

  public String toString () { return name; }
}
