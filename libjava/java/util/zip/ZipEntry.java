/* ZipEntry.java - Represents entries in a zip file archive
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

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

/**
 * Represents entries in a zip file archive.
 * An Entry cn be created by giving a name or by giving an already existing
 * ZipEntries whose values should be copied. The name normally represents a
 * file path name or directory name.
 */
public class ZipEntry implements ZipConstants, Cloneable
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
    if (name.length() > 65535)
      throw new IllegalArgumentException ();
    this.name = name;
  }

  /**
   * Creates a new ZipEntry using the fields of a given ZipEntry.
   * The comment, compressedSize, crc, extra, method, name, size, time and
   * relativeOffset fields are copied from the given entry.
   * Note that the contents of the extra byte array field is not cloned,
   * only the reference is copied.
   * The clone() method does clone the contents of the extra byte array if
   * needed.
   * @since 1.2
   */
  public ZipEntry (ZipEntry ent)
  {
    comment = ent.comment;
    compressedSize = ent.compressedSize;
    crc = ent.crc;
    extra = ent.extra;
    method = ent.method;
    name = ent.name;
    size = ent.size;
    time = ent.time;
    relativeOffset = ent.relativeOffset;
  }
 
  /**
   * Creates a clone of this ZipEntry. Calls <code>new ZipEntry (this)</code>
   * and creates a clone of the contents of the extra byte array field.
   *
   * @since 1.2
   */
  public Object clone ()
  {
    // JCL defines this as being the same as the copy constructor above,
    // except that value of the "extra" field is also copied.
    ZipEntry clone = new ZipEntry (this);
    clone.extra = (byte[]) extra.clone ();
    return clone;
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
 
  /**
   * Sets the compressedSize of this ZipEntry.
   * The new size must be between 0 and 0xffffffffL.
   * @since 1.2
   */
  public void setCompressedSize (long compressedSize)
  {
    if (compressedSize < 0 || compressedSize > 0xffffffffL)
      throw new IllegalArgumentException ();
    this.compressedSize = compressedSize;
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
 
  /**
   * Returns the hashcode of the name of this ZipEntry.
   */
  public int hashCode () { return name.hashCode (); }
}
