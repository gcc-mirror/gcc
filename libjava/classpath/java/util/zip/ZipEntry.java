/* ZipEntry.java --
   Copyright (C) 2001, 2002, 2004, 2005 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import java.util.Calendar;

/**
 * This class represents a member of a zip archive.  ZipFile and
 * ZipInputStream will give you instances of this class as information
 * about the members in an archive.  On the other hand ZipOutputStream
 * needs an instance of this class to create a new member.
 *
 * @author Jochen Hoenicke 
 */
public class ZipEntry implements ZipConstants, Cloneable
{
  private static final byte KNOWN_SIZE    = 1;
  private static final byte KNOWN_CSIZE   = 2;
  private static final byte KNOWN_CRC     = 4;
  private static final byte KNOWN_TIME    = 8;
  private static final byte KNOWN_DOSTIME = 16;
  private static final byte KNOWN_EXTRA   = 32;

  /** Immutable name of the entry */
  private final String name;
  /** Uncompressed size */
  private int size;
  /** Compressed size */
  private long compressedSize = -1;
  /** CRC of uncompressed data */
  private int crc;
  /** Comment or null if none */
  private String comment = null;
  /** The compression method. Either DEFLATED or STORED, by default -1. */
  private byte method = -1;
  /** Flags specifying what we know about this entry */
  private byte known = 0;
  /**
   * The 32bit DOS encoded format for the time of this entry. Only valid if
   * KNOWN_DOSTIME is set in known.
   */
  private int dostime;
  /**
   * The 64bit Java encoded millisecond time since the beginning of the epoch.
   * Only valid if KNOWN_TIME is set in known.
   */
  private long time;
  /** Extra data */
  private byte[] extra = null;

  int flags;              /* used by ZipOutputStream */
  int offset;             /* used by ZipFile and ZipOutputStream */

  /**
   * Compression method.  This method doesn't compress at all.
   */
  public static final int STORED = 0;
  /**
   * Compression method.  This method uses the Deflater.
   */
  public static final int DEFLATED = 8;

  /**
   * Creates a zip entry with the given name.
   * @param name the name. May include directory components separated
   * by '/'.
   *
   * @exception NullPointerException when name is null.
   * @exception IllegalArgumentException when name is bigger then 65535 chars.
   */
  public ZipEntry(String name)
  {
    int length = name.length();
    if (length > 65535)
      throw new IllegalArgumentException("name length is " + length);
    this.name = name;
  }

  /**
   * Creates a copy of the given zip entry.
   * @param e the entry to copy.
   */
  public ZipEntry(ZipEntry e)
  {
    this(e, e.name);
  }

  ZipEntry(ZipEntry e, String name)
  {
    this.name = name;
    known = e.known;
    size = e.size;
    compressedSize = e.compressedSize;
    crc = e.crc;
    dostime = e.dostime;
    time = e.time;
    method = e.method;
    extra = e.extra;
    comment = e.comment;
  }

  final void setDOSTime(int dostime)
  {
    this.dostime = dostime;
    known |= KNOWN_DOSTIME;
    known &= ~KNOWN_TIME;
  }

  final int getDOSTime()
  {
    if ((known & KNOWN_DOSTIME) != 0)
      return dostime;
    else  if ((known & KNOWN_TIME) != 0)
      {
       Calendar cal = Calendar.getInstance();
       cal.setTimeInMillis(time);
       dostime = (cal.get(Calendar.YEAR) - 1980 & 0x7f) << 25
          | (cal.get(Calendar.MONTH) + 1) << 21
          | (cal.get(Calendar.DAY_OF_MONTH)) << 16
          | (cal.get(Calendar.HOUR_OF_DAY)) << 11
          | (cal.get(Calendar.MINUTE)) << 5
          | (cal.get(Calendar.SECOND)) >> 1;
       known |= KNOWN_DOSTIME;
       return dostime;
      }
    else
      return 0;
  }

  /**
   * Creates a copy of this zip entry.
   */
  public Object clone()
  {
    // JCL defines this as being the same as the copy constructor above,
    // except that value of the "extra" field is also copied. Take care
    // that in the case of a subclass we use clone() rather than the copy
    // constructor.
    ZipEntry clone;
    if (this.getClass() == ZipEntry.class)
      clone = new ZipEntry(this);
    else
      {
       try
         {
          clone = (ZipEntry) super.clone();
         }
       catch (CloneNotSupportedException e)
         {
          throw new InternalError();
         }
      }
    if (extra != null)
      {
       clone.extra = new byte[extra.length];
       System.arraycopy(extra, 0, clone.extra, 0, extra.length);
      }
    return clone;
  }

  /**
   * Returns the entry name.  The path components in the entry are
   * always separated by slashes ('/').  
   */
  public String getName()
  {
    return name;
  }

  /**
   * Sets the time of last modification of the entry.
   * @time the time of last modification of the entry.
   */
  public void setTime(long time)
  {
    this.time = time;
    this.known |= KNOWN_TIME;
    this.known &= ~KNOWN_DOSTIME;
  }

  /**
   * Gets the time of last modification of the entry.
   * @return the time of last modification of the entry, or -1 if unknown.
   */
  public long getTime()
  {
    // The extra bytes might contain the time (posix/unix extension)
    parseExtra();

    if ((known & KNOWN_TIME) != 0)
      return time;
    else if ((known & KNOWN_DOSTIME) != 0)
      {
       int sec = 2 * (dostime & 0x1f);
       int min = (dostime >> 5) & 0x3f;
       int hrs = (dostime >> 11) & 0x1f;
       int day = (dostime >> 16) & 0x1f;
       int mon = ((dostime >> 21) & 0xf) - 1;
       int year = ((dostime >> 25) & 0x7f) + 1980; /* since 1900 */

       try
         {
          Calendar cal = Calendar.getInstance();
          cal.set(year, mon, day, hrs, min, sec);
          time = cal.getTimeInMillis();
          known |= KNOWN_TIME;
          return time;
	 }
       catch (RuntimeException ex)
         {
          /* Ignore illegal time stamp */
          known &= ~KNOWN_TIME;
          return -1;
         }
      }
    else
      return -1;
  }

  /**
   * Sets the size of the uncompressed data.
   * @exception IllegalArgumentException if size is not in 0..0xffffffffL
   */
  public void setSize(long size)
  {
    if ((size & 0xffffffff00000000L) != 0)
	throw new IllegalArgumentException();
    this.size = (int) size;
    this.known |= KNOWN_SIZE;
  }

  /**
   * Gets the size of the uncompressed data.
   * @return the size or -1 if unknown.
   */
  public long getSize()
  {
    return (known & KNOWN_SIZE) != 0 ? size & 0xffffffffL : -1L;
  }

  /**
   * Sets the size of the compressed data.
   */
  public void setCompressedSize(long csize)
  {
    this.compressedSize = csize;
  }

  /**
   * Gets the size of the compressed data.
   * @return the size or -1 if unknown.
   */
  public long getCompressedSize()
  {
    return compressedSize;
  }

  /**
   * Sets the crc of the uncompressed data.
   * @exception IllegalArgumentException if crc is not in 0..0xffffffffL
   */
  public void setCrc(long crc)
  {
    if ((crc & 0xffffffff00000000L) != 0)
	throw new IllegalArgumentException();
    this.crc = (int) crc;
    this.known |= KNOWN_CRC;
  }

  /**
   * Gets the crc of the uncompressed data.
   * @return the crc or -1 if unknown.
   */
  public long getCrc()
  {
    return (known & KNOWN_CRC) != 0 ? crc & 0xffffffffL : -1L;
  }

  /**
   * Sets the compression method.  Only DEFLATED and STORED are
   * supported.
   * @exception IllegalArgumentException if method is not supported.
   * @see ZipOutputStream#DEFLATED
   * @see ZipOutputStream#STORED 
   */
  public void setMethod(int method)
  {
    if (method != ZipOutputStream.STORED
	&& method != ZipOutputStream.DEFLATED)
	throw new IllegalArgumentException();
    this.method = (byte) method;
  }

  /**
   * Gets the compression method.  
   * @return the compression method or -1 if unknown.
   */
  public int getMethod()
  {
    return method;
  }

  /**
   * Sets the extra data.
   * @exception IllegalArgumentException if extra is longer than 0xffff bytes.
   */
  public void setExtra(byte[] extra)
  {
    if (extra == null) 
      {
	this.extra = null;
	return;
      }
    if (extra.length > 0xffff)
      throw new IllegalArgumentException();
    this.extra = extra;
  }

  private void parseExtra()
  {
    // Already parsed?
    if ((known & KNOWN_EXTRA) != 0)
      return;

    if (extra == null)
      {
	known |= KNOWN_EXTRA;
	return;
      }

    try
      {
	int pos = 0;
	while (pos < extra.length) 
	  {
	    int sig = (extra[pos++] & 0xff)
	      | (extra[pos++] & 0xff) << 8;
	    int len = (extra[pos++] & 0xff)
	      | (extra[pos++] & 0xff) << 8;
	    if (sig == 0x5455) 
	      {
		/* extended time stamp */
		int flags = extra[pos];
		if ((flags & 1) != 0)
		  {
		    long time = ((extra[pos+1] & 0xff)
			    | (extra[pos+2] & 0xff) << 8
			    | (extra[pos+3] & 0xff) << 16
			    | (extra[pos+4] & 0xff) << 24);
		    setTime(time*1000);
		  }
	      }
	    pos += len;
	  }
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
	/* be lenient */
      }

    known |= KNOWN_EXTRA;
    return;
  }

  /**
   * Gets the extra data.
   * @return the extra data or null if not set.
   */
  public byte[] getExtra()
  {
    return extra;
  }

  /**
   * Sets the entry comment.
   * @exception IllegalArgumentException if comment is longer than 0xffff.
   */
  public void setComment(String comment)
  {
    if (comment != null && comment.length() > 0xffff)
      throw new IllegalArgumentException();
    this.comment = comment;
  }

  /**
   * Gets the comment.
   * @return the comment or null if not set.
   */
  public String getComment()
  {
    return comment;
  }

  /**
   * Gets true, if the entry is a directory.  This is solely
   * determined by the name, a trailing slash '/' marks a directory.  
   */
  public boolean isDirectory()
  {
    int nlen = name.length();
    return nlen > 0 && name.charAt(nlen - 1) == '/';
  }

  /**
   * Gets the string representation of this ZipEntry.  This is just
   * the name as returned by getName().
   */
  public String toString()
  {
    return name;
  }

  /**
   * Gets the hashCode of this ZipEntry.  This is just the hashCode
   * of the name.  Note that the equals method isn't changed, though.
   */
  public int hashCode()
  {
    return name.hashCode();
  }
}
