/* java.util.VMTimeZone
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

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


package java.util;

import gnu.classpath.Configuration;

import java.io.*;

/**
 *
 */
final class VMTimeZone
{
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
	System.loadLibrary("javautil");
      }
  }
		
  /**
   * This method returns a time zone id string which is in the form
   * (standard zone name) or (standard zone name)(GMT offset) or
   * (standard zone name)(GMT offset)(daylight time zone name).  The
   * GMT offset can be in seconds, or where it is evenly divisible by
   * 3600, then it can be in hours.  The offset must be the time to
   * add to the local time to get GMT.  If a offset is given and the
   * time zone observes daylight saving then the (daylight time zone
   * name) must also be given (otherwise it is assumed the time zone
   * does not observe any daylight savings).
   * <p>
   * The result of this method is given to the method
   * TimeZone.getDefaultTimeZone(String) which tries to map the time
   * zone id to a known TimeZone.  See that method on how the returned
   * String is mapped to a real TimeZone object.
   * <p>
   * The reference implementation which is made for GNU/Posix like
   * systems calls <code>System.getenv("TZ")</code>,
   * <code>readTimeZoneFile("/etc/timezone")</code>,
   * <code>readtzFile("/etc/localtime")</code> and finally
   * <code>getSystemTimeZoneId()</code> till a supported TimeZone is
   * found through <code>TimeZone.getDefaultTimeZone(String)</code>.
   * If every method fails <code>null</code> is returned (which means
   * the TimeZone code will fall back on GMT as default time zone).
   * <p>
   * Note that this method is called inside a
   * <code>AccessController.doPrivileged()</code> block and runs with
   * the priviliges of the java.util system classes.  It will only be
   * called when the default time zone is not yet set, the system
   * property user.timezone isn't set and it is requested for the
   * first time.
   */
  static TimeZone getDefaultTimeZoneId()
  {
    TimeZone zone = null;

    // See if TZ environment variable is set and accessible.
    String tzid = System.getenv("TZ");
    if (tzid != null && !tzid.equals(""))
      zone = TimeZone.getDefaultTimeZone(tzid);

    // Try to parse /etc/timezone.
    if (zone == null)
      {
	tzid = readTimeZoneFile("/etc/timezone");
	if (tzid != null && !tzid.equals(""))
	  zone = TimeZone.getDefaultTimeZone(tzid);
      }
    
    // Try to parse /etc/localtime
    if (zone == null)
      {
	tzid = readtzFile("/etc/localtime");
	if (tzid != null && !tzid.equals(""))
	  zone = TimeZone.getDefaultTimeZone(tzid);
      }

    // Try some system specific way
    if (zone == null)
      {
	tzid = getSystemTimeZoneId();
	if (tzid != null && !tzid.equals(""))
	  zone = TimeZone.getDefaultTimeZone(tzid);
      }

    return zone;
  }

  /**
   * Tries to read the time zone name from a file. Only the first
   * consecutive letters, digits, slashes, dashes and underscores are
   * read from the file. If the file cannot be read or an IOException
   * occurs null is returned.
   * <p>
   * The /etc/timezone file is not standard, but a lot of systems have
   * it. If it exist the first line always contains a string
   * describing the timezone of the host of domain. Some systems
   * contain a /etc/TIMEZONE file which is used to set the TZ
   * environment variable (which is checked before /etc/timezone is
   * read).
   */
  private static String readTimeZoneFile(String file)
  {
    File f = new File(file);
    if (!f.exists())
      return null;

    InputStreamReader isr = null;
    try
      {
	FileInputStream fis = new FileInputStream(f);
	BufferedInputStream bis = new BufferedInputStream(fis);
	isr = new InputStreamReader(bis);
	
	StringBuffer sb = new StringBuffer();
	int i = isr.read();
	while (i != -1)
	  {
	    char c = (char) i;
	    if (Character.isLetter(c) || Character.isDigit(c)
		|| c == '/' || c == '-' || c == '_')
	      {
		sb.append(c);
		i = isr.read();
	      }
	    else
	      break;
	  }
	return sb.toString();
      }
    catch (IOException ioe)
      {
	// Parse error, not a proper tzfile.
	return null;
      }
    finally
      {
	try
	  {
	    if (isr != null)
	      isr.close();
	  }
	catch (IOException ioe)
	  {
	    // Error while close, nothing we can do.
	  }
      }
  }

  /**
   * Tries to read a file as a "standard" tzfile and return a time
   * zone id string as expected by <code>getDefaultTimeZone(String)</code>.
   * If the file doesn't exist, an IOException occurs or it isn't a tzfile
   * that can be parsed null is returned.
   * <p>
   * The tzfile structure (as also used by glibc) is described in the Olson
   * tz database archive as can be found at
   * <code>ftp://elsie.nci.nih.gov/pub/</code>.
   * <p>
   * At least the following platforms support the tzdata file format
   * and /etc/localtime (GNU/Linux, Darwin, Solaris and FreeBSD at
   * least). Some systems (like Darwin) don't start the file with the
   * required magic bytes 'TZif', this implementation can handle
   * that).
   */
  private static String readtzFile(String file)
  {
    File f = new File(file);
    if (!f.exists())
      return null;
    
    DataInputStream dis = null;
    try
      {
        FileInputStream fis = new FileInputStream(f);
        BufferedInputStream bis = new BufferedInputStream(fis);
        dis = new DataInputStream(bis);
	
        // Make sure we are reading a tzfile.
        byte[] tzif = new byte[4];
        dis.readFully(tzif);
        if (tzif[0] == 'T' && tzif[1] == 'Z'
            && tzif[2] == 'i' && tzif[3] == 'f')
	  // Reserved bytes, ttisgmtcnt, ttisstdcnt and leapcnt
	  skipFully(dis, 16 + 3 * 4);
	else
	  // Darwin has tzdata files that don't start with the TZif marker
	  skipFully(dis, 16 + 3 * 4 - 4);
	
	int timecnt = dis.readInt();
	int typecnt = dis.readInt();
	if (typecnt > 0)
	  {
	    int charcnt = dis.readInt();
	    // Transition times plus indexed transition times.
	    skipFully(dis, timecnt * (4 + 1));
	    
	    // Get last gmt_offset and dst/non-dst time zone names.
	    int abbrind = -1;
	    int dst_abbrind = -1;
	    int gmt_offset = 0;
	    while (typecnt-- > 0)
	      {
		// gmtoff
		int offset = dis.readInt();
		int dst = dis.readByte();
		if (dst == 0)
		  {
		    abbrind = dis.readByte();
		    gmt_offset = offset;
		  }
		else
		  dst_abbrind = dis.readByte();
	      }
	    
	    // gmt_offset is the offset you must add to UTC/GMT to
	    // get the local time, we need the offset to add to
	    // the local time to get UTC/GMT.
	    gmt_offset *= -1;
	    
	    // Turn into hours if possible.
	    if (gmt_offset % 3600 == 0)
	      gmt_offset /= 3600;
	    
	    if (abbrind >= 0)
	      {
		byte[] names = new byte[charcnt];
		dis.readFully(names);
		int j = abbrind;
		while (j < charcnt && names[j] != 0)
		  j++;
		
		String zonename = new String(names, abbrind, j - abbrind,
					     "ASCII");
		
		String dst_zonename;
		if (dst_abbrind >= 0)
		  {
		    j = dst_abbrind;
		    while (j < charcnt && names[j] != 0)
		      j++;
		    dst_zonename = new String(names, dst_abbrind,
					      j - dst_abbrind, "ASCII");
		  }
		else
		  dst_zonename = "";
		
		// Only use gmt offset when necessary.
		// Also special case GMT+/- timezones.
		String offset_string;
		if ("".equals(dst_zonename)
		    && (gmt_offset == 0
			|| zonename.startsWith("GMT+")
			|| zonename.startsWith("GMT-")))
		  offset_string = "";
		else
		  offset_string = Integer.toString(gmt_offset);
		
		String id = zonename + offset_string + dst_zonename;
		
		return id;
	      }
	  }
	
	// Something didn't match while reading the file.
	return null;
      }
    catch (IOException ioe)
      {
	// Parse error, not a proper tzfile.
	return null;
      }
    finally
      {
	try
	  {
	    if (dis != null)
	      dis.close();
	  }
	catch(IOException ioe)
	  {
	    // Error while close, nothing we can do.
	  }
      }
  }
  
  /**
   * Skips the requested number of bytes in the given InputStream.
   * Throws EOFException if not enough bytes could be skipped.
   * Negative numbers of bytes to skip are ignored.
   */
  private static void skipFully(InputStream is, long l) throws IOException
  {
    while (l > 0)
      {
        long k = is.skip(l);
        if (k <= 0)
          throw new EOFException();
        l -= k;
      }
  }

  /**
   * Tries to get the system time zone id through native code.
   */
  private static native String getSystemTimeZoneId();
}
