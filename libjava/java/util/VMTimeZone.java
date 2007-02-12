/* java.util.VMTimeZone
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2007
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


package java.util;

import gnu.classpath.Configuration;
import java.util.TimeZone;
import java.util.Calendar;
import java.util.GregorianCalendar;

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
	byte[] tzif = new byte[5];
	dis.readFully(tzif);
	int tzif2 = 4;
	if (tzif[0] == 'T' && tzif[1] == 'Z'
	    && tzif[2] == 'i' && tzif[3] == 'f')
	  {
	    if (tzif[4] >= '2')
	      tzif2 = 8;
	    // Reserved bytes
	    skipFully(dis, 16 - 1);
	  }
	else
	  // Darwin has tzdata files that don't start with the TZif marker
	  skipFully(dis, 16 - 5);

	String id = null;
	int ttisgmtcnt = dis.readInt();
	int ttisstdcnt = dis.readInt();
	int leapcnt = dis.readInt();
	int timecnt = dis.readInt();
	int typecnt = dis.readInt();
	int charcnt = dis.readInt();
	if (tzif2 == 8)
	  {
	    skipFully(dis, timecnt * (4 + 1) + typecnt * (4 + 1 + 1) + charcnt
			   + leapcnt * (4 + 4) + ttisgmtcnt + ttisstdcnt);

	    dis.readFully(tzif);
	    if (tzif[0] != 'T' || tzif[1] != 'Z' || tzif[2] != 'i'
		|| tzif[3] != 'f' || tzif[4] < '2')
	      return null;

	    // Reserved bytes
	    skipFully(dis, 16 - 1);
	    ttisgmtcnt = dis.readInt();
	    ttisstdcnt = dis.readInt();
	    leapcnt = dis.readInt();
	    timecnt = dis.readInt();
	    typecnt = dis.readInt();
	    charcnt = dis.readInt();
	  }
	if (typecnt > 0)
	  {
	    int seltimecnt = timecnt;
	    if (seltimecnt > 16)
	      seltimecnt = 16;

	    long[] times = new long[seltimecnt];
	    int[] types = new int[seltimecnt];

	    // Transition times
	    skipFully(dis, (timecnt - seltimecnt) * tzif2);

	    for (int i = 0; i < seltimecnt; i++)
	      if (tzif2 == 8)
		times[i] = dis.readLong();
	      else
		times[i] = (long) dis.readInt();

	    // Transition types
	    skipFully(dis, timecnt - seltimecnt);
	    for (int i = 0; i < seltimecnt; i++)
	      {
		types[i] = dis.readByte();
		if (types[i] < 0)
		  types[i] += 256;
	      }

	    // Get std/dst_offset and dst/non-dst time zone names.
	    int std_abbrind = -1;
	    int dst_abbrind = -1;
	    int std_offset = 0;
	    int dst_offset = 0;
	    int std_ind = -1;
	    int dst_ind = -1;

	    int alternation = 0;
	    if (seltimecnt >= 4 && types[0] != types[1]
		&& types[0] < typecnt && types[1] < typecnt)
	      {
		// Verify only two types are involved
		// in the transitions and they alternate.
		alternation = 1;
		for (int i = 2; i < seltimecnt; i++)
		  if (types[i] != types[i % 2])
		    alternation = 0;
	      }

	    // If a timezone previously used DST, but no longer does
	    // (or no longer will in the near future, say 5 years),
	    // then always pick only the std zone type corresponding
	    // to latest applicable transition.
	    if (seltimecnt > 0
		&& times[seltimecnt - 1]
		   < System.currentTimeMillis() / 1000 + 5 * 365 * 86400)
	      alternation = -1;

	    for (int i = 0; i < typecnt; i++)
	      {
		// gmtoff
		int offset = dis.readInt();
		int dst = dis.readByte();
		int abbrind = dis.readByte();
		if (dst == 0)
		  {
		    if (alternation == 0
			|| (alternation == 1
			    && (i == types[0] || i == types[1]))
			|| (alternation == -1 && i == types[seltimecnt - 1]))
		      {
			std_abbrind = abbrind;
			std_offset = offset * -1;
			std_ind = i;
		      }
		  }
		else if (alternation >= 0)
		  {
		    if (alternation == 0 || i == types[0] || i == types[1])
		      {
			dst_abbrind = abbrind;
			dst_offset = offset * -1;
			dst_ind = i;
		      }
		  }
	      }

	    if (std_abbrind >= 0)
	      {
		byte[] names = new byte[charcnt];
		dis.readFully(names);
		int j = std_abbrind;
		while (j < charcnt && names[j] != 0)
		  j++;

		String zonename = new String(names, std_abbrind,
					     j - std_abbrind, "ASCII");

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

		String[] change_spec = { null, null };
		if (dst_abbrind >= 0 && alternation > 0)
		  {
		    // Guess rules for the std->dst and dst->std transitions
		    // from the transition times since Epoch.
		    // tzdata actually uses only 3 forms of rules:
		    // fixed date within a month, e.g. change on April, 5th
		    // 1st weekday on or after Nth: change on Sun>=15 in April
		    // last weekday in a month: change on lastSun in April
		    GregorianCalendar cal
		      = new GregorianCalendar (TimeZone.getTimeZone("GMT"));

		    int[] values = new int[2 * 11];
		    int i;
		    for (i = seltimecnt - 1; i >= 0; i--)
		      {
			int base = (i % 2) * 11;
			int offset = types[i] == dst_ind ? std_offset : dst_offset;
			cal.setTimeInMillis((times[i] - offset) * 1000);
			if (i >= seltimecnt - 2)
			  {
			    values[base + 0] = cal.get(Calendar.YEAR);
			    values[base + 1] = cal.get(Calendar.MONTH);
			    values[base + 2] = cal.get(Calendar.DAY_OF_MONTH);
			    values[base + 3]
			      = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
			    values[base + 4] = cal.get(Calendar.DAY_OF_WEEK);
			    values[base + 5] = cal.get(Calendar.HOUR_OF_DAY);
			    values[base + 6] = cal.get(Calendar.MINUTE);
			    values[base + 7] = cal.get(Calendar.SECOND);
			    values[base + 8] = values[base + 2]; // Range start
			    values[base + 9] = values[base + 2]; // Range end
			    values[base + 10] = 0; // Determined type
			  }
			else
			  {
			    int year = cal.get(Calendar.YEAR);
			    int month = cal.get(Calendar.MONTH);
			    int day_of_month = cal.get(Calendar.DAY_OF_MONTH);
			    int month_days
			      = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
			    int day_of_week = cal.get(Calendar.DAY_OF_WEEK);
			    int hour = cal.get(Calendar.HOUR_OF_DAY);
			    int minute = cal.get(Calendar.MINUTE);
			    int second = cal.get(Calendar.SECOND);
			    if (year != values[base + 0] - 1
				|| month != values[base + 1]
				|| hour != values[base + 5]
				|| minute != values[base + 6]
				|| second != values[base + 7])
			      break;
			    if (day_of_week == values[base + 4])
			      {
				// Either a Sun>=8 or lastSun rule.
				if (day_of_month < values[base + 8])
				  values[base + 8] = day_of_month;
				if (day_of_month > values[base + 9])
				  values[base + 9] = day_of_month;
				if (values[base + 10] < 0)
				  break;
				if (values[base + 10] == 0)
				  {
				    values[base + 10] = 1;
				    // If day of month > 28, this is
				    // certainly lastSun rule.
				    if (values[base + 2] > 28)
				      values[base + 2] = 3;
				    // If day of month isn't in the last
				    // week, it can't be lastSun rule.
				    else if (values[base + 2]
					     <= values[base + 3] - 7)
				      values[base + 3] = 2;
				  }
				if (values[base + 10] == 1)
				  {
				    // If day of month is > 28, this is
				    // certainly lastSun rule.
				    if (day_of_month > 28)
				      values[base + 10] = 3;
				    // If day of month isn't in the last
				    // week, it can't be lastSun rule.
				    else if (day_of_month <= month_days - 7)
				      values[base + 10] = 2;
				  }
				else if ((values[base + 10] == 2
					  && day_of_month > 28)
					 || (values[base + 10] == 3
					     && day_of_month
						<= month_days - 7))
				  break;
			      }
			    else
			      {
				// Must be fixed day in month rule.
				if (day_of_month != values[base + 2]
				    || values[base + 10] > 0)
				  break;
				values[base + 4] = day_of_week;
				values[base + 10] = -1;
			      }
			    values[base + 0] -= 1;
			  }
		      }
		    if (i < 0)
		      {
			for (i = 0; i < 2; i++)
			  {
			    int base = 11 * i;
			    if (values[base + 10] == 0)
			      continue;
			    if (values[base + 10] == -1)
			      {
				int[] dayCount
				  = { 0, 31, 59, 90, 120, 151,
				      181, 212, 243, 273, 304, 334 };
				int d = dayCount[values[base + 1]
						 - Calendar.JANUARY];
				d += values[base + 2];
				change_spec[i] = ",J" + Integer.toString(d);
			      }
			    else if (values[base + 10] == 2)
			      {
				// If we haven't seen all days of the week,
				// we can't be sure what the rule really is.
				if (values[base + 8] + 6 != values[base + 9])
				  continue;

				// FIXME: Sun >= 5 is representable in
				// SimpleTimeZone, but not in POSIX TZ env
				// strings.  Should we change readtzFile
				// to actually return a SimpleTimeZone
				// rather than POSIX TZ string?
				if ((values[base + 8] % 7) != 1)
				  continue;

				int d;
				d = values[base + 1] - Calendar.JANUARY + 1;
				change_spec[i] = ",M" + Integer.toString(d);
				d = (values[base + 8] + 6) / 7;
				change_spec[i] += "." + Integer.toString(d);
				d = values[base + 4] - Calendar.SUNDAY;
				change_spec[i] += "." + Integer.toString(d);
			      }
			    else
			      {
				// If we don't know whether this is lastSun or
				// Sun >= 22 rule.  That can be either because
				// there was insufficient number of
				// transitions, or February, where it is quite
				// probable we haven't seen any 29th dates.
				// For February, assume lastSun rule, otherwise
				// punt.
				if (values[base + 10] == 1
				    && values[base + 1] != Calendar.FEBRUARY)
				  continue;

				int d;
				d = values[base + 1] - Calendar.JANUARY + 1;
				change_spec[i] = ",M" + Integer.toString(d);
				d = values[base + 4] - Calendar.SUNDAY;
				change_spec[i] += ".5." + Integer.toString(d);
			      }

			    // Don't add time specification if time is
			    // 02:00:00.
			    if (values[base + 5] != 2
				|| values[base + 6] != 0
				|| values[base + 7] != 0)
			      {
				int d = values[base + 5];
				change_spec[i] += "/" + Integer.toString(d);
				if (values[base + 6] != 0
				    || values[base + 7] != 0)
				  {
				    d = values[base + 6];
				    if (d < 10)
				      change_spec[i]
					+= ":0" + Integer.toString(d);
				    else
				      change_spec[i]
					+= ":" + Integer.toString(d);
				    d = values[base + 7];
				    if (d >= 10)
				      change_spec[i]
					+= ":" + Integer.toString(d);
				    else if (d > 0)
				      change_spec[i]
					+= ":0" + Integer.toString(d);
				  }
			      }
			  }
			if (types[0] == std_ind)
			  {
			    String tmp = change_spec[0];
			    change_spec[0] = change_spec[1];
			    change_spec[1] = tmp;
			  }
		      }
		  }

		// Only use gmt offset when necessary.
		// Also special case GMT+/- timezones.
		String offset_string, dst_offset_string = "";
		if (dst_abbrind < 0
		    && (std_offset == 0
			|| zonename.startsWith("GMT+")
			|| zonename.startsWith("GMT-")))
		  offset_string = "";
		else
		  {
		    offset_string = Integer.toString(std_offset / 3600);
		    int seconds = std_offset % 3600;
		    if (seconds != 0)
		      {
			if (seconds < 0)
			  seconds *= -1;
			if (seconds < 600)
			  offset_string
			    += ":0" + Integer.toString(seconds / 60);
			else
			  offset_string
			    += ":" + Integer.toString(seconds / 60);
			seconds = seconds % 60;
			if (seconds >= 10)
			  offset_string
			    += ":" + Integer.toString(seconds);
			else if (seconds > 0)
			  offset_string
			    += ":0" + Integer.toString(seconds);
		      }
		    if (dst_abbrind >= 0
			&& dst_offset != std_offset - 3600)
		      {
			dst_offset_string
			  = Integer.toString(dst_offset / 3600);
			seconds = dst_offset % 3600;
			if (seconds != 0)
			  {
			    if (seconds < 0)
			      seconds *= -1;
			    if (seconds < 600)
			      dst_offset_string
				+= ":0" + Integer.toString(seconds / 60);
			    else
			      dst_offset_string
				+= ":" + Integer.toString(seconds / 60);
			    seconds = seconds % 60;
			    if (seconds >= 10)
			      dst_offset_string
				+= ":" + Integer.toString(seconds);
			    else if (seconds > 0)
			      dst_offset_string
				+= ":0" + Integer.toString(seconds);
			  }
		      }
		  }

		if (dst_abbrind < 0)
		  id = zonename + offset_string;
		else if (change_spec[0] != null && change_spec[1] != null)
		  id = zonename + offset_string + dst_zonename
		       + dst_offset_string + change_spec[0] + change_spec[1];
	      }
	    else if (tzif2 == 8)
	      skipFully(dis, charcnt);
	  }
	else if (tzif2 == 8)
	  skipFully(dis, timecnt * (8 + 1) + typecnt * (4 + 1 + 1) + charcnt);

	if (tzif2 == 8)
	  {
	    // Skip over the rest of 64-bit data
	    skipFully(dis, leapcnt * (8 + 4) + ttisgmtcnt + ttisstdcnt);
	    if (dis.readByte() == '\n')
	      {
		String posixtz = dis.readLine();
		if (posixtz.length() > 0)
		  id = posixtz;
	      }
	  }

	return id;
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
