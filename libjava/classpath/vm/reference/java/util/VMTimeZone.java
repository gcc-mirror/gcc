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
import gnu.classpath.SystemProperties;
import gnu.java.util.ZoneInfo;
import java.util.TimeZone;

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
   * <code>ZoneInfo.readTZFile((String)null, "/etc/localtime")</code>
   * and finally <code>getSystemTimeZoneId()</code> till a supported
   * TimeZone is found through
   * <code>TimeZone.getDefaultTimeZone(String)</code>.
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
	zone = ZoneInfo.readTZFile((String) null, "/etc/localtime");
	if (zone != null)
	  {
	    // Try to find a more suitable ID for the /etc/localtime
	    // timezone.
	    // Sometimes /etc/localtime is a symlink to some
	    // /usr/share/zoneinfo/ file.
	    String id = null;
	    try
	      {
		id = new File("/etc/localtime").getCanonicalPath();
		if (id != null)
		  {
		    String zoneinfo_dir
		      = SystemProperties.getProperty("gnu.java.util.zoneinfo.dir");
		    if (zoneinfo_dir != null)
		      zoneinfo_dir
			= new File(zoneinfo_dir
				   + File.separatorChar).getCanonicalPath();
		    if (zoneinfo_dir != null && id.startsWith(zoneinfo_dir))
		      {
			int pos = zoneinfo_dir.length();
			while (pos < id.length()
			       && id.charAt(pos) == File.separatorChar)
			  pos++;
			if (pos < id.length())
			  id = id.substring(pos);
			else
			  id = null;
		      }
		    else
		      id = null;
		  }
	      }
	    catch (IOException ioe)
	      {
		id = null;
	      }

	    if (id == null)
	      id = readSysconfigClockFile("/etc/sysconfig/clock");

	    if (id != null)
	      zone.setID(id);
	  }
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
   * Tries to read the time zone name from a file.
   * If the file cannot be read or an IOException occurs null is returned.
   * <p>
   * The /etc/sysconfig/clock file is not standard, but a lot of systems
   * have it. The file is included by shell scripts and the timezone
   * name is defined in ZONE variable.
   * This routine should grok it with or without quotes:
   * ZONE=America/New_York
   * or
   * ZONE="Europe/London"
   */
  private static String readSysconfigClockFile(String file)
  {
    BufferedReader br = null;
    try
      {
	FileInputStream fis = new FileInputStream(file);
	BufferedInputStream bis = new BufferedInputStream(fis);
	br = new BufferedReader(new InputStreamReader(bis));

	for (String line = br.readLine(); line != null; line = br.readLine())
	  {
	    line = line.trim();
	    if (line.length() < 8 || !line.startsWith("ZONE="))
	      continue;
	    int posstart = 6;
	    int posend;
	    if (line.charAt(5) == '"')
	      posend = line.indexOf('"', 6);
	    else if (line.charAt(5) == '\'')
	      posend = line.indexOf('\'', 6);
	    else
	      {
		posstart = 5;
		posend = line.length();
	      }
	    if (posend < 0)
	      return null;
	    return line.substring(posstart, posend);
	  }
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
	    if (br != null)
	      br.close();
	  }
	catch (IOException ioe)
	  {
	    // Error while close, nothing we can do.
	  }
      }
  }

  /**
   * Tries to get the system time zone id through native code.
   */
  private static native String getSystemTimeZoneId();
}
