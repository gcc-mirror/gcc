// System.java - System-specific info.

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.util.Properties;
import java.util.PropertyPermission;
import java.util.TimeZone;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date August 27, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status: 1.1.  Some 1.2 methods missing.  Properties code not fully
 * implemented.
 */

public final class System
{
  public static native void arraycopy (Object src, int srcOffset,
				       Object dst, int dstOffset,
				       int count);

  public static native long currentTimeMillis ();

  // FIXME: When merging with Classpath, remember to remove the call to
  // getDefaultTimeZoneId from java.util.Timezone.
  private static native String getSystemTimeZone ();

  // Get the System Timezone as reported by the OS.  It should be in
  // the form PST8PDT so we'll need to parse it and check that it's valid.
  // The result is used to set the user.timezone property in init_properties.
  // FIXME: Using the code from Classpath for generating the System
  // Timezone IMO is suboptimal because it ignores whether the rules for
  // DST match up.
  private static String getDefaultTimeZoneId ()
  {
    String sysTimeZoneId = getSystemTimeZone ();

    // Check if this is a valid timezone.  Make sure the IDs match
    // since getTimeZone returns GMT if no match is found.
    TimeZone tz = TimeZone.getTimeZone (sysTimeZoneId);
    if (tz.getID ().equals (sysTimeZoneId))
      return sysTimeZoneId;

    // Check if the base part of sysTimeZoneId is a valid timezone that
    // matches with daylight usage and rawOffset.  Make sure the IDs match
    // since getTimeZone returns GMT if no match is found.
    // First find start of GMT offset info and any Daylight zone name.
    int startGMToffset = 0;
    int sysTimeZoneIdLength = sysTimeZoneId.length();
    for (int i = 0; i < sysTimeZoneIdLength && startGMToffset == 0; i++)
      {
        if (Character.isDigit (sysTimeZoneId.charAt (i)))
	  startGMToffset = i;
      }

    int startDaylightZoneName = 0;
    boolean usesDaylight = false;
    for (int i = sysTimeZoneIdLength - 1;
         i >= 0 && !Character.isDigit (sysTimeZoneId.charAt (i)); --i)
      {
        startDaylightZoneName = i;
      }
    if (startDaylightZoneName > 0)
      usesDaylight = true;

    int GMToffset = Integer.parseInt (startDaylightZoneName == 0 ?
      sysTimeZoneId.substring (startGMToffset) :
      sysTimeZoneId.substring (startGMToffset, startDaylightZoneName));

    // Offset could be in hours or seconds.  Convert to millis.
    if (GMToffset < 24)
      GMToffset *= 60 * 60;
    GMToffset *= -1000;

    String tzBasename = sysTimeZoneId.substring (0, startGMToffset);
    tz = TimeZone.getTimeZone (tzBasename);
    if (tz.getID ().equals (tzBasename) && tz.getRawOffset () == GMToffset)
      {
        boolean tzUsesDaylight = tz.useDaylightTime ();
        if (usesDaylight && tzUsesDaylight || !usesDaylight && !tzUsesDaylight)
          return tzBasename;
      }
  
    // If no match, see if a valid timezone has the same attributes as this
    // and then use it instead.
    String[] IDs = TimeZone.getAvailableIDs (GMToffset);
    for (int i = 0; i < IDs.length; ++i)
      {
	// FIXME: The daylight savings rules may not match the rules
	// for the desired zone.
        boolean IDusesDaylight =
	  TimeZone.getTimeZone (IDs[i]).useDaylightTime ();
        if (usesDaylight && IDusesDaylight || !usesDaylight && !IDusesDaylight)
	  return IDs[i];
      }

    // If all else fails, return null.
    return null;
  }

  public static void exit (int status)
  {
    Runtime.getRuntime().exit(status);
  }

  public static void gc ()
  {
    Runtime.getRuntime().gc();
  }

  // Marked deprecated in 1.1.  We implement what the JCL book says.
  public static String getenv (String name)
  {
    throw new Error ();
  }

  private static native void init_properties ();

  public static Properties getProperties ()
  {
    if (secman != null)
      secman.checkPropertiesAccess();
    if (properties == null)
      init_properties ();
    return properties;
  }

  public static String getProperty (String property)
  {
    if (secman != null)
      secman.checkPropertyAccess(property);
    if (properties == null)
      init_properties ();
    return properties.getProperty(property);
  }

  public static String getProperty (String property, String defval)
  {
    if (secman != null)
      secman.checkPropertyAccess(property);
    if (properties == null)
      init_properties ();
    return properties.getProperty(property, defval);
  }

  public static SecurityManager getSecurityManager ()
  {
    return secman;
  }

  public static native int identityHashCode (Object obj);

  public static void load (String pathname)
  {
    Runtime.getRuntime().load(pathname);
  }

  public static void loadLibrary (String libname)
  {
    Runtime.getRuntime().loadLibrary(libname);
  }

  public static void runFinalization ()
  {
    Runtime.getRuntime().runFinalization();
  }

  // Marked as deprecated in 1.2.
  public static void runFinalizersOnExit (boolean run)
  {
    Runtime.getRuntime().runFinalizersOnExit(run);
  }

  private static void checkSetIO ()
  {
    // In 1.1, we are supposed to call checkExec, but the argument is
    // not specified.  In 1.2, we are supposed to use checkPermission,
    // which doesn't exist in 1.1.
    if (secman != null)
      secman.checkExec("");
  }

  public static native void setErr (PrintStream newErr);
  public static native void setIn (InputStream newIn);
  public static native void setOut (PrintStream newOut);

  public static void setProperties (Properties props)
  {
    if (secman != null)
      secman.checkPropertiesAccess();
    synchronized (System.class)
    {
      properties = props;
    }
  }

  public static String setProperty (String key, String value)
  {
    if (secman != null)
      secman.checkPermission (new PropertyPermission (key, "write"));
    if (properties == null)
      init_properties ();
    return (String) properties.setProperty (key, value);
  }

  // TODO 1.2.
  // public static String mapLibraryName (String libname);

  public static void setSecurityManager (SecurityManager s)
  {
    if (secman != null)
      secman.checkPermission(new RuntimePermission("setSecurityManager"));
    secman = s;
  }

  // Public data.
  public static final InputStream in = new BufferedInputStream (new FileInputStream (FileDescriptor.in));

  public static final PrintStream out = new PrintStream (new BufferedOutputStream (new FileOutputStream (FileDescriptor.out)), true);

  public static final PrintStream err = new PrintStream (new BufferedOutputStream (new FileOutputStream (FileDescriptor.err)), true);

  // Don't allow System objects to be made.
  private System ()
  {
  }

  // Private data.
  private static SecurityManager secman = null;
  private static Properties properties = null;
}
