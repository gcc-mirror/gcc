/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 24, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3.
 * Status:  getAvailableIDs, getDefault, getTimeZone only know about GMT.
 */

public abstract class TimeZone implements java.io.Serializable, Cloneable
{
  public static final int SHORT = 0;
  public static final int LONG = 1;

  // The fields are as specified in Sun's "Serialized Form"
  // in the JDK 1.2 beta 4 API specification.
  String ID;

  static final TimeZone zoneGMT = new SimpleTimeZone(0, "GMT");

  private static TimeZone zoneDefault;

  public TimeZone ()
  {
  }

  public abstract int getOffset (int era, int year, int month,
				 int day, int dayOfWeek, int milliseconds);

  public abstract void setRawOffset (int offsetMillis);

  public abstract int getRawOffset ();

  public String getID () { return ID; }

  public void setID (String ID) { this.ID = ID; }

  public final String getDisplayName()
  {
    return ID;  // FIXME
  }

  // public final String getDisplayName (Local locale) { ... }  FIXME

  public final String getDisplayName (boolean daylight, int style)
  {
    return ID;  // FIXME
  }

  /*
  public final String getDisplayName (boolean daylight, int style, Locale locale)
  {
    return ID;  // FIXME
  }
  */

  public abstract boolean useDaylightTime();

  public abstract boolean inDaylightTime (Date date);

  public static synchronized TimeZone getTimeZone (String ID)
  {
    int i;
    for (i = 0; i < tzIDs.length; ++i)
      {
	if (ID.equals(tzIDs[i]))
	  break;
      }
    if (i == tzIDs.length)
      return null;

    if (timeZones[i] == null)
      {
	if (ID.equals("GMT"))
	  timeZones[i] = zoneGMT;
	else
	  timeZones[i] = new SimpleTimeZone (rawOffsets[i], tzIDs[i]);
      }

    return timeZones[i];
  }

  public static String[] getAvailableIDs()
  {
    return (String[]) tzIDs.clone();
  }

  public static String[] getAvailableIDs(int rawOffset)
  {
    int first, last;

    for (first = 0; first < rawOffsets.length; ++first)
      {
	if (rawOffset == rawOffsets[first])
	  break;
      }
    if (first == rawOffsets.length)
      return new String[0];
    for (last = first + 1; last < rawOffsets.length; ++last)
      {
	if (rawOffset != rawOffsets[last])
	  break;
      }

    String[] r = new String[last - first];
    for (int i = first; i < last; ++i)
      {
	r[i - first] = tzIDs[i];
      }

    return r;
  }

  private static synchronized TimeZone setDefault()
  {
    if (zoneDefault == null)
      {
	try
	  {
	    String id = System.getProperty("user.timezone");
	    if (id != null && ! id.equals("GMT"))
	      zoneDefault = getTimeZone(id);
	  }
	catch (Exception ex)
	  {
	  }
	if (zoneDefault == null)
	  zoneDefault = zoneGMT;
      }
    return zoneDefault;
  }

  public static TimeZone getDefault()
  {
    return zoneDefault == null ? setDefault() : zoneDefault;
  }

  public static void setDefault (TimeZone zone) { zoneDefault = zone; }

  public boolean hasSameRules (TimeZone other)
  {
    return this == other;
  }

  public Object clone ()
  {
    // Just use Object's generic cloner.
    return super.clone ();
  }

  // Names of timezones.  This array is kept in parallel with
  // rawOffsets.  This list comes from the JCL 1.1 book.
  private static final String[] tzIDs =
  {
    "MIT", "HST", "AST", "PST", "PNT",
    "MST", "CST", "EST", "IET", "PRT",
    "CNT", "AGT", "BET", "CAT", "GMT",
    "ECT", "EET", "ART", "EAT", "MET",
    "NET", "PLT", "IST", "BST", "VST",
    "CTT", "JST", "ACT", "AET", "SST",
    "NST"
  };
  // This holds raw offsets in milliseconds.
  // 3600000 == 60 * 60 * 1000
  private static final int[] rawOffsets =
  {
    -11 * 3600000, -10 * 3600000, -9 * 3600000, -8 * 3600000, -7 * 3600000,
    -7 * 3600000, -6 * 3600000, -5 * 3600000, -5 * 3600000, -4 * 3600000,
    -35 * 360000, -3 * 3600000, -3 * 3600000, -1 * 3600000, 0,
    1 * 3600000, 1 * 3600000, 2 * 3600000, 3 * 3600000, 35 * 360000,
    4 * 3600000, 5 * 3600000, 55 * 360000, 6 * 3600000, 7 * 3600000,
    8 * 3600000, 9 * 3600000, 95 * 360000, 10 * 3600000, 11 * 3600000,
    12 * 3600000
  };
  // This caches all the corresponding zone objects.
  private static TimeZone[] timeZones = new TimeZone[tzIDs.length];
}
