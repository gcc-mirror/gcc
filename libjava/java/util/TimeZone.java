/* Copyright (C) 1998, 1999  Cygnus Solutions

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

  public static TimeZone getTimeZone (String ID)
  {
    return zoneGMT;  // FIXME
  }

  public static String[] getAvailableIDs()
  { // FIXME - only knows about GMT
    String[] zones = new String[1];
    zones[0] = "GMT";
    return zones;
  }

  public static String[] getAvailableIDs(int rawOffset)
  {
    return rawOffset == 0 ? getAvailableIDs() : new String[0];  // FIXME
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

  // public Object clone ();
}
