/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 24, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3,
 * and "The Java Language Specification", ISBN 0-201-63451-1.
 * Status:  Unimplemented:  getAvailableLocales.
 *   No Locale knowledge.
 */

public abstract class Calendar implements java.io.Serializable, Cloneable
{
  public final static int JANUARY = 0;
  public final static int FEBRUARY = 1;
  public final static int MARCH = 2;
  public final static int APRIL = 3;
  public final static int MAY = 4;
  public final static int JUNE = 5;
  public final static int JULY = 6;
  public final static int AUGUST = 7;
  public final static int SEPTEMBER = 8;
  public final static int OCTOBER = 9;
  public final static int NOVEMBER = 10;
  public final static int DECEMBER = 11;
  public final static int UNDECIMBER = 12;

  public final static int SUNDAY = 1;
  public final static int MONDAY = 2;
  public final static int TUESDAY = 3;
  public final static int WEDNESDAY = 4;
  public final static int THURSDAY = 5;
  public final static int FRIDAY = 6;
  public final static int SATURDAY = 7;

  public final static int AM = 0;
  public final static int PM = 1;

  public final static int FIELD_COUNT = 17;

  // These constants are not docuemnted, but were determined using
  // a simple test program.
  public final static int ERA = 0;
  public final static int YEAR = 1;
  public final static int MONTH = 2;
  public final static int WEEK_OF_YEAR = 3;
  public final static int WEEK_OF_MONTH = 4;
  public final static int DATE = 5;
  public final static int DAY_OF_MONTH = 5;
  public final static int DAY_OF_YEAR = 6;
  public final static int DAY_OF_WEEK = 7;
  public final static int DAY_OF_WEEK_IN_MONTH = 8;
  public final static int AM_PM = 9;
  public final static int HOUR = 10;
  public final static int HOUR_OF_DAY = 11;
  public final static int MINUTE = 12;
  public final static int SECOND = 13;
  public final static int MILLISECOND = 14;
  public final static int ZONE_OFFSET = 15;
  public final static int DST_OFFSET = 16;

  // The fields are as specified in Sun's "Serialized Form"
  // in the JDK 1.2 beta 4 API specification.
  protected boolean areFieldsSet;
  protected int[] fields;
  private int firstDayOfWeek;
  protected boolean[] isSet;
  protected boolean isTimeSet;
  private boolean lenient;
  private int minimalDaysInFirstWeek;
  private int nextStamp;
  //private int serialVersionOnStream;
  protected long time;
  private TimeZone zone;

  protected Calendar ()
  {
    this (null, null);
  }

  protected Calendar (TimeZone zone, Locale loc)
  {
    fields = new int[FIELD_COUNT];
    isSet = new boolean[FIELD_COUNT];
    firstDayOfWeek = SUNDAY;  // Locale-dependent.  FIXME.
    this.zone = zone != null ? zone : TimeZone.getDefault();
  }

  public Object clone ()
  {
    try
      {
	return super.clone();
      }
    catch (CloneNotSupportedException ex)
      {
	throw new RuntimeException("internal error - "+ex);
      }
  }

  public static Calendar getInstance ()
  {
    return new GregorianCalendar ();
  }

  public static Calendar getInstance (TimeZone zone)
  {
    return new GregorianCalendar (zone);
  }

  public static Calendar getInstance (Locale locale)
  {
    return new GregorianCalendar (locale);
  }

  public static Calendar getInstance (TimeZone zone, Locale locale)
  {
    return new GregorianCalendar (zone, locale);
  }

  public boolean isLenient() { return lenient; }
  public void setLenient (boolean lenient) { this.lenient = lenient; }

  public int getFirstDayOfWeek ()
  {
    return firstDayOfWeek;
  }

  public void setFirstDayOfWeek (int value)
  {
    firstDayOfWeek = value;
  }

  public int getMinimalDaysInFirstWeek ()
  {
    return minimalDaysInFirstWeek;
  }

  public void setMinimalDaysInFirstWeek (int value)
  {
    minimalDaysInFirstWeek = value;
  }

  public TimeZone getTimeZone ()
  {
    return zone;
  }

  public void setTimeZone (TimeZone tz)
  {
    zone = tz;
  }

  abstract public void add(int fld, int amount);
  abstract public void roll (int fld, boolean up);

  public final void set (int year, int month, int date)
  {
    set(YEAR, year);
    set(MONTH, month);
    set(DATE, date);
  }

  public final void set (int year, int month, int date, int hour, int minute)
  {
    set(year, month, date);
    set(HOUR_OF_DAY, hour);
    set(MINUTE, minute);
  }

  public final void set (int year, int month, int date,
			 int hour, int minute, int second)
  {
    set(year, month, date, hour, minute);
    set(SECOND, second);
  }

  public final void set (int fld, int value)
  {
    if (! areFieldsSet) computeFields();
    fields[fld] = value;
    isTimeSet = false;
  }

  public final void clear (int fld)
  {
    fields[fld] = 0;
    isSet[fld] = false;
    areFieldsSet = false;
  }

  public final void clear ()
  {
    for (int fld = FIELD_COUNT;  --fld >= 0; )
      {
	fields[fld] = 0;
	isSet[fld] = false;
      }
    areFieldsSet = false;
  }

  protected void complete()
  {
    if (!isTimeSet) computeTime();
    if (!areFieldsSet) computeFields();
  }

  protected abstract void computeFields();
  protected abstract void computeTime();

  protected final int internalGet (int fld) { return fields[fld]; }

  public final int get(int fld)
  {
    complete();
    return fields[fld];
  }

  public abstract boolean after (Object cal);
  public abstract boolean before (Object cal);
  public abstract boolean equals (Object obj);

  protected long getTimeInMillis()
  {
    if (!isTimeSet) computeTime();
    return time;
  }

  public final Date getTime() { return new Date(getTimeInMillis()); }

  public final void setTime (Date date)
  {
    setTimeInMillis(date.getTime());
  }

  protected void setTimeInMillis (long millis)
  {
    time = millis;
    isTimeSet = true;
    clear();
  }

  abstract public int getMaximum(int fld);
  abstract public int getMinimum(int fld);
  abstract public int getGreatestMinimum(int fld);
  abstract public int getLeastMaximum(int fld);

  public final boolean isSet(int fld) { return isSet[fld]; }
}
