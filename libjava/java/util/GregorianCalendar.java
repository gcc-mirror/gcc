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
 * Status:  "leniency" is not handled, and neither is roll-over in
 *   add and roll.  This is partly because of unclear specification.
 *   hashCode has no spec.
 */

public class GregorianCalendar extends Calendar {
  public static final int BC = 0;
  public static final int AD = 1;

  // The fields are as specified in Sun's "Serialized Form"
  // in the JDK 1.2 beta 4 API specification.
  // Value from a simple test program (getGregorianChange.getTime()).
  long gregorianCutover = -12219292800000L;

  private final static int[] mins = {
    0 /* ERA */,
    1 /* YEAR */,
    0 /* MONTH */,
    0 /* WEEK_OF_YEAR */,
    0 /* WEEK_OF_MONTH */,
    1 /* DATE */,
    1 /* DAY_OF_YEAR */,
    1 /* DAY_OF_WEEK */,
    -1 /* DAY_OF_WEEK_IN_MONTH */,
    0 /* AM_PM */,
    0 /* HOUR */,
    0 /* HOUR_OF_DAY */,
    0 /* MINUTE */,
    0 /* SECOND */,
    0 /* MILLISECOND */,
    -43200000 /* ZONE_OFFSET */,
    0 /* DST_OFFSET */
  };

  private final static int[] maxs = {
    1 /* ERA */,
    5000000 /* YEAR */,
    11 /* MONTH */,
    54 /* WEEK_OF_YEAR */,
    6 /* WEEK_OF_MONTH */,
    31 /* DATE */,
    366 /* DAY_OF_YEAR */,
    7 /* DAY_OF_WEEK */,
    6 /* DAY_OF_WEEK_IN_MONTH */,
    1 /* AM_PM */,
    12 /* HOUR */,
    23 /* HOUR_OF_DAY */,
    59 /* MINUTE */,
    59 /* SECOND */,
    999 /* MILLISECOND */,
    43200000 /* ZONE_OFFSET */,
    3600000 /* DST_OFFSET */
  };

  private final static int[] leastMaximums = {
    1 /* ERA */,
    5000000 /* YEAR */,
    11 /* MONTH */,
    53 /* WEEK_OF_YEAR */,
    6 /* WEEK_OF_MONTH */,
    28 /* DATE */,
    365 /* DAY_OF_YEAR */,
    7 /* DAY_OF_WEEK */,
    4 /* DAY_OF_WEEK_IN_MONTH */,
    1 /* AM_PM */,
    11 /* HOUR */,
    23 /* HOUR_OF_DAY */,
    59 /* MINUTE */,
    59 /* SECOND */,
    999 /* MILLISECOND */,
    43200000 /* ZONE_OFFSET */,
    3600000 /* DST_OFFSET */
  };

  public GregorianCalendar ()
  {
    this(null, null);
  }

  public GregorianCalendar (TimeZone zone)
  {
    this (zone, null);
  }

  public GregorianCalendar (Locale locale)
  {
    this (null, locale);
  }

  public GregorianCalendar (TimeZone zone, Locale locale)
  {
    super (zone, locale);
    setDefaultTime ();
  }

  public GregorianCalendar (int year, int month, int date)
  {
    this((TimeZone) null);
    setDefaultTime ();
    set (year, month, date);
  }

  public GregorianCalendar (int year, int month, int date,
			    int hour, int minute)
  {
    this((TimeZone) null);
    setDefaultTime ();
    set (year, month, date, hour, minute);
  }

  public GregorianCalendar (int year, int month, int date,
			    int hour, int minute, int second)
  {
    this((TimeZone) null);
    setDefaultTime ();
    set (year, month, date, hour, minute, second);
  }

  private final void setDefaultTime ()
  {
    setTimeInMillis (System.currentTimeMillis());
  }

  public int getMinimum(int calfield) { return mins[calfield]; }
  public int getGreatestMinimum(int calfield) { return mins[calfield]; }
  public int getMaximum(int calfield) { return maxs[calfield]; }
  public int getLeastMaximum(int calfield) { return leastMaximums[calfield]; }

  protected native void computeFields();

  protected native void computeTime();

  public void add (int fld, int amount)
  {
    if (fld >= ZONE_OFFSET)
      throw new IllegalArgumentException("bad field to add");
    fields[fld] += amount;
    adjust(fld);
  }

  public void roll (int fld, boolean up)
  {
    if (fld >= ZONE_OFFSET)
      throw new IllegalArgumentException("bad field to roll");

    int old = fields[fld];
    if (up)
      {
	fields[fld] = old == getMaximum(fld) ? getMinimum(fld)
	  : old + 1;
      }
    else
      {
	fields[fld] = old == getMinimum(fld) ? getMaximum(fld)
	  : old - 1;
      }
  }

  private void adjust (int fld)
  {
    int value = fields[fld];
    int radix = maxs[fld] + 1;
    switch (fld)
      {
      case MONTH:
      case SECOND:
      case MILLISECOND:
	if (value >= radix)
	  {
	    int next = value / radix;
	    fields[fld] = value - radix * next;
	    fields[fld - 1] += next;
	    adjust(fld - 1);
	  }
	else if (value < 0) // min[fld]
	  {
	    int next = (value - radix - 1) / radix;
	    fields[fld] = value - radix * next;
	    fields[fld - 1] += next;
            adjust(fld - 1);
	  }
	break;
      }
  }

  public final Date getGregorianChange() { return new Date(gregorianCutover); }
  public void setGregorianChange (Date date)
  { gregorianCutover = date.getTime(); }

  public boolean isLeapYear(int year)
  {
    if ((year % 4) != 0)
      return false;
    if ((year % 100) != 0 || (year % 400) == 0)
      return true;
    // year divisible by 100 but not 400.
    GregorianCalendar date = new GregorianCalendar(year, FEBRUARY, 28);
    return gregorianCutover < date.getTimeInMillis();
  }

  public boolean after (Object cal)
  {
    return cal instanceof Calendar
      && getTimeInMillis() > ((Calendar) cal).getTimeInMillis();
  }

  public boolean before (Object cal)
  {
    return cal instanceof Calendar
      && getTimeInMillis() < ((Calendar) cal).getTimeInMillis();
  }

  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof GregorianCalendar))
      return false;
    GregorianCalendar other = (GregorianCalendar) obj;

    for (int i = FIELD_COUNT;  --i >= 0; )
      {
	boolean set = isSet[i];
	if (set != other.isSet[i]
	    || (set && fields[i] != other.fields[i]))
	  return false;
      }
    if (areFieldsSet != other.areFieldsSet
	|| isTimeSet != other.isTimeSet
	|| (isTimeSet && time != other.time)
	|| getFirstDayOfWeek() != other.getFirstDayOfWeek()
	|| getMinimalDaysInFirstWeek() != other.getMinimalDaysInFirstWeek()
	|| isLenient() != other.isLenient()
	|| ! getTimeZone().equals(other.getTimeZone()))
      return false;
    return true;
  }

  public int hashCode ()
  {
    int hashcode = 0;
    for (int i = FIELD_COUNT;  --i >= 0; )
      {
	if (isSet[i])
	  hashcode += 37 * fields[i];
      }
    if (isTimeSet)
      hashcode += 89 * time;
    return hashcode;
  }
}
