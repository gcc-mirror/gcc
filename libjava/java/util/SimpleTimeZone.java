/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

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
 * Status:  Does not know how to figure out if daylight savings time
 *   is in effect;  hence only correct for zones without DST.
 *   No known spec for hashCode.
 */

public class SimpleTimeZone extends TimeZone
{
  // The fields are as specified in Sun's "Serialized Form"
  // in the JDK 1.2 beta 4 API specification.

  int dstSavings = 60 * 60 * 1000;

  int rawOffset;

  // int serialVersionOnStream;

  int startDay;
  int startDayOfWeek;
  int startMode;  /// Seems to be JDK 1.2 only.

  int startMonth;

  int startTime;

  int startYear;

  int endDay;

  int endDayOfWeek;

  int endMode;  // Seems to be JDK 1.2 only.

  int endMonth;

  int endTime;

  // byte[] monthLength;

  boolean useDaylight;

  public SimpleTimeZone (int rawOffset, String ID)
  {
    setID(ID);
    this.rawOffset = rawOffset;
  }

  public SimpleTimeZone (int rawOffset, String ID,
			 int startMonth, int startDay,
			 int startDayOfWeek, int startTime,
			 int endMonth, int endDay,
			 int endDayOfWeek, int endTime)
  {
    this(rawOffset, ID);
    setStartRule (startMonth, startDay, startDayOfWeek, startTime);
    setEndRule (endMonth, endDay, endDayOfWeek, endTime);
  }

  public int getRawOffset() { return rawOffset; }
  public void setRawOffset (int offsetMillis) { rawOffset = offsetMillis; }

  public int getOffset (int era, int year, int month, int day,
			int dayOfWeek, int millis)
  {
    int offset = getRawOffset();
    if (useDaylight)
      {
	if (startYear != 0
	    && (year < startYear || era == GregorianCalendar.BC))
	  return offset;
	boolean midYearSummer = startMonth < endMonth;
	if (midYearSummer ? (month < startMonth || month > endMonth)
	    : (month < startMonth && month > endMonth))
	  return offset; // Definitely not DST.
	if (midYearSummer ? (month > startMonth && month < endMonth)
	    : (month > startMonth || month < endMonth))
	  return offset + dstSavings;  // Definitely DST.
	// Now it gets more complicated.  Bail for now.
	throw new Error("not implemented - SimpleTimeZone.getOffset");
      }
    return offset;
  }

  public boolean useDaylightTime() { return useDaylight; }

  public boolean inDaylightTime(Date date)
  {
    if (! useDaylight)
      return false;
    throw new Error("not implemented - SimpleTimeZone.inDaylightTime");
  }

  public int getDSTSavings () { return dstSavings; }

  public void setDSTSavings (int millisSavedDuringDST)
  { dstSavings = millisSavedDuringDST; }

  public void setStartRule (int month, int dayOfWeekInMonth,
			    int dayOfWeek, int time)
  {
    this.startMonth = month;
    this.startDay = dayOfWeekInMonth;
    this.startDayOfWeek = dayOfWeek;
    this.startTime = time;
    this.useDaylight = true;
  }

  public void setEndRule (int month, int dayOfWeekInMonth,
			    int dayOfWeek, int time)
  {
    this.endMonth = month;
    this.endDay = dayOfWeekInMonth;
    this.endDayOfWeek = dayOfWeek;
    this.endTime = time;
    this.useDaylight = true;
  }

  public void setStartYear (int year)
  {
    this.startYear = startYear;
  }

  public boolean hasSameRules (TimeZone other)
  {
    if (this == other)
      return true;
    if (! (other instanceof SimpleTimeZone))
      return false;
    SimpleTimeZone o = (SimpleTimeZone) other;
    if (rawOffset != o.rawOffset)
      return false;
    if (useDaylight != o.useDaylight)
      return false;
    if (! useDaylight)
      return true;
    return startDay == o.startDay
      && startDayOfWeek == o.startDayOfWeek
      && startMonth == o.startMonth
      && startTime == o.startTime
      && endDay == o.endDay
      && endDayOfWeek == o.endDayOfWeek
      && endMonth == o.endMonth
      && endTime == o.endTime
      && startYear == o.startYear
      && startMode == o.startMode
      && endMode == o.endMode;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof SimpleTimeZone))
      return false;
    SimpleTimeZone other = (SimpleTimeZone) obj;
    return getID() == other.getID() && hasSameRules(other);
  }

  public Object clone ()
  {
    // We know the superclass just call's Object's generic cloner.
    return super.clone ();
  }

  public String toString ()
  {
    // The docs don't say much about how we might implement this.
    // We choose a debugging implementation.
    return ("dstSavings " + dstSavings
	    + "; rawOffset " + rawOffset
	    + "; startDay " + startDay
	    + "; startDayOfWeek " + startDayOfWeek
	    + "; startMode " + startMode
	    + "; startMonth " + startMonth
	    + "; startTime " + startTime
	    + "; startYear " + startYear
	    + "; endDay " + endDay
	    + "; endDayOfWeek " + endDayOfWeek
	    + "; endMode " + endMode
	    + "; endMonth " + endMonth
	    + "; endTime " + endTime
	    + "; useDaylight " + useDaylight);
  }

  public int hashCode ()
  {
    // FIXME - this does not folow any spec (since none is public)!
    int hash = rawOffset;
    if (useDaylight)
      hash += dstSavings + startYear + startMode + endMode
	+ startDay + startDayOfWeek + startMonth + startTime
	+ endDay + endDayOfWeek + endMonth + endTime;
    return hash;
  }
}
