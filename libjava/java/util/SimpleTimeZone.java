/* java.util.SimpleTimeZone
   Copyright (C) 1998, 1999, 2000, 2003, 2004, 2005  Free Software Foundation, Inc.

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


/**
 * This class represents a simple time zone offset and handles
 * daylight savings.  It can only handle one daylight savings rule, so
 * it can't represent historical changes.
 *
 * This object is tightly bound to the Gregorian calendar.  It assumes
 * a regular seven days week, and the month lengths are that of the
 * Gregorian Calendar.  It can only handle daylight savings for years
 * lying in the AD era.
 *
 * @see Calendar
 * @see GregorianCalender
 * @author Jochen Hoenicke
 */
public class SimpleTimeZone extends TimeZone
{
  /**
   * The raw time zone offset in milliseconds to GMT, ignoring
   * daylight savings.
   * @serial
   */
  private int rawOffset;

  /**
   * True, if this timezone uses daylight savings, false otherwise.
   * @serial
   */
  private boolean useDaylight;

  /**
   * The daylight savings offset.  This is a positive offset in
   * milliseconds with respect to standard time.  Typically this
   * is one hour, but for some time zones this may be half an hour.
   * @serial
   * @since JDK1.1.4
   */
  private int dstSavings = 60 * 60 * 1000;

  /**
   * The first year, in which daylight savings rules applies.
   * @serial
   */
  private int startYear;
  private static final int DOM_MODE = 1;
  private static final int DOW_IN_MONTH_MODE = 2;
  private static final int DOW_GE_DOM_MODE = 3;
  private static final int DOW_LE_DOM_MODE = 4;

  /**
   * The mode of the start rule. This takes one of the following values:
   * <dl>
   * <dt>DOM_MODE (1)</dt>
   * <dd> startDay contains the day in month of the start date,
   * startDayOfWeek is unused. </dd>
   * <dt>DOW_IN_MONTH_MODE (2)</dt>
   * <dd> The startDay gives the day of week in month, and
   * startDayOfWeek the day of week.  For example startDay=2 and
   * startDayOfWeek=Calender.SUNDAY specifies that the change is on
   * the second sunday in that month.  You must make sure, that this
   * day always exists (ie. don't specify the 5th sunday).
   * </dd>
   * <dt>DOW_GE_DOM_MODE (3)</dt>
   * <dd> The start is on the first startDayOfWeek on or after
   * startDay.  For example startDay=13 and
   * startDayOfWeek=Calendar.FRIDAY specifies that the daylight
   * savings start on the first FRIDAY on or after the 13th of that
   * Month. Make sure that the change is always in the given month, or
   * the result is undefined.
   * </dd>
   * <dt>DOW_LE_DOM_MONTH (4)</dt>
   * <dd> The start is on the first startDayOfWeek on or before the
   * startDay.  Make sure that the change is always in the given
   * month, or the result is undefined.
   </dd>
   * </dl>
   * @serial */
  private int startMode;

  /**
   * The month in which daylight savings start.  This is one of the
   * constants Calendar.JANUARY, ..., Calendar.DECEMBER.
   * @serial
   */
  private int startMonth;

  /**
   * This variable can have different meanings.  See startMode for details
   * @see #startMode;
   * @serial
   */
  private int startDay;

  /**
   * This variable specifies the day of week the change takes place.  If
   * startMode == DOM_MODE, this is undefined.
   * @serial
   * @see #startMode;
   */
  private int startDayOfWeek;

  /**
   * This variable specifies the time of change to daylight savings.
   * This time is given in milliseconds after midnight local
   * standard time.
   * @serial
   */
  private int startTime;

  /**
   * This variable specifies the mode that startTime is specified in.  By
   * default it is WALL_TIME, but can also be STANDARD_TIME or UTC_TIME.  For
   * startTime, STANDARD_TIME and WALL_TIME are equivalent.
   * @serial
   */
  private int startTimeMode = WALL_TIME;

  /**
   * The month in which daylight savings ends.  This is one of the
   * constants Calendar.JANUARY, ..., Calendar.DECEMBER.
   * @serial
   */
  private int endMonth;

  /**
   * This variable gives the mode for the end of daylight savings rule.
   * It can take the same values as startMode.
   * @serial
   * @see #startMode
   */
  private int endMode;

  /**
   * This variable can have different meanings.  See startMode for details
   * @serial
   * @see #startMode;
   */
  private int endDay;

  /**
   * This variable specifies the day of week the change takes place.  If
   * endMode == DOM_MODE, this is undefined.
   * @serial
   * @see #startMode;
   */
  private int endDayOfWeek;

  /**
   * This variable specifies the time of change back to standard time.
   * This time is given in milliseconds after midnight local
   * standard time.
   * @serial
   */
  private int endTime;

  /**
   * This variable specifies the mode that endTime is specified in.  By
   * default it is WALL_TIME, but can also be STANDARD_TIME or UTC_TIME.
   * @serial
   */
  private int endTimeMode = WALL_TIME;

  /**
   * This variable points to a deprecated array from JDK 1.1.  It is
   * ignored in JDK 1.2 but streamed out for compatibility with JDK 1.1.
   * The array contains the lengths of the months in the year and is
   * assigned from a private static final field to avoid allocating
   * the array for every instance of the object.
   * Note that static final fields are not serialized.
   * @serial
   */
  private byte[] monthLength = monthArr;
  private static final byte[] monthArr = 
                                         {
                                           31, 28, 31, 30, 31, 30, 31, 31, 30,
                                           31, 30, 31
                                         };

  /**
   * The version of the serialized data on the stream.
   * <dl>
   * <dt>0 or not present on stream</dt>
   * <dd> JDK 1.1.3 or earlier, only provides this fields:
   * rawOffset, startDay, startDayOfWeek, startMonth, startTime,
   * startYear, endDay, endDayOfWeek, endMonth, endTime
   * </dd>
   * <dd> JDK 1.1.4 or later. This includes three new fields, namely
   * startMode, endMode and dstSavings.  And there is a optional section
   * as described in writeObject.
   * </dd>
   * </dl>
   *
   * XXX - JDK 1.2 Beta 4 docu states 1.1.4, but my 1.1.5 has the old
   * version.
   *
   * When streaming out this class it is always written in the latest
   * version.
   * @serial
   * @since JDK1.1.4
   */
  private int serialVersionOnStream = 2;
  private static final long serialVersionUID = -403250971215465050L;

  /**
   * Constant to indicate that start and end times are specified in standard
   * time, without adjusting for daylight savings.
   */
  public static final int STANDARD_TIME = 1;

  /**
   * Constant to indicate that start and end times are specified in wall
   * time, adjusting for daylight savings.  This is the default.
   */
  public static final int WALL_TIME = 0;

  /**
   * Constant to indicate that start and end times are specified in UTC.
   */
  public static final int UTC_TIME = 2;

  /**
   * Create a <code>SimpleTimeZone</code> with the given time offset
   * from GMT and without daylight savings.
   * @param rawOffset the time offset from GMT in milliseconds.
   * @param id The identifier of this time zone.
   */
  public SimpleTimeZone(int rawOffset, String id)
  {
    this.rawOffset = rawOffset;
    setID(id);
    useDaylight = false;
    startYear = 0;
  }

  /**
   * Create a <code>SimpleTimeZone</code> with the given time offset
   * from GMT and with daylight savings.  The start/end parameters
   * can have different meaning (replace WEEKDAY with a real day of
   * week). Only the first two meanings were supported by earlier
   * versions of jdk.
   *
   * <dl>
   * <dt><code>day &gt; 0, dayOfWeek = Calendar.WEEKDAY</code></dt>
   * <dd>The start/end of daylight savings is on the <code>day</code>-th
   * <code>WEEKDAY</code> in the given month. </dd>
   * <dt><code>day &lt; 0, dayOfWeek = Calendar.WEEKDAY</code></dt>
   * <dd>The start/end of daylight savings is on the <code>-day</code>-th
   * <code>WEEKDAY</code> counted from the <i>end</i> of the month. </dd>
   * <dt><code>day &gt; 0, dayOfWeek = 0</code></dt>
   * <dd>The start/end of daylight is on the <code>day</code>-th day of
   * the month. </dd>
   * <dt><code>day &gt; 0, dayOfWeek = -Calendar.WEEKDAY</code></dt>
   * <dd>The start/end of daylight is on the first WEEKDAY on or after
   * the <code>day</code>-th day of the month.  You must make sure that
   * this day lies in the same month. </dd>
   * <dt><code>day &lt; 0, dayOfWeek = -Calendar.WEEKDAY</code></dt>
   * <dd>The start/end of daylight is on the first WEEKDAY on or
   * <i>before</i> the <code>-day</code>-th day of the month.  You
   * must make sure that this day lies in the same month. </dd>
   * </dl>
   *
   * If you give a non existing month, a day that is zero, or too big,
   * or a dayOfWeek that is too big,  the result is undefined.
   *
   * The start rule must have a different month than the end rule.
   * This restriction shouldn't hurt for all possible time zones.
   *
   * @param rawOffset The time offset from GMT in milliseconds.
   * @param id  The identifier of this time zone.
   * @param startMonth The start month of daylight savings; use the
   * constants in Calendar.
   * @param startday A day in month or a day of week number, as
   * described above.
   * @param startDayOfWeek The start rule day of week; see above.
   * @param startTime A time in millis in standard time.
   * @param endMonth The end month of daylight savings; use the
   * constants in Calendar.
   * @param endday A day in month or a day of week number, as
   * described above.
   * @param endDayOfWeek The end rule day of week; see above.
   * @param endTime A time in millis in standard time.
   * @throws IllegalArgumentException if parameters are invalid or out of
   * range.
   */
  public SimpleTimeZone(int rawOffset, String id, int startMonth,
                        int startDayOfWeekInMonth, int startDayOfWeek,
                        int startTime, int endMonth, int endDayOfWeekInMonth,
                        int endDayOfWeek, int endTime)
  {
    this.rawOffset = rawOffset;
    setID(id);
    useDaylight = true;

    setStartRule(startMonth, startDayOfWeekInMonth, startDayOfWeek, startTime);
    setEndRule(endMonth, endDayOfWeekInMonth, endDayOfWeek, endTime);
    if (startMonth == endMonth)
      throw new IllegalArgumentException("startMonth and endMonth must be different");
    this.startYear = 0;
  }

  /**
   * This constructs a new SimpleTimeZone that supports a daylight savings
   * rule.  The parameter are the same as for the constructor above, except
   * there is the additional dstSavaings parameter.
   *
   * @param dstSavings the amount of savings for daylight savings
   * time in milliseconds.  This must be positive.
   * @since 1.2
   */
  public SimpleTimeZone(int rawOffset, String id, int startMonth,
                        int startDayOfWeekInMonth, int startDayOfWeek,
                        int startTime, int endMonth, int endDayOfWeekInMonth,
                        int endDayOfWeek, int endTime, int dstSavings)
  {
    this(rawOffset, id, startMonth, startDayOfWeekInMonth, startDayOfWeek,
         startTime, endMonth, endDayOfWeekInMonth, endDayOfWeek, endTime);

    this.dstSavings = dstSavings;
  }

  /**
   * This constructs a new SimpleTimeZone that supports a daylight savings
   * rule.  The parameter are the same as for the constructor above, except
   * there are the additional startTimeMode, endTimeMode, and dstSavings
   * parameters.
   *
   * @param startTimeMode the mode that start times are specified in.  One of
   * WALL_TIME, STANDARD_TIME, or UTC_TIME.
   * @param endTimeMode the mode that end times are specified in.  One of
   * WALL_TIME, STANDARD_TIME, or UTC_TIME.
   * @param dstSavings the amount of savings for daylight savings
   * time in milliseconds.  This must be positive.
   * @throws IllegalArgumentException if parameters are invalid or out of
   * range.
   * @since 1.4
   */
  public SimpleTimeZone(int rawOffset, String id, int startMonth,
                        int startDayOfWeekInMonth, int startDayOfWeek,
                        int startTime, int startTimeMode, int endMonth,
                        int endDayOfWeekInMonth, int endDayOfWeek,
                        int endTime, int endTimeMode, int dstSavings)
  {
    this.rawOffset = rawOffset;
    setID(id);
    useDaylight = true;

    if (startTimeMode < WALL_TIME || startTimeMode > UTC_TIME)
      throw new IllegalArgumentException("startTimeMode must be one of WALL_TIME, STANDARD_TIME, or UTC_TIME");
    if (endTimeMode < WALL_TIME || endTimeMode > UTC_TIME)
      throw new IllegalArgumentException("endTimeMode must be one of WALL_TIME, STANDARD_TIME, or UTC_TIME");
    this.startTimeMode = startTimeMode;
    this.endTimeMode = endTimeMode;

    setStartRule(startMonth, startDayOfWeekInMonth, startDayOfWeek, startTime);
    setEndRule(endMonth, endDayOfWeekInMonth, endDayOfWeek, endTime);
    if (startMonth == endMonth)
      throw new IllegalArgumentException("startMonth and endMonth must be different");
    this.startYear = 0;

    this.dstSavings = dstSavings;
  }

  /**
   * Sets the first year, where daylight savings applies.  The daylight
   * savings rule never apply for years in the BC era.  Note that this
   * is gregorian calendar specific.
   * @param year the start year.
   */
  public void setStartYear(int year)
  {
    startYear = year;
    useDaylight = true;
  }

  /**
   * Checks if the month, day, dayOfWeek arguments are in range and
   * returns the mode of the rule.
   * @param month the month parameter as in the constructor
   * @param day the day parameter as in the constructor
   * @param dayOfWeek the day of week parameter as in the constructor
   * @return the mode of this rule see startMode.
   * @exception IllegalArgumentException if parameters are out of range.
   * @see #SimpleTimeZone(int, String, int, int, int, int, int, int, int, int)
   * @see #startMode
   */
  private int checkRule(int month, int day, int dayOfWeek)
  {
    if (month < 0 || month > 11)
      throw new IllegalArgumentException("month out of range");

    int daysInMonth = getDaysInMonth(month, 1);
    if (dayOfWeek == 0)
      {
	if (day <= 0 || day > daysInMonth)
	  throw new IllegalArgumentException("day out of range");
	return DOM_MODE;
      }
    else if (dayOfWeek > 0)
      {
	if (Math.abs(day) > (daysInMonth + 6) / 7)
	  throw new IllegalArgumentException("dayOfWeekInMonth out of range");
	if (dayOfWeek > Calendar.SATURDAY)
	  throw new IllegalArgumentException("dayOfWeek out of range");
	return DOW_IN_MONTH_MODE;
      }
    else
      {
	if (day == 0 || Math.abs(day) > daysInMonth)
	  throw new IllegalArgumentException("day out of range");
	if (dayOfWeek < -Calendar.SATURDAY)
	  throw new IllegalArgumentException("dayOfWeek out of range");
	if (day < 0)
	  return DOW_LE_DOM_MODE;
	else
	  return DOW_GE_DOM_MODE;
      }
  }

  /**
   * Sets the daylight savings start rule.  You must also set the
   * end rule with <code>setEndRule</code> or the result of
   * getOffset is undefined.  For the parameters see the ten-argument
   * constructor above.
   *
   * @param month The month where daylight savings start, zero
   * based.  You should use the constants in Calendar.
   * @param day A day of month or day of week in month.
   * @param dayOfWeek The day of week where daylight savings start.
   * @param time The time in milliseconds standard time where daylight
   * savings start.
   * @see SimpleTimeZone
   */
  public void setStartRule(int month, int day, int dayOfWeek, int time)
  {
    this.startMode = checkRule(month, day, dayOfWeek);
    this.startMonth = month;
    this.startDay = day;
    this.startDayOfWeek = Math.abs(dayOfWeek);
    if (this.startTimeMode == WALL_TIME || this.startTimeMode == STANDARD_TIME)
      this.startTime = time;
    else
      // Convert from UTC to STANDARD
      this.startTime = time + this.rawOffset;
    useDaylight = true;
  }

  /**
   * Sets the daylight savings start rule.  You must also set the
   * end rule with <code>setEndRule</code> or the result of
   * getOffset is undefined.  For the parameters see the ten-argument
   * constructor above.
   *
   * Note that this API isn't incredibly well specified.  It appears that the
   * after flag must override the parameters, since normally, the day and
   * dayofweek can select this.  I.e., if day < 0 and dayOfWeek < 0, on or
   * before mode is chosen.  But if after == true, this implementation
   * overrides the signs of the other arguments.  And if dayOfWeek == 0, it
   * falls back to the behavior in the other APIs.  I guess this should be
   * checked against Sun's implementation.
   *
   * @param month The month where daylight savings start, zero
   * based.  You should use the constants in Calendar.
   * @param day A day of month or day of week in month.
   * @param dayOfWeek The day of week where daylight savings start.
   * @param time The time in milliseconds standard time where daylight
   * savings start.
   * @param after If true, day and dayOfWeek specify first day of week on or
   * after day, else first day of week on or before.
   * @since 1.2
   * @see SimpleTimeZone
   */
  public void setStartRule(int month, int day, int dayOfWeek, int time,
                           boolean after)
  {
    // FIXME: XXX: Validate that checkRule and offset processing work with on
    // or before mode.
    this.startDay = after ? Math.abs(day) : -Math.abs(day);
    this.startDayOfWeek = after ? Math.abs(dayOfWeek) : -Math.abs(dayOfWeek);
    this.startMode = (dayOfWeek != 0)
                     ? (after ? DOW_GE_DOM_MODE : DOW_LE_DOM_MODE)
                     : checkRule(month, day, dayOfWeek);
    this.startDay = Math.abs(this.startDay);
    this.startDayOfWeek = Math.abs(this.startDayOfWeek);

    this.startMonth = month;

    if (this.startTimeMode == WALL_TIME || this.startTimeMode == STANDARD_TIME)
      this.startTime = time;
    else
      // Convert from UTC to STANDARD
      this.startTime = time + this.rawOffset;
    useDaylight = true;
  }

  /**
   * Sets the daylight savings start rule.  You must also set the
   * end rule with <code>setEndRule</code> or the result of
   * getOffset is undefined.  For the parameters see the ten-argument
   * constructor above.
   *
   * @param month The month where daylight savings start, zero
   * based.  You should use the constants in Calendar.
   * @param day A day of month or day of week in month.
   * @param time The time in milliseconds standard time where daylight
   * savings start.
   * @see SimpleTimeZone
   * @since 1.2
   */
  public void setStartRule(int month, int day, int time)
  {
    setStartRule(month, day, 0, time);
  }

  /**
   * Sets the daylight savings end rule.  You must also set the
   * start rule with <code>setStartRule</code> or the result of
   * getOffset is undefined. For the parameters see the ten-argument
   * constructor above.
   *
   * @param month The end month of daylight savings.
   * @param day A day in month, or a day of week in month.
   * @param dayOfWeek A day of week, when daylight savings ends.
   * @param time A time in millis in standard time.
   * @see #setStartRule
   */
  public void setEndRule(int month, int day, int dayOfWeek, int time)
  {
    this.endMode = checkRule(month, day, dayOfWeek);
    this.endMonth = month;
    this.endDay = day;
    this.endDayOfWeek = Math.abs(dayOfWeek);
    if (this.endTimeMode == WALL_TIME)
      this.endTime = time;
    else if (this.endTimeMode == STANDARD_TIME)
      // Convert from STANDARD to DST
      this.endTime = time + this.dstSavings;
    else
      // Convert from UTC to DST
      this.endTime = time + this.rawOffset + this.dstSavings;
    useDaylight = true;
  }

  /**
   * Sets the daylight savings end rule.  You must also set the
   * start rule with <code>setStartRule</code> or the result of
   * getOffset is undefined. For the parameters see the ten-argument
   * constructor above.
   *
   * Note that this API isn't incredibly well specified.  It appears that the
   * after flag must override the parameters, since normally, the day and
   * dayofweek can select this.  I.e., if day < 0 and dayOfWeek < 0, on or
   * before mode is chosen.  But if after == true, this implementation
   * overrides the signs of the other arguments.  And if dayOfWeek == 0, it
   * falls back to the behavior in the other APIs.  I guess this should be
   * checked against Sun's implementation.
   *
   * @param month The end month of daylight savings.
   * @param day A day in month, or a day of week in month.
   * @param dayOfWeek A day of week, when daylight savings ends.
   * @param time A time in millis in standard time.
   * @param after If true, day and dayOfWeek specify first day of week on or
   * after day, else first day of week on or before.
   * @since 1.2
   * @see #setStartRule
   */
  public void setEndRule(int month, int day, int dayOfWeek, int time,
                         boolean after)
  {
    // FIXME: XXX: Validate that checkRule and offset processing work with on
    // or before mode.
    this.endDay = after ? Math.abs(day) : -Math.abs(day);
    this.endDayOfWeek = after ? Math.abs(dayOfWeek) : -Math.abs(dayOfWeek);
    this.endMode = (dayOfWeek != 0)
                   ? (after ? DOW_GE_DOM_MODE : DOW_LE_DOM_MODE)
                   : checkRule(month, day, dayOfWeek);
    this.endDay = Math.abs(this.endDay);
    this.endDayOfWeek = Math.abs(endDayOfWeek);

    this.endMonth = month;

    if (this.endTimeMode == WALL_TIME)
      this.endTime = time;
    else if (this.endTimeMode == STANDARD_TIME)
      // Convert from STANDARD to DST
      this.endTime = time + this.dstSavings;
    else
      // Convert from UTC to DST
      this.endTime = time + this.rawOffset + this.dstSavings;
    useDaylight = true;
  }

  /**
   * Sets the daylight savings end rule.  You must also set the
   * start rule with <code>setStartRule</code> or the result of
   * getOffset is undefined. For the parameters see the ten-argument
   * constructor above.
   *
   * @param month The end month of daylight savings.
   * @param day A day in month, or a day of week in month.
   * @param dayOfWeek A day of week, when daylight savings ends.
   * @param time A time in millis in standard time.
   * @see #setStartRule
   */
  public void setEndRule(int month, int day, int time)
  {
    setEndRule(month, day, 0, time);
  }

  /**
   * Gets the time zone offset, for current date, modified in case of
   * daylight savings.  This is the offset to add to UTC to get the local
   * time.
   *
   * In the standard JDK the results given by this method may result in
   * inaccurate results at the end of February or the beginning of March.
   * To avoid this, you should use Calendar instead:
   * <code>offset = cal.get(Calendar.ZONE_OFFSET)
   * + cal.get(Calendar.DST_OFFSET);</code>
   *
   * This version doesn't suffer this inaccuracy.
   *
   * The arguments don't follow the approach for setting start and end rules.
   * The day must be a positive number and dayOfWeek must be a positive value
   * from Calendar.  dayOfWeek is redundant, but must match the other values
   * or an inaccurate result may be returned.
   *
   * @param era the era of the given date
   * @param year the year of the given date
   * @param month the month of the given date, 0 for January.
   * @param day the day of month
   * @param dayOfWeek the day of week; this must match the other fields.
   * @param millis the millis in the day (in local standard time)
   * @return the time zone offset in milliseconds.
   * @throws IllegalArgumentException if arguments are incorrect.
   */
  public int getOffset(int era, int year, int month, int day, int dayOfWeek,
                       int millis)
  {
    int daysInMonth = getDaysInMonth(month, year);
    if (day < 1 || day > daysInMonth)
      throw new IllegalArgumentException("day out of range");
    if (dayOfWeek < Calendar.SUNDAY || dayOfWeek > Calendar.SATURDAY)
      throw new IllegalArgumentException("dayOfWeek out of range");
    if (month < Calendar.JANUARY || month > Calendar.DECEMBER)
      throw new IllegalArgumentException("month out of range:" + month);

    // This method is called by Calendar, so we mustn't use that class.
    int daylightSavings = 0;
    if (useDaylight && era == GregorianCalendar.AD && year >= startYear)
      {
	// This does only work for Gregorian calendars :-(
	// This is mainly because setStartYear doesn't take an era.
	boolean afterStart = ! isBefore(year, month, day, dayOfWeek, millis,
	                                startMode, startMonth, startDay,
	                                startDayOfWeek, startTime);
	boolean beforeEnd = isBefore(year, month, day, dayOfWeek,
				     millis + dstSavings,
	                             endMode, endMonth, endDay, endDayOfWeek,
	                             endTime);

	if (startMonth < endMonth)
	  // use daylight savings, if the date is after the start of
	  // savings, and before the end of savings.
	  daylightSavings = afterStart && beforeEnd ? dstSavings : 0;
	else
	  // use daylight savings, if the date is before the end of
	  // savings, or after the start of savings.
	  daylightSavings = beforeEnd || afterStart ? dstSavings : 0;
      }
    return rawOffset + daylightSavings;
  }

  /**
   * Returns the time zone offset to GMT in milliseconds, ignoring
   * day light savings.
   * @return the time zone offset.
   */
  public int getRawOffset()
  {
    return rawOffset;
  }

  /**
   * Sets the standard time zone offset to GMT.
   * @param rawOffset The time offset from GMT in milliseconds.
   */
  public void setRawOffset(int rawOffset)
  {
    this.rawOffset = rawOffset;
  }

  /**
   * Gets the daylight savings offset.  This is a positive offset in
   * milliseconds with respect to standard time.  Typically this
   * is one hour, but for some time zones this may be half an our.
   * @return the daylight savings offset in milliseconds.
   *
   * @since 1.2
   */
  public int getDSTSavings()
  {
    return dstSavings;
  }

  /**
   * Sets the daylight savings offset.  This is a positive offset in
   * milliseconds with respect to standard time.
   *
   * @param dstSavings the daylight savings offset in milliseconds.
   *
   * @since 1.2
   */
  public void setDSTSavings(int dstSavings)
  {
    if (dstSavings <= 0)
      throw new IllegalArgumentException("illegal value for dstSavings");

    this.dstSavings = dstSavings;
  }

  /**
   * Returns if this time zone uses daylight savings time.
   * @return true, if we use daylight savings time, false otherwise.
   */
  public boolean useDaylightTime()
  {
    return useDaylight;
  }

  /**
   * Returns the number of days in the given month.
   * Uses gregorian rules prior to 1582 (The default and earliest cutover)
   * @param month The month, zero based; use one of the Calendar constants.
   * @param year  The year.
   */
  private int getDaysInMonth(int month, int year)
  {
    if (month == Calendar.FEBRUARY)
      {
	if ((year & 3) != 0)
	  return 28;

	// Assume default Gregorian cutover, 
	// all years prior to this must be Julian
	if (year < 1582)
	  return 29;

	// Gregorian rules 
	return ((year % 100) != 0 || (year % 400) == 0) ? 29 : 28;
      }
    else
      return monthArr[month];
  }

  /**
   * Checks if the date given in calXXXX, is before the change between
   * dst and standard time.
   * @param calYear the year of the date to check (for leap day checking).
   * @param calMonth the month of the date to check.
   * @param calDay the day of month of the date to check.
   * @param calDayOfWeek the day of week of the date to check.
   * @param calMillis the millis of day of the date to check (standard time).
   * @param mode  the change mode; same semantic as startMode.
   * @param month the change month; same semantic as startMonth.
   * @param day   the change day; same semantic as startDay.
   * @param dayOfWeek the change day of week;
   * @param millis the change time in millis since midnight standard time.
   * same semantic as startDayOfWeek.
   * @return true, if cal is before the change, false if cal is on
   * or after the change.
   */
  private boolean isBefore(int calYear, int calMonth, int calDayOfMonth,
                           int calDayOfWeek, int calMillis, int mode,
                           int month, int day, int dayOfWeek, int millis)
  {
    // This method is called by Calendar, so we mustn't use that class.
    // We have to do all calculations by hand.
    // check the months:
    // XXX - this is not correct:
    // for the DOW_GE_DOM and DOW_LE_DOM modes the change date may
    // be in a different month.
    if (calMonth != month)
      return calMonth < month;

    // check the day:
    switch (mode)
      {
      case DOM_MODE:
	if (calDayOfMonth != day)
	  return calDayOfMonth < day;
	break;
      case DOW_IN_MONTH_MODE:
        {
	  // This computes the day of month of the day of type
	  // "dayOfWeek" that lies in the same (sunday based) week as cal.
	  calDayOfMonth += (dayOfWeek - calDayOfWeek);

	  // Now we convert it to 7 based number (to get a one based offset
	  // after dividing by 7).  If we count from the end of the
	  // month, we get want a -7 based number counting the days from 
	  // the end:
	  if (day < 0)
	    calDayOfMonth -= getDaysInMonth(calMonth, calYear) + 7;
	  else
	    calDayOfMonth += 6;

	  //  day > 0                    day < 0
	  //  S  M  T  W  T  F  S        S  M  T  W  T  F  S
	  //     7  8  9 10 11 12         -36-35-34-33-32-31
	  // 13 14 15 16 17 18 19      -30-29-28-27-26-25-24
	  // 20 21 22 23 24 25 26      -23-22-21-20-19-18-17
	  // 27 28 29 30 31 32 33      -16-15-14-13-12-11-10
	  // 34 35 36                   -9 -8 -7
	  // Now we calculate the day of week in month:
	  int week = calDayOfMonth / 7;

	  //  day > 0                    day < 0
	  //  S  M  T  W  T  F  S        S  M  T  W  T  F  S
	  //     1  1  1  1  1  1          -5 -5 -4 -4 -4 -4
	  //  1  2  2  2  2  2  2       -4 -4 -4 -3 -3 -3 -3
	  //  2  3  3  3  3  3  3       -3 -3 -3 -2 -2 -2 -2
	  //  3  4  4  4  4  4  4       -2 -2 -2 -1 -1 -1 -1
	  //  4  5  5                   -1 -1 -1
	  if (week != day)
	    return week < day;

	  if (calDayOfWeek != dayOfWeek)
	    return calDayOfWeek < dayOfWeek;

	  // daylight savings starts/ends  on the given day.
	  break;
        }
      case DOW_LE_DOM_MODE:
	// The greatest sunday before or equal December, 12
	// is the same as smallest sunday after or equal December, 6.
	day = Math.abs(day) - 6;
      case DOW_GE_DOM_MODE:
	// Calculate the day of month of the day of type
	// "dayOfWeek" that lies before (or on) the given date.
	calDayOfMonth -= (calDayOfWeek < dayOfWeek ? 7 : 0) + calDayOfWeek
	- dayOfWeek;
	if (calDayOfMonth < day)
	  return true;
	if (calDayOfWeek != dayOfWeek || calDayOfMonth >= day + 7)
	  return false;

	// now we have the same day
	break;
      }

    // the millis decides:
    return (calMillis < millis);
  }

  /**
   * Determines if the given date is in daylight savings time.
   * @return true, if it is in daylight savings time, false otherwise.
   */
  public boolean inDaylightTime(Date date)
  {
    Calendar cal = Calendar.getInstance(this);
    cal.setTime(date);
    return (cal.get(Calendar.DST_OFFSET) != 0);
  }

  /**
   * Generates the hashCode for the SimpleDateFormat object.  It is
   * the rawOffset, possibly, if useDaylightSavings is true, xored
   * with startYear, startMonth, startDayOfWeekInMonth, ..., endTime.
   */
  public synchronized int hashCode()
  {
    return rawOffset
           ^ (useDaylight
              ? startMonth ^ startDay ^ startDayOfWeek ^ startTime ^ endMonth
              ^ endDay ^ endDayOfWeek ^ endTime : 0);
  }

  public synchronized boolean equals(Object o)
  {
    if (this == o)
      return true;
    if (! (o instanceof SimpleTimeZone))
      return false;
    SimpleTimeZone zone = (SimpleTimeZone) o;
    if (zone.hashCode() != hashCode() || ! getID().equals(zone.getID())
        || rawOffset != zone.rawOffset || useDaylight != zone.useDaylight)
      return false;
    if (! useDaylight)
      return true;
    return (startYear == zone.startYear && startMonth == zone.startMonth
           && startDay == zone.startDay
           && startDayOfWeek == zone.startDayOfWeek
           && startTime == zone.startTime
           && startTimeMode == zone.startTimeMode && endMonth == zone.endMonth
           && endDay == zone.endDay && endDayOfWeek == zone.endDayOfWeek
           && endTime == zone.endTime && endTimeMode == zone.endTimeMode);
  }

  /**
   * Test if the other time zone uses the same rule and only
   * possibly differs in ID.  This implementation for this particular
   * class will return true if the other object is a SimpleTimeZone,
   * the raw offsets and useDaylight are identical and if useDaylight
   * is true, also the start and end datas are identical.
   * @return true if this zone uses the same rule.
   */
  public boolean hasSameRules(TimeZone other)
  {
    if (this == other)
      return true;
    if (! (other instanceof SimpleTimeZone))
      return false;
    SimpleTimeZone zone = (SimpleTimeZone) other;
    if (zone.hashCode() != hashCode() || rawOffset != zone.rawOffset
        || useDaylight != zone.useDaylight)
      return false;
    if (! useDaylight)
      return true;
    return (startYear == zone.startYear && startMonth == zone.startMonth
           && startDay == zone.startDay
           && startDayOfWeek == zone.startDayOfWeek
           && startTime == zone.startTime
           && startTimeMode == zone.startTimeMode && endMonth == zone.endMonth
           && endDay == zone.endDay && endDayOfWeek == zone.endDayOfWeek
           && endTime == zone.endTime && endTimeMode == zone.endTimeMode);
  }

  /**
   * Returns a string representation of this SimpleTimeZone object.
   * @return a string representation of this SimpleTimeZone object.
   */
  public String toString()
  {
    // the test for useDaylight is an incompatibility to jdk1.2, but
    // I think this shouldn't hurt.
    return getClass().getName() + "[" + "id=" + getID() + ",offset="
           + rawOffset + ",dstSavings=" + dstSavings + ",useDaylight="
           + useDaylight
           + (useDaylight
              ? ",startYear=" + startYear + ",startMode=" + startMode
              + ",startMonth=" + startMonth + ",startDay=" + startDay
              + ",startDayOfWeek=" + startDayOfWeek + ",startTime="
              + startTime + ",startTimeMode=" + startTimeMode + ",endMode="
              + endMode + ",endMonth=" + endMonth + ",endDay=" + endDay
              + ",endDayOfWeek=" + endDayOfWeek + ",endTime=" + endTime
              + ",endTimeMode=" + endTimeMode : "") + "]";
  }

  /**
   * Reads a serialized simple time zone from stream.
   * @see #writeObject
   */
  private void readObject(java.io.ObjectInputStream input)
    throws java.io.IOException, ClassNotFoundException
  {
    input.defaultReadObject();
    if (serialVersionOnStream == 0)
      {
	// initialize the new fields to default values.
	dstSavings = 60 * 60 * 1000;
	endMode = DOW_IN_MONTH_MODE;
	startMode = DOW_IN_MONTH_MODE;
	startTimeMode = WALL_TIME;
	endTimeMode = WALL_TIME;
	serialVersionOnStream = 2;
      }
    else
      {
	int length = input.readInt();
	byte[] byteArray = new byte[length];
	input.read(byteArray, 0, length);
	if (length >= 4)
	  {
	    // Lets hope that Sun does extensions to the serialized
	    // form in a sane manner.
	    startDay = byteArray[0];
	    startDayOfWeek = byteArray[1];
	    endDay = byteArray[2];
	    endDayOfWeek = byteArray[3];
	  }
      }
  }

  /**
   * Serializes this object to a stream.  @serialdata The object is
   * first written in the old JDK 1.1 format, so that it can be read
   * by by the old classes.  This means, that the
   * <code>start/endDay(OfWeek)</code>-Fields are written in the
   * DOW_IN_MONTH_MODE rule, since this was the only supported rule
   * in 1.1.
   *
   * In the optional section, we write first the length of an byte
   * array as int and afterwards the byte array itself.  The byte
   * array contains in this release four elements, namely the real
   * startDay, startDayOfWeek endDay, endDayOfWeek in that Order.
   * These fields are needed, because for compatibility reasons only
   * approximative values are written to the required section, as
   * described above.
   */
  private void writeObject(java.io.ObjectOutputStream output)
    throws java.io.IOException
  {
    byte[] byteArray = new byte[]
                       {
                         (byte) startDay, (byte) startDayOfWeek, (byte) endDay,
                         (byte) endDayOfWeek
                       };

    /* calculate the approximation for JDK 1.1 */
    switch (startMode)
      {
      case DOM_MODE:
	startDayOfWeek = Calendar.SUNDAY; // random day of week

      // fall through
      case DOW_GE_DOM_MODE:
      case DOW_LE_DOM_MODE:
	startDay = (startDay + 6) / 7;
      }
    switch (endMode)
      {
      case DOM_MODE:
	endDayOfWeek = Calendar.SUNDAY;

      // fall through
      case DOW_GE_DOM_MODE:
      case DOW_LE_DOM_MODE:
	endDay = (endDay + 6) / 7;
      }

    // the required part:
    output.defaultWriteObject();
    // the optional part:
    output.writeInt(byteArray.length);
    output.write(byteArray, 0, byteArray.length);
  }
}
