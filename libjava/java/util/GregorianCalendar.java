/* java.util.GregorianCalendar
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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
 * This class represents the Gregorian calendar, that is used in most
 * countries all over the world.  It does also handle the Julian calendar
 * for dates smaller than the date of the change to the Gregorian calendar.
 * This change date is different from country to country, you can set it with
 * <code>setGregorianChange</code>
 *
 * The Gregorian calendar differs from the Julian calendar by a different
 * leap year rule (no leap year every 100 years, except if year is divisible
 * by 400).  The non existing days that were omited when the change took
 * place are interpreted as gregorian date
 *
 * There are to eras available for the Gregorian calendar, namely BC and AD.
 *
 * @see Calendar
 * @see TimeZone
 */
public class GregorianCalendar extends Calendar
{
  /**
   * Constant representing the era BC (before Christ).
   */
  public static final int BC = 0;
  
  /**
   * Constant representing the era AD (Anno Domini).
   */
  public static final int AD = 1;

  /**
   * The point at which the Gregorian calendar rules were used.
   * This is locale dependent; the default for most catholic
   * countries is midnight (UTC) on October 5, 1582 (Julian),
   * or October 15, 1582 (Gregorian).
   */
  private long gregorianCutover;

  static final long serialVersionUID = -8125100834729963327L;

  /**
   * The name of the resource bundle.
   */
  private static final String bundleName = "gnu.java.locale.Calendar";

  /**
   * Constructs a new GregorianCalender representing the current
   * time, using the default time zone and the default locale.  
   */
  public GregorianCalendar()
  {
    this(TimeZone.getDefault(), Locale.getDefault());
  }
  
  /**
   * Constructs a new GregorianCalender representing the current
   * time, using the specified time zone and the default locale.  
   * @param zone a time zone.
   */
  public GregorianCalendar(TimeZone zone)
  {
    this(zone, Locale.getDefault());
  }
  
  /**
   * Constructs a new GregorianCalender representing the current
   * time, using the default time zone and the specified locale.  
   * @param locale a locale.
   */
  public GregorianCalendar(Locale locale)
  {
    this(TimeZone.getDefault(), locale);
  }

  /**
   * Constructs a new GregorianCalender representing the current
   * time with the given time zone and the given locale.
   * @param zone a time zone.  
   * @param locale a locale.  
   */
  public GregorianCalendar(TimeZone zone, Locale locale)
  {
    super(zone, locale);
    ResourceBundle rb = ResourceBundle.getBundle(bundleName, locale);
    gregorianCutover = ((Date) rb.getObject("gregorianCutOver")).getTime();
    setTimeInMillis(System.currentTimeMillis());
  }

  /**
   * Constructs a new GregorianCalendar representing midnight on the
   * given date with the default time zone and locale.
   * @param year corresponds to the YEAR time field.
   * @param month corresponds to the MONTH time field.
   * @param day corresponds to the DAY time field.
   */
  public GregorianCalendar(int year, int month, int day)
  {
    super();
    set(year, month, day);
  }

  /**
   * Constructs a new GregorianCalendar representing midnight on the
   * given date with the default time zone and locale.
   * @param year corresponds to the YEAR time field.
   * @param month corresponds to the MONTH time field.
   * @param day corresponds to the DAY time field.
   * @param hour corresponds to the HOUR_OF_DAY time field.
   * @param minute corresponds to the MINUTE time field.
   */
  public GregorianCalendar(int year, int month, int day, int hour, int minute)
  {
    super();
    set(year, month, day, hour, minute);
  }

  /**
   * Constructs a new GregorianCalendar representing midnight on the
   * given date with the default time zone and locale.
   * @param year corresponds to the YEAR time field.
   * @param month corresponds to the MONTH time field.
   * @param day corresponds to the DAY time field.
   * @param hour corresponds to the HOUR_OF_DAY time field.
   * @param minute corresponds to the MINUTE time field.
   * @param second corresponds to the SECOND time field.
   */
  public GregorianCalendar(int year, int month, int day,
			   int hour, int minute, int second)
  {
    super();
    set(year, month, day, hour, minute, second);
  }

  /**
   * Sets the date of the switch from Julian dates to Gregorian dates.
   * You can use <code>new Date(Long.MAX_VALUE)</code> to use a pure
   * Julian calendar, or <code>Long.MIN_VALUE</code> for a pure Gregorian
   * calendar.
   * @param date the date of the change.
   */
  public void setGregorianChange(Date date)
  {
    gregorianCutover = date.getTime();
  }

  /**
   * Gets the date of the switch from Julian dates to Gregorian dates.
   * @return the date of the change.
   */
  public final Date getGregorianChange()
  {
    return new Date(gregorianCutover);
  }

  /**
   * Determines if the given year is a leap year.  The result is
   * undefined if the gregorian change took place in 1800, so that
   * the end of february is skiped and you give that year
   * (well...).<br>
   *
   * The year should be positive and you can't give an ERA.  But
   * remember that before 4 BC there wasn't a consistent leap year
   * rule, so who cares.
   *
   * @param year a year use nonnegative value for BC.
   * @return true, if the given year is a leap year, false otherwise.  */
  public boolean isLeapYear(int year)
  {
    if ((year & 3) != 0)
      // Only years divisible by 4 can be leap years
      return false;

    // compute the linear day of the 29. February of that year.
    // The 13 is the number of days, that were omitted in the Gregorian
    // Calender until the epoch.
    int julianDay = (((year-1) * (365*4+1)) >> 2) + (31+29 - 
        (((1970-1) * (365*4+1)) / 4 + 1 - 13));
    
    // If that day is smaller than the gregorianChange the julian
    // rule applies:  This is a leap year since it is divisible by 4.
    if (julianDay * (24 * 60 * 60 * 1000L) < gregorianCutover)
      return true;

    return ((year % 100) != 0 || (year % 400) == 0);
  }

  /**
   * Get the linear time in milliseconds since the epoch.  If you
   * specify a nonpositive year it is interpreted as BC as
   * following: 0 is 1 BC, -1 is 2 BC and so on.  The date is
   * interpreted as gregorian if the change occurred before that date.
   *
   * @param year the year of the date.
   * @param dayOfYear the day of year of the date; 1 based.
   * @param millis the millisecond in that day.
   * @return the days since the epoch, may be negative.  */
  private long getLinearTime(int year, int dayOfYear, int millis)
  {
    // The 13 is the number of days, that were omitted in the Gregorian
    // Calender until the epoch.
    // We shift right by 2 instead of dividing by 4, to get correct
    // results for negative years (and this is even more efficient).
    int julianDay = ((year * (365 * 4 + 1)) >> 2) + dayOfYear -
      ((1970 * (365 * 4 + 1)) / 4 + 1 - 13);
    long time = julianDay * (24 * 60 * 60 * 1000L) + millis;

    if (time >= gregorianCutover)
      {
	// subtract the days that are missing in gregorian calendar
	// with respect to julian calendar.
	//
	// Okay, here we rely on the fact that the gregorian
	// calendar was introduced in the AD era.  This doesn't work
	// with negative years.
	//
	// The additional leap year factor accounts for the fact that
	// a leap day is not seen on Jan 1 of the leap year.
	int gregOffset = (year / 400) - (year / 100) + 2;
	if (isLeapYear (year, true) && dayOfYear < 31 + 29)
	  --gregOffset;
	time += gregOffset * (24 * 60 * 60 * 1000L);
      }
    return time;
  }

  private int getWeekDay(int year, int dayOfYear)
  {
    int day =
      (int) (getLinearTime(year, dayOfYear, 0) / (24 * 60 * 60 * 1000L));

    // The epoch was a thursday.
    int weekday = (day + THURSDAY) % 7;
    if (weekday <= 0)
      weekday += 7;
    return weekday;
  }

  /**
   * Calculate the dayOfYear from the fields array.  
   * The relativeDays is used, to account for weeks that begin before
   * the gregorian change and end after it.<br>
   *
   * We return two values, the first is used to determine, if we
   * should use Gregorian calendar or Julian calendar, in case of
   * the change year, the second is a relative day after the given
   * day.  This is necessary for week calculation in the year in
   * which gregorian change occurs. <br>
   *
   * @param year the year, negative for BC.
   * @return an array of two int values, the first containing a reference
   * day of current year, the second a relative count since this reference
   * day.  */
  private int[] getDayOfYear(int year)
  {
    if (isSet[MONTH])
      {
	int dayOfYear;
	if (fields[MONTH] > FEBRUARY)
	  {

	    // The months after February are regular:
	    // 9 is an offset found by try and error.
	    dayOfYear = (fields[MONTH] * (31 + 30 + 31 + 30 + 31) - 9) / 5;
	    if (isLeapYear(year))
	      dayOfYear++;
	  }
	else
	    dayOfYear = 31 * fields[MONTH];

	if (isSet[DAY_OF_MONTH])
	  {
	    return new int[]
	    {
	    dayOfYear + fields[DAY_OF_MONTH], 0};
	  }
	if (isSet[WEEK_OF_MONTH] && isSet[DAY_OF_WEEK])
	  {
	    // the weekday of the first day in that month is:
	    int weekday = getWeekDay(year, ++dayOfYear);

	    return new int[]
	    {
	      dayOfYear,
		// the day of week in the first week
		// (weeks starting on sunday) is:
	      fields[DAY_OF_WEEK] - weekday +
		// Now jump to the right week and correct the possible
		// error made by assuming sunday is the first week day.
	      7 * (fields[WEEK_OF_MONTH]
		   + (fields[DAY_OF_WEEK] < getFirstDayOfWeek()? 0 : -1)
		   + (weekday < getFirstDayOfWeek()? -1 : 0))};
	  }
	if (isSet[DAY_OF_WEEK] && isSet[DAY_OF_WEEK_IN_MONTH])
	  {
	    // the weekday of the first day in that month is:
	    int weekday = getWeekDay(year, ++dayOfYear);
	    return new int[] { 
		  dayOfYear,
		  fields[DAY_OF_WEEK] - weekday +
		  7 * (fields[DAY_OF_WEEK_IN_MONTH]
		       + (fields[DAY_OF_WEEK] < weekday ? 0 : -1))};
	  }
      }

    // MONTH + something did not succeed.
    if (isSet[DAY_OF_YEAR])
      {
	return new int[] {0, fields[DAY_OF_YEAR]};
      }
      
    if (isSet[DAY_OF_WEEK] && isSet[WEEK_OF_YEAR])
      {
	int dayOfYear = getMinimalDaysInFirstWeek();
	// the weekday of the day, that begins the first week 
	// in that year is:
	int weekday = getWeekDay(year, dayOfYear);

	return new int[] { 
	    dayOfYear,
	      // the day of week in the first week
	      // (weeks starting on sunday) is:
	    fields[DAY_OF_WEEK] - weekday
	      // Now jump to the right week and correct the possible
	      // error made by assuming sunday is the first week day.
	    + 7 * (fields[WEEK_OF_YEAR]
		   + (fields[DAY_OF_WEEK] < getFirstDayOfWeek()? 0 : -1)
		   + (weekday < getFirstDayOfWeek()? -1 : 0))};
      }

    // As last resort return Jan, 1st.
    return new int[] {1, 0};
  }

  /**
   * Converts the time field values (<code>fields</code>) to
   * milliseconds since the epoch UTC (<code>time</code>). 
   */
  protected synchronized void computeTime()
  {
    int era = isSet[ERA] ? fields[ERA] : AD;
    int year = isSet[YEAR] ? fields[YEAR] : 1970;
    if (era == BC)
      year = 1 - year;

    int[] daysOfYear = getDayOfYear(year);

    int hour = 0;
    if (isSet[HOUR_OF_DAY])
      hour = fields[HOUR_OF_DAY];
    else if (isSet[HOUR])
      {
	hour = fields[HOUR];
        if (isSet[AM_PM] && fields[AM_PM] == PM)
	  hour += 12;
      }

    int minute = isSet[MINUTE] ? fields[MINUTE] : 0;
    int second = isSet[SECOND] ? fields[SECOND] : 0;
    int millis = isSet[MILLISECOND] ? fields[MILLISECOND] : 0;
    int millisInDay;

    if (isLenient())
      {
	// prevent overflow
	long allMillis = (((hour * 60L) + minute) * 60L + second) * 1000L
	  + millis;
	daysOfYear[1] += allMillis / (24 * 60 * 60 * 1000L);
	millisInDay = (int) (allMillis % (24 * 60 * 60 * 1000L));
      }
    else
      {
	if (hour < 0 || hour >= 24 || minute < 0 || minute > 59
	    || second < 0 || second > 59 || millis < 0 || millis >= 1000)
	  throw new IllegalArgumentException();
	millisInDay = (((hour * 60) + minute) * 60 + second) * 1000 + millis;
      }
    time = getLinearTime(year, daysOfYear[0], millisInDay);

    // Add the relative days after calculating the linear time, to
    // get right behaviour when jumping over the gregorianCutover.
    time += daysOfYear[1] * (24 * 60 * 60 * 1000L);


    TimeZone zone = getTimeZone();
    int rawOffset = isSet[ZONE_OFFSET]
      ? fields[ZONE_OFFSET] : zone.getRawOffset();

    int dayOfYear = daysOfYear[0] + daysOfYear[1];
    int month = (dayOfYear * 5 + 3) / (31 + 30 + 31 + 30 + 31);
    int day = (6 + (dayOfYear * 5 + 3) % (31 + 30 + 31 + 30 + 31)) / 5;
    int weekday = ((int) (time / (24 * 60 * 60 * 1000L)) + THURSDAY) % 7;
    if (weekday <= 0)
      weekday += 7;
    int dstOffset = isSet[DST_OFFSET]
      ? fields[DST_OFFSET] : (zone.getOffset((year < 0) ? BC : AD,
					     (year < 0) ? 1 - year : year,
					     month, day, weekday, millisInDay)
			      - zone.getRawOffset());
    time -= rawOffset + dstOffset;
    isTimeSet = true;
  }

  /**
   * Determines if the given year is a leap year.  
   *
   * The year should be positive and you can't give an ERA.  But
   * remember that before 4 BC there wasn't a consistent leap year
   * rule, so who cares.
   *
   * @param year a year use nonnegative value for BC.
   * @param gregorian if true, use gregorian leap year rule.
   * @return true, if the given year is a leap year, false otherwise.  */
  private boolean isLeapYear(int year, boolean gregorian)
  {
    if ((year & 3) != 0)
      // Only years divisible by 4 can be leap years
      return false;

    if (!gregorian)
      return true;

    // We rely on AD area here.
    return ((year % 100) != 0 || (year % 400) == 0);
  }

  /**
   * Get the linear day in days since the epoch, using the
   * Julian or Gregorian calendar as specified.  If you specify a
   * nonpositive year it is interpreted as BC as following: 0 is 1
   * BC, -1 is 2 BC and so on.  
   *
   * @param year the year of the date.
   * @param dayOfYear the day of year of the date; 1 based.
   * @param gregorian True, if we should use Gregorian rules.
   * @return the days since the epoch, may be negative.  */
  private int getLinearDay(int year, int dayOfYear, boolean gregorian)
  {
    // The 13 is the number of days, that were omitted in the Gregorian
    // Calender until the epoch.
    // We shift right by 2 instead of dividing by 4, to get correct
    // results for negative years (and this is even more efficient).
    int julianDay = ((year * (365 * 4 + 1)) >> 2) + dayOfYear -
      ((1970 * (365 * 4 + 1)) / 4 + 1 - 13);

    if (gregorian)
      {
	// subtract the days that are missing in gregorian calendar
	// with respect to julian calendar.
	//
	// Okay, here we rely on the fact that the gregorian
	// calendar was introduced in the AD era.  This doesn't work
	// with negative years.
	//
	// The additional leap year factor accounts for the fact that
	// a leap day is not seen on Jan 1 of the leap year.
	int gregOffset = (year / 400) - (year / 100) + 2;
	if (isLeapYear (year, true) && dayOfYear < 31 + 29)
	  --gregOffset;
	julianDay += gregOffset;
      }
    return julianDay;
  }

  /**
   * Converts the given linear day into era, year, month,
   * day_of_year, day_of_month, day_of_week, and writes the result
   * into the fields array.
   * @param day the linear day.  
   */
  private void calculateDay(int day, boolean gregorian)
  {
    // the epoch is a Thursday.
    int weekday = (day + THURSDAY) % 7;
    if (weekday <= 0)
      weekday += 7;
    fields[DAY_OF_WEEK] = weekday;

    // get a first approximation of the year.  This may be one 
    // year to big.
    int year = 1970 + (gregorian
		       ? ((day - 100) * 400) / (365 * 400 + 100 - 4 + 1)
		       : ((day - 100) * 4) / (365 * 4 + 1));
    if (day >= 0)
      year++;

    int firstDayOfYear = getLinearDay(year, 1, gregorian);

    // Now look in which year day really lies.
    if (day < firstDayOfYear)
      {
	year--;
	firstDayOfYear = getLinearDay(year, 1, gregorian);
      }

    day -= firstDayOfYear - 1;	// day of year,  one based.

    fields[DAY_OF_YEAR] = day;
    if (year <= 0)
      {
	fields[ERA] = BC;
	fields[YEAR] = 1 - year;
      }
    else
      {
	fields[ERA] = AD;
	fields[YEAR] = year;
      }

    int leapday = isLeapYear(year, gregorian) ? 1 : 0;
    if (day <= 31 + 28 + leapday)
      {
	fields[MONTH] = day / 32;	// 31->JANUARY, 32->FEBRUARY
	fields[DAY_OF_MONTH] = day - 31 * fields[MONTH];
      }
    else
      {
	// A few more magic formulas
	int scaledDay = (day - leapday) * 5 + 8;
	fields[MONTH] = scaledDay / (31 + 30 + 31 + 30 + 31);
	fields[DAY_OF_MONTH] = (scaledDay % (31 + 30 + 31 + 30 + 31)) / 5 + 1;
      }
  }

  /**
   * Converts the milliseconds since the epoch UTC
   * (<code>time</code>) to time fields
   * (<code>fields</code>). 
   */
  protected synchronized void computeFields()
  {
    boolean gregorian = (time >= gregorianCutover);

    TimeZone zone = getTimeZone();
    fields[ZONE_OFFSET] = zone.getRawOffset();
    long localTime = time + fields[ZONE_OFFSET];

    int day = (int) (localTime / (24 * 60 * 60 * 1000L));
    int millisInDay = (int) (localTime % (24 * 60 * 60 * 1000L));
    if (millisInDay < 0)
      {
	millisInDay += (24 * 60 * 60 * 1000);
	day--;
      }

    calculateDay(day, gregorian);
    fields[DST_OFFSET] =
      zone.getOffset(fields[ERA], fields[YEAR], fields[MONTH],
		     fields[DAY_OF_MONTH], fields[DAY_OF_WEEK],
		     millisInDay) - fields[ZONE_OFFSET];

    millisInDay += fields[DST_OFFSET];
    if (millisInDay >= 24 * 60 * 60 * 1000)
      {
	millisInDay -= 24 * 60 * 60 * 1000;
	calculateDay(++day, gregorian);
      }

    fields[DAY_OF_WEEK_IN_MONTH] = (fields[DAY_OF_MONTH] + 6) / 7;

    // which day of the week are we (0..6), relative to getFirstDayOfWeek
    int relativeWeekday = (7 + fields[DAY_OF_WEEK] - getFirstDayOfWeek()) % 7;

    fields[WEEK_OF_MONTH] = (fields[DAY_OF_MONTH] - relativeWeekday + 6) / 7;

    int weekOfYear = (fields[DAY_OF_YEAR] - relativeWeekday + 6) / 7;

    // Do the Correction: getMinimalDaysInFirstWeek() is always in the 
    // first week.
    int minDays = getMinimalDaysInFirstWeek();
    int firstWeekday =
      (7 + getWeekDay(fields[YEAR], minDays) - getFirstDayOfWeek()) % 7;
    if (minDays - firstWeekday < 1)
      weekOfYear++;
    fields[WEEK_OF_YEAR] = weekOfYear;


    int hourOfDay = millisInDay / (60 * 60 * 1000);
    fields[AM_PM] = (hourOfDay < 12) ? AM : PM;
    int hour = hourOfDay % 12;
    fields[HOUR] = (hour == 0) ? 12 : hour;
    fields[HOUR_OF_DAY] = hourOfDay;
    millisInDay %= (60 * 60 * 1000);
    fields[MINUTE] = millisInDay / (60 * 1000);
    millisInDay %= (60 * 1000);
    fields[SECOND] = millisInDay / (1000);
    fields[MILLISECOND] = millisInDay % 1000;


    areFieldsSet = isSet[ERA] = isSet[YEAR] = isSet[MONTH] =
      isSet[WEEK_OF_YEAR] = isSet[WEEK_OF_MONTH] =
      isSet[DAY_OF_MONTH] = isSet[DAY_OF_YEAR] = isSet[DAY_OF_WEEK] =
      isSet[DAY_OF_WEEK_IN_MONTH] = isSet[AM_PM] = isSet[HOUR] =
      isSet[HOUR_OF_DAY] = isSet[MINUTE] = isSet[SECOND] =
      isSet[MILLISECOND] = isSet[ZONE_OFFSET] = isSet[DST_OFFSET] = true;

  }

  /**
   * Compares the given calender with this.  
   * @param o the object to that we should compare.
   * @return true, if the given object is a calendar, that represents
   * the same time (but doesn't necessary have the same fields).
   * @XXX Should we check if time zones, locale, cutover etc. are equal?
   */
  public boolean equals(Object o)
  {
    if (!(o instanceof GregorianCalendar))
      return false;

    GregorianCalendar cal = (GregorianCalendar) o;
    return (cal.getTimeInMillis() == getTimeInMillis());
  }

//     /**
//      * Compares the given calender with this.  
//      * @param o the object to that we should compare.
//      * @return true, if the given object is a calendar, and this calendar
//      * represents a smaller time than the calender o.
//      */
//     public boolean before(Object o) {
//         if (!(o instanceof GregorianCalendar))
//             return false;

//         GregorianCalendar cal = (GregorianCalendar) o;
//         return (cal.getTimeInMillis() < getTimeInMillis());
//     }

//     /**
//      * Compares the given calender with this.  
//      * @param o the object to that we should compare.
//      * @return true, if the given object is a calendar, and this calendar
//      * represents a bigger time than the calender o.
//      */
//     public boolean after(Object o) {
//         if (!(o instanceof GregorianCalendar))
//             return false;

//         GregorianCalendar cal = (GregorianCalendar) o;
//         return (cal.getTimeInMillis() > getTimeInMillis());
//     }

  /**
   * Adds the specified amount of time to the given time field.  The
   * amount may be negative to subtract the time.  If the field overflows
   * it does what you expect: Jan, 25 + 10 Days is Feb, 4.
   * @param field the time field. One of the time field constants.
   * @param amount the amount of time.
   */
  public void add(int field, int amount)
  {
    switch (field)
      {
      case YEAR:
	complete();
	fields[YEAR] += amount;
	isTimeSet = false;
	break;
      case MONTH:
	complete();
	int months = fields[MONTH] + amount;
	fields[YEAR] += months / 12;
	fields[MONTH] = months % 12;
	if (fields[MONTH] < 0)
	  {
	    fields[MONTH] += 12;
	    fields[YEAR]--;
	  }
	isTimeSet = false;
	int maxDay = getActualMaximum(DAY_OF_MONTH);
	if (fields[DAY_OF_MONTH] > maxDay)
	  {
	    fields[DAY_OF_MONTH] = maxDay;
	    isTimeSet = false;
	  }
	break;
      case DAY_OF_MONTH:
      case DAY_OF_YEAR:
      case DAY_OF_WEEK:
	if (!isTimeSet)
	  computeTime();
	time += amount * (24 * 60 * 60 * 1000L);
	areFieldsSet = false;
	break;
      case WEEK_OF_YEAR:
      case WEEK_OF_MONTH:
      case DAY_OF_WEEK_IN_MONTH:
	if (!isTimeSet)
	  computeTime();
	time += amount * (7 * 24 * 60 * 60 * 1000L);
	areFieldsSet = false;
	break;
      case AM_PM:
	if (!isTimeSet)
	  computeTime();
	time += amount * (12 * 60 * 60 * 1000L);
	areFieldsSet = false;
	break;
      case HOUR:
      case HOUR_OF_DAY:
	if (!isTimeSet)
	  computeTime();
	time += amount * (60 * 60 * 1000L);
	areFieldsSet = false;
	break;
      case MINUTE:
	if (!isTimeSet)
	  computeTime();
	time += amount * (60 * 1000L);
	areFieldsSet = false;
	break;
      case SECOND:
	if (!isTimeSet)
	  computeTime();
	time += amount * (1000L);
	areFieldsSet = false;
	break;
      case MILLISECOND:
	if (!isTimeSet)
	  computeTime();
	time += amount;
	areFieldsSet = false;
	break;
      case ZONE_OFFSET:
	complete();
	fields[ZONE_OFFSET] += amount;
	time -= amount;
	break;
      case DST_OFFSET:
	complete();
	fields[DST_OFFSET] += amount;
	isTimeSet = false;
	break;
      default:
	throw new IllegalArgumentException
	  ("Unknown Calendar field: " + field);
      }
  }


  /**
   * Rolls the specified time field up or down.  This means add one
   * to the specified field, but don't change the other fields.  If
   * the maximum for this field is reached, start over with the 
   * minimum value.  
   *
   * <strong>Note:</strong> There may be situation, where the other
   * fields must be changed, e.g rolling the month on May, 31. 
   * The date June, 31 is automatically converted to July, 1. 
   * This requires lenient settings.
   *
   * @param field the time field. One of the time field constants.
   * @param up the direction, true for up, false for down.
   */
  public void roll(int field, boolean up)
  {
    roll(field, up ? 1 : -1);
  }

  private void cleanUpAfterRoll(int field, int delta)
  {
    switch (field)
      {
      case ERA:
      case YEAR:
      case MONTH:
	// check that day of month is still in correct range
	if (fields[DAY_OF_MONTH] > getActualMaximum(DAY_OF_MONTH))
	  fields[DAY_OF_MONTH] = getActualMaximum(DAY_OF_MONTH);
	isTimeSet = false;
	isSet[WEEK_OF_MONTH] = false;
	isSet[DAY_OF_WEEK] = false;
	isSet[DAY_OF_WEEK_IN_MONTH] = false;
	isSet[DAY_OF_YEAR] = false;
	isSet[WEEK_OF_YEAR] = false;
	break;

      case DAY_OF_MONTH:
	isSet[WEEK_OF_MONTH] = false;
	isSet[DAY_OF_WEEK] = false;
	isSet[DAY_OF_WEEK_IN_MONTH] = false;
	isSet[DAY_OF_YEAR] = false;
	isSet[WEEK_OF_YEAR] = false;
	time += delta * (24 * 60 * 60 * 1000L);
	break;

      case WEEK_OF_MONTH:
	isSet[DAY_OF_MONTH] = false;
	isSet[DAY_OF_WEEK_IN_MONTH] = false;
	isSet[DAY_OF_YEAR] = false;
	isSet[WEEK_OF_YEAR] = false;
	time += delta * (7 * 24 * 60 * 60 * 1000L);
	break;
      case DAY_OF_WEEK_IN_MONTH:
	isSet[DAY_OF_MONTH] = false;
	isSet[WEEK_OF_MONTH] = false;
	isSet[DAY_OF_YEAR] = false;
	isSet[WEEK_OF_YEAR] = false;
	time += delta * (7 * 24 * 60 * 60 * 1000L);
	break;
      case DAY_OF_YEAR:
	isSet[MONTH] = false;
	isSet[DAY_OF_MONTH] = false;
	isSet[WEEK_OF_MONTH] = false;
	isSet[DAY_OF_WEEK_IN_MONTH] = false;
	isSet[DAY_OF_WEEK] = false;
	isSet[WEEK_OF_YEAR] = false;
	time += delta * (24 * 60 * 60 * 1000L);
	break;
      case WEEK_OF_YEAR:
	isSet[MONTH] = false;
	isSet[DAY_OF_MONTH] = false;
	isSet[WEEK_OF_MONTH] = false;
	isSet[DAY_OF_WEEK_IN_MONTH] = false;
	isSet[DAY_OF_YEAR] = false;
	time += delta * (7 * 24 * 60 * 60 * 1000L);
	break;

      case AM_PM:
	isSet[HOUR_OF_DAY] = false;
	time += delta * (12 * 60 * 60 * 1000L);
	break;
      case HOUR:
	isSet[HOUR_OF_DAY] = false;
	time += delta * (60 * 60 * 1000L);
	break;
      case HOUR_OF_DAY:
	isSet[HOUR] = false;
	isSet[AM_PM] = false;
	time += delta * (60 * 60 * 1000L);
	break;

      case MINUTE:
	time += delta * (60 * 1000L);
	break;
      case SECOND:
	time += delta * (1000L);
	break;
      case MILLISECOND:
	time += delta;
	break;
      }
  }

  /**
   * Rolls the specified time field by the given amount.  This means
   * add amount to the specified field, but don't change the other
   * fields.  If the maximum for this field is reached, start over
   * with the minimum value and vice versa for negative amounts.
   *
   * <strong>Note:</strong> There may be situation, where the other
   * fields must be changed, e.g rolling the month on May, 31. 
   * The date June, 31 is automatically corrected to June, 30.
   *
   * @param field the time field. One of the time field constants.
   * @param amount the amount by which we should roll.
   */
  public void roll(int field, int amount)
  {
    switch (field)
      {
      case DAY_OF_WEEK:
	// day of week is special: it rolls automatically
	add(field, amount);
	return;
      case ZONE_OFFSET:
      case DST_OFFSET:
	throw new IllegalArgumentException("Can't roll time zone");
      }
    complete();
    int min = getActualMinimum(field);
    int range = getActualMaximum(field) - min + 1;
    int oldval = fields[field];
    int newval = (oldval - min + range + amount) % range + min;
    if (newval < min)
      newval += range;
    fields[field] = newval;
    cleanUpAfterRoll(field, newval - oldval);
  }

  private static final int[] minimums =
      { BC,       1,  0,  0, 1,  1,   1,   SUNDAY, 1, 
        AM,  1,  0,  1,  1,   1, -(12*60*60*1000),               0 };

  private static final int[] maximums =
      { AD, 5000000, 11, 53, 5, 31, 366, SATURDAY, 5, 
        PM, 12, 23, 59, 59, 999, +(12*60*60*1000), (12*60*60*1000) };

  /**
   * Gets the smallest value that is allowed for the specified field.
   * @param field the time field. One of the time field constants.
   * @return the smallest value.
   */
  public int getMinimum(int field)
  {
    return minimums[field];
  }

  /**
   * Gets the biggest value that is allowed for the specified field.
   * @param field the time field. One of the time field constants.
   * @return the biggest value.
   */
  public int getMaximum(int field)
  {
    return maximums[field];
  }


  /**
   * Gets the greatest minimum value that is allowed for the specified field.
   * @param field the time field. One of the time field constants.
   * @return the greatest minimum value.
   */
  public int getGreatestMinimum(int field)
  {
    if (field == WEEK_OF_YEAR)
      return 1;
    return minimums[field];
  }

  /**
   * Gets the smallest maximum value that is allowed for the
   * specified field.  For example this is 28 for DAY_OF_MONTH.
   * @param field the time field. One of the time field constants.
   * @return the least maximum value.  
   * @since jdk1.2
   */
  public int getLeastMaximum(int field)
  {
    switch (field)
      {
      case WEEK_OF_YEAR:
	return 52;
      case DAY_OF_MONTH:
	return 28;
      case DAY_OF_YEAR:
	return 365;
      case DAY_OF_WEEK_IN_MONTH:
      case WEEK_OF_MONTH:
	return 4;
      default:
	return maximums[field];
      }
  }

  /**
   * Gets the actual minimum value that is allowed for the specified field.
   * This value is dependent on the values of the other fields.  Note that
   * this calls <code>complete()</code> if not enough fields are set.  This
   * can have ugly side effects.
   * @param field the time field. One of the time field constants.
   * @return the actual minimum value.
   * @since jdk1.2
   */
  public int getActualMinimum(int field)
  {
    if (field == WEEK_OF_YEAR)
      {
	int min = getMinimalDaysInFirstWeek();
	if (min == 0)
	  return 1;
	if (!areFieldsSet || !isSet[ERA] || !isSet[YEAR])
	  complete();

	int year = fields[ERA] == AD ? fields[YEAR] : 1 - fields[YEAR];
	int weekday = getWeekDay(year, min);
	if ((7 + weekday - getFirstDayOfWeek()) % 7 >= min - 1)
	  return 1;
	return 0;
      }
    return minimums[field];
  }

  /**
   * Gets the actual maximum value that is allowed for the specified field.
   * This value is dependent on the values of the other fields.  Note that
   * this calls <code>complete()</code> if not enough fields are set.  This
   * can have ugly side effects.
   * @param field the time field. One of the time field constants.
   * @return the actual maximum value.  
   */
  public int getActualMaximum(int field)
  {
    switch (field)
      {
      case WEEK_OF_YEAR:
	{
	  if (!areFieldsSet || !isSet[ERA] || !isSet[YEAR])
	    complete();
	  // This is wrong for the year that contains the gregorian change.
	  // I.e it gives the weeks in the julian year or in the gregorian
	  // year in that case.
	  int year = fields[ERA] == AD ? fields[YEAR] : 1 - fields[YEAR];
	  int lastDay = isLeapYear(year) ? 366 : 365;
	  int weekday = getWeekDay(year, lastDay);
	  int week = (lastDay + 6
		      - (7 + weekday - getFirstDayOfWeek()) % 7) / 7;

	  int minimalDays = getMinimalDaysInFirstWeek();
	  int firstWeekday = getWeekDay(year, minimalDays);
	  if (minimalDays - (7 + firstWeekday - getFirstDayOfWeek()) % 7 < 1)
	    return week + 1;
	}
	case DAY_OF_MONTH:
	{
	  if (!areFieldsSet || !isSet[MONTH])
	    complete();
	  int month = fields[MONTH];
	  // If you change this, you should also change 
	  // SimpleTimeZone.getDaysInMonth();
	  if (month == FEBRUARY)
	    {
	      if (!isSet[YEAR] || !isSet[ERA])
		complete();
	      int year = fields[ERA] == AD ? fields[YEAR] : 1 - fields[YEAR];
	      return isLeapYear(year) ? 29 : 28;
	    }
	  else if (month < AUGUST)
	    return 31 - (month & 1);
	  else
	    return 30 + (month & 1);
	}
      case DAY_OF_YEAR:
	{
	  if (!areFieldsSet || !isSet[ERA] || !isSet[YEAR])
	    complete();
	  int year = fields[ERA] == AD ? fields[YEAR] : 1 - fields[YEAR];
	  return isLeapYear(year) ? 366 : 365;
	}
      case DAY_OF_WEEK_IN_MONTH:
	{
	  // This is wrong for the month that contains the gregorian change.
	  int daysInMonth = getActualMaximum(DAY_OF_MONTH);
	  // That's black magic, I know
	  return (daysInMonth - (fields[DAY_OF_MONTH] - 1) % 7 + 6) / 7;
	}
      case WEEK_OF_MONTH:
	{
	  int daysInMonth = getActualMaximum(DAY_OF_MONTH);
	  int weekday = (daysInMonth - fields[DAY_OF_MONTH]
			 + fields[DAY_OF_WEEK] - SUNDAY) % 7 + SUNDAY;
	  return (daysInMonth + 6
		  - (7 + weekday - getFirstDayOfWeek()) % 7) / 7;
	}
      default:
	return maximums[field];
      }
  }
}
