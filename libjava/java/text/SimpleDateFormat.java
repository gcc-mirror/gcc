/* SimpleDateFormat.java -- A class for parsing/formating simple 
   date constructs
   Copyright (C) 1998, 1999, 2000, 2001, 2003 Free Software Foundation, Inc.

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


package java.text;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Locale;
import java.util.TimeZone;
import java.util.SimpleTimeZone;
import java.io.ObjectInputStream;
import java.io.IOException;

/**
 * SimpleDateFormat provides convenient methods for parsing and formatting
 * dates using Gregorian calendars (see java.util.GregorianCalendar). 
 */
public class SimpleDateFormat extends DateFormat 
{
  /** A pair class used by SimpleDateFormat as a compiled representation
   *  of a format string.
   */
  private class FieldSizePair 
  {
    public int field;
    public int size;

    /** Constructs a pair with the given field and size values */
    public FieldSizePair(int f, int s) {
      field = f;
      size = s;
    }
  }

  private transient ArrayList tokens;
  private DateFormatSymbols formatData;  // formatData
  private Date defaultCenturyStart;
  private transient int defaultCentury;
  private String pattern;
  private int serialVersionOnStream = 1; // 0 indicates JDK1.1.3 or earlier
  private static final long serialVersionUID = 4774881970558875024L;

  // This string is specified in the JCL.  We set it here rather than
  // do a DateFormatSymbols(Locale.US).getLocalPatternChars() since
  // someone could theoretically change those values (though unlikely).
  private static final String standardChars = "GyMdkHmsSEDFwWahKz";

  private void readObject(ObjectInputStream stream)
    throws IOException, ClassNotFoundException
  {
    stream.defaultReadObject();
    if (serialVersionOnStream < 1)
      {
        computeCenturyStart ();
	serialVersionOnStream = 1;
      }
    else
      // Ensure that defaultCentury gets set.
      set2DigitYearStart(defaultCenturyStart);

    // Set up items normally taken care of by the constructor.
    tokens = new ArrayList();
    compileFormat(pattern);
  }

  private void compileFormat(String pattern) 
  {
    // Any alphabetical characters are treated as pattern characters
    // unless enclosed in single quotes.

    char thisChar;
    int pos;
    int field;
    FieldSizePair current = null;

    for (int i=0; i<pattern.length(); i++) {
      thisChar = pattern.charAt(i);
      field = formatData.getLocalPatternChars().indexOf(thisChar);
      if (field == -1) {
	current = null;
	if ((thisChar >= 'A' && thisChar <= 'Z')
	    || (thisChar >= 'a' && thisChar <= 'z')) {
	  // Not a valid letter
	  tokens.add(new FieldSizePair(-1,0));
	} else if (thisChar == '\'') {
	  // Quoted text section; skip to next single quote
	  pos = pattern.indexOf('\'',i+1);
	  if (pos == -1) {
	    // This ought to be an exception, but spec does not
	    // let us throw one.
	    tokens.add(new FieldSizePair(-1,0));
	  }
	  if ((pos+1 < pattern.length()) && (pattern.charAt(pos+1) == '\'')) {
	    tokens.add(pattern.substring(i+1,pos+1));
	  } else {
	    tokens.add(pattern.substring(i+1,pos));
	  }
	  i = pos;
	} else {
	  // A special character
	  tokens.add(new Character(thisChar));
	}
      } else {
	// A valid field
	if ((current != null) && (field == current.field)) {
	  current.size++;
	} else {
	  current = new FieldSizePair(field,1);
	  tokens.add(current);
	}
      }
    }
  }

  public String toString() 
  {
    StringBuffer output = new StringBuffer();
    Iterator i = tokens.iterator();
    while (i.hasNext()) {
      output.append(i.next().toString());
    }
    return output.toString();
  }

  /**
   * Constructs a SimpleDateFormat using the default pattern for
   * the default locale.
   */
  public SimpleDateFormat() 
  {
    /*
     * There does not appear to be a standard API for determining 
     * what the default pattern for a locale is, so use package-scope
     * variables in DateFormatSymbols to encapsulate this.
     */
    super();
    Locale locale = Locale.getDefault();
    calendar = new GregorianCalendar(locale);
    computeCenturyStart();
    tokens = new ArrayList();
    formatData = new DateFormatSymbols(locale);
    pattern = (formatData.dateFormats[DEFAULT] + ' '
	       + formatData.timeFormats[DEFAULT]);
    compileFormat(pattern);
    numberFormat = NumberFormat.getInstance(locale);
    numberFormat.setGroupingUsed (false);
    numberFormat.setParseIntegerOnly (true);
  }
  
  /**
   * Creates a date formatter using the specified pattern, with the default
   * DateFormatSymbols for the default locale.
   */
  public SimpleDateFormat(String pattern) 
  {
    this(pattern, Locale.getDefault());
  }

  /**
   * Creates a date formatter using the specified pattern, with the default
   * DateFormatSymbols for the given locale.
   */
  public SimpleDateFormat(String pattern, Locale locale) 
  {
    super();
    calendar = new GregorianCalendar(locale);
    computeCenturyStart();
    tokens = new ArrayList();
    formatData = new DateFormatSymbols(locale);
    compileFormat(pattern);
    this.pattern = pattern;
    numberFormat = NumberFormat.getInstance(locale);
    numberFormat.setGroupingUsed (false);
    numberFormat.setParseIntegerOnly (true);
  }

  /**
   * Creates a date formatter using the specified pattern. The
   * specified DateFormatSymbols will be used when formatting.
   */
  public SimpleDateFormat(String pattern, DateFormatSymbols formatData)
  {
    super();
    calendar = new GregorianCalendar();
    computeCenturyStart ();
    tokens = new ArrayList();
    this.formatData = formatData;
    compileFormat(pattern);
    this.pattern = pattern;
    numberFormat = NumberFormat.getInstance();
    numberFormat.setGroupingUsed (false);
    numberFormat.setParseIntegerOnly (true);
  }

  // What is the difference between localized and unlocalized?  The
  // docs don't say.

  /**
   * This method returns a string with the formatting pattern being used
   * by this object.  This string is unlocalized.
   *
   * @return The format string.
   */
  public String toPattern()
  {
    return pattern;
  }

  /**
   * This method returns a string with the formatting pattern being used
   * by this object.  This string is localized.
   *
   * @return The format string.
   */
  public String toLocalizedPattern()
  {
    String localChars = formatData.getLocalPatternChars();
    return applyLocalizedPattern (pattern, standardChars, localChars);
  }

  /**
   * This method sets the formatting pattern that should be used by this
   * object.  This string is not localized.
   *
   * @param pattern The new format pattern.
   */
  public void applyPattern(String pattern)
  {
    tokens = new ArrayList();
    compileFormat(pattern);
    this.pattern = pattern;
  }

  /**
   * This method sets the formatting pattern that should be used by this
   * object.  This string is localized.
   *
   * @param pattern The new format pattern.
   */
  public void applyLocalizedPattern(String pattern)
  {
    String localChars = formatData.getLocalPatternChars();
    pattern = applyLocalizedPattern (pattern, localChars, standardChars);
    applyPattern(pattern);
  }

  private String applyLocalizedPattern(String pattern,
				       String oldChars, String newChars)
  {
    int len = pattern.length();
    StringBuffer buf = new StringBuffer(len);
    boolean quoted = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = pattern.charAt(i);
	if (ch == '\'')
	  quoted = ! quoted;
	if (! quoted)
	  {
	    int j = oldChars.indexOf(ch);
	    if (j >= 0)
	      ch = newChars.charAt(j);
	  }
	buf.append(ch);
      }
    return buf.toString();
  }

  /** 
   * Returns the start of the century used for two digit years.
   *
   * @return A <code>Date</code> representing the start of the century
   * for two digit years.
   */
  public Date get2DigitYearStart()
  {
    return defaultCenturyStart;
  }

  /**
   * Sets the start of the century used for two digit years.
   *
   * @param date A <code>Date</code> representing the start of the century for
   * two digit years.
   */
  public void set2DigitYearStart(Date date)
  {
    defaultCenturyStart = date;
    calendar.clear();
    calendar.setTime(date);
    int year = calendar.get(Calendar.YEAR);
    defaultCentury = year - (year % 100);
  }

  /**
   * This method returns the format symbol information used for parsing
   * and formatting dates.
   *
   * @return The date format symbols.
   */
  public DateFormatSymbols getDateFormatSymbols()
  {
    return formatData;
  }

  /**
   * This method sets the format symbols information used for parsing
   * and formatting dates.
   *
   * @param formatData The date format symbols.
   */
   public void setDateFormatSymbols(DateFormatSymbols formatData)
   {
     this.formatData = formatData;
   }

  /**
   * This methods tests whether the specified object is equal to this
   * object.  This will be true if and only if the specified object:
   * <p>
   * <ul>
   * <li>Is not <code>null</code>.
   * <li>Is an instance of <code>SimpleDateFormat</code>.
   * <li>Is equal to this object at the superclass (i.e., <code>DateFormat</code>)
   *     level.
   * <li>Has the same formatting pattern.
   * <li>Is using the same formatting symbols.
   * <li>Is using the same century for two digit years.
   * </ul>
   *
   * @param obj The object to compare for equality against.
   *
   * @return <code>true</code> if the specified object is equal to this object,
   * <code>false</code> otherwise.
   */
  public boolean equals(Object o)
  {
    if (!super.equals(o))
      return false;

    if (!(o instanceof SimpleDateFormat))
      return false;

    SimpleDateFormat sdf = (SimpleDateFormat)o;

    if (defaultCentury != sdf.defaultCentury)
      return false;

    if (!toPattern().equals(sdf.toPattern()))
      return false;

    if (!getDateFormatSymbols().equals(sdf.getDateFormatSymbols()))
      return false;

    return true;
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return A hash value for this object.
   */
  public int hashCode()
  {
    return super.hashCode() ^ toPattern().hashCode() ^ defaultCentury ^
      getDateFormatSymbols().hashCode();
  }


  /**
   * Formats the date input according to the format string in use,
   * appending to the specified StringBuffer.  The input StringBuffer
   * is returned as output for convenience.
   */
  public StringBuffer format(Date date, StringBuffer buffer, FieldPosition pos)
  {
    String temp;
    calendar.setTime(date);
    
    // go through ArrayList, filling in fields where applicable, else toString
    Iterator i = tokens.iterator();
    while (i.hasNext()) {
      Object o = i.next();
      if (o instanceof FieldSizePair) {
	FieldSizePair p = (FieldSizePair) o;
	int beginIndex = buffer.length();
	switch (p.field) {
	case ERA_FIELD:
	  buffer.append(formatData.eras[calendar.get(Calendar.ERA)]);
	  break;
	case YEAR_FIELD:
	  // If we have two digits, then we truncate.  Otherwise, we
	  // use the size of the pattern, and zero pad.
	  if (p.size == 2)
	    {
	      temp = String.valueOf(calendar.get(Calendar.YEAR));
	      buffer.append(temp.substring(temp.length() - 2));
	    }
	  else
	    withLeadingZeros(calendar.get(Calendar.YEAR), p.size, buffer);
	  break;
	case MONTH_FIELD:
	  if (p.size < 3)
	    withLeadingZeros(calendar.get(Calendar.MONTH)+1,p.size,buffer);
	  else if (p.size < 4)
	    buffer.append(formatData.shortMonths[calendar.get(Calendar.MONTH)]);
	  else
	    buffer.append(formatData.months[calendar.get(Calendar.MONTH)]);
	  break;
	case DATE_FIELD:
	  withLeadingZeros(calendar.get(Calendar.DATE),p.size,buffer);
	  break;
	case HOUR_OF_DAY1_FIELD: // 1-24
	  withLeadingZeros(((calendar.get(Calendar.HOUR_OF_DAY)+23)%24)+1,p.size,buffer);
	  break;
	case HOUR_OF_DAY0_FIELD: // 0-23
	  withLeadingZeros(calendar.get(Calendar.HOUR_OF_DAY),p.size,buffer);
	  break;
	case MINUTE_FIELD:
	  withLeadingZeros(calendar.get(Calendar.MINUTE),p.size,buffer);
	  break;
	case SECOND_FIELD:
	  withLeadingZeros(calendar.get(Calendar.SECOND),p.size,buffer);
	  break;
	case MILLISECOND_FIELD:
	  withLeadingZeros(calendar.get(Calendar.MILLISECOND),p.size,buffer);
	  break;
	case DAY_OF_WEEK_FIELD:
	  if (p.size < 4)
	    buffer.append(formatData.shortWeekdays[calendar.get(Calendar.DAY_OF_WEEK)]);
	  else
	    buffer.append(formatData.weekdays[calendar.get(Calendar.DAY_OF_WEEK)]);
	  break;
	case DAY_OF_YEAR_FIELD:
	  withLeadingZeros(calendar.get(Calendar.DAY_OF_YEAR),p.size,buffer);
	  break;
	case DAY_OF_WEEK_IN_MONTH_FIELD:
	  withLeadingZeros(calendar.get(Calendar.DAY_OF_WEEK_IN_MONTH),p.size,buffer);
	  break;
	case WEEK_OF_YEAR_FIELD:
	  withLeadingZeros(calendar.get(Calendar.WEEK_OF_YEAR),p.size,buffer);
	  break;
	case WEEK_OF_MONTH_FIELD:
	  withLeadingZeros(calendar.get(Calendar.WEEK_OF_MONTH),p.size,buffer);
	  break;
	case AM_PM_FIELD:
	  buffer.append(formatData.ampms[calendar.get(Calendar.AM_PM)]);
	  break;
	case HOUR1_FIELD: // 1-12
	  withLeadingZeros(((calendar.get(Calendar.HOUR)+11)%12)+1,p.size,buffer);
	  break;
	case HOUR0_FIELD: // 0-11
	  withLeadingZeros(calendar.get(Calendar.HOUR),p.size,buffer);
	  break;
	case TIMEZONE_FIELD:
	  TimeZone zone = calendar.getTimeZone();
	  boolean isDST = calendar.get(Calendar.DST_OFFSET) != 0;
	  // FIXME: XXX: This should be a localized time zone.
	  String zoneID = zone.getDisplayName(isDST, p.size > 3 ? TimeZone.LONG : TimeZone.SHORT);
	  buffer.append(zoneID);
	  break;
	default:
	  throw new IllegalArgumentException("Illegal pattern character");
	}
	if (pos != null && p.field == pos.getField())
	  {
	    pos.setBeginIndex(beginIndex);
	    pos.setEndIndex(buffer.length());
	  }
      } else {
	buffer.append(o.toString());
      }
    }
    return buffer;
  }

  private void withLeadingZeros(int value, int length, StringBuffer buffer) 
  {
    String valStr = String.valueOf(value);
    for (length -= valStr.length(); length > 0; length--)
      buffer.append('0');
    buffer.append(valStr);
  }

  private final boolean expect (String source, ParsePosition pos, char ch)
  {
    int x = pos.getIndex();
    boolean r = x < source.length() && source.charAt(x) == ch;
    if (r)
      pos.setIndex(x + 1);
    else
      pos.setErrorIndex(x);
    return r;
  }

  /**
   * This method parses the specified string into a date.
   * 
   * @param dateStr The date string to parse.
   * @param pos The input and output parse position
   *
   * @return The parsed date, or <code>null</code> if the string cannot be
   * parsed.
   */
  public Date parse (String dateStr, ParsePosition pos)
  {
    int fmt_index = 0;
    int fmt_max = pattern.length();

    calendar.clear();
    boolean saw_timezone = false;
    int quote_start = -1;
    boolean is2DigitYear = false;
    for (; fmt_index < fmt_max; ++fmt_index)
      {
	char ch = pattern.charAt(fmt_index);
	if (ch == '\'')
	  {
	    int index = pos.getIndex();
	    if (fmt_index < fmt_max - 1
		&& pattern.charAt(fmt_index + 1) == '\'')
	      {
		if (! expect (dateStr, pos, ch))
		  return null;
		++fmt_index;
	      }
	    else
	      quote_start = quote_start < 0 ? fmt_index : -1;
	    continue;
	  }

	if (quote_start != -1
	    || ((ch < 'a' || ch > 'z')
		&& (ch < 'A' || ch > 'Z')))
	  {
	    if (! expect (dateStr, pos, ch))
	      return null;
	    continue;
	  }

	// We've arrived at a potential pattern character in the
	// pattern.
	int first = fmt_index;
	while (++fmt_index < fmt_max && pattern.charAt(fmt_index) == ch)
	  ;
	int fmt_count = fmt_index - first;

	// We might need to limit the number of digits to parse in
	// some cases.  We look to the next pattern character to
	// decide.
	boolean limit_digits = false;
	if (fmt_index < fmt_max
	    && standardChars.indexOf(pattern.charAt(fmt_index)) >= 0)
	  limit_digits = true;
	--fmt_index;

	// We can handle most fields automatically: most either are
	// numeric or are looked up in a string vector.  In some cases
	// we need an offset.  When numeric, `offset' is added to the
	// resulting value.  When doing a string lookup, offset is the
	// initial index into the string array.
	int calendar_field;
	boolean is_numeric = true;
	String[] match = null;
	int offset = 0;
	boolean maybe2DigitYear = false;
	switch (ch)
	  {
	  case 'd':
	    calendar_field = Calendar.DATE;
	    break;
	  case 'D':
	    calendar_field = Calendar.DAY_OF_YEAR;
	    break;
	  case 'F':
	    calendar_field = Calendar.DAY_OF_WEEK_IN_MONTH;
	    break;
	  case 'E':
	    is_numeric = false;
	    offset = 1;
	    calendar_field = Calendar.DAY_OF_WEEK;
	    match = (fmt_count <= 3
		     ? formatData.getShortWeekdays()
		     : formatData.getWeekdays());
	    break;
	  case 'w':
	    calendar_field = Calendar.WEEK_OF_YEAR;
	    break;
	  case 'W':
	    calendar_field = Calendar.WEEK_OF_MONTH;
	    break;
	  case 'M':
	    calendar_field = Calendar.MONTH;
	    if (fmt_count <= 2)
	      offset = -1;
	    else
	      {
		is_numeric = false;
		match = (fmt_count <= 3
			 ? formatData.getShortMonths()
			 : formatData.getMonths());
	      }
	    break;
	  case 'y':
	    calendar_field = Calendar.YEAR;
	    if (fmt_count <= 2)
	      maybe2DigitYear = true;
	    break;
	  case 'K':
	    calendar_field = Calendar.HOUR;
	    break;
	  case 'h':
	    calendar_field = Calendar.HOUR;
	    break;
	  case 'H':
	    calendar_field = Calendar.HOUR_OF_DAY;
	    break;
	  case 'k':
	    calendar_field = Calendar.HOUR_OF_DAY;
	    break;
	  case 'm':
	    calendar_field = Calendar.MINUTE;
	    break;
	  case 's':
	    calendar_field = Calendar.SECOND;
	    break;
	  case 'S':
	    calendar_field = Calendar.MILLISECOND;
	    break;
	  case 'a':
	    is_numeric = false;
	    calendar_field = Calendar.AM_PM;
	    match = formatData.getAmPmStrings();
	    break;
	  case 'z':
	    // We need a special case for the timezone, because it
	    // uses a different data structure than the other cases.
	    is_numeric = false;
	    calendar_field = Calendar.DST_OFFSET;
	    String[][] zoneStrings = formatData.getZoneStrings();
	    int zoneCount = zoneStrings.length;
	    int index = pos.getIndex();
	    boolean found_zone = false;
	    for (int j = 0;  j < zoneCount;  j++)
	      {
		String[] strings = zoneStrings[j];
		int k;
		for (k = 1; k < strings.length; ++k)
		  {
		    if (dateStr.startsWith(strings[k], index))
		      break;
		  }
		if (k != strings.length)
		  {
		    found_zone = true;
		    saw_timezone = true;
		    TimeZone tz = TimeZone.getTimeZone (strings[0]);
		    calendar.set (Calendar.ZONE_OFFSET, tz.getRawOffset ());
		    offset = 0;
		    if (k > 2 && tz instanceof SimpleTimeZone)
		      {
			SimpleTimeZone stz = (SimpleTimeZone) tz;
			offset = stz.getDSTSavings ();
		      }
		    pos.setIndex(index + strings[k].length());
		    break;
		  }
	      }
	    if (! found_zone)
	      {
		pos.setErrorIndex(pos.getIndex());
		return null;
	      }
	    break;
	  default:
	    pos.setErrorIndex(pos.getIndex());
	    return null;
	  }

	// Compute the value we should assign to the field.
	int value;
	int index = -1;
	if (is_numeric)
	  {
	    numberFormat.setMinimumIntegerDigits(fmt_count);
	    if (limit_digits)
	      numberFormat.setMaximumIntegerDigits(fmt_count);
	    if (maybe2DigitYear)
	      index = pos.getIndex();
	    Number n = numberFormat.parse(dateStr, pos);
	    if (pos == null || ! (n instanceof Long))
	      return null;
	    value = n.intValue() + offset;
	  }
	else if (match != null)
	  {
	    index = pos.getIndex();
	    int i;
	    for (i = offset; i < match.length; ++i)
	      {
		if (dateStr.startsWith(match[i], index))
		  break;
	      }
	    if (i == match.length)
	      {
		pos.setErrorIndex(index);
		return null;
	      }
	    pos.setIndex(index + match[i].length());
	    value = i;
	  }
	else
	  value = offset;
	  
	if (maybe2DigitYear)
	  {
	    // Parse into default century if the numeric year string has 
	    // exactly 2 digits.
	    int digit_count = pos.getIndex() - index;
	    if (digit_count == 2)
	      is2DigitYear = true;
	  }

	// Assign the value and move on.
	calendar.set(calendar_field, value);
      }
    
    if (is2DigitYear)
      {
	// Apply the 80-20 heuristic to dermine the full year based on 
	// defaultCenturyStart. 
	int year = defaultCentury + calendar.get(Calendar.YEAR);
	calendar.set(Calendar.YEAR, year);
	if (calendar.getTime().compareTo(defaultCenturyStart) < 0)
	  calendar.set(Calendar.YEAR, year + 100);      
      }

    try
      {
	if (! saw_timezone)
	  {
	    // Use the real rules to determine whether or not this
	    // particular time is in daylight savings.
	    calendar.clear (Calendar.DST_OFFSET);
	    calendar.clear (Calendar.ZONE_OFFSET);
	  }
        return calendar.getTime();
      }
    catch (IllegalArgumentException x)
      {
        pos.setErrorIndex(pos.getIndex());
	return null;
      }
  }

  // Compute the start of the current century as defined by
  // get2DigitYearStart.
  private void computeCenturyStart()
  {
    int year = calendar.get(Calendar.YEAR);
    calendar.set(Calendar.YEAR, year - 80);
    set2DigitYearStart(calendar.getTime());
  }
}
