/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.util.*;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 25, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  parse is not implemented.
 */

public class SimpleDateFormat extends DateFormat
{
  private Date defaultCenturyStart;
  private DateFormatSymbols formatData;
  private String pattern;

  public SimpleDateFormat ()
  {
    this("dd/MM/yy HH:mm", Locale.getDefault());
  }

  public SimpleDateFormat (String pattern)
  {
    this(pattern, Locale.getDefault());
  }

  public SimpleDateFormat (String pattern, Locale locale)
  {
    this.pattern = pattern;
    this.calendar = Calendar.getInstance(locale);
    this.numberFormat = NumberFormat.getInstance(locale);
    numberFormat.setGroupingUsed(false);
    this.formatData = new DateFormatSymbols (locale);
  }

  public SimpleDateFormat (String pattern, DateFormatSymbols formatData)
  {
    this.pattern = pattern;
    this.formatData = formatData;
    this.calendar = Calendar.getInstance();
    this.numberFormat = NumberFormat.getInstance();
    numberFormat.setGroupingUsed(false);
  }

  public Date get2DigitYearStart()
  {
    return defaultCenturyStart;
  }

  public void set2DigitYearStart(Date startDate)
  {
    defaultCenturyStart = startDate;
  }

  public DateFormatSymbols getDateFormatSymbols ()
  {
    return formatData;
  }

  public void setDateFormatSymbols (DateFormatSymbols value)
  {
    formatData = value;
  }

  public String toPattern ()
  {
    return pattern;
  }

  public void applyPattern (String pattern)
  {
    this.pattern = pattern;
  }

  private String applyLocalizedPattern (String pattern,
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

  public void applyLocalizedPattern (String pattern)
  {
    String localChars = formatData.getLocalPatternChars();
    String standardChars = DateFormatSymbols.localPatternCharsDefault;
    pattern = applyLocalizedPattern (pattern, localChars, standardChars);
    applyPattern(pattern);
  }

  public String toLocalizedPattern ()
  {
    String localChars = formatData.getLocalPatternChars();
    String standardChars = DateFormatSymbols.localPatternCharsDefault;
    return applyLocalizedPattern (pattern, standardChars, localChars);
  }

  private final void append (StringBuffer buf, int value, int numDigits)
  {
    numberFormat.setMinimumIntegerDigits(numDigits);
    numberFormat.format(value, buf, null);
  }

  public StringBuffer format (Date date, StringBuffer buf, FieldPosition pos)
  {
    Calendar calendar = (Calendar) this.calendar.clone();
    calendar.setTime(date);
    int len = pattern.length();
    int quoteStart = -1;
    for (int i = 0;  i < len;  i++)
      {
	char ch = pattern.charAt(i);
	if (ch == '\'')
	  {
	    // We must do a little lookahead to see if we have two
	    // single quotes embedded in quoted text.
	    if (i < len - 1 && pattern.charAt(i + 1) == '\'')
	      {
		++i;
		buf.append(ch);
	      }
	    else
	      quoteStart = quoteStart < 0 ? i : -1;
	  }
	// From JCL: any characters in the pattern that are not in
	// the ranges of [a..z] and [A..Z] are treated as quoted
	// text.
	else if (quoteStart != -1
	    || ((ch < 'a' || ch > 'z')
		&& (ch < 'A' || ch > 'Z')))
	  buf.append(ch);
	else
	  {
	    int first = i;
	    int value;
	    while (++i < len && pattern.charAt(i) == ch) ;
	    int count = i - first; // Number of repetions of ch in pattern.
	    int beginIndex = buf.length();
	    int field;
	    i--;  // Skip all but last instance of ch in pattern.
	    switch (ch)
	      {
	      case 'd':
		append(buf, calendar.get(Calendar.DATE), count);
		field = DateFormat.DATE_FIELD;
		break;
	      case 'D':
		append(buf, calendar.get(Calendar.DAY_OF_YEAR), count);
		field = DateFormat.DAY_OF_YEAR_FIELD;
		break;
	      case 'F':
		append(buf, calendar.get(Calendar.DAY_OF_WEEK_IN_MONTH),count);
		field = DateFormat.DAY_OF_WEEK_IN_MONTH_FIELD;
		break;
	      case 'E':
		value = calendar.get(calendar.DAY_OF_WEEK);
		buf.append(count <= 3 ? formatData.getShortWeekdays()[value]
			   : formatData.getWeekdays()[value]);
		field = DateFormat.DAY_OF_WEEK_FIELD;
		break;
	      case 'w':
		append(buf, calendar.get(Calendar.WEEK_OF_YEAR), count); 
		field = DateFormat.WEEK_OF_YEAR_FIELD;
                break;
	      case 'W':
		append(buf, calendar.get(Calendar.WEEK_OF_MONTH), count); 
		field = DateFormat.WEEK_OF_MONTH_FIELD;
                break;
	      case 'M':
		value = calendar.get(Calendar.MONTH);
		if (count <= 2)
		  append(buf, value + 1, count);
		else
		  buf.append(count <= 3 ? formatData.getShortMonths()[value]
			   : formatData.getMonths()[value]);
		field = DateFormat.MONTH_FIELD;
		break;
	      case 'y':
		value = calendar.get(Calendar.YEAR);
		append(buf, count <= 2 ? value % 100 : value, count);
		field = DateFormat.YEAR_FIELD;
		break;
	      case 'K':
		append(buf, calendar.get(Calendar.HOUR), count);
		field = DateFormat.HOUR0_FIELD;
		break;
	      case 'h':
		value = ((calendar.get(Calendar.HOUR) + 11) % 12) + 1;
		append(buf, value, count);
		field = DateFormat.HOUR1_FIELD;
		break;
	      case 'H':
		append(buf, calendar.get(Calendar.HOUR_OF_DAY), count);
		field = DateFormat.HOUR_OF_DAY0_FIELD;
		break;
	      case 'k':
		value = ((calendar.get(Calendar.HOUR_OF_DAY) + 23) % 24) + 1;
		append(buf, value, count);
		field = DateFormat.HOUR_OF_DAY1_FIELD;
		break;
	      case 'm':
		append(buf, calendar.get(Calendar.MINUTE), count);
		field = DateFormat.MINUTE_FIELD;
		break;
	      case 's':
		append(buf, calendar.get(Calendar.SECOND), count);
		field = DateFormat.SECOND_FIELD;
		break;
	      case 'S':
		append(buf, calendar.get(Calendar.MILLISECOND), count);
		field = DateFormat.MILLISECOND_FIELD;
		break;
	      case 'a':
		value = calendar.get(calendar.AM_PM);
		buf.append(formatData.getAmPmStrings()[value]);
		field = DateFormat.AM_PM_FIELD;
		break;
	      case 'z':
		String zoneID = calendar.getTimeZone().getID();
		String[][] zoneStrings = formatData.getZoneStrings();
		int zoneCount = zoneStrings.length;
		for (int j = 0;  j < zoneCount;  j++)
		  {
		    String[] strings = zoneStrings[j];
		    if (zoneID.equals(strings[0]))
		      {
			j = count > 3 ? 2 : 1;
			if (calendar.get(Calendar.DST_OFFSET) != 0)
			  j+=2;
			zoneID = strings[j];
			break;
		      }
		  }
		buf.append(zoneID);
		field = DateFormat.TIMEZONE_FIELD;
		break;
	      default:
		// Note that the JCL is actually somewhat
		// contradictory here.  It defines the pattern letters
		// to be a particular list, but also says that a
		// pattern containing an invalid pattern letter must
		// throw an exception.  It doesn't describe what an
		// invalid pattern letter might be, so we just assume
		// it is any letter in [a-zA-Z] not explicitly covered
		// above.
		throw new RuntimeException("bad format string");
	      }
	    if (pos != null && field == pos.getField())
	      {
		pos.setBeginIndex(beginIndex);
		pos.setEndIndex(buf.length());
	      }
	  }
      }
    return buf;
  }

  private final boolean expect (String source, ParsePosition pos,
				char ch)
  {
    int x = pos.getIndex();
    boolean r = x < source.length() && source.charAt(x) == ch;
    if (r)
      pos.setIndex(x + 1);
    else
      pos.setErrorIndex(x);
    return r;
  }

  public Date parse (String source, ParsePosition pos)
  {
    int fmt_index = 0;
    int fmt_max = pattern.length();

    calendar.clear();
    int quote_start = -1;
    for (; fmt_index < fmt_max; ++fmt_index)
      {
	char ch = pattern.charAt(fmt_index);
	if (ch == '\'')
	  {
	    int index = pos.getIndex();
	    if (fmt_index < fmt_max - 1
		&& pattern.charAt(fmt_index + 1) == '\'')
	      {
		if (! expect (source, pos, ch))
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
	    if (! expect (source, pos, ch))
	      return null;
	    continue;
	  }

	// We've arrived at a potential pattern character in the
	// pattern.
	int first = fmt_index;
	while (++fmt_index < fmt_max && pattern.charAt(fmt_index) == ch)
	  ;
	int count = fmt_index - first;
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
	int zone_number = 0;
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
	    match = (count <= 3
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
	    if (count <= 2)
	      ;
	    else
	      {
		is_numeric = false;
		match = (count <= 3
			 ? formatData.getShortMonths()
			 : formatData.getMonths());
	      }
	    break;
	  case 'y':
	    calendar_field = Calendar.YEAR;
	    if (count <= 2)
	      offset = 1900;
	    break;
	  case 'K':
	    calendar_field = Calendar.HOUR;
	    break;
	  case 'h':
	    calendar_field = Calendar.HOUR;
	    offset = -1;
	    break;
	  case 'H':
	    calendar_field = Calendar.HOUR_OF_DAY;
	    break;
	  case 'k':
	    calendar_field = Calendar.HOUR_OF_DAY;
	    offset = -1;
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
		    if (source.startsWith(strings[k], index))
		      break;
		  }
		if (k != strings.length)
		  {
		    if (k > 2)
		      ;		// FIXME: dst.
		    zone_number = 0; // FIXME: dst.
		    // FIXME: raw offset to SimpleTimeZone const.
		    calendar.setTimeZone(new SimpleTimeZone (1, strings[0]));
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
	if (is_numeric)
	  {
	    numberFormat.setMinimumIntegerDigits(count);
	    Number n = numberFormat.parse(source, pos);
	    if (pos == null || ! (n instanceof Long))
	      return null;
	    value = n.intValue() + offset;
	  }
	else if (match != null)
	  {
	    int index = pos.getIndex();
	    int i;
	    for (i = offset; i < match.length; ++i)
	      {
		if (source.startsWith(match[i], index))
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
	  value = zone_number;

	// Assign the value and move on.
	try
	  {
	    calendar.set(calendar_field, value);
	  }
	// FIXME: what exception is thrown on an invalid
	// non-lenient set?
	catch (IllegalArgumentException x)
	  {
	    pos.setErrorIndex(pos.getIndex());
	    return null;
	  }
      }

    return calendar.getTime();
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof SimpleDateFormat) || ! super.equals(obj) )
      return false;
    SimpleDateFormat other = (SimpleDateFormat) obj;
    return (DateFormatSymbols.equals(pattern, other.pattern)
	    && DateFormatSymbols.equals(formatData, other.formatData)
	    && DateFormatSymbols.equals(defaultCenturyStart,
					other.defaultCenturyStart));
  }

  public Object clone ()
  {
    // We know the superclass just call's Object's generic cloner.
    return super.clone ();
  }

  public int hashCode ()
  {
    int hash = super.hashCode();
    if (pattern != null)
      hash ^= pattern.hashCode();
    return hash;
  }
}
