/* SimpleDateFormat.java -- A class for parsing/formating simple 
   date constructs
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.text;

import java.util.Calendar;
import java.util.Date;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;
import java.util.SimpleTimeZone;
import java.util.Vector;
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

  private transient Vector tokens;
  private DateFormatSymbols formatData;  // formatData
  private Date defaultCenturyStart = 
    new Date(System.currentTimeMillis() - (80*365*24*60*60*1000));
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
        defaultCenturyStart =
	  new Date(System.currentTimeMillis() - (80*365*24*60*60*1000));
	serialVersionOnStream = 1;
      }

    // Set up items normally taken care of by the constructor.
    tokens = new Vector();
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
	if (Character.isLetter(thisChar)) {
	  // Not a valid letter
	  tokens.addElement(new FieldSizePair(-1,0));
	} else if (thisChar == '\'') {
	  // Quoted text section; skip to next single quote
	  pos = pattern.indexOf('\'',i+1);
	  if (pos == -1) {
	    // This ought to be an exception, but spec does not
	    // let us throw one.
	    tokens.addElement(new FieldSizePair(-1,0));
	  }
	  if ((pos+1 < pattern.length()) && (pattern.charAt(pos+1) == '\'')) {
	    tokens.addElement(pattern.substring(i+1,pos+1));
	  } else {
	    tokens.addElement(pattern.substring(i+1,pos));
	  }
	  i = pos;
	} else {
	  // A special character
	  tokens.addElement(new Character(thisChar));
	}
      } else {
	// A valid field
	if ((current != null) && (field == current.field)) {
	  current.size++;
	} else {
	  current = new FieldSizePair(field,1);
	  tokens.addElement(current);
	}
      }
    }
  }
    
  public String toString() 
  {
    StringBuffer output = new StringBuffer();
    Enumeration e = tokens.elements();
    while (e.hasMoreElements()) {
      output.append(e.nextElement().toString());
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
    tokens = new Vector();
    formatData = new DateFormatSymbols(locale);
    pattern = formatData.dateFormats[DEFAULT]+' '+formatData.timeFormats[DEFAULT];
    compileFormat(pattern);
    numberFormat = NumberFormat.getInstance(locale);
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
    tokens = new Vector();
    formatData = new DateFormatSymbols(locale);
    compileFormat(pattern);
    this.pattern = pattern;
    numberFormat = NumberFormat.getInstance(locale);
  }

  /**
   * Creates a date formatter using the specified pattern. The
   * specified DateFormatSymbols will be used when formatting.
   */
  public SimpleDateFormat(String pattern, DateFormatSymbols formatData) {
    super();
    calendar = new GregorianCalendar();
    // FIXME: XXX: Is it really necessary to set the timezone?
    // The Calendar constructor is supposed to take care of this.
    calendar.setTimeZone(TimeZone.getDefault());
    tokens = new Vector();
    this.formatData = formatData;
    compileFormat(pattern);
    this.pattern = pattern;
    numberFormat = NumberFormat.getInstance();
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
    tokens = new Vector();
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
    if (o == null)
      return false;

    if (!super.equals(o))
      return false;

    if (!(o instanceof SimpleDateFormat))
      return false;

    SimpleDateFormat sdf = (SimpleDateFormat)o;

    if (!toPattern().equals(sdf.toPattern()))
      return false;

    if (!get2DigitYearStart().equals(sdf.get2DigitYearStart()))
      return false;

    if (!getDateFormatSymbols().equals(sdf.getDateFormatSymbols()))
      return false;

    return true;
  }


  /**
   * Formats the date input according to the format string in use,
   * appending to the specified StringBuffer.  The input StringBuffer
   * is returned as output for convenience.
   */
  public StringBuffer format(Date date, StringBuffer buffer, FieldPosition pos) {
    String temp;
    Calendar theCalendar = (Calendar) calendar.clone();
    theCalendar.setTime(date);
    
    // go through vector, filling in fields where applicable, else toString
    Enumeration e = tokens.elements();
    while (e.hasMoreElements()) {
      Object o = e.nextElement();
      if (o instanceof FieldSizePair) {
	FieldSizePair p = (FieldSizePair) o;
	int beginIndex = buffer.length();
	switch (p.field) {
	case ERA_FIELD:
	  buffer.append(formatData.eras[theCalendar.get(Calendar.ERA)]);
	  break;
	case YEAR_FIELD:
	  temp = String.valueOf(theCalendar.get(Calendar.YEAR));
	  if (p.size < 4)
	    buffer.append(temp.substring(temp.length()-2));
	  else
	    buffer.append(temp);
	  break;
	case MONTH_FIELD:
	  if (p.size < 3)
	    withLeadingZeros(theCalendar.get(Calendar.MONTH)+1,p.size,buffer);
	  else if (p.size < 4)
	    buffer.append(formatData.shortMonths[theCalendar.get(Calendar.MONTH)]);
	  else
	    buffer.append(formatData.months[theCalendar.get(Calendar.MONTH)]);
	  break;
	case DATE_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.DATE),p.size,buffer);
	  break;
	case HOUR_OF_DAY1_FIELD: // 1-24
	  withLeadingZeros(((theCalendar.get(Calendar.HOUR_OF_DAY)+23)%24)+1,p.size,buffer);
	  break;
	case HOUR_OF_DAY0_FIELD: // 0-23
	  withLeadingZeros(theCalendar.get(Calendar.HOUR_OF_DAY),p.size,buffer);
	  break;
	case MINUTE_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.MINUTE),p.size,buffer);
	  break;
	case SECOND_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.SECOND),p.size,buffer);
	  break;
	case MILLISECOND_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.MILLISECOND),p.size,buffer);
	  break;
	case DAY_OF_WEEK_FIELD:
	  if (p.size < 4)
	    buffer.append(formatData.shortWeekdays[theCalendar.get(Calendar.DAY_OF_WEEK)]);
	  else
	    buffer.append(formatData.weekdays[theCalendar.get(Calendar.DAY_OF_WEEK)]);
	  break;
	case DAY_OF_YEAR_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.DAY_OF_YEAR),p.size,buffer);
	  break;
	case DAY_OF_WEEK_IN_MONTH_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.DAY_OF_WEEK_IN_MONTH),p.size,buffer);
	  break;
	case WEEK_OF_YEAR_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.WEEK_OF_YEAR),p.size,buffer);
	  break;
	case WEEK_OF_MONTH_FIELD:
	  withLeadingZeros(theCalendar.get(Calendar.WEEK_OF_MONTH),p.size,buffer);
	  break;
	case AM_PM_FIELD:
	  buffer.append(formatData.ampms[theCalendar.get(Calendar.AM_PM)]);
	  break;
	case HOUR1_FIELD: // 1-12
	  withLeadingZeros(((theCalendar.get(Calendar.HOUR)+11)%12)+1,p.size,buffer);
	  break;
	case HOUR0_FIELD: // 0-11
	  withLeadingZeros(theCalendar.get(Calendar.HOUR),p.size,buffer);
	  break;
	case TIMEZONE_FIELD:
	  TimeZone zone = theCalendar.getTimeZone();
	  boolean isDST = theCalendar.get(Calendar.DST_OFFSET) != 0;
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

  private void withLeadingZeros(int value, int length, StringBuffer buffer) {
    String valStr = String.valueOf(value);
    for (length -= valStr.length(); length > 0; length--)
      buffer.append('0');
    buffer.append(valStr);
  }

  private int indexInArray(String dateStr, int index, String[] values) {
    int l1 = dateStr.length()-index;
    int l2;

    for (int i=0; i < values.length; i++) {
      if (values[i] == null)
        continue;

      l2 = values[i].length();
      //System.err.println(values[i] + " " + dateStr.substring(index,index+l2));
      if ((l1 >= l2) && (dateStr.substring(index,index+l2).equals(values[i])))
	return i;
    }
    return -1;
  }

  /*
   * Get the actual year value, converting two digit years if necessary.
   */
  private int processYear(int val)
  {
    if (val > 100)
      return val;

    Date d = get2DigitYearStart();
    Calendar c = Calendar.getInstance();
    c.setTime(d);
    int y = c.get(YEAR_FIELD);

    return ((y / 100) * 100) + val;
  }

  /*
   * Ok, we ignore the format string and just try to parse what we can
   * out of the string.  We need, month, day, year at a minimum. The real
   * killer is stuff like XX/XX/XX.  How do we interpret that?  Is is the
   * US style MM/DD/YY or the European style DD/MM/YY. Or is it YYYY/MM/DD?
   * I'm an American, so I guess you know which one I'm choosing....
   */
  private Date parseLenient(String dateStr, ParsePosition pos)
  {
    int month = -1;
    int day = -1;
    int year = -1;
    int era = -1;
    int hour = -1;
    int hour24 = -1;
    int minute = -1;
    int second = -1;
    int millis = -1;
    int ampm = -1;
    int last = -1;
    TimeZone tz = null;
    char lastsep = ' ';
    char nextchar = ' ';

    Calendar cal = (Calendar)calendar.clone();
    cal.clear();
    cal.setTime(new Date(0));

    int index = pos.getIndex();
    String buf = dateStr.substring(index, dateStr.length());

    top:
    for(;;)
      {

        // Are we at the end of the string?  If so, make sure we have
        // enough data and return. // FIXME: Also detect sufficient data
        // and return by setting buf to "" on an unparsible string.
        if (buf.equals(""))
          {
            pos.setIndex(index);

            // This is the minimum we need
            if ((month == -1) || (day == -1) || (year == -1))
              {
                pos.setErrorIndex(index);
                return null;
              }

             if (tz != null)
               cal.setTimeZone(tz);

             cal.set(Calendar.YEAR, year);
             cal.set(Calendar.MONTH, month - 1);
             cal.set(Calendar.DATE, day);           

             if (ampm == 0)
               cal.set(Calendar.AM_PM, Calendar.AM);
             else if (ampm == 1)
               cal.set(Calendar.AM_PM, Calendar.PM);

             // If am/pm not set, we assume 24 hour day
             if (hour != -1)
               {
                 if (ampm == -1)
                   cal.set(Calendar.HOUR_OF_DAY, hour);
                 else
                   {
                     if (ampm == 0)
                       {
                         if (hour == 12)
                           hour = 0;
                       }
                     else
                       {
                         if (hour != 12)
                           hour += 12;
                       }
    
                     cal.set(Calendar.HOUR_OF_DAY, hour);
                   }
               }

             if (minute != -1)
               cal.set(Calendar.MINUTE, minute);

             if (second != -1)
               cal.set(Calendar.SECOND, second);

             if (millis != -1)
               cal.set(Calendar.MILLISECOND, millis);

             if (era == 0)
               cal.set(Calendar.ERA, GregorianCalendar.BC);
             else if (era == 1)
               cal.set(Calendar.ERA, GregorianCalendar.AD);

             return cal.getTime();
          }

        // Skip over whitespace and expected punctuation
        char c = buf.charAt(0);
        boolean comma_found = false;
        while(Character.isWhitespace(c) || (c == ':') || 
              (c == ',') || (c == '.') || (c == '/'))
          {
            lastsep = c;
            if (c == ',') // This is a total and utter crock
              comma_found = true;
            buf = buf.substring(1);
            if (buf.equals(""))
              continue;
            c = buf.charAt(0);
          }

        if (comma_found == true)
          lastsep = ',';

        // Is it a month name?
        for (int i = 0; i < formatData.months.length; i++)
          if ((formatData.months[i] != null) 
              && buf.startsWith(formatData.months[i]))
            {
              month = i + 1;
              buf = buf.substring(formatData.months[i].length());
              index += formatData.months[i].length();
              last = MONTH_FIELD;
              continue top;
            }

        // Is it a short month name?
        for (int i = 0; i < formatData.shortMonths.length; i++)
          if ((formatData.shortMonths[i] != null) 
              && buf.startsWith(formatData.shortMonths[i]))
            {
              month = i + 1;
              buf = buf.substring(formatData.shortMonths[i].length());
              index += formatData.shortMonths[i].length();
              last = MONTH_FIELD;
              continue top;
            }

        // Is it a weekday name?
        for (int i = 0; i < formatData.weekdays.length; i++)
          if ((formatData.weekdays[i] != null)
              && buf.startsWith(formatData.weekdays[i]))
            {
              buf = buf.substring(formatData.weekdays[i].length());
              index += formatData.weekdays[i].length();
              last = DAY_OF_WEEK_FIELD;
              continue top;
            }

        // Is it a short weekday name?
        for (int i = 0; i < formatData.shortWeekdays.length; i++)
          if ((formatData.shortWeekdays[i] != null)
              && buf.startsWith(formatData.shortWeekdays[i]))
            {
              buf = buf.substring(formatData.shortWeekdays[i].length());
              index += formatData.shortWeekdays[i].length();
              last = DAY_OF_WEEK_FIELD;
              continue top;
            }

        // Is this an am/pm string?
        for (int i = 0; i < formatData.ampms.length; i++) {
          if ((formatData.ampms[i] != null)
              && buf.toLowerCase().startsWith(formatData.ampms[i].toLowerCase()))
            {
              ampm = i;
              buf = buf.substring(formatData.ampms[i].length());
              index += formatData.ampms[i].length();
              last = AM_PM_FIELD;
              continue top;
            }
        }

        // See if we have a number
        c = buf.charAt(0);
        String nbrstr = "";
        while (Character.isDigit(c))
          {
            nbrstr = nbrstr + c;
            buf = buf.substring(1);
            if (buf.equals(""))
              break;
            c = buf.charAt(0);
          }

        // If we didn't get a number, try for a timezone, otherwise set buf
        // to "" and loop to see if we are done.
        if (nbrstr.equals(""))
          {
            // Ok, try for a timezone name
            while(!Character.isWhitespace(c) && (c != ',') && (c != '.') &&
                  (c != ':') && (c != '/'))
              {
                nbrstr = nbrstr + c;
                buf = buf.substring(1);
                if (buf.equals(""))
                  break;
                c = buf.charAt(0);
              }
            TimeZone tmptz = TimeZone.getTimeZone(nbrstr);
             
            // We get GMT on failure, so be sure we asked for it.
            if (tmptz.getID().equals("GMT"))
              {
                if (!nbrstr.equals("GMT"))
                  {
                    buf = "";
                    continue top;
                  }
              }

            tz = tmptz;
            last = TIMEZONE_FIELD;
            index += nbrstr.length();
            continue top;
          }

        // Convert to integer
        int val = 0;
        try
          {
            val = Integer.parseInt(nbrstr);
          }
        catch(Exception e)
          {
            return null; // Shouldn't happen
          }

        if (!buf.equals(""))
          nextchar = buf.charAt(0);
        else
          nextchar = ' ';

        // Figure out which value to assign to
        // I make bad US assumptions about MM/DD/YYYY
        if (last == DAY_OF_WEEK_FIELD)
          {
            day = val;
            last = DATE_FIELD;
          }
        else if ((last == MONTH_FIELD) && (day != -1))
          {
            year = processYear(val);
            last = YEAR_FIELD;
          }
        else if (last == MONTH_FIELD)
          {
            day = val;
            last = DATE_FIELD;
          }
        else if (last == -1)
          {
            // Assume month
            if ((val < 13) && (val > 0))
              {
                month = val;
                last = MONTH_FIELD;
              }
            // Assume year. This only works for two digit years that aren't
            // between 01 and 12
            else
              {
                year = processYear(val);
                last = YEAR_FIELD;
              } 
          }
        else if ((last == YEAR_FIELD) && ((nextchar == '/') ||
                 (nextchar == '.')))
          {
            month = val;
            last = MONTH_FIELD;
          }
        else if (last == YEAR_FIELD)
          {
            hour = val;
            last = HOUR0_FIELD;
          }
        else if ((last == DATE_FIELD) && ((nextchar == '/') ||
                 (nextchar == '.') || buf.equals("")))
          {
            year = processYear(val);
            last = YEAR_FIELD;
          }
        else if ((last == DATE_FIELD) && ((lastsep == '/') ||
                 (lastsep == '.') || (lastsep == ',')))
          {
            year = processYear(val);
            last = YEAR_FIELD;
          }
        else if (last == DATE_FIELD)
          {
            hour = val;
            last = HOUR0_FIELD;
          }
        else if (last == HOUR0_FIELD)
          {
            minute = val;
            last = MINUTE_FIELD;
          }
        else if (last == MINUTE_FIELD)
          {
            second = val;
            last = SECOND_FIELD;
          }
        else if (lastsep == '.') 
          {
            ; // This is milliseconds or something.  Ignore it
            last = WEEK_OF_YEAR_FIELD; // Just a random value
          }
        else // It is year. I have spoken!
          {
            year = processYear(val);
            last = YEAR_FIELD;
          }
      }
  }

  private int parseLeadingZeros(String dateStr, ParsePosition pos,
                                FieldSizePair p)
  {
    int value;
    int index = pos.getIndex();
    String buf = null;

    if (p.size == 1)
      {
        char c = dateStr.charAt(index+1);
        if ((dateStr.charAt(index) == '1') && 
             Character.isDigit(dateStr.charAt(index+1)))
          buf = dateStr.substring(index, index+2);
        else
          buf = dateStr.substring(index, index+1);
        pos.setIndex(index + buf.length());
      }
    else if (p.size == 2)
      {
        buf = dateStr.substring(index, index+2);
        pos.setIndex(index+2);
      }
    else if (p.size == 3)
      {
        buf = dateStr.substring(index, index+3);
        pos.setIndex(index+3);
      }
    else
      {
        buf = dateStr.substring(index, index+4);
        pos.setIndex(index+4);
      }
    try
      {
        value = Integer.parseInt(buf);
      }
    catch(NumberFormatException nfe)
      {
        pos.setIndex(index);
        pos.setErrorIndex(index);
        return -1;
      } 

    return value;
  }

  /*
   * Note that this method doesn't properly protect against
   * StringIndexOutOfBoundsException.  FIXME
   */
  private Date parseStrict(String dateStr, ParsePosition pos)
  {
    // start looking at position pos.index
    Enumeration e = tokens.elements();
    Calendar theCalendar = (Calendar) calendar.clone();
    theCalendar.clear();
    theCalendar.setTime(new Date(0));

    int value, index, hour = -1;
    String buf;
    while (pos.getIndex() < dateStr.length()) {
      Object o = e.nextElement();
      if (o instanceof FieldSizePair) {
	FieldSizePair p = (FieldSizePair) o;
	switch (p.field) {

	case ERA_FIELD:
	  value = indexInArray(dateStr,pos.getIndex(),formatData.eras);
	  if (value == -1) {
	    pos.setErrorIndex(pos.getIndex());
	    return null;
	  }
	  pos.setIndex(pos.getIndex() + formatData.eras[value].length());
	  theCalendar.set(Calendar.ERA,value);
	  break;

	case YEAR_FIELD:
          String y;
	  if (p.size < 4)
            y = dateStr.substring(pos.getIndex(), pos.getIndex() + 2);
          else
            y = dateStr.substring(pos.getIndex(), pos.getIndex() + 4);
            
          int year;
          try
            {
              year = Integer.parseInt(y);
            }
          catch(NumberFormatException nfe)
            {
              pos.setErrorIndex(pos.getIndex());
              return null;
            }

	  if (p.size < 4)
            year += get2DigitYearStart().getYear();

          theCalendar.set(Calendar.YEAR, year);
	  if (p.size < 4)
            pos.setIndex(pos.getIndex()+2);
          else
            pos.setIndex(pos.getIndex()+4);
	  break;

	case MONTH_FIELD:
          if (p.size > 2)
            {
              index = pos.getIndex();

	      value = indexInArray(dateStr,pos.getIndex(),
                 (p.size == 3) ? formatData.shortMonths : formatData.months);
	      if (value == -1) 
                {
	          pos.setErrorIndex(pos.getIndex());
	          return null;
	        }
              if (p.size == 3)
                pos.setIndex(index + formatData.shortMonths[value].length());
              else
                pos.setIndex(index + formatData.months[value].length());
              theCalendar.set(Calendar.MONTH, value);
              break;
            }

          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;

          theCalendar.set(Calendar.MONTH, value);
          break;

	case DATE_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;

          theCalendar.set(Calendar.DATE, value);
	  break;

	case HOUR_OF_DAY1_FIELD:
	case HOUR_OF_DAY0_FIELD:
          index = pos.getIndex();
          buf = dateStr.substring(index, index+2);
          try
            {
              value = Integer.parseInt(buf);
            }
          catch(NumberFormatException nfe)
            {
              return null;
            }
          if (p.field == HOUR_OF_DAY0_FIELD)
           // theCalendar.set(Calendar.HOUR_OF_DAY, value);
            hour = value + 1;
          else
           // theCalendar.set(Calendar.HOUR_OF_DAY, value-1);
            hour = value;
          pos.setIndex(index+2);

	  break;

	case MINUTE_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;

          theCalendar.set(Calendar.MINUTE, value);
	  break;

	case SECOND_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;

          theCalendar.set(Calendar.SECOND, value);
	  break;

	case MILLISECOND_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;
         
          theCalendar.set(Calendar.MILLISECOND, value);
	  break;

	case DAY_OF_WEEK_FIELD:
	  value = indexInArray(dateStr,pos.getIndex(),(p.size < 4) ? formatData.shortWeekdays : formatData.weekdays);
	  if (value == -1) {
	    pos.setErrorIndex(pos.getIndex());
	    return null;
	  }
	  pos.setIndex(pos.getIndex() + ((p.size < 4) ? formatData.shortWeekdays[value].length()
	    : formatData.weekdays[value].length()));
	  // Note: Calendar.set(Calendar.DAY_OF_WEEK,value) does not work
	  // as implemented in jdk1.1.5 (possibly DAY_OF_WEEK is meant to
	  // be read-only). Instead, calculate number of days offset.
	  theCalendar.add(Calendar.DATE,value 
			  - theCalendar.get(Calendar.DAY_OF_WEEK));
	  // in JDK, this seems to clear the hours, so we'll do the same.
	  theCalendar.set(Calendar.HOUR_OF_DAY,0);
	  break;

	case DAY_OF_YEAR_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;
         
	  theCalendar.set(Calendar.DAY_OF_YEAR, value);
	  break;

        // Just parse and ignore
	case DAY_OF_WEEK_IN_MONTH_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;
         
	  break;

        // Just parse and ignore
	case WEEK_OF_YEAR_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;
         
	  break;

        // Just parse and ignore
	case WEEK_OF_MONTH_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;

	  break;

	case AM_PM_FIELD:
	  value = indexInArray(dateStr,pos.getIndex(),formatData.ampms);
	  if (value == -1) {
	    pos.setErrorIndex(pos.getIndex());
	    return null;
	  }
	  pos.setIndex(pos.getIndex() + formatData.ampms[value].length());
	  theCalendar.set(Calendar.AM_PM,value);
	  break;

	case HOUR1_FIELD:
	case HOUR0_FIELD:
          value = parseLeadingZeros(dateStr, pos, p);
          if (value == -1)
            return null;
          if (p.field == HOUR1_FIELD)
            theCalendar.set(Calendar.HOUR, value);
          if (p.field == HOUR0_FIELD)
            theCalendar.set(Calendar.HOUR, value+1);
	  break;

	  /*
	case TIMEZONE_FIELD:
	  // TODO: FIXME: XXX
	  break;
	  */

	default:
	  throw new IllegalArgumentException("Illegal pattern character: " +
             p.field);
	} // end switch
      } else if (o instanceof String) {
	String ostr = (String) o;
	if (dateStr.substring(pos.getIndex(),pos.getIndex()+ostr.length()).equals(ostr)) {
	  pos.setIndex(pos.getIndex() + ostr.length());
	} else {
	  pos.setErrorIndex(pos.getIndex());
	  return null;
	}
      } else if (o instanceof Character) {
	Character ochar = (Character) o;
	if (dateStr.charAt(pos.getIndex()) == ochar.charValue()) {
	  pos.setIndex(pos.getIndex() + 1);
	} else {
	  pos.setErrorIndex(pos.getIndex());
	  return null;
	}
      }
    }

    if (hour != -1)
      {
        if (theCalendar.get(Calendar.AM_PM) == Calendar.PM)
          {
            if (hour == 12)
              theCalendar.set(Calendar.HOUR_OF_DAY, 12);
            else
              theCalendar.set(Calendar.HOUR_OF_DAY, hour + 12);
          }
        else
          {
            if (hour == 12)
              theCalendar.set(Calendar.HOUR_OF_DAY, 0);
            else
              theCalendar.set(Calendar.HOUR_OF_DAY, hour);
          }
      }

    return theCalendar.getTime();
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
  public Date parse(String dateStr, ParsePosition pos) {
    if (isLenient())
       return parseLenient(dateStr, pos);
    else
       return parseStrict(dateStr, pos);

  }
}

