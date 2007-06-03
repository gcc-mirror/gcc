/* SimpleDateFormat.java -- A class for parsing/formating simple 
   date constructs
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005
   Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.java.text.AttributedFormatBuffer;
import gnu.java.text.FormatBuffer;
import gnu.java.text.FormatCharacterIterator;
import gnu.java.text.StringFormatBuffer;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Locale;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * SimpleDateFormat provides convenient methods for parsing and formatting
 * dates using Gregorian calendars (see java.util.GregorianCalendar). 
 */
public class SimpleDateFormat extends DateFormat 
{
  /** 
   * This class is used by <code>SimpleDateFormat</code> as a
   * compiled representation of a format string.  The field
   * ID, size, and character used are stored for each sequence
   * of pattern characters.
   */
  private class CompiledField
  {
    /**
     * The ID of the field within the local pattern characters.
     * Package private for use in out class.
     */
    int field;

    /**
     * The size of the character sequence.
     * Package private for use in out class.
     */
    int size;

    /**
     * The character used.
     */
    private char character;

    /** 
     * Constructs a compiled field using the
     * the given field ID, size and character
     * values.
     *
     * @param f the field ID.
     * @param s the size of the field.
     * @param c the character used.
     */
    public CompiledField(int f, int s, char c)
    {
      field = f;
      size = s;
      character = c;
    }

    /**
     * Retrieves the ID of the field relative to
     * the local pattern characters.
     */
    public int getField()
    {
      return field;
    }

    /**
     * Retrieves the size of the character sequence.
     */
    public int getSize()
    {
      return size;
    }

    /**
     * Retrieves the character used in the sequence.
     */
    public char getCharacter()
    {
      return character;
    }

    /**
     * Returns a <code>String</code> representation
     * of the compiled field, primarily for debugging
     * purposes.
     *
     * @return a <code>String</code> representation.
     */
    public String toString()
    {
      StringBuffer builder;

      builder = new StringBuffer(getClass().getName());
      builder.append("[field=");
      builder.append(field);
      builder.append(", size=");
      builder.append(size);
      builder.append(", character=");
      builder.append(character);
      builder.append("]");

      return builder.toString();
    }
  }

  /**
   * A list of <code>CompiledField</code>s,
   * representing the compiled version of the pattern.
   *
   * @see CompiledField
   * @serial Ignored.
   */
  private transient ArrayList tokens;

  /**
   * The localised data used in formatting,
   * such as the day and month names in the local
   * language, and the localized pattern characters.
   *
   * @see DateFormatSymbols
   * @serial The localisation data.  May not be null.
   */
  private DateFormatSymbols formatData;

  /**
   * The date representing the start of the century
   * used for interpreting two digit years.  For
   * example, 24/10/2004 would cause two digit
   * years to be interpreted as representing
   * the years between 2004 and 2104.
   *
   * @see #get2DigitYearStart()
   * @see #set2DigitYearStart(java.util.Date)
   * @see Date
   * @serial The start date of the century for parsing two digit years.
   *         May not be null.
   */
  private Date defaultCenturyStart;

  /**
   * The year at which interpretation of two
   * digit years starts.
   *
   * @see #get2DigitYearStart()
   * @see #set2DigitYearStart(java.util.Date)
   * @serial Ignored.
   */
  private transient int defaultCentury;

  /**
   * The non-localized pattern string.  This
   * only ever contains the pattern characters
   * stored in standardChars.  Localized patterns
   * are translated to this form.
   *
   * @see #applyPattern(String)
   * @see #applyLocalizedPattern(String)
   * @see #toPattern()
   * @see #toLocalizedPattern()
   * @serial The non-localized pattern string.  May not be null.
   */
  private String pattern;

  /**
   * The version of serialized data used by this class.
   * Version 0 only includes the pattern and formatting
   * data.  Version 1 adds the start date for interpreting
   * two digit years.
   *
   * @serial This specifies the version of the data being serialized.
   *         Version 0 (or no version) specifies just <code>pattern</code>
   *         and <code>formatData</code>.  Version 1 adds
   *         the <code>defaultCenturyStart</code>.  This implementation
   *         always writes out version 1 data.
   */
  private int serialVersionOnStream = 1; // 0 indicates JDK1.1.3 or earlier

  /**
   * For compatability.
   */
  private static final long serialVersionUID = 4774881970558875024L;

  // This string is specified in the root of the CLDR.  We set it here
  // rather than doing a DateFormatSymbols(Locale.US).getLocalPatternChars()
  // since someone could theoretically change those values (though unlikely).
  private static final String standardChars = "GyMdkHmsSEDFwWahKzYeugAZ";

  /**
   * Reads the serialized version of this object.
   * If the serialized data is only version 0,
   * then the date for the start of the century
   * for interpreting two digit years is computed.
   * The pattern is parsed and compiled following the process
   * of reading in the serialized data.
   *
   * @param stream the object stream to read the data from.
   * @throws IOException if an I/O error occurs.
   * @throws ClassNotFoundException if the class of the serialized data
   *         could not be found.
   * @throws InvalidObjectException if the pattern is invalid.
   */ 
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
    try
      {
	compileFormat(pattern);
      }
    catch (IllegalArgumentException e)
      {
	throw new InvalidObjectException("The stream pattern was invalid.");
      }
  }

  /**
   * Compiles the supplied non-localized pattern into a form
   * from which formatting and parsing can be performed.
   * This also detects errors in the pattern, which will
   * be raised on later use of the compiled data.
   *
   * @param pattern the non-localized pattern to compile.
   * @throws IllegalArgumentException if the pattern is invalid.
   */
  private void compileFormat(String pattern) 
  {
    // Any alphabetical characters are treated as pattern characters
    // unless enclosed in single quotes.

    char thisChar;
    int pos;
    int field;
    CompiledField current = null;

    for (int i = 0; i < pattern.length(); i++)
      {
	thisChar = pattern.charAt(i);
	field = standardChars.indexOf(thisChar);
	if (field == -1)
	  {
	    current = null;
	    if ((thisChar >= 'A' && thisChar <= 'Z')
		|| (thisChar >= 'a' && thisChar <= 'z'))
	      {
		// Not a valid letter
		throw new IllegalArgumentException("Invalid letter "
						   + thisChar +
						   " encountered at character "
						   + i + ".");
	      }
	    else if (thisChar == '\'')
	      {
		// Quoted text section; skip to next single quote
		pos = pattern.indexOf('\'', i + 1);
		// First look for '' -- meaning a single quote.
		if (pos == i + 1)
		  tokens.add("'");
		else
		  {
		    // Look for the terminating quote.  However, if we
		    // see a '', that represents a literal quote and
		    // we must iterate.
		    StringBuffer buf = new StringBuffer();
		    int oldPos = i + 1;
		    do
		      {
			if (pos == -1)
			  throw new IllegalArgumentException("Quotes starting at character "
							     + i +
							     " not closed.");
			buf.append(pattern.substring(oldPos, pos));
			if (pos + 1 >= pattern.length()
			    || pattern.charAt(pos + 1) != '\'')
			  break;
			buf.append('\'');
			oldPos = pos + 2;
			pos = pattern.indexOf('\'', pos + 2);
		      }
		    while (true);
		    tokens.add(buf.toString());
		  }
		i = pos;
	      }
	    else
	      {
		// A special character
		tokens.add(new Character(thisChar));
	      }
	  }
	else
	  {
	    // A valid field
	    if ((current != null) && (field == current.field))
	      current.size++;
	    else
	      {
		current = new CompiledField(field, 1, thisChar);
		tokens.add(current);
	      }
	  }
      }
  }

  /**
   * Returns a string representation of this
   * class.
   *
   * @return a string representation of the <code>SimpleDateFormat</code>
   *         instance.
   */
  public String toString() 
  {
    StringBuffer output = new StringBuffer(getClass().getName());
    output.append("[tokens=");
    output.append(tokens);
    output.append(", formatData=");
    output.append(formatData);
    output.append(", defaultCenturyStart=");
    output.append(defaultCenturyStart);
    output.append(", defaultCentury=");
    output.append(defaultCentury);
    output.append(", pattern=");
    output.append(pattern);
    output.append(", serialVersionOnStream=");
    output.append(serialVersionOnStream);
    output.append(", standardChars=");
    output.append(standardChars);
    output.append("]");
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
    numberFormat.setMaximumFractionDigits (0);
  }
  
  /**
   * Creates a date formatter using the specified non-localized pattern,
   * with the default DateFormatSymbols for the default locale.
   *
   * @param pattern the pattern to use.
   * @throws NullPointerException if the pattern is null.
   * @throws IllegalArgumentException if the pattern is invalid.
   */
  public SimpleDateFormat(String pattern) 
  {
    this(pattern, Locale.getDefault());
  }

  /**
   * Creates a date formatter using the specified non-localized pattern,
   * with the default DateFormatSymbols for the given locale.
   *
   * @param pattern the non-localized pattern to use.
   * @param locale the locale to use for the formatting symbols.
   * @throws NullPointerException if the pattern is null.
   * @throws IllegalArgumentException if the pattern is invalid.
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
    numberFormat.setMaximumFractionDigits (0);
  }

  /**
   * Creates a date formatter using the specified non-localized
   * pattern. The specified DateFormatSymbols will be used when
   * formatting.
   *
   * @param pattern the non-localized pattern to use.
   * @param formatData the formatting symbols to use.
   * @throws NullPointerException if the pattern or formatData is null.
   * @throws IllegalArgumentException if the pattern is invalid.
   */
  public SimpleDateFormat(String pattern, DateFormatSymbols formatData)
  {
    super();
    calendar = new GregorianCalendar();
    computeCenturyStart ();
    tokens = new ArrayList();
    if (formatData == null)
      throw new NullPointerException("formatData");
    this.formatData = formatData;
    compileFormat(pattern);
    this.pattern = pattern;
    numberFormat = NumberFormat.getInstance();
    numberFormat.setGroupingUsed (false);
    numberFormat.setParseIntegerOnly (true);
    numberFormat.setMaximumFractionDigits (0);
  }

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
    return translateLocalizedPattern(pattern, standardChars, localChars);
  }

  /**
   * This method sets the formatting pattern that should be used by this
   * object.  This string is not localized.
   *
   * @param pattern The new format pattern.
   * @throws NullPointerException if the pattern is null.
   * @throws IllegalArgumentException if the pattern is invalid.
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
   * @throws NullPointerException if the pattern is null.
   * @throws IllegalArgumentException if the pattern is invalid.
   */
  public void applyLocalizedPattern(String pattern)
  {
    String localChars = formatData.getLocalPatternChars();
    pattern = translateLocalizedPattern(pattern, localChars, standardChars);
    applyPattern(pattern);
  }

  /**
   * Translates either from or to a localized variant of the pattern
   * string.  For example, in the German locale, 't' (for 'tag') is
   * used instead of 'd' (for 'date').  This method translates
   * a localized pattern (such as 'ttt') to a non-localized pattern
   * (such as 'ddd'), or vice versa.  Non-localized patterns use
   * a standard set of characters, which match those of the U.S. English
   * locale.
   *
   * @param pattern the pattern to translate.
   * @param oldChars the old set of characters (used in the pattern).
   * @param newChars the new set of characters (which will be used in the
   *                 pattern).
   * @return a version of the pattern using the characters in
   *         <code>newChars</code>.
   */
  private String translateLocalizedPattern(String pattern,
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
   * This method returns a copy of the format symbol information used
   * for parsing and formatting dates.
   *
   * @return a copy of the date format symbols.
   */
  public DateFormatSymbols getDateFormatSymbols()
  {
    return (DateFormatSymbols) formatData.clone();
  }

  /**
   * This method sets the format symbols information used for parsing
   * and formatting dates.
   *
   * @param formatData The date format symbols.
   * @throws NullPointerException if <code>formatData</code> is null.
   */
   public void setDateFormatSymbols(DateFormatSymbols formatData)
   {
     if (formatData == null)
       {
	 throw new
	   NullPointerException("The supplied format data was null.");
       }
     this.formatData = formatData;
   }

  /**
   * This methods tests whether the specified object is equal to this
   * object.  This will be true if and only if the specified object:
   * <p>
   * <ul>
   * <li>Is not <code>null</code>.</li>
   * <li>Is an instance of <code>SimpleDateFormat</code>.</li>
   * <li>Is equal to this object at the superclass (i.e., <code>DateFormat</code>)
   *     level.</li>
   * <li>Has the same formatting pattern.</li>
   * <li>Is using the same formatting symbols.</li>
   * <li>Is using the same century for two digit years.</li>
   * </ul>
   *
   * @param o The object to compare for equality against.
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
  private void formatWithAttribute(Date date, FormatBuffer buffer, FieldPosition pos)
  {
    String temp;
    AttributedCharacterIterator.Attribute attribute;
    calendar.setTime(date);

    // go through vector, filling in fields where applicable, else toString
    Iterator iter = tokens.iterator();
    while (iter.hasNext())
      {
	Object o = iter.next();
	if (o instanceof CompiledField)
	  {
	    CompiledField cf = (CompiledField) o;
	    int beginIndex = buffer.length();
	    
	    switch (cf.getField())
	      {
	      case ERA_FIELD:
		buffer.append (formatData.eras[calendar.get (Calendar.ERA)], DateFormat.Field.ERA);
		break;
	      case YEAR_FIELD:
		// If we have two digits, then we truncate.  Otherwise, we
		// use the size of the pattern, and zero pad.
		buffer.setDefaultAttribute (DateFormat.Field.YEAR);
		if (cf.getSize() == 2)
		  {
		    temp = "00"+String.valueOf (calendar.get (Calendar.YEAR));
		    buffer.append (temp.substring (temp.length() - 2));
		  }
		else
		  withLeadingZeros (calendar.get (Calendar.YEAR), cf.getSize(), buffer);
		break;
	      case MONTH_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.MONTH);
		if (cf.getSize() < 3)
		  withLeadingZeros (calendar.get (Calendar.MONTH) + 1, cf.getSize(), buffer);
		else if (cf.getSize() < 4)
		  buffer.append (formatData.shortMonths[calendar.get (Calendar.MONTH)]);
		else
		  buffer.append (formatData.months[calendar.get (Calendar.MONTH)]);
		break;
	      case DATE_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.DAY_OF_MONTH);
		withLeadingZeros (calendar.get (Calendar.DATE), cf.getSize(), buffer);
		break;
	      case HOUR_OF_DAY1_FIELD: // 1-24
		buffer.setDefaultAttribute(DateFormat.Field.HOUR_OF_DAY1);
		withLeadingZeros ( ((calendar.get (Calendar.HOUR_OF_DAY) + 23) % 24) + 1, 
				   cf.getSize(), buffer);
		break;
	      case HOUR_OF_DAY0_FIELD: // 0-23
		buffer.setDefaultAttribute (DateFormat.Field.HOUR_OF_DAY0);
		withLeadingZeros (calendar.get (Calendar.HOUR_OF_DAY), cf.getSize(), buffer);
		break;
	      case MINUTE_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.MINUTE);
		withLeadingZeros (calendar.get (Calendar.MINUTE),
				  cf.getSize(), buffer);
		break;
	      case SECOND_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.SECOND);
		withLeadingZeros(calendar.get (Calendar.SECOND), 
				 cf.getSize(), buffer);
		break;
	      case MILLISECOND_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.MILLISECOND);
		withLeadingZeros (calendar.get (Calendar.MILLISECOND), cf.getSize(), buffer);
		break;
	      case DAY_OF_WEEK_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.DAY_OF_WEEK);
		if (cf.getSize() < 4)
		  buffer.append (formatData.shortWeekdays[calendar.get (Calendar.DAY_OF_WEEK)]);
		else
		  buffer.append (formatData.weekdays[calendar.get (Calendar.DAY_OF_WEEK)]);
		break;
	      case DAY_OF_YEAR_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.DAY_OF_YEAR);
		withLeadingZeros (calendar.get (Calendar.DAY_OF_YEAR), cf.getSize(), buffer);
		break;
	      case DAY_OF_WEEK_IN_MONTH_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.DAY_OF_WEEK_IN_MONTH);
		withLeadingZeros (calendar.get (Calendar.DAY_OF_WEEK_IN_MONTH), 
				 cf.getSize(), buffer);
		break;
	      case WEEK_OF_YEAR_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.WEEK_OF_YEAR);
		withLeadingZeros (calendar.get (Calendar.WEEK_OF_YEAR),
				  cf.getSize(), buffer);
		break;
	      case WEEK_OF_MONTH_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.WEEK_OF_MONTH);
		withLeadingZeros (calendar.get (Calendar.WEEK_OF_MONTH),
				  cf.getSize(), buffer);
		break;
	      case AM_PM_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.AM_PM);
		buffer.append (formatData.ampms[calendar.get (Calendar.AM_PM)]);
		break;
	      case HOUR1_FIELD: // 1-12
		buffer.setDefaultAttribute (DateFormat.Field.HOUR1);
		withLeadingZeros (((calendar.get (Calendar.HOUR) + 11) % 12) + 1,
				  cf.getSize(), buffer);
		break;
	      case HOUR0_FIELD: // 0-11
		buffer.setDefaultAttribute (DateFormat.Field.HOUR0);
		withLeadingZeros (calendar.get (Calendar.HOUR), cf.getSize(), buffer);
		break;
	      case TIMEZONE_FIELD:
		buffer.setDefaultAttribute (DateFormat.Field.TIME_ZONE);
		TimeZone zone = calendar.getTimeZone();
		boolean isDST = calendar.get (Calendar.DST_OFFSET) != 0;
		// FIXME: XXX: This should be a localized time zone.
		String zoneID = zone.getDisplayName
		  (isDST, cf.getSize() > 3 ? TimeZone.LONG : TimeZone.SHORT);
		buffer.append (zoneID);
		break;
	      case RFC822_TIMEZONE_FIELD:
		buffer.setDefaultAttribute(DateFormat.Field.RFC822_TIME_ZONE);
		int pureMinutes = (calendar.get(Calendar.ZONE_OFFSET) +
				   calendar.get(Calendar.DST_OFFSET)) / (1000 * 60);
		String sign = (pureMinutes < 0) ? "-" : "+";
                pureMinutes = Math.abs(pureMinutes);
		int hours = pureMinutes / 60;
		int minutes = pureMinutes % 60;
		buffer.append(sign);
		withLeadingZeros(hours, 2, buffer);
		withLeadingZeros(minutes, 2, buffer);
		break;
	      default:
		throw new IllegalArgumentException ("Illegal pattern character " +
						    cf.getCharacter());
	      }
	    if (pos != null && (buffer.getDefaultAttribute() == pos.getFieldAttribute()
				|| cf.getField() == pos.getField()))
	      {
		pos.setBeginIndex(beginIndex);
		pos.setEndIndex(buffer.length());
	      }
	  } 
      else
	{  
	  buffer.append(o.toString(), null);
	}
      }
  }
  
  public StringBuffer format(Date date, StringBuffer buffer, FieldPosition pos)
  {
    formatWithAttribute(date, new StringFormatBuffer (buffer), pos);

    return buffer;
  }

  public AttributedCharacterIterator formatToCharacterIterator(Object date)
    throws IllegalArgumentException
  {
    if (date == null)
      throw new NullPointerException("null argument");
    if (!(date instanceof Date))
      throw new IllegalArgumentException("argument should be an instance of java.util.Date");

    AttributedFormatBuffer buf = new AttributedFormatBuffer();
    formatWithAttribute((Date)date, buf,
			null);
    buf.sync();
        
    return new FormatCharacterIterator(buf.getBuffer().toString(),
				       buf.getRanges(),
				       buf.getAttributes());
  }

  private void withLeadingZeros(int value, int length, FormatBuffer buffer) 
  {
    String valStr = String.valueOf(value);
    for (length -= valStr.length(); length > 0; length--)
      buffer.append('0');
    buffer.append(valStr);
  }

  private boolean expect(String source, ParsePosition pos, char ch)
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
    try
      {
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
                if (quote_start == -1 && ch == ' ')
                  {
                    // A single unquoted space in the pattern may match
                    // any number of spaces in the input.
                    int index = pos.getIndex();
                    int save = index;
                    while (index < dateStr.length()
                           && Character.isWhitespace(dateStr.charAt(index)))
                      ++index;
                    if (index > save)
                      pos.setIndex(index);
                    else
                      {
                        // Didn't see any whitespace.
                        pos.setErrorIndex(index);
                        return null;
                      }
                  }
                else if (! expect (dateStr, pos, ch))
		  return null;
		continue;
	      }
	    
	    // We've arrived at a potential pattern character in the
	    // pattern.
	    int fmt_count = 1;
	    while (++fmt_index < fmt_max && pattern.charAt(fmt_index) == ch)
	      {
		++fmt_count;
	      }
	    
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
	    int offset = 0;
	    boolean maybe2DigitYear = false;
	    boolean oneBasedHour = false;
	    boolean oneBasedHourOfDay = false;
	    Integer simpleOffset;
	    String[] set1 = null;
	    String[] set2 = null;
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
		set1 = formatData.getWeekdays();
		set2 = formatData.getShortWeekdays();
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
		    set1 = formatData.getMonths();
		    set2 = formatData.getShortMonths();
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
		oneBasedHour = true;
		break;
	      case 'H':
		calendar_field = Calendar.HOUR_OF_DAY;
		break;
	      case 'k':
		calendar_field = Calendar.HOUR_OF_DAY;
		oneBasedHourOfDay = true;
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
		set1 = formatData.getAmPmStrings();
		break;
	      case 'z':
	      case 'Z':
		// We need a special case for the timezone, because it
		// uses a different data structure than the other cases.
		is_numeric = false;
		calendar_field = Calendar.ZONE_OFFSET;
		String[][] zoneStrings = formatData.getZoneStrings();
		int zoneCount = zoneStrings.length;
		int index = pos.getIndex();
		boolean found_zone = false;
		simpleOffset = computeOffset(dateStr.substring(index), pos);
		if (simpleOffset != null)
		  {
		    found_zone = true;
		    saw_timezone = true;
		    calendar.set(Calendar.DST_OFFSET, 0);
		    offset = simpleOffset.intValue();
		  }
		else
		  {
		    for (int j = 0;  j < zoneCount;  j++)
		      {
			String[] strings = zoneStrings[j];
			int k;
			for (k = 0; k < strings.length; ++k)
			  {
			    if (dateStr.startsWith(strings[k], index))
			      break;
			  }
			if (k != strings.length)
			  {
			    found_zone = true;
			    saw_timezone = true;
			    TimeZone tz = TimeZone.getTimeZone (strings[0]);
			    // Check if it's a DST zone or ordinary 
			    if(k == 3 || k == 4)
			      calendar.set (Calendar.DST_OFFSET, tz.getDSTSavings());
			    else
			      calendar.set (Calendar.DST_OFFSET, 0);
                            offset = tz.getRawOffset ();
			    pos.setIndex(index + strings[k].length());
			    break;
			  }
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
		if (maybe2DigitYear)
		  index = pos.getIndex();
		Number n = null;
		if (limit_digits)
		  {
		    // numberFormat.setMaximumIntegerDigits(fmt_count) may
		    // not work as expected. So we explicitly use substring
		    // of dateStr.
		    int origPos = pos.getIndex();
		    pos.setIndex(0);
		    n = numberFormat.parse(dateStr.substring(origPos, origPos + fmt_count), pos);
		    pos.setIndex(origPos + pos.getIndex());
		  }
		else
		  n = numberFormat.parse(dateStr, pos);
		if (pos == null || ! (n instanceof Long))
		  return null;
		value = n.intValue() + offset;
	      }
	    else if (set1 != null)
	      {
		index = pos.getIndex();
		int i;
		boolean found = false;
		for (i = offset; i < set1.length; ++i)
		  {
		    if (set1[i] != null)
		      if (dateStr.toUpperCase().startsWith(set1[i].toUpperCase(),
							   index))
			{
			  found = true;
			  pos.setIndex(index + set1[i].length());
			  break;
			}
		  }
		if (!found && set2 != null)
		  {
		    for (i = offset; i < set2.length; ++i)
		      {
			if (set2[i] != null)
			  if (dateStr.toUpperCase().startsWith(set2[i].toUpperCase(),
							       index))
			    {
			      found = true;
			      pos.setIndex(index + set2[i].length());
			      break;
			    }
		      }
		  }
		if (!found)
		  {
		    pos.setErrorIndex(index);
		    return null;
		  }
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
		  {
		    is2DigitYear = true;
		    value += defaultCentury;
		  }
	      }
	    
	    // Calendar uses 0-based hours. 
	    // I.e. 00:00 AM is midnight, not 12 AM or 24:00
	    if (oneBasedHour && value == 12)
	      value = 0;

	    if (oneBasedHourOfDay && value == 24)
	      value = 0;
	    
	    // Assign the value and move on.
	    calendar.set(calendar_field, value);
	  }
    
	if (is2DigitYear)
	  {
	    // Apply the 80-20 heuristic to dermine the full year based on 
	    // defaultCenturyStart. 
	    int year = calendar.get(Calendar.YEAR);
	    if (calendar.getTime().compareTo(defaultCenturyStart) < 0)
	      calendar.set(Calendar.YEAR, year + 100);      
	  }
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

  /**
   * <p>
   * Computes the time zone offset in milliseconds
   * relative to GMT, based on the supplied
   * <code>String</code> representation.
   * </p>
   * <p>
   * The supplied <code>String</code> must be a three
   * or four digit signed number, with an optional 'GMT'
   * prefix.  The first one or two digits represents the hours,
   * while the last two represent the minutes.  The
   * two sets of digits can optionally be separated by
   * ':'.  The mandatory sign prefix (either '+' or '-')
   * indicates the direction of the offset from GMT.
   * </p>
   * <p>
   * For example, 'GMT+0200' specifies 2 hours after
   * GMT, while '-05:00' specifies 5 hours prior to
   * GMT.  The special case of 'GMT' alone can be used
   * to represent the offset, 0.
   * </p>
   * <p>
   * If the <code>String</code> can not be parsed,
   * the result will be null.  The resulting offset
   * is wrapped in an <code>Integer</code> object, in
   * order to allow such failure to be represented.
   * </p>
   *
   * @param zoneString a string in the form 
   *        (GMT)? sign hours : minutes
   *        where sign = '+' or '-', hours
   *        is a one or two digits representing
   *        a number between 0 and 23, and
   *        minutes is two digits representing
   *        a number between 0 and 59.
   * @return the parsed offset, or null if parsing
   *         failed.
   */
  private Integer computeOffset(String zoneString, ParsePosition pos)
  {
    Pattern pattern = 
      Pattern.compile("(GMT)?([+-])([012])?([0-9]):?([0-9]{2})");
    Matcher matcher = pattern.matcher(zoneString);

    // Match from start, but ignore trailing parts
    boolean hasAll = matcher.lookingAt();
    try
      {
	// Do we have at least the sign, hour and minute?
	matcher.group(2);
	matcher.group(4);
	matcher.group(5);
      }
    catch (IllegalStateException ise)
      {
	hasAll = false;
      }
    if (hasAll)
      {
	int sign = matcher.group(2).equals("+") ? 1 : -1;
	int hour = Integer.parseInt(matcher.group(4));
	if (!matcher.group(3).equals(""))
	  hour += (Integer.parseInt(matcher.group(3)) * 10);
	int minutes = Integer.parseInt(matcher.group(5));

	if (hour > 23)
	  return null;
	int offset = sign * ((hour * 60) + minutes) * 60000;

	// advance the index
	pos.setIndex(pos.getIndex() + matcher.end());
	return new Integer(offset);
      }
    else if (zoneString.startsWith("GMT"))
      {
	pos.setIndex(pos.getIndex() + 3);
	return new Integer(0);
      }
    return null;
  }

  // Compute the start of the current century as defined by
  // get2DigitYearStart.
  private void computeCenturyStart()
  {
    int year = calendar.get(Calendar.YEAR);
    calendar.set(Calendar.YEAR, year - 80);
    set2DigitYearStart(calendar.getTime());
  }

  /**
   * Returns a copy of this instance of
   * <code>SimpleDateFormat</code>.  The copy contains
   * clones of the formatting symbols and the 2-digit
   * year century start date.
   */
  public Object clone()
  {
    SimpleDateFormat clone = (SimpleDateFormat) super.clone();
    clone.setDateFormatSymbols((DateFormatSymbols) formatData.clone());
    clone.set2DigitYearStart((Date) defaultCenturyStart.clone());
    return clone;
  }

}
