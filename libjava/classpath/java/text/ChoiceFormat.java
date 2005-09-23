/* ChoiceFormat.java -- Format over a range of numbers
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005
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

import java.util.Vector;

/**
 * This class allows a format to be specified based on a range of numbers.
 * To use this class, first specify two lists of formats and range terminators.
 * These lists must be arrays of equal length.  The format of index 
 * <code>i</code> will be selected for value <code>X</code> if 
 * <code>terminator[i] &lt;= X &lt; limit[i + 1]</code>.  If the value X is not
 * included in any range, then either the first or last format will be 
 * used depending on whether the value X falls outside the range.
 * <p>
 * This sounds complicated, but that is because I did a poor job of
 * explaining it.  Consider the following example:
 * <p>
 *
<pre>terminators = { 1, ChoiceFormat.nextDouble(1) }
formats = { "file", "files" }</pre>
 *
 * <p>
 * In this case if the actual number tested is one or less, then the word
 * "file" is used as the format value.  If the number tested is greater than
 * one, then "files" is used.  This allows plurals to be handled
 * gracefully.  Note the use of the method <code>nextDouble</code>.  This
 * method selects the next highest double number than its argument.  This
 * effectively makes any double greater than 1.0 cause the "files" string
 * to be selected.  (Note that all terminator values are specified as
 * doubles.
 * <p>
 * Note that in order for this class to work properly, the range terminator
 * array must be sorted in ascending order and the format string array
 * must be the same length as the terminator array.
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @date March 9, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.1.
 */
public class ChoiceFormat extends NumberFormat
{
  /**
   * This method sets new range terminators and format strings for this
   * object based on the specified pattern. This pattern is of the form 
   * "term#string|term#string...".  For example "1#Sunday|2#Monday|#Tuesday".
   *
   * @param newPattern The pattern of terminators and format strings.
   *
   * @exception IllegalArgumentException If the pattern is not valid
   */
  public void applyPattern (String newPattern)
  {
    // Note: we assume the same kind of quoting rules apply here.
    // This isn't explicitly documented.  But for instance we accept
    // '#' as a literal hash in a format string.
    int index = 0, max = newPattern.length();
    Vector stringVec = new Vector ();
    Vector limitVec = new Vector ();
    StringBuffer buf = new StringBuffer ();
    
    while (true)
      {
	// Find end of double.
	int dstart = index;
	while (index < max)
	  {
	    char c = newPattern.charAt(index);
	    if (c == '#' || c == '\u2064' || c == '<')
	      break;
	    ++index;
	  }
	
	if (index == max)
	  throw new IllegalArgumentException ("unexpected end of text");
	Double d = new Double (newPattern.substring(dstart, index));

	if (newPattern.charAt(index) == '<')
	  d = new Double (nextDouble (d.doubleValue()));

	limitVec.addElement(d);

	// Scan text.
	++index;
	buf.setLength(0);
	while (index < max)
	  {
	    char c = newPattern.charAt(index);
	    if (c == '\'' && index < max + 1
		&& newPattern.charAt(index + 1) == '\'')
	      {
		buf.append(c);
		++index;
	      }
	    else if (c == '\'' && index < max + 2)
	      {
		buf.append(newPattern.charAt(index + 1));
		index += 2;
	      }
	    else if (c == '|')
	      break;
	    else
	      buf.append(c);
	    ++index;
	  }

	stringVec.addElement(buf.toString());
	if (index == max)
	  break;
	++index;
      }

    choiceFormats = new String[stringVec.size()];
    stringVec.copyInto(choiceFormats);

    choiceLimits = new double[limitVec.size()];
    for (int i = 0; i < choiceLimits.length; ++i)
      {
	Double d = (Double) limitVec.elementAt(i);
	choiceLimits[i] = d.doubleValue();
      }
  }

  /**
   * This method initializes a new instance of <code>ChoiceFormat</code> that
   * generates its range terminator and format string arrays from the
   * specified pattern.  This pattern is of the form 
   * "term#string|term#string...".  For example "1#Sunday|2#Monday|#Tuesday".
   * This is the same pattern type used by the <code>applyPattern</code>
   * method.
   *
   * @param newPattern The pattern of terminators and format strings.
   *
   * @exception IllegalArgumentException If the pattern is not valid
   */
  public ChoiceFormat (String newPattern)
  {
    super ();
    applyPattern (newPattern);
  }

  /**
   * This method initializes a new instance of <code>ChoiceFormat</code> that
   * will use the specified range terminators and format strings.
   *
   * @param choiceLimits The array of range terminators
   * @param choiceFormats The array of format strings
   */
  public ChoiceFormat (double[] choiceLimits, String[] choiceFormats)
  {
    super ();
    setChoices (choiceLimits, choiceFormats);
  }

  /**
   * This method tests this object for equality with the specified 
   * object.  This will be true if and only if:
   * <ul>
   * <li>The specified object is not <code>null</code>.</li>
   * <li>The specified object is an instance of <code>ChoiceFormat</code>.</li>
   * <li>The termination ranges and format strings are identical to
   *     this object's. </li>
   * </ul>
   *
   * @param obj The object to test for equality against.
   *
   * @return <code>true</code> if the specified object is equal to
   * this one, <code>false</code> otherwise. 
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof ChoiceFormat))
      return false;
    ChoiceFormat cf = (ChoiceFormat) obj;
    if (choiceLimits.length != cf.choiceLimits.length)
      return false;
    for (int i = choiceLimits.length - 1; i >= 0; --i)
      {
	if (choiceLimits[i] != cf.choiceLimits[i]
	    || !choiceFormats[i].equals(cf.choiceFormats[i]))
	  return false;
      }
    return true;
  }

  /**
   * This method appends the appropriate format string to the specified
   * <code>StringBuffer</code> based on the supplied <code>long</code>
   * argument.
   *
   * @param num The number used for determine (based on the range
   *               terminators) which format string to append. 
   * @param appendBuf The <code>StringBuffer</code> to append the format string 
   *                  to.
   * @param pos Unused.
   *
   * @return The <code>StringBuffer</code> with the format string appended.
   */
  public StringBuffer format (long num, StringBuffer appendBuf,
			      FieldPosition pos)
  {
    return format ((double) num, appendBuf, pos);
  }

  /**
   * This method appends the appropriate format string to the specified
   * <code>StringBuffer</code> based on the supplied <code>double</code>
   * argument.
   *
   * @param num The number used for determine (based on the range
   *               terminators) which format string to append. 
   * @param appendBuf The <code>StringBuffer</code> to append the format string to.
   * @param pos Unused.
   *
   * @return The <code>StringBuffer</code> with the format string appended.
   */
  public StringBuffer format (double num, StringBuffer appendBuf,
			      FieldPosition pos)
  {
    if (choiceLimits.length == 0)
      return appendBuf;

    int index = 0;
    if (! Double.isNaN(num) && num >= choiceLimits[0])
      {
	for (; index < choiceLimits.length - 1; ++index)
	  {
	    if (choiceLimits[index] <= num && num < choiceLimits[index + 1])
	      break;
	  }
      }

    return appendBuf.append(choiceFormats[index]);
  }

  /**
   * This method returns the list of format strings in use.
   *
   * @return The list of format objects.
   */
  public Object[] getFormats ()
  {
    return (Object[]) choiceFormats.clone();
  }

  /**
   * This method returns the list of range terminators in use.
   *
   * @return The list of range terminators.
   */
  public double[] getLimits ()
  {
    return (double[]) choiceLimits.clone();
  }

  /**
   * This method returns a hash value for this object
   * 
   * @return A hash value for this object.
   */
  public int hashCode ()
  {
    int hash = 0;
    for (int i = 0; i < choiceLimits.length; ++i)
      {
	long v = Double.doubleToLongBits(choiceLimits[i]);
	hash ^= (v ^ (v >>> 32));
	hash ^= choiceFormats[i].hashCode();
      }
    return hash;
  }

  /**
   * This method returns the lowest possible double greater than the 
   * specified double.  If the specified double value is equal to
   * <code>Double.NaN</code> then that is the value returned.
   *
   * @param d The specified double
   *
   * @return The lowest double value greater than the specified double.
   */
  public static final double nextDouble (double d)
  {
    return nextDouble (d, true);
  }

  /**
   * This method returns a double that is either the next highest double
   * or next lowest double compared to the specified double depending on the
   * value of the passed boolean parameter.  If the boolean parameter is
   * <code>true</code>, then the lowest possible double greater than the 
   * specified double will be returned.  Otherwise the highest possible
   * double less than the specified double will be returned.
   *
   * @param d The specified double
   * @param next <code>true</code> to return the next highest
   *                 double, <code>false</code> otherwise. 
   *
   * @return The next highest or lowest double value.
   */
  public static double nextDouble (double d, boolean next)
  {
    if (Double.isInfinite(d) || Double.isNaN(d))
      return d;

    long bits = Double.doubleToLongBits(d);

    long mantMask = (1L << mantissaBits) - 1;
    long mantissa = bits & mantMask;

    long expMask = (1L << exponentBits) - 1;
    long exponent = (bits >>> mantissaBits) & expMask;

    if (next ^ (bits < 0)) // Increment magnitude
      {
	if (mantissa == (1L << mantissaBits) - 1)
	  {
	    mantissa = 0L;
	    exponent++;
	     
	    // Check for absolute overflow.
	    if (exponent >= (1L << mantissaBits))
	      return (bits > 0) ? Double.POSITIVE_INFINITY 
		: Double.NEGATIVE_INFINITY;		      
	  }
	else
	  mantissa++;
      }
    else // Decrement magnitude
      {
	if (exponent == 0L && mantissa == 0L)
	  {
	    // The only case where there is a change of sign
	    return next ? Double.MIN_VALUE : -Double.MIN_VALUE;
	  }
	else
	  {
	    if (mantissa == 0L)
	      {
		mantissa = (1L << mantissaBits) - 1;
		exponent--;
	      }
	    else
	      mantissa--;
	  }
      }

    long result = bits < 0 ? 1 : 0;
    result = (result << exponentBits) | exponent;
    result = (result << mantissaBits) | mantissa;
    return Double.longBitsToDouble(result);
  }

  /**
   * I'm not sure what this method is really supposed to do, as it is
   * not documented.
   */
  public Number parse (String sourceStr, ParsePosition pos)
  {
    int index = pos.getIndex();
    for (int i = 0; i < choiceLimits.length; ++i)
      {
	if (sourceStr.startsWith(choiceFormats[i], index))
	  {
	    pos.setIndex(index + choiceFormats[i].length());
	    return new Double (choiceLimits[i]);
	  }
      }
    pos.setErrorIndex(index);
    return new Double (Double.NaN);
  }

  /**
   * This method returns the highest possible double less than the 
   * specified double.  If the specified double value is equal to
   * <code>Double.NaN</code> then that is the value returned.
   *
   * @param d The specified double
   *
   * @return The highest double value less than the specified double.
   */
  public static final double previousDouble (double d)
  {
    return nextDouble (d, false);
  }

  /**
   * This method sets new range terminators and format strings for this
   * object.
   *
   * @param choiceLimits The new range terminators
   * @param choiceFormats The new choice formats
   */
  public void setChoices (double[] choiceLimits, String[] choiceFormats)
  {
    if (choiceLimits == null || choiceFormats == null)
      throw new NullPointerException ();
    if (choiceLimits.length != choiceFormats.length)
      throw new IllegalArgumentException ();
    this.choiceFormats = (String[]) choiceFormats.clone();
    this.choiceLimits = (double[]) choiceLimits.clone();
  }

  private void quoteString (StringBuffer dest, String text)
  {
    int max = text.length();
    for (int i = 0; i < max; ++i)
      {
	char c = text.charAt(i);
	if (c == '\'')
	  {
	    dest.append(c);
	    dest.append(c);
	  }
	else if (c == '#' || c == '|' || c == '\u2064' || c == '<')
	  {
	    dest.append('\'');
	    dest.append(c);
	    dest.append('\'');
	  }
	else
	  dest.append(c);
      }
  }

  /**
   * This method returns the range terminator list and format string list
   * as a <code>String</code> suitable for using with the 
   * <code>applyPattern</code> method.
   *
   * @return A pattern string for this object
   */
  public String toPattern ()
  {
    StringBuffer result = new StringBuffer ();
    for (int i = 0; i < choiceLimits.length; ++i)
      {
	result.append(choiceLimits[i]);
	result.append('#');
	quoteString (result, choiceFormats[i]);
      }
    return result.toString();
  }

  /**
   * This is the list of format strings.  Note that this variable is
   * specified by the serialization spec of this class.
   */
  private String[] choiceFormats;

  /**
   * This is the list of range terminator values.  Note that this variable is
   * specified by the serialization spec of this class.
   */
  private double[] choiceLimits;

  // Number of mantissa bits in double.
  private static final int mantissaBits = 52;
  // Number of exponent bits in a double.
  private static final int exponentBits = 11;

  private static final long serialVersionUID = 1795184449645032964L;
}
