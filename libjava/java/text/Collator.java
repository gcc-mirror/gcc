/* Collator.java -- Perform locale dependent String comparisons.
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

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Comparator;

/**
 * This class is the abstract superclass of classes which perform 
 * locale dependent <code>String</code> comparisons.  A caller requests
 * an instance of <code>Collator</code> for a particular locale using
 * the <code>getInstance()</code> static method in this class.  That method
 * will return a locale specific subclass of <code>Collator</code> which
 * can be used to perform <code>String</code> comparisons for that locale.
 * If a subclass of <code>Collator</code> cannot be located for a particular
 * locale, a default instance for the current locale will be returned.  
 *
 * In addition to setting the correct locale, there are two additional
 * settings that can be adjusted to affect <code>String</code> comparisons:
 * strength and decomposition.  The strength value determines the level
 * of signficance of character differences required for them to sort
 * differently.  (For example, whether or not capital letters are considered
 * different from lower case letters).  The decomposition value affects how
 * variants of the same character are treated for sorting purposes.  (For
 * example, whether or not an accent is signficant or not).  These settings
 * are described in detail in the documentation for the methods and values
 * that are related to them.
 *
 * @author Tom Tromey <tromey@cygnus.com>
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @date March 18, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status: Mostly complete, but parts stubbed out.  Look for FIXME.
 */
public abstract class Collator implements Comparator, Cloneable
{
  /**
   * This constant is a strength value which indicates that only primary
   * differences between characters will be considered signficant.  As an
   * example, two completely different English letters such as 'a' and 'b'
   * are considered to have a primary difference.
   */
  public static final int PRIMARY = 0;
  
  /**
   * This constant is a strength value which indicates that only secondary
   * or primary differences between characters will be considered
   * significant.  An example of a secondary difference between characters
   * are instances of the same letter with different accented forms.
   */
  public static final int SECONDARY = 1;
  
  /**
   * This constant is a strength value which indicates that tertiary,
   * secondary, and primary differences will be considered during sorting.
   * An example of a tertiary difference is capitalization of a given letter.
   * This is the default value for the strength setting.
   */
  public static final int TERTIARY = 2;
  
  /**
   * This constant is a strength value which indicates that any difference
   * at all between character values are considered significant.
   */
  public static final int IDENTICAL = 3;
  
  /**
   * This constant indicates that accented characters won't be decomposed
   * when performing comparisons.  This will yield the fastest results, but
   * will only work correctly in call cases for languages which do not
   * use accents such as English.
   */
  public static final int NO_DECOMPOSITION = 0;
  
  /**
   * This constant indicates that only characters which are canonical variants
   * in Unicode 2.0 will be decomposed prior to performing comparisons.  This
   * will cause accented languages to be sorted correctly.  This is the
   * default decomposition value.
   */
  public static final int CANONICAL_DECOMPOSITION = 1;
  
  /**
   * This constant indicates that both canonical variants and compatibility
   * variants in Unicode 2.0 will be decomposed prior to performing
   * comparisons.  This is the slowest mode, but is required to get the
   * correct sorting for certain languages with certain special formats.
   */
  public static final int FULL_DECOMPOSITION = 2;

  /**
   * This method initializes a new instance of <code>Collator</code> to have
   * the default strength (TERTIARY) and decomposition 
   * (CANONICAL_DECOMPOSITION) settings.  This constructor is protected and
   * is for use by subclasses only.  Non-subclass callers should use the
   * static <code>getInstance()</code> methods of this class to instantiate
   * <code>Collation</code> objects for the desired locale.
   */
  protected Collator ()
  {
    strength = TERTIARY;
    decmp = CANONICAL_DECOMPOSITION;
  }

  /**
   * This method compares the two <code>String</code>'s and returns an
   * integer indicating whether or not the first argument is less than,
   * equal to, or greater than the second argument.  The comparison is
   * performed according to the rules of the locale for this 
   * <code>Collator</code> and the strength and decomposition rules in
   * effect.
   *
   * @param str1 The first object to compare
   * @param str2 The second object to compare
   *
   * @return A negative integer if str1 < str2, 0 if str1 == str2, or
   * a positive integer if str1 > str2. 
   */
  public abstract int compare (String source, String target);

  /**
   * This method compares the two <code>Object</code>'s and returns an
   * integer indicating whether or not the first argument is less than,
   * equal to, or greater than the second argument.  These two objects
   * must be <code>String</code>'s or an exception will be thrown.
   *
   * @param obj1 The first object to compare
   * @param obj2 The second object to compare
   *
   * @return A negative integer if obj1 < obj2, 0 if obj1 == obj2, or
   * a positive integer if obj1 > obj2. 
   *
   * @exception ClassCastException If the arguments are not instances
   * of <code>String</code>. 
   */
  public int compare (Object o1, Object o2)
  {
    return compare ((String) o1, (String) o2);
  }

  /**
   * This method tests the specified object for equality against this
   * object.  This will be true if and only if the following conditions are
   * met:
   * <ul>
   * <li>The specified object is not <code>null</code>.
   * <li>The specified object is an instance of <code>Collator</code>.
   * <li>The specified object has the same strength and decomposition
   * settings as this object.
   * </ul>
   *
   * @param obj The <code>Object</code> to test for equality against
   *            this object. 
   *
   * @return <code>true</code> if the specified object is equal to
   * this one, <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof Collator))
      return false;
    Collator c = (Collator) obj;
    return decmp == c.decmp && strength == c.strength;
  }

  /**
   * This method tests whether the specified <code>String</code>'s are equal
   * according to the collation rules for the locale of this object and
   * the current strength and decomposition settings.
   *
   * @param str1 The first <code>String</code> to compare
   * @param str2 The second <code>String</code> to compare
   *
   * @return <code>true</code> if the two strings are equal,
   * <code>false</code> otherwise. 
   */
  public boolean equals (String source, String target)
  {
    return compare (source, target) == 0;
  }

  /**
   * This method returns a copy of this <code>Collator</code> object.
   *
   * @return A duplicate of this object.
   */
  public Object clone ()
  {
    try
      {
	return super.clone ();
      }
    catch (CloneNotSupportedException _)
      {
	return null;
      }
  }

  /**
   * This method returns an array of <code>Locale</code> objects which is
   * the list of locales for which <code>Collator</code> objects exist.
   *
   * @return The list of locales for which <code>Collator</code>'s exist.
   */
  public static synchronized Locale[] getAvailableLocales ()
  {
    // FIXME
    Locale[] l = new Locale[1];
    l[0] = Locale.US;
    return l;
  }

  /**
   * This method transforms the specified <code>String</code> into a
   * <code>CollationKey</code> for faster comparisons.  This is useful when
   * comparisons against a string might be performed multiple times, such
   * as during a sort operation.
   *
   * @param str The <code>String</code> to convert.
   *
   * @return A <code>CollationKey</code> for the specified <code>String</code>.
   */
  public abstract CollationKey getCollationKey (String source);

  /**
   * This method returns the current decomposition setting for this
   * object.  This * will be one of NO_DECOMPOSITION,
   * CANONICAL_DECOMPOSITION, or * FULL_DECOMPOSITION.  See the
   * documentation for those constants for an * explanation of this
   * setting.
   *
   * @return The current decomposition setting.
   */
  public synchronized int getDecomposition ()
  {
    return decmp;
  }

  /**
   * This method returns an instance of <code>Collator</code> for the
   * default locale.
   *
   * @return A <code>Collator</code> for the default locale.
   */
  public static Collator getInstance ()
  {
    return getInstance (Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>Collator</code> for the
   * specified locale.  If no <code>Collator</code> exists for the desired
   * locale, a <code>Collator</code> for the default locale will be returned.
   *
   * @param locale The desired localed to load a <code>Collator</code> for.
   *
   * @return A <code>Collator</code> for the requested locale
   */
  public static Collator getInstance (Locale loc)
  {
    ResourceBundle res;
    String pattern;
    try
      {
	res = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
				       loc);
	pattern = res.getString("collation_rules");
      }
    catch (MissingResourceException x)
      {
	return null;
      }
    try
      {
	return new RuleBasedCollator (pattern);
      }
    catch (ParseException x)
      {
	return null;
      }
  }

  /**
   * This method returns the current strength setting for this object.  This
   * will be one of PRIMARY, SECONDARY, TERTIARY, or IDENTICAL.  See the
   * documentation for those constants for an explanation of this setting.
   *
   * @return The current strength setting.
   */
  public synchronized int getStrength ()
  {
    return strength;
  }

  /**
   * This method returns a hash code value for this object.
   *
   * @return A hash value for this object.
   */
  public abstract int hashCode ();

  /**
   * This method sets the decomposition setting for this object to the
   * specified value.  This must be one of NO_DECOMPOSITION,
   * CANONICAL_DECOMPOSITION, or FULL_DECOMPOSITION.  Otherwise an
   * exception will be thrown.  See the documentation for those
   * contants for an explanation of this setting.
   *
   * @param decmp The new decomposition setting.
   *
   * @exception IllegalArgumentException If the requested
   * decomposition setting is not valid.
   */
  public synchronized void setDecomposition (int mode)
  {
    if (mode != NO_DECOMPOSITION
	&& mode != CANONICAL_DECOMPOSITION
	&& mode != FULL_DECOMPOSITION)
      throw new IllegalArgumentException ();
    decmp = mode;
  }

  /**
   * This method sets the strength setting for this object to the specified
   * value.  This must be one of PRIMARY, SECONDARY, TERTIARY, or IDENTICAL.
   * Otherwise an exception is thrown. See the documentation for these
   * constants for an explanation of this setting.
   * 
   * @param strength The new strength setting.
   *
   * @exception IllegalArgumentException If the requested strength
   * setting value is not valid.
   */
  public synchronized void setStrength (int strength)
  {
    if (strength != PRIMARY && strength != SECONDARY
	&& strength != TERTIARY && strength != IDENTICAL)
      throw new IllegalArgumentException ();
    this.strength = strength;
  }

  // Decompose a single character and append results to the buffer.
  native final void decomposeCharacter (char c, StringBuffer buf);

  /**
   * This is the current collation decomposition setting.
   */
  int decmp;

  /**
   * This is the current collation strength setting.
   */
  int strength;
}
