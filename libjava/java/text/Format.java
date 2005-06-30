/* Format.java -- Abstract superclass for formatting/parsing strings.
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005  Free Software Foundation, Inc.

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

import gnu.java.text.FormatCharacterIterator;

import java.io.Serializable;

/**
 * This class is the abstract superclass of classes that format and parse
 * data to/from <code>Strings</code>.  It is guaranteed that any 
 * <code>String</code> produced by a concrete subclass of <code>Format</code>
 * will be parseable by that same subclass.
 * <p>
 * In addition to implementing the abstract methods in this class, subclasses
 * should provide static factory methods of the form 
 * <code>getInstance()</code> and <code>getInstance(Locale)</code> if the
 * subclass loads different formatting/parsing schemes based on locale.
 * These subclasses should also implement a static method called
 * <code>getAvailableLocales()</code> which returns an array of 
 * available locales in the current runtime environment.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 */
public abstract class Format implements Serializable, Cloneable
{
  /**
   * For compatability with Sun's JDK 1.4.2 rev. 5
   */
  static final long serialVersionUID = -299282585814624189L;

  public static class Field extends AttributedCharacterIterator.Attribute
  {
    static final long serialVersionUID = 276966692217360283L;
   
    protected Field(String name)
    {
      super(name);
    }
  }
  
  /**
   * This method initializes a new instance of <code>Format</code>.
   * It performs no actions, but acts as a default constructor for
   * subclasses.
   */
  public Format ()
  {
  }

  /**
   * This method formats an <code>Object</code> into a <code>String</code>.
   * 
   * @param obj The <code>Object</code> to format.
   *
   * @return The formatted <code>String</code>.
   *
   * @exception IllegalArgumentException If the <code>Object</code>
   * cannot be formatted. 
   */
  public final String format(Object obj) throws IllegalArgumentException
  {
    StringBuffer sb = new StringBuffer ();
    format (obj, sb, new FieldPosition (0));
    return sb.toString ();
  }

  /**
   * This method formats an <code>Object</code> into a <code>String</code> and
   * appends the <code>String</code> to a <code>StringBuffer</code>.
   *
   * @param obj The <code>Object</code> to format.
   * @param sb The <code>StringBuffer</code> to append to.
   * @param pos The desired <code>FieldPosition</code>, which is also
   *            updated by this call. 
   *
   * @return The updated <code>StringBuffer</code>.
   *
   * @exception IllegalArgumentException If the <code>Object</code>
   * cannot be formatted. 
   */
  public abstract StringBuffer format (Object obj, StringBuffer sb,
				       FieldPosition pos)
    throws IllegalArgumentException;

  /**
   * This method parses a <code>String</code> and converts the parsed 
   * contents into an <code>Object</code>.
   *
   * @param str The <code>String</code> to parse.
   *
   * @return The resulting <code>Object</code>.
   *
   * @exception ParseException If the <code>String</code> cannot be parsed.
   */
  public Object parseObject (String str) throws ParseException
  {
    ParsePosition pos = new ParsePosition(0);
    Object result = parseObject (str, pos);
    if (result == null)
      {
	int index = pos.getErrorIndex();
	if (index < 0)
	  index = pos.getIndex();
	throw new ParseException("parseObject failed", index);
      }
    return result;
  }

  /**
   * This method parses a <code>String</code> and converts the parsed
   * contents into an <code>Object</code>. 
   *
   * @param str The <code>String</code> to parse.
   * @param pos The starting parse index on input, the ending parse
   *            index on output. 
   *
   * @return The parsed <code>Object</code>, or <code>null</code> in
   *         case of error.
   */
  public abstract Object parseObject (String str, ParsePosition pos);

  public AttributedCharacterIterator formatToCharacterIterator(Object obj)
  {
    return new FormatCharacterIterator(format(obj), null, null);
  }

  /**
   * Creates a copy of this object.
   *
   * @return The copied <code>Object</code>.
   */
  public Object clone ()
  {
    try
      {
	return super.clone ();
      }
    catch (CloneNotSupportedException e)
      {
	return null;
      }
  }
}
