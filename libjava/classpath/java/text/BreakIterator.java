/* BreakIterator.java -- Breaks text into elements
   Copyright (C) 1998, 1999, 2001, 2004, 2005, 2007
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

import gnu.java.locale.LocaleHelper;

import gnu.java.text.CharacterBreakIterator;
import gnu.java.text.LineBreakIterator;
import gnu.java.text.SentenceBreakIterator;
import gnu.java.text.WordBreakIterator;

import java.text.spi.BreakIteratorProvider;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.ServiceLoader;

/**
 * This class iterates over text elements such as words, lines, sentences,
 * and characters.  It can only iterate over one of these text elements at
 * a time.  An instance of this class configured for the desired iteration
 * type is created by calling one of the static factory methods, not
 * by directly calling a constructor.
 *
 * The standard iterators created by the factory methods in this
 * class will be valid upon creation.  That is, their methods will
 * not cause exceptions if called before you call setText().
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @date March 19, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.1.
 */
public abstract class BreakIterator implements Cloneable
{
  /**
   * This value is returned by the <code>next()</code> and
   * <code>previous</code> in order to indicate that the end of the
   * text has been reached.
   */
  // The value was discovered by writing a test program.
  public static final int DONE = -1;

  /**
   * This method initializes a new instance of <code>BreakIterator</code>.
   * This protected constructor is available to subclasses as a default
   * no-arg superclass constructor.
   */
  protected BreakIterator ()
  {
  }

  /**
   * Create a clone of this object.
   */
  public Object clone ()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        return null;
      }
  }
  
  /**
   * This method returns the index of the current text element boundary.
   *
   * @return The current text boundary.
   */
  public abstract int current ();

  /**
   * This method returns the first text element boundary in the text being
   * iterated over.
   *
   * @return The first text boundary.
   */
  public abstract int first ();

  /**
   * This methdod returns the offset of the text element boundary following
   * the specified offset.
   *
   * @param pos The text index from which to find the next text boundary.
   *
   * @return The next text boundary following the specified index.
   */
  public abstract int following (int pos);

  /**
   * This method returns a list of locales for which instances of
   * <code>BreakIterator</code> are available.
   *
   * @return A list of available locales
   */
  public static synchronized Locale[] getAvailableLocales ()
  {
    Locale[] l = new Locale[1];
    l[0] = Locale.US;
    return l;
  }

  private static BreakIterator getInstance (String type, Locale loc)
  {
    String className;
    try
      {
	ResourceBundle res
	  = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
				     loc, ClassLoader.getSystemClassLoader());
	className = res.getString(type);
      }
    catch (MissingResourceException x)
      {
	return null;
      }
    try
      {
	Class k = Class.forName(className);
	return (BreakIterator) k.newInstance();
      }
    catch (ClassNotFoundException x1)
      {
	return null;
      }
    catch (InstantiationException x2)
      {
	return null;
      }
    catch (IllegalAccessException x3)
      {
	return null;
      }
  }

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over characters as defined in the default locale.
   *
   * @return A <code>BreakIterator</code> instance for the default locale.
   */
  public static BreakIterator getCharacterInstance ()
  {
    return getCharacterInstance (Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over characters as defined in the specified locale.
   *
   * @param locale The desired locale.
   *
   * @return A <code>BreakIterator</code> instance for the specified locale.
   */
  public static BreakIterator getCharacterInstance (Locale locale)
  {
    BreakIterator r = getInstance("CharacterIterator", locale);
    if (r != null)
      return r;
    for (BreakIteratorProvider p :
	   ServiceLoader.load(BreakIteratorProvider.class))
      {
	for (Locale loc : p.getAvailableLocales())
	  {
	    if (loc.equals(locale))
	      {
		BreakIterator bi = p.getCharacterInstance(locale);
		if (bi != null)
		  return bi;
		break;
	      }
	  }
      }
    if (locale.equals(Locale.ROOT))
      return new CharacterBreakIterator();
    return getCharacterInstance(LocaleHelper.getFallbackLocale(locale));
  }

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over line breaks as defined in the default locale.
   *
   * @return A <code>BreakIterator</code> instance for the default locale.
   */
  public static BreakIterator getLineInstance ()
  {
    return getLineInstance (Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over line breaks as defined in the specified locale.
   *
   * @param locale The desired locale.
   *
   * @return A <code>BreakIterator</code> instance for the default locale.
   */
  public static BreakIterator getLineInstance (Locale locale)
  {
    BreakIterator r = getInstance ("LineIterator", locale);
    if (r != null)
      return r;
    for (BreakIteratorProvider p :
	   ServiceLoader.load(BreakIteratorProvider.class))
      {
	for (Locale loc : p.getAvailableLocales())
	  {
	    if (loc.equals(locale))
	      {
		BreakIterator bi = p.getLineInstance(locale);
		if (bi != null)
		  return bi;
		break;
	      }
	  }
      }
    if (locale.equals(Locale.ROOT))
      return new LineBreakIterator();
    return getLineInstance(LocaleHelper.getFallbackLocale(locale));
  }

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over sentences as defined in the default locale.
   *
   * @return A <code>BreakIterator</code> instance for the default locale.
   */
  public static BreakIterator getSentenceInstance ()
  {
    return getSentenceInstance (Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over sentences as defined in the specified locale.
   *
   * @param locale The desired locale.
   *
   * @return A <code>BreakIterator</code> instance for the default locale.
   */
  public static BreakIterator getSentenceInstance (Locale locale)
  {
    BreakIterator r = getInstance ("SentenceIterator", locale);
    if (r != null)
      return r;
    for (BreakIteratorProvider p :
	   ServiceLoader.load(BreakIteratorProvider.class))
      {
	for (Locale loc : p.getAvailableLocales())
	  {
	    if (loc.equals(locale))
	      {
		BreakIterator bi = p.getSentenceInstance(locale);
		if (bi != null)
		  return bi;
		break;
	      }
	  }
      }
    if (locale.equals(Locale.ROOT))
      return new SentenceBreakIterator();
    return getSentenceInstance(LocaleHelper.getFallbackLocale(locale));
  }

  /**
   * This method returns the text this object is iterating over as a
   * <code>CharacterIterator</code>.
   *
   * @return The text being iterated over.
   */
  public abstract CharacterIterator getText ();

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over words as defined in the default locale.
   *
   * @return A <code>BreakIterator</code> instance for the default locale.
   */
  public static BreakIterator getWordInstance ()
  {
    return getWordInstance (Locale.getDefault());
  }

  /**
   * This method returns an instance of <code>BreakIterator</code> that will
   * iterate over words as defined in the specified locale.  
   *
   * @param locale The desired locale.
   *
   * @return A <code>BreakIterator</code> instance for the default locale.
   */
  public static BreakIterator getWordInstance (Locale locale)
  {
    BreakIterator r = getInstance ("WordIterator", locale);
    if (r != null)
      return r;
    for (BreakIteratorProvider p :
	   ServiceLoader.load(BreakIteratorProvider.class))
      {
	for (Locale loc : p.getAvailableLocales())
	  {
	    if (loc.equals(locale))
	      {
		BreakIterator bi = p.getWordInstance(locale);
		if (bi != null)
		  return bi;
		break;
	      }
	  }
      }
    if (locale.equals(Locale.ROOT))
      return new WordBreakIterator();
    return getWordInstance(LocaleHelper.getFallbackLocale(locale));
  }

  /**
   * This method tests whether or not the specified position is a text
   * element boundary.
   *
   * @param pos The text position to test.
   *
   * @return <code>true</code> if the position is a boundary,
   * <code>false</code> otherwise. 
   */
  public boolean isBoundary (int pos)
  {
    if (pos == 0)
      return true;
    return following (pos - 1) == pos;
  }

  /**
   * This method returns the last text element boundary in the text being
   * iterated over.
   *
   * @return The last text boundary.
   */
  public abstract int last ();

  /**
   * This method returns the text element boundary following the current
   * text position.
   *
   * @return The next text boundary.
   */
  public abstract int next ();

  /**
   * This method returns the n'th text element boundary following the current
   * text position.
   *
   * @param n The number of text element boundaries to skip.
   *
   * @return The next text boundary.
   */
  public abstract int next (int n);

  /**
   * This methdod returns the offset of the text element boundary preceding
   * the specified offset.
   *
   * @param pos The text index from which to find the preceding text boundary. 
   *
   * @returns The next text boundary preceding the specified index.
   */
  public int preceding (int pos)
  {
    if (following (pos) == DONE)
      last ();
    while (previous () >= pos)
      ;
    return current ();
  }

  /**
   * This method returns the text element boundary preceding the current
   * text position.
   *
   * @return The previous text boundary.
   */
  public abstract int previous ();

  /**
   * This method sets the text string to iterate over.
   *
   * @param newText The <code>String</code> to iterate over.
   */
  public void setText (String newText)
  {
    setText (new StringCharacterIterator (newText));
  }

  /**
   * This method sets the text to iterate over from the specified
   * <code>CharacterIterator</code>.
   * 
   * @param newText The desired <code>CharacterIterator</code>.
   */
  public abstract void setText (CharacterIterator newText);
}
