// BreakIterator.java - Iterate over logical breaks in text.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.io.Serializable;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 19, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.1.
 */

public abstract class BreakIterator implements Cloneable, Serializable
{
  // The value was discovered by writing a test program.
  public static final int DONE = -1;

  protected BreakIterator ()
  {
  }

  public abstract int current ();
  public abstract int first ();
  public abstract int following (int pos);

  public static synchronized Locale[] getAvailableLocales ()
  {
    // FIXME.
    return null;
  }

  private static BreakIterator getInstance (String type, Locale loc)
  {
    String className;
    try
      {
	ResourceBundle res
	  = ResourceBundle.getBundle("gnu.gcj.text.LocaleData", loc);
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

  public static BreakIterator getCharacterInstance ()
  {
    return getCharacterInstance (Locale.getDefault());
  }

  public static BreakIterator getCharacterInstance (Locale loc)
  {
    BreakIterator r = getInstance ("CharacterIterator", loc);
    if (r == null)
      r = new gnu.gcj.text.CharacterBreakIterator ();
    return r;
  }

  public static BreakIterator getLineInstance ()
  {
    return getLineInstance (Locale.getDefault());
  }

  public static BreakIterator getLineInstance (Locale loc)
  {
    BreakIterator r = getInstance ("LineIterator", loc);
    if (r == null)
      r = new gnu.gcj.text.LineBreakIterator ();
    return r;
  }

  public static BreakIterator getSentenceInstance ()
  {
    return getSentenceInstance (Locale.getDefault());
  }

  public static BreakIterator getSentenceInstance (Locale loc)
  {
    BreakIterator r = getInstance ("SentenceIterator", loc);
    if (r == null)
      r = new gnu.gcj.text.SentenceBreakIterator ();
    return r;
  }

  public abstract CharacterIterator getText ();

  public static BreakIterator getWordInstance ()
  {
    return getWordInstance (Locale.getDefault());
  }

  public static BreakIterator getWordInstance (Locale loc)
  {
    BreakIterator r = getInstance ("WordIterator", loc);
    if (r == null)
      r = new gnu.gcj.text.WordBreakIterator ();
    return r;
  }

  public boolean isBoundary (int pos)
  {
    if (pos == 0)
      return true;
    return following (pos - 1) == pos;
  }

  public abstract int last ();
  public abstract int next ();
  public abstract int next (int n);

  public int preceding (int pos)
  {
    if (following (pos) == DONE)
      last ();
    while (previous () >= pos)
      ;
    return current ();
  }

  public abstract int previous ();

  public void setText (String newText)
  {
    setText (new StringCharacterIterator (newText));
  }

  public abstract void setText (CharacterIterator newText);
}
