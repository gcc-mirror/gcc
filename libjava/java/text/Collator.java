// Collator.java - Locale-sensitive string comparison.

/* Copyright (C) 1999, 2000  Red Hat, Inc.

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
 * @date March 18, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status: Mostly complete, but parts stubbed out.  Look for FIXME.
 */

public abstract class Collator implements Cloneable, Serializable
{
  public static final int NO_DECOMPOSITION = 0;
  public static final int CANONICAL_DECOMPOSITION = 1;
  public static final int FULL_DECOMPOSITION = 2;

  public static final int PRIMARY = 0;
  public static final int SECONDARY = 1;
  public static final int TERTIARY = 2;
  public static final int IDENTICAL = 3;

  protected Collator ()
  {
    strength = TERTIARY;
    decmp = CANONICAL_DECOMPOSITION;
  }

  public abstract int compare (String source, String target);

  public boolean equals (Object obj)
  {
    if (! (obj instanceof Collator))
      return false;
    Collator c = (Collator) obj;
    return decmp == c.decmp && strength == c.strength;
  }

  public boolean equals (String source, String target)
  {
    return compare (source, target) == 0;
  }

  public Object clone ()
  {
    return super.clone ();
  }

  public static synchronized Locale[] getAvailableLocales ()
  {
    // FIXME.
    return null;
  }

  public abstract CollationKey getCollationKey (String source);

  public synchronized int getDecomposition ()
  {
    return decmp;
  }

  public static Collator getInstance ()
  {
    return getInstance (Locale.getDefault());
  }

  public static Collator getInstance (Locale loc)
  {
    ResourceBundle res;
    String pattern;
    try
      {
	res = ResourceBundle.getBundle("gnu.gcj.text.LocaleData", loc);
	pattern = res.getString("collatorRule");
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

  public synchronized int getStrength ()
  {
    return strength;
  }

  public abstract int hashCode ();

  public synchronized void setDecomposition (int mode)
  {
    if (mode != NO_DECOMPOSITION
	&& mode != CANONICAL_DECOMPOSITION
	&& mode != FULL_DECOMPOSITION)
      throw new IllegalArgumentException ();
    decmp = mode;
  }

  public synchronized void setStrength (int strength)
  {
    if (strength != PRIMARY && strength != SECONDARY
	&& strength != TERTIARY && strength != IDENTICAL)
      throw new IllegalArgumentException ();
    this.strength = strength;
  }

  // Decompose a single character and append results to the buffer.
  protected native final void decomposeCharacter (char c, StringBuffer buf);

  // These names are fixed by the serialization spec.
  protected int decmp;
  protected int strength;
}
