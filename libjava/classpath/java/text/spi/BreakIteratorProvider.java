/* BreakIteratorProvider.java -- Providers of localized instances
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package java.text.spi;

import java.text.BreakIterator;

import java.util.Locale;

import java.util.spi.LocaleServiceProvider;

/**
 * A {@link BreakIteratorProvider} provides localized
 * instances of {@link java.text.BreakIterator}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public abstract class BreakIteratorProvider
  extends LocaleServiceProvider
{

  /**
   * Constructs a new {@link BreakIteratorProvider}.
   * Provided for implicit invocation by subclasses.
   */
  protected BreakIteratorProvider()
  {
  }

  /**
   * Returns a {@link java.text.BreakIterator} instance
   * for character breaks in the specified
   * {@link java.util.Locale}.
   *
   * @param locale the desired locale.
   * @return the localized instance for character breaks.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.text.BreakIterator#getCharacterInstance(java.util.Locale)
   */
  public abstract BreakIterator getCharacterInstance(Locale locale);

  /**
   * Returns a {@link java.text.BreakIterator} instance
   * for line breaks in the specified {@link java.util.Locale}.
   *
   * @param locale the desired locale.
   * @return the localized instance for line breaks.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.text.BreakIterator#getLineInstance(java.util.Locale)
   */
  public abstract BreakIterator getLineInstance(Locale locale);

  /**
   * Returns a {@link java.text.BreakIterator} instance
   * for sentence breaks in the specified
   * {@link java.util.Locale}.
   *
   * @param locale the desired locale.
   * @return the localized instance for sentence breaks.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.text.BreakIterator#getSentenceInstance(java.util.Locale)
   */
  public abstract BreakIterator getSentenceInstance(Locale locale);

  /**
   * Returns a {@link java.text.BreakIterator} instance
   * for word breaks in the specified
   * {@link java.util.Locale}.
   *
   * @param locale the desired locale.
   * @return the localized instance for word breaks.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.text.BreakIterator#getWordInstance(java.util.Locale)
   */
  public abstract BreakIterator getWordInstance(Locale locale);

}
