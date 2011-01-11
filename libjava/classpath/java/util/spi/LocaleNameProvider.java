/* LocaleNameProvider.java -- Providers of localized locale names
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

package java.util.spi;

import java.util.Locale;

/**
 * A {@link LocaleNameProvider} provides localized
 * versions of the names that represent a particular
 * locale.  Note that a <code>null</code> value may
 * be returned, which should be treated as a lack of
 * support for the specified {@link Locale}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public abstract class LocaleNameProvider
  extends LocaleServiceProvider
{

  /**
   * Constructs a new {@link LocaleNameProvider}.
   * Provided for implicit invocation by subclasses.
   */
  protected LocaleNameProvider()
  {
  }

  /**
   * Returns the localized name for the specified ISO 3166
   * country in the supplied {@link java.util.Locale}.
   * For example, if the country code is <code>"DE"</code>,
   * this method will return <code>"Germany"</code> for
   * {@link Locale.ENGLISH} but <code>"Deutschland"</code>
   * for {@link Locale.GERMANY}.  If the name of the country
   * in the given locale is not supported, <code>null</code>
   * is returned.
   *
   * @param countryCode the ISO 3166 country code, consisting
   *                    of two uppercase letters from 'A' to 'Z'
   * @param locale the locale to express the country in.
   * @return the country name, or <code>null</code> if one is
   *         not available.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the country code is
   *                                  not in the correct format
   *                                  or the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.util.Locale#getDisplayCountry(java.util.Locale)
   */
  public abstract String getDisplayCountry(String countryCode,
                                           Locale locale);

  /**
   * Returns the localized name for the specified ISO 639
   * language in the supplied {@link java.util.Locale}.
   * For example, if the language code is <code>"de"</code>,
   * this method will return <code>"German"</code> for
   * {@link Locale.ENGLISH} but <code>"Deutsch"</code>
   * for {@link Locale.GERMANY}.  If the name of the language
   * in the given locale is not supported, <code>null</code>
   * is returned.
   *
   * @param langCode the ISO 639 language code, consisting
   *                 of two lowercase letters from 'a' to 'z'
   * @param locale the locale to express the language in.
   * @return the country name, or <code>null</code> if one is
   *         not available.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the language code is
   *                                  not in the correct format
   *                                  or the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.util.Locale#getDisplayLanguage(java.util.Locale)
   */
  public abstract String getDisplayLanguage(String langCode,
                                            Locale locale);

  /**
   * Returns the localized name for the specified variant
   * in the supplied {@link java.util.Locale}.  If the name
   * of the variant in the given locale is not supported,
   * <code>null</code> is returned.
   *
   * @param variant the variant.
   * @param locale the locale to express the variant in.
   * @return the localized variant, or <code>null</code> if one is
   *         not available.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.util.Locale#getDisplayVariant(java.util.Locale)
   */
  public abstract String getDisplayVariant(String variant,
                                           Locale locale);

}
