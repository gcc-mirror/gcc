/* CurrencyNameProvider.java -- Providers of localized currency symbols
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
 * A {@link CurrencyNameProvider} provides localized
 * versions of the symbols that represent a particular
 * currency.  Note that currency symbols are regarded
 * as names, and thus a <code>null</code> value may
 * be returned, which should be treated as a lack of
 * support for the specified {@link Locale}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public abstract class CurrencyNameProvider
  extends LocaleServiceProvider
{

  /**
   * Constructs a new {@link CurrencyNameProvider}.
   * Provided for implicit invocation by subclasses.
   */
  protected CurrencyNameProvider()
  {
  }

  /**
   * <p>
   * This method returns the symbol which precedes or follows a
   * value in this particular currency.  The returned value is
   * the symbol used to denote the currency in the specified locale.
   * </p>
   * <p>
   * For example, a supplied locale may specify a different symbol
   * for the currency, due to conflicts with its own currency.
   * This would be the case with the American currency, the dollar.
   * Locales that also use a dollar-based currency (e.g. Canada, Australia)
   * need to differentiate the American dollar using 'US$' rather than '$'.
   * So, supplying one of these locales to <code>getSymbol()</code> would
   * return this value, rather than the standard '$'.
   * </p>
   * <p>
   * In cases where there is no such symbol for a particular currency,
   * <code>null</code> should be returned.
   * </p>
   *
   * @param currencyCode the ISO 4217 currency code, consisting
   *                     of three uppercase letters from 'A' to 'Z'
   * @param locale the locale to express the symbol in.
   * @return the currency symbol, or <code>null</code> if one is
   *         unavailable.
   * @throws NullPointerException if the locale is null.
   * @throws IllegalArgumentException if the currency code is
   *                                  not in the correct format
   *                                  or the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.util.Currency#getSymbol(java.util.Locale)
   */
  public abstract String getSymbol(String currencyCode, Locale locale);

}
