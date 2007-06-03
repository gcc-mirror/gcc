/* LocaleServiceProvider.java -- Superclass of locale SPIs
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
 * <p>
 * This is the superclass of all the {@link Locale} service
 * provider interfaces or SPIs.  The locale SPIs are used
 * to allow for the provision of additional support for
 * locale-specific data.  The runtime environment has its
 * own collection of locale data, but these interfaces allow
 * this to be extended by external classes.
 * </p>
 * <p>
 * Service providers are created as concrete implementations
 * of these interfaces, and accessed using the extension
 * mechanism, realised by {@link ServiceLoader}.  When a factory
 * method of one of the locale-specific classes (such as
 * {@link java.text.DateFormatSymbols} or {@link java.util.Currency})
 * is called, the runtime environment is first asked to
 * provide data for the specified locale.  If the runtime
 * environment fails to provide this, then the offer is
 * made to service providers which implement the appropriate
 * interface.
 * </p>
 * <p>
 * Each provider implements the method specified by this
 * class, {@link #getAvailableLocales()}.  This method is
 * called first to determine whether the provider will be of
 * any use in providing data for the specified locale.  If
 * a provider is found to be capable, then a more specific
 * method appropriate to the class requiring the data will
 * be called.  In the case of {@link java.text.DateFormatSymbols},
 * this would be
 * {@link java.text.spi.DateFormatSymbols#getInstance(Locale)}.
 * </p>
 * <p>
 * If neither a service provider nor the runtime environment
 * itself can fulfill the request, a fallback procedure is
 * engaged.  The locale is modified by applying the first
 * applicable rule:
 * </p>
 * <ol>
 * <li>If the variant contains a <code>'_'</code>, then
 * this and everything following it is trimmed.</li>
 * <li>If the variant is non-empty, it is converted to
 * an empty string.</li>
 * <li>If the country is non-empty, it is converted to
 * an empty string.</li>
 * <li>If the language is non-empty, it is converted to
 * an empty string.</li>
 * </ol>
 * <p>
 * The modified locale is then used to start the same
 * process again.  The root locale (@link java.util.Locale#ROOT}
 * must be supported by the runtime environment in order
 * to terminate this cycle.
 * </p>
 * <p>
 * Note that any names returned by the providers may
 * be <code>null</code>.  Returning a <code>null</code>
 * name is considered equivalent to not supporting a
 * particular locale.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public abstract class LocaleServiceProvider
{

  /**
   * Constructs a new {@link LocaleServiceProvider}.
   * Provided for implicit invocation by subclasses.
   */
  protected LocaleServiceProvider()
  {
  }

  /**
   * Returns an array of {@link Locale} instances,
   * for which the provider can supply localized data.
   *
   * @return an array of supported locales.
   */
  public abstract Locale[] getAvailableLocales();

}
