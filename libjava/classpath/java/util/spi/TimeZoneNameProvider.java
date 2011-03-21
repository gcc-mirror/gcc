/* TimeZoneNameProvider.java -- Providers of localized currency symbols
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
 * A {@link TimeZoneNameProvider} provides localized
 * versions of the names that represent a particular
 * timezone.  A <code>null</code> value may
 * be returned, which should be treated as a lack of
 * support for the specified {@link Locale}.  The names
 * from this class are also used by
 * {@link DateFormatSymbols#getZoneStrings()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public abstract class TimeZoneNameProvider
  extends LocaleServiceProvider
{

  /**
   * Constructs a new {@link TimeZoneNameProvider}.
   * Provided for implicit invocation by subclasses.
   */
  protected TimeZoneNameProvider()
  {
  }

  /**
   * Returns a name for the specified time zone identifier
   * localized to the supplied {@link java.util.Locale}.
   * The time zone identifier is either <code>"GMT"</code>
   * or one of the identifiers from the public domain "tz
   * database" found at <a href="ftp://elsie.nci.nih.gov/pub/">
   * ftp://elsie.nci.nih.gov/pub</a>.  Note that a translated
   * name for the daylight savings time variant should be returned,
   * even if the timezone has not observed daylight savings
   * time in the past.  If the name of the timezone
   * in the given locale is not supported, <code>null</code>
   * is returned.
   *
   * @param id a time zone identifier.
   * @param daylight true if the daylight savings time variant
   *                 should be returned.
   * @param style either {@link java.util.TimeZone.LONG} or
   *              {@link java.util.TimeZone.SHORT}
   * @param locale the locale to express the timezone in.
   * @return the localized time zone name, or <code>null</code>
   *         if one is not available.
   * @throws NullPointerException if the identifer or locale is null.
   * @throws IllegalArgumentException if the style is invalid
   *                                  or the locale is not one
   *                                  returned by
   *                                  {@link getAvailableLocales()}
   * @see java.util.TimeZone#getDisplayName(boolean,int,java.util.Locale)
   */
  public abstract String getDisplayName(String id, boolean daylight,
                                        int style, Locale locale);

}
