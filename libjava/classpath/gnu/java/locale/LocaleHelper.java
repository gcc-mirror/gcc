/* LocaleHelper.java -- helper routines for localization
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package gnu.java.locale;

import java.text.Collator;
import java.util.Locale;

/**
 * This class provides common helper methods
 * for handling localized data.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 *
 * @see java.util.Locale
 * @see java.util.ResourceBundle
 */
public class LocaleHelper
{
  /**
   * <p>
   * This method is used by the localized name lookup methods to
   * retrieve the next locale to try.  The next locale is derived
   * from the supplied locale by applying the first applicable
   * rule from the following:
   * </p>
   * <ol>
   * <li>If the variant contains a <code>'_'</code>, then
   * this and everything following it is trimmed.</li>
   * <li>If the variant is non-empty, it is converted to
   * an empty string.</li>
   * <li>If the country is non-empty, it is converted to
   * an empty string.</li>
   * <li>If the language is non-empty, it is converted to
   * an empty string (forming {@link java.util.Locale#ROOT})</li>
   * </ol>
   * <p>
   * The base fallback locale is {@link java.util.Locale#ROOT}.
   * </p>
   *
   * @param locale the locale for which a localized piece of
   *               data could not be obtained.
   * @return the next fallback locale to try.
   */
  public static Locale getFallbackLocale(Locale locale)
  {
    String language = locale.getLanguage();
    String country = locale.getCountry();
    String variant = locale.getVariant();
    int uscore = variant.indexOf('_');
    if (uscore != -1)
      return new Locale(language, country,
			variant.substring(0, uscore));
    if (!variant.isEmpty())
      return new Locale(language, country, "");
    if (!country.isEmpty())
      return new Locale(language, "", "");
    return Locale.ROOT;
  }

  /**
   * Return an array of all the locales for which there is a
   * {@link Collator} instance.  A new array is returned each time. 
   */
  public static Locale[] getCollatorLocales()
  {
    // For now we don't bother caching.  This is probably
    // not called very frequently.  And, we would have to
    // clone the array anyway.
    if (LocaleData.collatorLocaleNames.length == 0)
      return new Locale[] { Locale.US };
    Locale[] result = new Locale[LocaleData.collatorLocaleNames.length];
    for (int i = 0; i < result.length; ++i)
      {
        String language;
        String region = "";
        String variant = "";
        String name = LocaleData.collatorLocaleNames[i];

        language = name.substring(0, 2);

        if (name.length() > 2)
          region = name.substring(3);

        int index = region.indexOf("_");
        if (index > 0)
          {
            variant = region.substring(index + 1);
            region = region.substring(0, index - 1);
          }

        result[i] = new Locale(language, region, variant);
      }
    return result;
  }

  /**
   * Return the number of locales we know of.
   */
  public static int getLocaleCount()
  {
    return LocaleData.localeNames.length;
  }

  /**
   * Return the Nth locale name.
   */
  public static String getLocaleName(int n)
  {
    return LocaleData.localeNames[n];
  }
}

