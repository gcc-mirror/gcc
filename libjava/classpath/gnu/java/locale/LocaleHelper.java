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

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

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
   * This method is used by the localized name lookup methods to retrieve
   * the localized name of a particular piece of locale data.    
   * If the display name can not be localized to the supplied
   * locale, it will fall back on other output in the following order:
   *
   * <ul>
   * <li>the localized name in the default locale</li>
   * <li>the localized name in English (optional)</li>
   * <li>the localized name in the root locale bundle (optional)</li>
   * <li>the localized input string</li>
   * </ul>
   * <p>
   * If the supplied key is merely the empty string, then the empty string is
   * returned.
   * </p>
   *
   * @param inLocale the locale to use for formatting the display string.
   * @param key the locale data used as a key to the localized lookup tables.
   * @param name the name of the hashtable containing the localized data.
   * @param checkEnglish true if the method should fall back on data
   *        from the English locale.
   * @param checkRoot true if the method should fall back on data from the
   *        unlocalized root locale.
   * @return a <code>String</code>, hopefully containing the localized
   *         variant of the input data.
   * @throws NullPointerException if <code>inLocale</code> is null. 
   */
  public static String getLocalizedString(Locale inLocale, String key,
					  String name, boolean checkEnglish,
					  boolean checkRoot)
  {
    String localizedString;
    String property;

    if (key.equals(""))
      return "";
    property = name + "." + key;
    /* Localize to inLocale */
    try
      {
        localizedString =
	  ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
				   inLocale).getString(property);
      }
    catch (MissingResourceException exception)
      {
	localizedString = null;
      }
    /* Localize to default locale */
    if (localizedString == null)
      {
	try 
	  {
	    ResourceBundle bundle;
	    
	    bundle = 
	      ResourceBundle.getBundle("gnu.java.locale.LocaleInformation");
	    localizedString = bundle.getString(property);
	  }
	catch (MissingResourceException exception)
	  {
	    localizedString = null;
	  }
      }
    /* Localize to English */
    if (localizedString == null && checkEnglish)
      {
	try
	  {
	    localizedString = 
	      ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
				       Locale.ENGLISH).getString(property);
	  }
	catch (MissingResourceException exception)
	  {
	    localizedString = null;
	  }
      }
    /* Return unlocalized version */
    if (localizedString == null && checkRoot)
      {
	try
	  {
	    localizedString = 
	      ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
				       new Locale("","","")
				       ).getString(property);
	  }
	catch (MissingResourceException exception)
	  {
	    localizedString = null;
	  }
      }
    /* Return original input string */
    if (localizedString == null)
      {
	localizedString = key;
      }
    return localizedString;
  }
}

