/* Messages.java -- I18N related helper class
 Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.tools.keytool;

import gnu.classpath.Configuration;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;

/**
 * An initially generated Eclipse helper class to ease the use of localized
 * messages.
 * <p>
 * Enriched to handle localized message formats.
 */
class Messages
{
  private static final Logger log = Logger.getLogger(Messages.class.getName());
  private static final String BUNDLE_NAME = "gnu.classpath.tools.keytool.messages";
  private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);
  private static final Map CACHED_FORMATS = new HashMap(5);

  private Messages()
  {
    super();
  }

  public static String getString(String key)
  {
    try
      {
        return RESOURCE_BUNDLE.getString(key);
      }
    catch (MissingResourceException e)
      {
        return constructMessage(key, null);
      }
  }

  public static String getFormattedString(String key, Object args)
  {
    MessageFormat mf = (MessageFormat) CACHED_FORMATS.get(key);
    if (mf == null)
      {
        String formatString = getString(key);
        if (formatString.startsWith("!"))
          return constructMessage(key, args);

          mf = new MessageFormat(formatString);
          CACHED_FORMATS.put(key, mf);
      }

    // if the argument is not an array, then build one consisting of the
    // sole argument before passing it to the format() method
    try
      {
        if (args instanceof Object[])
          return mf.format(args);

        return mf.format(new Object[] { args });
      }
    catch (IllegalArgumentException x)
      {
        if (Configuration.DEBUG)
          log.fine("Exception while rendering a message format keyed by ["
                   + key + "]: " + mf.toPattern());
        return constructMessage(mf.toPattern(), args);
      }
  }

  private static final String constructMessage(String m, Object args)
  {
    if (args == null)
      return '!' + m + '!';

    return '!' + m + '!' + String.valueOf(args) + '!';
  }
}
