/* DateFormatSymbols.java -- Format over a range of numbers
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005, 2006  Free Software Foundation, Inc.

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


package java.text;

import gnu.java.locale.LocaleHelper;

import java.io.IOException;

import java.text.spi.DateFormatSymbolsProvider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.ServiceLoader;
import java.util.TimeZone;

import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;

import java.util.regex.Pattern;

import java.util.spi.TimeZoneNameProvider;

/**
 * This class acts as container for locale specific date/time formatting
 * information such as the days of the week and the months of the year.
 *
 * @author Per Bothner (bothner@cygnus.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @date October 24, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3.
 * Status:  Believed complete and correct.
 */
public class DateFormatSymbols implements java.io.Serializable, Cloneable
{
  /**
   * The set of properties for obtaining the metazone data.
   */
  private static transient final Properties properties;

  /**
   * Reads in the properties.
   */
  static
  {
    properties = new Properties();
    try
      {
        properties.load(DateFormatSymbols.class.getResourceAsStream("metazones.properties"));
      }
    catch (IOException exception)
      {
        System.out.println("Failed to load weeks resource: " + exception);
      }
  }

  private static final Pattern ZONE_SEP = Pattern.compile("\u00a9");

  private static final Pattern FIELD_SEP = Pattern.compile("\u00ae");

  /**
   * Class for storing DateFormatSymbols data parsed from the property files.
   */
  private static class DFSData
  {
    private String[] ampms;
    private String[] eras;
    private String localPatternChars;
    private String[] months;
    private String[] shortMonths;
    private String[] weekdays;
    private String[] shortWeekdays;
    private String[] dateFormats;
    private String[] timeFormats;
    private String[][] runtimeZoneStrings;

    /**
     * Construct a new instance with the parsed data.
     *
     * @param ampms strings for "am" and "pm".
     * @param eras strings for calendar eras.
     * @param localPatternChars localised pattern characters.
     * @param months strings for the months of the year.
     * @param shortMonths short strings for the months of the year.
     * @param weekdays strings for the days of the week.
     * @param shortWeekdays short strings for the days of the week.
     * @param dateFormats localised date formats.
     * @param timeFormats localised time formats.
     * @param runtimeZoneStrings localised time zone names.
     */
    public DFSData(String[] ampms, String[] eras, String localPatternChars,
                   String[] months, String[] shortMonths, String[] weekdays,
                   String[] shortWeekdays, String[] dateFormats,
                   String[] timeFormats, String[][] runtimeZoneStrings)
    {
      this.ampms = ampms;
      this.eras = eras;
      this.localPatternChars = localPatternChars;
      this.months = months;
      this.shortMonths = shortMonths;
      this.weekdays = weekdays;
      this.shortWeekdays = shortWeekdays;
      this.dateFormats = dateFormats;
      this.timeFormats = timeFormats;
      this.runtimeZoneStrings = runtimeZoneStrings;
    }

    /**
     * Accessor for the AM/PM data.
     *
     * @return the AM/PM strings.
     */
    public String[] getAMPMs()
    {
      return ampms.clone();
    }

    /**
     * Accessor for the era data.
     *
     * @return the era strings.
     */
    public String[] getEras()
    {
      return eras.clone();
    }

    /**
     * Accessor for the local pattern characters.
     *
     * @return the local pattern characters.
     */
    public String getLocalPatternChars()
    {
      return localPatternChars;
    }

    /**
     * Accessor for the months of the year (long form).
     *
     * @return the months of the year (long form).
     */
    public String[] getMonths()
    {
      return months.clone();
    }

    /**
     * Accessor for the months of the year (short form).
     *
     * @return the months of the year (short form).
     */
    public String[] getShortMonths()
    {
      return shortMonths.clone();
    }

    /**
     * Accessor for the days of the week (long form).
     *
     * @return the days of the week (long form).
     */
    public String[] getWeekdays()
    {
      return weekdays.clone();
    }

    /**
     * Accessor for the days of the week (short form).
     *
     * @return the days of the week (short form).
     */
    public String[] getShortWeekdays()
    {
      return shortWeekdays.clone();
    }

    /**
     * Accessor for the date formats.
     *
     * @return the date formats.
     */
    public String[] getDateFormats()
    {
      return dateFormats.clone();
    }

    /**
     * Accessor for the time formats.
     *
     * @return the time formats.
     */
    public String[] getTimeFormats()
    {
      return timeFormats.clone();
    }

    /**
     * Accessor for the zone strings.
     *
     * @return the zone strings.
     */
    public String[][] getZoneStrings()
    {
      // Perform a deep clone so subarrays aren't modifiable
      String[][] clone = runtimeZoneStrings.clone();
      for (int a = 0; a < clone.length; ++a)
        clone[a] = runtimeZoneStrings[a].clone();
      return clone;
    }

  }

  private static final ConcurrentMap<Locale, DFSData> dataCache = new ConcurrentHashMap<Locale, DFSData>();

  String[] ampms;
  String[] eras;
  private String localPatternChars;
  String[] months;
  String[] shortMonths;
  String[] shortWeekdays;
  String[] weekdays;

  /**
   * The timezone strings supplied by the runtime.
   */
  private String[][] runtimeZoneStrings;

  /**
   * Custom timezone strings supplied by {@link #setZoneStrings()}.
   */
  private String[][] zoneStrings;

  private static final long serialVersionUID = -5987973545549424702L;

  // The order of these prefixes must be the same as in DateFormat
  private static final String[] formatPrefixes =
  {
    "full", "long", "medium", "short"
  };

  // These are each arrays with a value for SHORT, MEDIUM, LONG, FULL,
  // and DEFAULT (constants defined in java.text.DateFormat).  While
  // not part of the official spec, we need a way to get at locale-specific
  // default formatting patterns.  They are declared package scope so
  // as to be easily accessible where needed (DateFormat, SimpleDateFormat).
  transient String[] dateFormats;
  transient String[] timeFormats;

  /**
   * Compiles a string array for a property using data from each of the locales in the
   * hierarchy as necessary.
   *
   * @param bundles the locale hierarchy, starting with the most specific.
   * @param name the name of the property.
   * @param size the size the array should be when complete.
   * @return a completed string array.
   */
  private static String[] getStringArray(List<ResourceBundle> bundles, String name, int size)
  {
    return getStringArray(bundles, name, size, null);
  }

  /**
   * Compiles a string array for a property using data from each of the locales in the
   * hierarchy as necessary.  If non-null, the fallback array is also used for "sideways"
   * inheritance (e.g. if there is no short name for a month, the long name is used rather
   * than the empty string).
   *
   * @param bundles the locale hierarchy, starting with the most specific.
   * @param name the name of the property.
   * @param size the size the array should be when complete.
   * @param fallback an array of long name fallback strings for data with both long and short names.
   * @return a completed string array.
   */
  private static String[] getStringArray(List<ResourceBundle> bundles, String name, int size,
                                         String[] fallback)
  {
    String[] data = new String[size];
    Arrays.fill(data, "");
    // Populate array with data from each locale back to the root, starting with the most specific
    for (int a = 0; a < bundles.size(); ++a)
      {
        String localeData = bundles.get(a).getString(name);
        String[] array = FIELD_SEP.split(localeData, size);
        for (int b = 0; b < data.length; ++b)
          {
            if (array.length > b && array[b] != null && data[b].isEmpty() && !array[b].isEmpty())
              data[b] = array[b];
          }
      }
    // Replace any remaining empty strings with data from the fallback array, if non-null
    if (fallback != null && fallback.length == size)
      {
        for (int a = 0; a < data.length; ++a)
          {
            if (data[a].isEmpty() && fallback[a] != null && !fallback[a].isEmpty())
              data[a] = fallback[a];
          }
      }
    return data;
  }

  private static String[][] getZoneStrings(List<ResourceBundle> bundles, Locale locale)
  {
    List<String[]> allZones = new ArrayList<String[]>();
    try
      {
        Map<String,String[]> systemZones = new HashMap<String,String[]>();
        for (ResourceBundle bundle : bundles)
          {
            String country = locale.getCountry();
            String data = bundle.getString("zoneStrings");
            String[] zones = ZONE_SEP.split(data);
            for (int a = 0; a < zones.length; ++a)
              {
                String[] strings = FIELD_SEP.split(zones[a]);
                String type = properties.getProperty(strings[0] + "." + country);
                if (type == null)
                  type = properties.getProperty(strings[0] + ".DEFAULT");
                if (type != null)
                  strings[0] = type;
                if (strings.length < 5)
                  {
                    String[] newStrings = new String[5];
                    System.arraycopy(strings, 0, newStrings, 0, strings.length);
                    for (int b = strings.length; b < newStrings.length; ++b)
                      newStrings[b] = "";
                    strings = newStrings;
                  }
                String[] existing = systemZones.get(strings[0]);
                if (existing != null && existing.length > 1)
                  {
                    for (int b = 1; b < existing.length; ++b)
                      if (!existing[b].equals(""))
                        strings[b] = existing[b];
                  }
                systemZones.put(strings[0], strings);
              }
          }
        /* Final sanity check for missing values */
        for (String[] zstrings : systemZones.values())
          {
            if (zstrings[1].equals("") && zstrings[2].equals(""))
              {
                for (Map.Entry<Object,Object> entry : properties.entrySet())
                  {
                    String val = (String) entry.getValue();
                    if (val.equals(zstrings[0]))
                      {
                        String key = (String) entry.getKey();
                        String metazone = key.substring(0, key.indexOf("."));
                        String type = properties.getProperty(metazone + "." + locale.getCountry());
                        if (type == null)
                          type = properties.getProperty(metazone + ".DEFAULT");
                        if (type != null)
                          {
                            String[] ostrings = systemZones.get(type);
                            zstrings[1] = ostrings[1];
                            zstrings[2] = ostrings[2];
                          }
                      }
                  }
              }
          }
        allZones.addAll(systemZones.values());
      }
    catch (MissingResourceException e)
      {
        /* This means runtime support for the locale
         * is not available, so we just include providers. */
      }
    for (TimeZoneNameProvider p :
           ServiceLoader.load(TimeZoneNameProvider.class))
      {
        for (Locale loc : p.getAvailableLocales())
          {
            if (loc.equals(locale))
              {
                for (String id : TimeZone.getAvailableIDs())
                  {
                    String[] z = new String[5];
                    z[0] = id;
                    z[1] = p.getDisplayName(id, false,
                                            TimeZone.LONG,
                                            locale);
                    z[2] = p.getDisplayName(id, false,
                                            TimeZone.SHORT,
                                            locale);
                    z[3] = p.getDisplayName(id, true,
                                            TimeZone.LONG,
                                            locale);
                    z[4] = p.getDisplayName(id, true,
                                            TimeZone.SHORT,
                                            locale);
                    allZones.add(z);
                  }
                break;
              }
          }
      }
    return allZones.toArray(new String[allZones.size()][]);
  }

  /**
   * Retrieve the date or time formats for a specific key e.g.
   * asking for "DateFormat" will return an array containing the
   * full, long, medium and short date formats localised for
   * the locales in the specified bundle.
   *
   * @param bundles the stack of bundles to check, most-specific first.
   * @param key the type of format to retrieve.
   * @param an array of localised strings for each format prefix.
   */
  private static String[] formatsForKey(List<ResourceBundle> bundles, String key)
  {
    String[] values = new String[formatPrefixes.length];

    for (int i = 0; i < formatPrefixes.length; i++)
      values[i] = getString(bundles, formatPrefixes[i] + key);

    return values;
  }

  /**
   * Simple wrapper around extracting a {@code String} from a
   * {@code ResourceBundle}.  Keep searching less-specific locales
   * until a non-null non-empty value is found.
   *
   * @param bundles the stack of bundles to check, most-specific first.
   * @param key the key of the value to retrieve.
   * @return the first non-null non-empty String found or the last
   *         retrieved if one isn't found.
   */
  private static String getString(List<ResourceBundle> bundles, String key)
  {
    String val = null;
    for (ResourceBundle bundle : bundles)
      {
        val = bundle.getString(key);
        if (val != null && !val.isEmpty())
          return val;
      }
    return val;
  }

  /**
   * Retrieves the locale data from the property files and constructs a
   * {@code DFSData} instance for it.
   *
   * @param the locale for which data should be retrieved.
   * @return the parsed data.
   * @throws MissingResourceException if the resources for the specified
   *                                  locale could not be found or loaded.
   */
  private static DFSData retrieveData(Locale locale)
    throws MissingResourceException
  {
    DFSData data = dataCache.get(locale);
    if (data == null)
      {
        ClassLoader ldr = ClassLoader.getSystemClassLoader();
        List<ResourceBundle> bundles = new ArrayList<ResourceBundle>();
        ResourceBundle res
          = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation", locale, ldr);
        bundles.add(res);
        Locale resLocale = res.getLocale();
        while (resLocale != Locale.ROOT)
          {
            res = ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
                                           LocaleHelper.getFallbackLocale(resLocale), ldr);
            bundles.add(res);
            resLocale = res.getLocale();
          }
        String[] lMonths = getStringArray(bundles, "months", 13);
        String[] lWeekdays = getStringArray(bundles, "weekdays", 8);
        data = new DFSData(getStringArray(bundles, "ampms", 2),
                           getStringArray(bundles, "eras", 2),
                           getString(bundles, "localPatternChars"),
                           lMonths, getStringArray(bundles, "shortMonths", 13, lMonths),
                           lWeekdays, getStringArray(bundles, "shortWeekdays", 8, lWeekdays),
                           formatsForKey(bundles, "DateFormat"),
                           formatsForKey(bundles, "TimeFormat"),
                           getZoneStrings(bundles, locale));
        DFSData cachedData = dataCache.putIfAbsent(locale, data);
        // Use the earlier version if another thread beat us to it.
        if (cachedData != null)
          data = cachedData;
      }
    return data;
  }

  /**
   * This method initializes a new instance of <code>DateFormatSymbols</code>
   * by loading the date format information for the specified locale.
   * This constructor only obtains instances using the runtime's resources;
   * to also include {@link java.text.spi.DateFormatSymbolsProvider} instances,
   * call {@link #getInstance(java.util.Locale)} instead.
   *
   * @param locale The locale for which date formatting symbols should
   *               be loaded.
   * @throws MissingResourceException if the resources for the specified
   *                                  locale could not be found or loaded.
   * @see #getInstance(java.util.Locale)
   */
  public DateFormatSymbols (Locale locale)
    throws MissingResourceException
  {
    DFSData data = retrieveData(locale);
    ampms = data.getAMPMs();
    eras = data.getEras();
    localPatternChars = data.getLocalPatternChars();
    months = data.getMonths();
    shortMonths = data.getShortMonths();
    weekdays = data.getWeekdays();
    shortWeekdays = data.getShortWeekdays();
    dateFormats = data.getDateFormats();
    timeFormats = data.getTimeFormats();
    runtimeZoneStrings = data.getZoneStrings();
  }

  /**
   * This method loads the format symbol information for the default
   * locale. This constructor only obtains instances using the runtime's resources;
   * to also include {@link java.text.spi.DateFormatSymbolsProvider} instances,
   * call {@link #getInstance()} instead.
   *
   * @throws MissingResourceException if the resources for the default
   *                                  locale could not be found or loaded.
   * @see #getInstance()
   */
  public DateFormatSymbols()
    throws MissingResourceException
  {
    this (Locale.getDefault());
  }

  /**
   * This method returns the list of strings used for displaying AM or PM.
   * This is a two element <code>String</code> array indexed by
   * <code>Calendar.AM</code> and <code>Calendar.PM</code>
   *
   * @return The list of AM/PM display strings.
   */
  public String[] getAmPmStrings()
  {
    return ampms;
  }

  /**
    * This method returns the list of strings used for displaying eras
    * (e.g., "BC" and "AD").  This is a two element <code>String</code>
    * array indexed by <code>Calendar.BC</code> and <code>Calendar.AD</code>.
    *
    * @return The list of era disply strings.
    */
  public String[] getEras()
  {
    return eras;
  }

  /**
    * This method returns the pattern character information for this
    * object.  This is an 18 character string that contains the characters
    * that are used in creating the date formatting strings in
    * <code>SimpleDateFormat</code>.   The following are the character
    * positions in the string and which format character they correspond
    * to (the character in parentheses is the default value in the US English
    * locale):
    * <p>
    * <ul>
    * <li>0 - era (G)</li>
    * <li>1 - year (y)</li>
    * <li>2 - month (M)</li>
    * <li>3 - day of month (d)</li>
    * <li>4 - hour out of 12, from 1-12 (h)</li>
    * <li>5 - hour out of 24, from 0-23 (H)</li>
    * <li>6 - minute (m)</li>
    * <li>7 - second (s)</li>
    * <li>8 - millisecond (S)</li>
    * <li>9 - date of week (E)</li>
    * <li>10 - date of year (D)</li>
    * <li>11 - day of week in month, eg. "4th Thur in Nov" (F)</li>
    * <li>12 - week in year (w)</li>
    * <li>13 - week in month (W)</li>
    * <li>14 - am/pm (a)</li>
    * <li>15 - hour out of 24, from 1-24 (k)</li>
    * <li>16 - hour out of 12, from 0-11 (K)</li>
    * <li>17 - time zone (z)</li>
    * </ul>
    *
    * @return The format patter characters
    */
  public String getLocalPatternChars()
  {
    return localPatternChars;
  }

  /**
   * This method returns the list of strings used for displaying month
   * names (e.g., "January" and "February").  This is a thirteen element
   * string array indexed by <code>Calendar.JANUARY</code> through
   * <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
   * elements because some calendars have thriteen months.
   *
   * @return The list of month display strings.
   */
  public String[] getMonths ()
  {
    return months;
  }

  /**
   * This method returns the list of strings used for displaying abbreviated
   * month names (e.g., "Jan" and "Feb").  This is a thirteen element
   * <code>String</code> array indexed by <code>Calendar.JANUARY</code>
   * through <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
   * elements because some calendars have thirteen months.
   *
   * @return The list of abbreviated month display strings.
   */
  public String[] getShortMonths ()
  {
    return shortMonths;
  }

  /**
   * This method returns the list of strings used for displaying abbreviated
   * weekday names (e.g., "Sun" and "Mon").  This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @return This list of abbreviated weekday display strings.
   */
  public String[] getShortWeekdays ()
  {
    return shortWeekdays;
  }

  /**
   * This method returns the list of strings used for displaying weekday
   * names (e.g., "Sunday" and "Monday").  This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @return This list of weekday display strings.
   */
  public String[] getWeekdays ()
  {
    return weekdays;
  }

  /**
   * This method returns this list of localized timezone display strings.
   * This is a two dimensional <code>String</code> array where each row in
   * the array contains five values:
   * <P>
   * <ul>
   * <li>0 - The non-localized time zone id string.</li>
   * <li>1 - The long name of the time zone (standard time).</li>
   * <li>2 - The short name of the time zone (standard time).</li>
   * <li>3 - The long name of the time zone (daylight savings time).</li>
   * <li>4 - the short name of the time zone (daylight savings time).</li>
   * </ul>
   * <p>
   * If {@link #setZoneStrings(String[][])} has been called, then the value
   * passed to this will be returned.  Otherwise the returned array contains
   * zone names provided by the runtime environment and any
   * {@link java.util.spi.TimeZoneProvider} instances.
   * </p>
   *
   * @return The list of time zone display strings.
   * @see #setZoneStrings(String[][])
   */
  public String[][] getZoneStrings()
  {
    if (zoneStrings != null)
      return zoneStrings;
    return runtimeZoneStrings;
  }

  /**
   * This method sets the list of strings used to display AM/PM values to
   * the specified list.
   * This is a two element <code>String</code> array indexed by
   * <code>Calendar.AM</code> and <code>Calendar.PM</code>
   *
   * @param value The new list of AM/PM display strings.
   */
  public void setAmPmStrings (String[] value)
  {
    if(value==null)
      throw new NullPointerException();
    ampms = value;
  }

  /**
   * This method sets the list of strings used to display time eras to
   * to the specified list.
   * This is a two element <code>String</code>
   * array indexed by <code>Calendar.BC</code> and <code>Calendar.AD</code>.
   *
   * @param labels The new list of era display strings.
   */
  public void setEras (String[] labels)
  {
    if(labels==null)
      throw new NullPointerException();
    eras = labels;
  }

  /**
    * This method sets the list of characters used to specific date/time
    * formatting strings.
    * This is an 18 character string that contains the characters
    * that are used in creating the date formatting strings in
    * <code>SimpleDateFormat</code>.   The following are the character
    * positions in the string and which format character they correspond
    * to (the character in parentheses is the default value in the US English
    * locale):
    * <p>
    * <ul>
    * <li>0 - era (G)</li>
    * <li>1 - year (y)</li>
    * <li>2 - month (M)</li>
    * <li>3 - day of month (d)</li>
    * <li>4 - hour out of 12, from 1-12 (h)</li>
    * <li>5 - hour out of 24, from 0-23 (H)</li>
    * <li>6 - minute (m)</li>
    * <li>7 - second (s)</li>
    * <li>8 - millisecond (S)</li>
    * <li>9 - date of week (E)</li>
    * <li>10 - date of year (D)</li>
    * <li>11 - day of week in month, eg. "4th Thur in Nov" (F)</li>
    * <li>12 - week in year (w)</li>
    * <li>13 - week in month (W)</li>
    * <li>14 - am/pm (a)</li>
    * <li>15 - hour out of 24, from 1-24 (k)</li>
    * <li>16 - hour out of 12, from 0-11 (K)</li>
    * <li>17 - time zone (z)</li>
    * </ul>
    *
    * @param chars The new format pattern characters
    */
  public void setLocalPatternChars (String chars)
  {
    if(chars==null)
      throw new NullPointerException();
    localPatternChars = chars;
  }

  /**
    * This method sets the list of strings used to display month names.
    * This is a thirteen element
    * string array indexed by <code>Calendar.JANUARY</code> through
    * <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
    * elements because some calendars have thriteen months.
    *
    * @param labels The list of month display strings.
    */
  public void setMonths (String[] labels)
  {
    if(labels==null)
      throw new NullPointerException();
    months = labels;
  }

  /**
   * This method sets the list of strings used to display abbreviated month
   * names.
   * This is a thirteen element
   * <code>String</code> array indexed by <code>Calendar.JANUARY</code>
   * through <code>Calendar.UNDECEMBER</code>.  Note that there are thirteen
   * elements because some calendars have thirteen months.
   *
   * @param labels The new list of abbreviated month display strings.
   */
  public void setShortMonths (String[] labels)
  {
    if(labels==null)
      throw new NullPointerException();
    shortMonths = labels;
  }

  /**
   * This method sets the list of strings used to display abbreviated
   * weekday names.
   * This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @param labels This list of abbreviated weekday display strings.
   */
  public void setShortWeekdays (String[] labels)
  {
    if(labels==null)
      throw new NullPointerException();
    shortWeekdays = labels;
  }

  /**
   * This method sets the list of strings used to display weekday names.
   * This is an eight element
   * <code>String</code> array indexed by <code>Calendar.SUNDAY</code>
   * through <code>Calendar.SATURDAY</code>.  Note that the first element
   * of this array is ignored.
   *
   * @param labels This list of weekday display strings.
   */
  public void setWeekdays (String[] labels)
  {
    if(labels==null)
      throw new NullPointerException();
    weekdays = labels;
  }

  /**
   * This method sets the list of display strings for time zones.
   * This is a two dimensional <code>String</code> array where each row in
   * the array contains five values:
   * <P>
   * <ul>
   * <li>0 - The non-localized time zone id string.</li>
   * <li>1 - The long name of the time zone (standard time).</li>
   * <li>2 - The short name of the time zone (standard time).</li>
   * <li>3 - The long name of the time zone (daylight savings time).</li>
   * <li>4 - the short name of the time zone (daylight savings time).</li>
   * </ul>
   *
   * @params zones The list of time zone display strings.
   */
  public void setZoneStrings (String[][] zones)
  {
    if(zones==null)
      throw new NullPointerException();
    zoneStrings = zones;
  }

  /* Does a "deep" equality test - recurses into arrays. */
  private static boolean equals (Object x, Object y)
  {
    if (x == y)
      return true;
    if (x == null || y == null)
      return false;
    if (! (x instanceof Object[]) || ! (y instanceof Object[]))
      return x.equals(y);
    Object[] xa = (Object[]) x;
    Object[] ya = (Object[]) y;
    if (xa.length != ya.length)
      return false;
    for (int i = xa.length;  --i >= 0; )
      {
        if (! equals(xa[i], ya[i]))
          return false;
      }
    return true;
  }

  private static int hashCode (Object x)
  {
    if (x == null)
      return 0;
    if (! (x instanceof Object[]))
      return x.hashCode();
    Object[] xa = (Object[]) x;
    int hash = 0;
    for (int i = 0;  i < xa.length;  i++)
      hash = 37 * hashCode(xa[i]);
    return hash;
  }

  /**
   * This method tests a specified object for equality against this object.
   * This will be true if and only if the specified object:
   * <p>
   * <ul>
   * <li> Is not <code>null</code>.</li>
   * <li> Is an instance of <code>DateFormatSymbols</code>.</li>
   * <li> Contains identical formatting symbols to this object.</li>
   * </ul>
   *
   * @param obj The <code>Object</code> to test for equality against.
   *
   * @return <code>true</code> if the specified object is equal to this one,
   * <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof DateFormatSymbols))
      return false;
    DateFormatSymbols other = (DateFormatSymbols) obj;
    return (equals(ampms, other.ampms)
            && equals(eras, other.eras)
            && equals(localPatternChars, other.localPatternChars)
            && equals(months, other.months)
            && equals(shortMonths, other.shortMonths)
            && equals(shortWeekdays, other.shortWeekdays)
            && equals(weekdays, other.weekdays)
            && equals(zoneStrings, other.zoneStrings));
  }

  /**
   * Returns a new copy of this object.
   *
   * @return A copy of this object
   */
  public Object clone ()
  {
    try
      {
        return super.clone ();
      }
    catch (CloneNotSupportedException e)
      {
        return null;
      }
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return A hash value for this object.
   */
  public int hashCode ()
  {
    return (hashCode(ampms)
            ^ hashCode(eras)
            ^ hashCode(localPatternChars)
            ^ hashCode(months)
            ^ hashCode(shortMonths)
            ^ hashCode(shortWeekdays)
            ^ hashCode(weekdays)
            ^ hashCode(zoneStrings));
  }

  /**
   * Returns a {@link DateFormatSymbols} instance for the
   * default locale obtained from either the runtime itself
   * or one of the installed
   * {@link java.text.spi.DateFormatSymbolsProvider} instances.
   * This is equivalent to calling
   * <code>getInstance(Locale.getDefault())</code>.
   *
   * @return a {@link DateFormatSymbols} instance for the default
   *         locale.
   * @since 1.6
   */
  public static final DateFormatSymbols getInstance()
  {
    return getInstance(Locale.getDefault());
  }

  /**
   * Returns a {@link DateFormatSymbols} instance for the
   * specified locale obtained from either the runtime itself
   * or one of the installed
   * {@link java.text.spi.DateFormatSymbolsProvider} instances.
   *
   * @param locale the locale for which an instance should be
   *               returned.
   * @return a {@link DateFormatSymbols} instance for the specified
   *         locale.
   * @throws NullPointerException if <code>locale</code> is
   *                              <code>null</code>.
   * @since 1.6
   */
  public static final DateFormatSymbols getInstance(Locale locale)
  {
    try
      {
        DateFormatSymbols syms = new DateFormatSymbols(locale);
        return syms;
      }
    catch (MissingResourceException e)
      {
        /* This means runtime support for the locale
         * is not available, so we check providers. */
      }
    for (DateFormatSymbolsProvider p :
           ServiceLoader.load(DateFormatSymbolsProvider.class))
      {
        for (Locale loc : p.getAvailableLocales())
          {
            if (loc.equals(locale))
              {
                DateFormatSymbols syms = p.getInstance(locale);
                if (syms != null)
                  return syms;
                break;
              }
          }
      }
    return getInstance(LocaleHelper.getFallbackLocale(locale));
  }

}
