/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 24, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3,
 * and "The Java Language Specification", ISBN 0-201-63451-1.
 * Status:  None of the getDisplayXXX or getISO3XXX methods are implemented.
 */
public final class Locale implements java.io.Serializable, Cloneable
{
  // The fields are as specified in Sun's "Serialized Form"
  // in the JDK 1.2 beta 4 API specification.
  private String country;
  private int hashcode;
  private String language;
  private String variant;
  private static Locale defaultLocale;

  // These are as specified in the JDK 1.2 AP documentation


  // LANGUAGE constants ... country-neutral
  public static final Locale CHINESE = new Locale ("zh", "");
  public static final Locale ENGLISH = new Locale ("en", "");
  public static final Locale FRENCH = new Locale ("fr", "");
  public static final Locale GERMAN = new Locale ("de", "");
  public static final Locale ITALIAN = new Locale ("it", "");
  public static final Locale JAPANESE = new Locale ("ja", "");
  public static final Locale KOREAN = new Locale ("ko", "");

  // COUNTRY constants ... countries can be multi-lingual
  public static final Locale CANADA = new Locale ("en", "CA");
  public static final Locale CANADA_FRENCH = new Locale ("fr", "CA");
  public static final Locale FRANCE = new Locale ("fr", "FR");
  public static final Locale GERMANY = new Locale ("de", "DE");
  public static final Locale ITALY = new Locale ("it", "IT");
  public static final Locale JAPAN = new Locale ("ja", "JP");
  public static final Locale KOREA = new Locale ("ko", "KR");
  public static final Locale UK = new Locale ("en", "GB");
  public static final Locale US = new Locale ("en", "US");

  // Chinese has multiple scripts and political bodies
  public static final Locale SIMPLIFIED_CHINESE = new Locale ("zh", "CN");
  public static final Locale TRADITIONAL_CHINESE = new Locale ("zh", "TW");
  public static final Locale PRC = SIMPLIFIED_CHINESE;
  public static final Locale TAIWAN = TRADITIONAL_CHINESE;
  public static final Locale CHINA = PRC;

  public Locale (String languageCode, String countryCode)
  {
    this (languageCode, countryCode, "");
  }

  public Locale (String languageCode, String countryCode,
		 String variantCode)
  {
    language = languageCode.toLowerCase();
    country = countryCode.toUpperCase();
    variant = variantCode.toUpperCase();
    hashcode = (languageCode.hashCode()
		^ countryCode.hashCode()
		^ variantCode.hashCode());
  }

  public Object clone ()
  {
    return (Object) new Locale (language, country, variant);
  }

  public boolean equals (Object obj)
    {
      if (! (obj instanceof Locale))
	return false;
      Locale loc = (Locale) obj;
      return (language.equals(loc.language)
	      && country.equals(loc.country)
	      && variant.equals(loc.variant));
    }

  public String getCountry ()
  {
    return country;
  }

  public String getLanguage ()
  {
    return language;
  }

  public String getVariant ()
  {
    return variant;
  }

  public int hashCode ()
  {
    return hashcode;
  }

  private static synchronized Locale setDefault()
  {
    if (defaultLocale != null)
      return defaultLocale;
    String language = System.getProperty("user.language");
    String country = System.getProperty("user.region");
    defaultLocale = new Locale (language == null ? "en" : language,
				country == null ? "" : country);
    return defaultLocale;
  }

  public static Locale getDefault ()
  {
    return defaultLocale == null ? setDefault() : defaultLocale;
  }

  public static void setDefault (Locale newLocale)
  {
    defaultLocale = newLocale;
  }

  public String toString ()
  {
    StringBuffer result = new StringBuffer(20);
    result.append(language);
    if (country.length() > 0)
      {
	result.append('_');
	result.append(country);
	if (variant.length() > 0)
	  {
	    result.append('_');
	    result.append(variant);
	  }
      }
    return result.toString();
  }
}
