/* Locale.java -- i18n locales
   Copyright (C) 1998, 1999, 2001, 2002, 2005, 2006  Free Software Foundation, Inc.

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


package java.util;

import gnu.classpath.SystemProperties;
import gnu.java.locale.LocaleHelper;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.util.spi.LocaleNameProvider;

/**
 * Locales represent a specific country and culture. Classes which can be
 * passed a Locale object tailor their information for a given locale. For
 * instance, currency number formatting is handled differently for the USA
 * and France.
 *
 * <p>Locales are made up of a language code, a country code, and an optional
 * set of variant strings. Language codes are represented by
 * <a href="http://www.ics.uci.edu/pub/ietf/http/related/iso639.txt">
 * ISO 639:1988</a> w/ additions from ISO 639/RA Newsletter No. 1/1989
 * and a decision of the Advisory Committee of ISO/TC39 on August 8, 1997.
 *
 * <p>Country codes are represented by
 * <a href="http://www.chemie.fu-berlin.de/diverse/doc/ISO_3166.html">
 * ISO 3166</a>. Variant strings are vendor and browser specific. Standard
 * variant strings include "POSIX" for POSIX, "WIN" for MS-Windows, and
 * "MAC" for Macintosh. When there is more than one variant string, they must
 * be separated by an underscore (U+005F).
 *
 * <p>The default locale is determined by the values of the system properties
 * user.language, user.country (or user.region), and user.variant, defaulting
 * to "en_US". Note that the locale does NOT contain the conversion and
 * formatting capabilities (for that, use ResourceBundle and java.text).
 * Rather, it is an immutable tag object for identifying a given locale, which
 * is referenced by these other classes when they must make locale-dependent
 * decisions.
 *
 * @see ResourceBundle
 * @see java.text.Format
 * @see java.text.NumberFormat
 * @see java.text.Collator
 * @author Jochen Hoenicke
 * @author Paul Fisher
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.1
 * @status updated to 1.4
 */
public final class Locale implements Serializable, Cloneable
{
  /** Locale which represents the English language. */
  public static final Locale ENGLISH = getLocale("en");

  /** Locale which represents the French language. */
  public static final Locale FRENCH = getLocale("fr");

  /** Locale which represents the German language. */
  public static final Locale GERMAN = getLocale("de");

  /** Locale which represents the Italian language. */
  public static final Locale ITALIAN = getLocale("it");

  /** Locale which represents the Japanese language. */
  public static final Locale JAPANESE = getLocale("ja");

  /** Locale which represents the Korean language. */
  public static final Locale KOREAN = getLocale("ko");

  /** Locale which represents the Chinese language. */
  public static final Locale CHINESE = getLocale("zh");

  /** Locale which represents the Chinese language as used in China. */
  public static final Locale SIMPLIFIED_CHINESE = getLocale("zh", "CN");

  /**
   * Locale which represents the Chinese language as used in Taiwan.
   * Same as TAIWAN Locale.
   */
  public static final Locale TRADITIONAL_CHINESE = getLocale("zh", "TW");

  /** Locale which represents France. */
  public static final Locale FRANCE = getLocale("fr", "FR");

  /** Locale which represents Germany. */
  public static final Locale GERMANY = getLocale("de", "DE");

  /** Locale which represents Italy. */
  public static final Locale ITALY = getLocale("it", "IT");

  /** Locale which represents Japan. */
  public static final Locale JAPAN = getLocale("ja", "JP");

  /** Locale which represents Korea. */
  public static final Locale KOREA = getLocale("ko", "KR");

  /**
   * Locale which represents China.
   * Same as SIMPLIFIED_CHINESE Locale.
   */
  public static final Locale CHINA = SIMPLIFIED_CHINESE;

  /**
   * Locale which represents the People's Republic of China.
   * Same as CHINA Locale.
   */
  public static final Locale PRC = CHINA;

  /**
   * Locale which represents Taiwan.
   * Same as TRADITIONAL_CHINESE Locale.
   */
  public static final Locale TAIWAN = TRADITIONAL_CHINESE;

  /** Locale which represents the United Kingdom. */
  public static final Locale UK = getLocale("en", "GB");

  /** Locale which represents the United States. */
  public static final Locale US = getLocale("en", "US");

  /** Locale which represents the English speaking portion of Canada. */
  public static final Locale CANADA = getLocale("en", "CA");

  /** Locale which represents the French speaking portion of Canada. */
  public static final Locale CANADA_FRENCH = getLocale("fr", "CA");

  /** The root locale, used as the base case in lookups by
   *  locale-sensitive operations.
   */
  public static final Locale ROOT = new Locale("","","");

  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 9149081749638150636L;

  /**
   * The language code, as returned by getLanguage().
   *
   * @serial the languange, possibly ""
   */
  private String language;

  /**
   * The country code, as returned by getCountry().
   *
   * @serial the country, possibly ""
   */
  private String country;

  /**
   * The variant code, as returned by getVariant().
   *
   * @serial the variant, possibly ""
   */
  private String variant;

  /**
   * This is the cached hashcode. When writing to stream, we write -1.
   *
   * @serial should be -1 in serial streams
   */
  private int hashcode;

  /**
   * Array storing all available locales.
   */
  private static transient Locale[] availableLocales;

  /**
   * Locale cache. Only created locale objects are stored.
   * Contains all supported locales when getAvailableLocales()
   * got called.
   */
  private static transient HashMap localeMap;
  
  /**
   * The default locale. Except for during bootstrapping, this should never be
   * null. Note the logic in the main constructor, to detect when
   * bootstrapping has completed.
   */
  private static Locale defaultLocale;

  static {
    String language = SystemProperties.getProperty("user.language", "en");
    String country  = SystemProperties.getProperty("user.country", "US");
    String region   = SystemProperties.getProperty("user.region", null);
    String variant  = SystemProperties.getProperty("user.variant", "");

    defaultLocale = getLocale(language,
                              (region != null) ? region : country,
                              variant);
  }

  /**
   * Array storing all the available two-letter ISO639 languages.
   */
  private static transient String[] languageCache;

  /**
   * Array storing all the available two-letter ISO3166 country codes.
   */
  private static transient String[] countryCache;

  /**
   * Retrieves the locale with the specified language from the cache.
   *
   * @param language the language of the locale to retrieve.
   * @return the locale.
   */ 
  private static Locale getLocale(String language)
  {
    return getLocale(language, "", "");
  }
  
  /**
   * Retrieves the locale with the specified language and country
   * from the cache.
   *
   * @param language the language of the locale to retrieve.
   * @param country the country of the locale to retrieve.
   * @return the locale.
   */ 
  private static Locale getLocale(String language, String country)
  {
    return getLocale(language, country, "");
  }
  
  /**
   * Retrieves the locale with the specified language, country
   * and variant from the cache.
   *
   * @param language the language of the locale to retrieve.
   * @param country the country of the locale to retrieve.
   * @param variant the variant of the locale to retrieve.
   * @return the locale.
   */ 
  private static Locale getLocale(String language, String country, String variant)
  {
    if (localeMap == null)
      localeMap = new HashMap(256);

    String name = language + "_" + country + "_" + variant;
    Locale locale = (Locale) localeMap.get(name);

    if (locale == null)
      {
	locale = new Locale(language, country, variant);
	localeMap.put(name, locale);
      }

    return locale;
  }
  
  /**
   * Convert new iso639 codes to the old ones.
   *
   * @param language the language to check
   * @return the appropriate code
   */
  private String convertLanguage(String language)
  {
    if (language.equals(""))
      return language;
    language = language.toLowerCase();
    int index = "he,id,yi".indexOf(language);
    if (index != -1)
      return "iw,in,ji".substring(index, index + 2);
    return language;
  }

  /**
   * Creates a new locale for the given language and country.
   *
   * @param language lowercase two-letter ISO-639 A2 language code
   * @param country uppercase two-letter ISO-3166 A2 contry code
   * @param variant vendor and browser specific
   * @throws NullPointerException if any argument is null
   */
  public Locale(String language, String country, String variant)
  {
    // During bootstrap, we already know the strings being passed in are
    // the correct capitalization, and not null. We can't call
    // String.toUpperCase during this time, since that depends on the
    // default locale.
    if (defaultLocale != null)
      {
        language = convertLanguage(language).intern();
        country = country.toUpperCase().intern();
        variant = variant.intern();
      }
    this.language = language;
    this.country = country;
    this.variant = variant;
    hashcode = language.hashCode() ^ country.hashCode() ^ variant.hashCode();
  }

  /**
   * Creates a new locale for the given language and country.
   *
   * @param language lowercase two-letter ISO-639 A2 language code
   * @param country uppercase two-letter ISO-3166 A2 country code
   * @throws NullPointerException if either argument is null
   */
  public Locale(String language, String country)
  {
    this(language, country, "");
  }

  /**
   * Creates a new locale for a language.
   *
   * @param language lowercase two-letter ISO-639 A2 language code
   * @throws NullPointerException if either argument is null
   * @since 1.4
   */
  public Locale(String language)
  {
    this(language, "", "");
  }

  /**
   * Returns the default Locale. The default locale is generally once set
   * on start up and then never changed. Normally you should use this locale
   * for everywhere you need a locale. The initial setting matches the
   * default locale, the user has chosen.
   *
   * @return the default locale for this virtual machine
   */
  public static Locale getDefault()
  {
    return defaultLocale;
  }

  /**
   * Changes the default locale. Normally only called on program start up.
   * Note that this doesn't change the locale for other programs. This has
   * a security check,
   * <code>PropertyPermission("user.language", "write")</code>, because of
   * its potential impact to running code.
   *
   * @param newLocale the new default locale
   * @throws NullPointerException if newLocale is null
   * @throws SecurityException if permission is denied
   */
  public static void setDefault(Locale newLocale)
  {
    if (newLocale == null)
      throw new NullPointerException();
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new PropertyPermission("user.language", "write"));
    defaultLocale = newLocale;
  }

  /**
   * Returns the list of available locales.
   *
   * @return the installed locales
   */
  public static synchronized Locale[] getAvailableLocales()
  {
    if (availableLocales == null)
      {
        int len = LocaleHelper.getLocaleCount();
        availableLocales = new Locale[len];

        for (int i = 0; i < len; i++)
          {
            String language;
            String country = "";
            String variant = "";
            String name = LocaleHelper.getLocaleName(i);

            language = name.substring(0, 2);

            if (name.length() > 2)
              country = name.substring(3);

	    int index = country.indexOf("_");
	    if (index > 0)
	      {
		variant = country.substring(index + 1);
		country = country.substring(0, index - 1);
	      }

            availableLocales[i] = getLocale(language, country, variant);
          }
      }
    
    return (Locale[]) availableLocales.clone();
  }

  /**
   * Returns a list of all 2-letter uppercase country codes as defined
   * in ISO 3166.
   *
   * @return a list of acceptable country codes
   */
  public static String[] getISOCountries()
  {
    if (countryCache == null)
      {
	countryCache = getISOStrings("territories");
      }

    return (String[]) countryCache.clone();
  }

  /**
   * Returns a list of all 2-letter lowercase language codes as defined
   * in ISO 639 (both old and new variant).
   *
   * @return a list of acceptable language codes
   */
  public static String[] getISOLanguages()
  {
    if (languageCache == null)
      {
	languageCache = getISOStrings("languages");
      }
    return (String[]) languageCache.clone();
  }

  /**
   * Returns the set of keys from the specified resource hashtable, filtered
   * so that only two letter strings are returned.
   *
   * @param tableName the name of the table from which to retrieve the keys.
   * @return an array of two-letter strings.
   */
  private static String[] getISOStrings(String tableName)
  {
    int count = 0;
    ResourceBundle bundle =
      ResourceBundle.getBundle("gnu.java.locale.LocaleInformation");
    Enumeration e = bundle.getKeys();
    ArrayList tempList = new ArrayList();

    while (e.hasMoreElements())
      {
	String key = (String) e.nextElement();
	
	if (key.startsWith(tableName + "."))
	  {
	    String str = key.substring(tableName.length() + 1);

	    if (str.length() == 2
		&& Character.isLetter(str.charAt(0))
		&& Character.isLetter(str.charAt(1)))
	      {
		tempList.add(str);
		++count;
	      }
	  }
      }

    String[] strings = new String[count];
    
    for (int a = 0; a < count; ++a)
      strings[a] = (String) tempList.get(a);
    
    return strings;
  }

  /**
   * Returns the language code of this locale. Some language codes have changed
   * as ISO 639 has evolved; this returns the old name, even if you built
   * the locale with the new one.
   *
   * @return language code portion of this locale, or an empty String
   */
  public String getLanguage()
  {
    return language;
  }

  /**
   * Returns the country code of this locale.
   *
   * @return country code portion of this locale, or an empty String
   */
  public String getCountry()
  {
    return country;
  }

  /**
   * Returns the variant code of this locale.
   *
   * @return the variant code portion of this locale, or an empty String
   */
  public String getVariant()
  {
    return variant;
  }

  /**
   * Gets the string representation of the current locale. This consists of
   * the language, the country, and the variant, separated by an underscore.
   * The variant is listed only if there is a language or country. Examples:
   * "en", "de_DE", "_GB", "en_US_WIN", "de__POSIX", "fr__MAC".
   *
   * @return the string representation of this Locale
   * @see #getDisplayName()
   */
  public String toString()
  {
    if (language.length() == 0 && country.length() == 0)
      return "";
    else if (country.length() == 0 && variant.length() == 0)
      return language;
    StringBuffer result = new StringBuffer(language);
    result.append('_').append(country);
    if (variant.length() != 0)
      result.append('_').append(variant);
    return result.toString();
  }

  /**
   * Returns the three-letter ISO language abbrevation of this locale.
   *
   * @throws MissingResourceException if the three-letter code is not known
   */
  public String getISO3Language()
  {
    // We know all strings are interned so we can use '==' for better performance.
    if (language == "")
      return "";
    int index
      = ("aa,ab,af,am,ar,as,ay,az,ba,be,bg,bh,bi,bn,bo,br,ca,co,cs,cy,da,"
         + "de,dz,el,en,eo,es,et,eu,fa,fi,fj,fo,fr,fy,ga,gd,gl,gn,gu,ha,iw,"
         + "hi,hr,hu,hy,ia,in,ie,ik,in,is,it,iu,iw,ja,ji,jw,ka,kk,kl,km,kn,"
         + "ko,ks,ku,ky,la,ln,lo,lt,lv,mg,mi,mk,ml,mn,mo,mr,ms,mt,my,na,ne,"
         + "nl,no,oc,om,or,pa,pl,ps,pt,qu,rm,rn,ro,ru,rw,sa,sd,sg,sh,si,sk,"
         + "sl,sm,sn,so,sq,sr,ss,st,su,sv,sw,ta,te,tg,th,ti,tk,tl,tn,to,tr,"
         + "ts,tt,tw,ug,uk,ur,uz,vi,vo,wo,xh,ji,yo,za,zh,zu")
      .indexOf(language);

    if (index % 3 != 0 || language.length() != 2)
      throw new MissingResourceException
        ("Can't find ISO3 language for " + language,
         "java.util.Locale", language);

    // Don't read this aloud. These are the three letter language codes.
    return
      ("aarabkaframharaasmaymazebakbelbulbihbisbenbodbrecatcoscescymdandeu"
       + "dzoellengepospaesteusfasfinfijfaofrafrygaigdhglggrngujhauhebhinhrv"
       + "hunhyeinaindileipkindislitaikuhebjpnyidjawkatkazkalkhmkankorkaskur"
       + "kirlatlinlaolitlavmlgmrimkdmalmonmolmarmsamltmyanaunepnldnorociorm"
       + "oripanpolpusporquerohrunronruskinsansndsagsrpsinslkslvsmosnasomsqi"
       + "srpsswsotsunsweswatamteltgkthatirtuktgltsntonturtsotattwiuigukrurd"
       + "uzbvievolwolxhoyidyorzhazhozul")
      .substring(index, index + 3);
  }

  /**
   * Returns the three-letter ISO country abbrevation of the locale.
   *
   * @throws MissingResourceException if the three-letter code is not known
   */
  public String getISO3Country()
  {
    // We know all strings are interned so we can use '==' for better performance.
    if (country == "")
      return "";
    int index
      = ("AD,AE,AF,AG,AI,AL,AM,AN,AO,AQ,AR,AS,AT,AU,AW,AZ,BA,BB,BD,BE,BF,"
         + "BG,BH,BI,BJ,BM,BN,BO,BR,BS,BT,BV,BW,BY,BZ,CA,CC,CF,CG,CH,CI,CK,"
         + "CL,CM,CN,CO,CR,CU,CV,CX,CY,CZ,DE,DJ,DK,DM,DO,DZ,EC,EE,EG,EH,ER,"
         + "ES,ET,FI,FJ,FK,FM,FO,FR,FX,GA,GB,GD,GE,GF,GH,GI,GL,GM,GN,GP,GQ,"
         + "GR,GS,GT,GU,GW,GY,HK,HM,HN,HR,HT,HU,ID,IE,IL,IN,IO,IQ,IR,IS,IT,"
         + "JM,JO,JP,KE,KG,KH,KI,KM,KN,KP,KR,KW,KY,KZ,LA,LB,LC,LI,LK,LR,LS,"
         + "LT,LU,LV,LY,MA,MC,MD,MG,MH,MK,ML,MM,MN,MO,MP,MQ,MR,MS,MT,MU,MV,"
         + "MW,MX,MY,MZ,NA,NC,NE,NF,NG,NI,NL,NO,NP,NR,NU,NZ,OM,PA,PE,PF,PG,"
         + "PH,PK,PL,PM,PN,PR,PT,PW,PY,QA,RE,RO,RU,RW,SA,SB,SC,SD,SE,SG,SH,"
         + "SI,SJ,SK,SL,SM,SN,SO,SR,ST,SV,SY,SZ,TC,TD,TF,TG,TH,TJ,TK,TM,TN,"
         + "TO,TP,TR,TT,TV,TW,TZ,UA,UG,UM,US,UY,UZ,VA,VC,VE,VG,VI,VN,VU,WF,"
         + "WS,YE,YT,YU,ZA,ZM,ZR,ZW")
      .indexOf(country);

    if (index % 3 != 0 || country.length() != 2)
      throw new MissingResourceException
        ("Can't find ISO3 country for " + country,
         "java.util.Locale", country);

    // Don't read this aloud. These are the three letter country codes.
    return
      ("ANDAREAFGATGAIAALBARMANTAGOATAARGASMAUTAUSABWAZEBIHBRBBGDBELBFABGR"
       + "BHRBDIBENBMUBRNBOLBRABHSBTNBVTBWABLRBLZCANCCKCAFCOGCHECIVCOKCHLCMR"
       + "CHNCOLCRICUBCPVCXRCYPCZEDEUDJIDNKDMADOMDZAECUESTEGYESHERIESPETHFIN"
       + "FJIFLKFSMFROFRAFXXGABGBRGRDGEOGUFGHAGIBGRLGMBGINGLPGNQGRCSGSGTMGUM"
       + "GNBGUYHKGHMDHNDHRVHTIHUNIDNIRLISRINDIOTIRQIRNISLITAJAMJORJPNKENKGZ"
       + "KHMKIRCOMKNAPRKKORKWTCYMKAZLAOLBNLCALIELKALBRLSOLTULUXLVALBYMARMCO"
       + "MDAMDGMHLMKDMLIMMRMNGMACMNPMTQMRTMSRMLTMUSMDVMWIMEXMYSMOZNAMNCLNER"
       + "NFKNGANICNLDNORNPLNRUNIUNZLOMNPANPERPYFPNGPHLPAKPOLSPMPCNPRIPRTPLW"
       + "PRYQATREUROMRUSRWASAUSLBSYCSDNSWESGPSHNSVNSJMSVKSLESMRSENSOMSURSTP"
       + "SLVSYRSWZTCATCDATFTGOTHATJKTKLTKMTUNTONTMPTURTTOTUVTWNTZAUKRUGAUMI"
       + "USAURYUZBVATVCTVENVGBVIRVNMVUTWLFWSMYEMMYTYUGZAFZMBZARZWE")
      .substring(index, index + 3);
  }

  /**
   * Gets the country name suitable for display to the user, formatted
   * for the default locale.  This has the same effect as
   * <pre>
   * getDisplayLanguage(Locale.getDefault());
   * </pre>
   *
   * @return the language name of this locale localized to the default locale,
   *         with the ISO code as backup
   */
  public String getDisplayLanguage()
  {
    return getDisplayLanguage(defaultLocale);
  }

  /**
   * <p>
   * Gets the name of the language specified by this locale, in a form suitable
   * for display to the user.  If possible, the display name will be localized
   * to the specified locale.  For example, if the locale instance is
   * <code>Locale.GERMANY</code>, and the specified locale is <code>Locale.UK</code>,
   * the result would be 'German'.  Using the German locale would instead give
   * 'Deutsch'.  If the display name can not be localized to the supplied
   * locale, it will fall back on other output in the following order:
   * </p>
   * <ul>
   * <li>the display name in the default locale</li>
   * <li>the display name in English</li>
   * <li>the ISO code</li>
   * </ul>
   * <p>
   * If the language is unspecified by this locale, then the empty string is
   * returned.
   * </p>
   *
   * @param inLocale the locale to use for formatting the display string.
   * @return the language name of this locale localized to the given locale,
   *         with the default locale, English and the ISO code as backups.
   * @throws NullPointerException if the supplied locale is null.
   */
  public String getDisplayLanguage(Locale inLocale)
  {
    if (language.isEmpty())
      return "";
    try
      {
	ResourceBundle res =
          ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
                                   inLocale,
                                   ClassLoader.getSystemClassLoader());

        return res.getString("languages." + language);
      }
    catch (MissingResourceException e)
      {
	/* This means runtime support for the locale
	 * is not available, so we check providers. */
      }
    for (LocaleNameProvider p :
	   ServiceLoader.load(LocaleNameProvider.class))
      {
	for (Locale loc : p.getAvailableLocales())
	  {
	    if (loc.equals(inLocale))
	      {
		String locLang = p.getDisplayLanguage(language,
						      inLocale);
		if (locLang != null)
		  return locLang;
		break;
	      }
	  }
      }
    if (inLocale.equals(Locale.ROOT)) // Base case
      return language;
    return getDisplayLanguage(LocaleHelper.getFallbackLocale(inLocale));
  }

  /**
   * Returns the country name of this locale localized to the
   * default locale. If the localized is not found, the ISO code
   * is returned. This has the same effect as
   * <pre>
   * getDisplayCountry(Locale.getDefault());
   * </pre>
   *
   * @return the country name of this locale localized to the given locale,
   *         with the ISO code as backup
   */
  public String getDisplayCountry()
  {
    return getDisplayCountry(defaultLocale);
  }

  /**
   * <p>
   * Gets the name of the country specified by this locale, in a form suitable
   * for display to the user.  If possible, the display name will be localized
   * to the specified locale.  For example, if the locale instance is
   * <code>Locale.GERMANY</code>, and the specified locale is <code>Locale.UK</code>,
   * the result would be 'Germany'.  Using the German locale would instead give
   * 'Deutschland'.  If the display name can not be localized to the supplied
   * locale, it will fall back on other output in the following order:
   * </p>
   * <ul>
   * <li>the display name in the default locale</li>
   * <li>the display name in English</li>
   * <li>the ISO code</li>
   * </ul>
   * <p>
   * If the country is unspecified by this locale, then the empty string is
   * returned.
   * </p>
   *
   * @param inLocale the locale to use for formatting the display string.
   * @return the country name of this locale localized to the given locale,
   *         with the default locale, English and the ISO code as backups.
   * @throws NullPointerException if the supplied locale is null.
   */
  public String getDisplayCountry(Locale inLocale)
  {
    if (country.isEmpty())
      return "";
    try
      {
        ResourceBundle res =
          ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
                                   inLocale,
                                   ClassLoader.getSystemClassLoader());
    
        return res.getString("territories." + country);
      }
    catch (MissingResourceException e)
      {
	/* This means runtime support for the locale
	 * is not available, so we check providers. */
      }
    for (LocaleNameProvider p :
	   ServiceLoader.load(LocaleNameProvider.class))
      {
	for (Locale loc : p.getAvailableLocales())
	  {
	    if (loc.equals(inLocale))
	      {
		String locCountry = p.getDisplayCountry(country,
							inLocale);
		if (locCountry != null)
		  return locCountry;
		break;
	      }
	  }
      }
    if (inLocale.equals(Locale.ROOT)) // Base case
      return country;
    return getDisplayCountry(LocaleHelper.getFallbackLocale(inLocale));
  }

  /**
   * Returns the variant name of this locale localized to the
   * default locale. If the localized is not found, the variant code
   * itself is returned. This has the same effect as
   * <pre>
   * getDisplayVariant(Locale.getDefault());
   * </pre>
   *
   * @return the variant code of this locale localized to the given locale,
   *         with the ISO code as backup
   */
  public String getDisplayVariant()
  {
    return getDisplayVariant(defaultLocale);
  }


  /**
   * <p>
   * Gets the name of the variant specified by this locale, in a form suitable
   * for display to the user.  If possible, the display name will be localized
   * to the specified locale.  For example, if the locale instance is a revised
   * variant, and the specified locale is <code>Locale.UK</code>, the result
   * would be 'REVISED'.  Using the German locale would instead give
   * 'Revidiert'.  If the display name can not be localized to the supplied
   * locale, it will fall back on other output in the following order:
   * </p>
   * <ul>
   * <li>the display name in the default locale</li>
   * <li>the display name in English</li>
   * <li>the ISO code</li>
   * </ul>
   * <p>
   * If the variant is unspecified by this locale, then the empty string is
   * returned.
   * </p>
   *
   * @param inLocale the locale to use for formatting the display string.
   * @return the variant name of this locale localized to the given locale,
   *         with the default locale, English and the ISO code as backups.
   * @throws NullPointerException if the supplied locale is null.
   */
  public String getDisplayVariant(Locale inLocale)
  {
    if (variant.isEmpty())
      return "";
    try
      {
        ResourceBundle res =
          ResourceBundle.getBundle("gnu.java.locale.LocaleInformation",
                                   inLocale,
                                   ClassLoader.getSystemClassLoader());
    
        return res.getString("variants." + variant);
      }
    catch (MissingResourceException e)
      {
	/* This means runtime support for the locale
	 * is not available, so we check providers. */
      }
    for (LocaleNameProvider p :
	   ServiceLoader.load(LocaleNameProvider.class))
      {
	for (Locale loc : p.getAvailableLocales())
	  {
	    if (loc.equals(inLocale))
	      {
		String locVar = p.getDisplayVariant(variant,
						    inLocale);
		if (locVar != null)
		  return locVar;
		break;
	      }
	  }
      }
    if (inLocale.equals(Locale.ROOT)) // Base case
      return country;
    return getDisplayVariant(LocaleHelper.getFallbackLocale(inLocale));
  }

  /**
   * Gets all local components suitable for display to the user, formatted
   * for the default locale. For the language component, getDisplayLanguage
   * is called. For the country component, getDisplayCountry is called.
   * For the variant set component, getDisplayVariant is called.
   *
   * <p>The returned String will be one of the following forms:<br>
   * <pre>
   * language (country, variant)
   * language (country)
   * language (variant)
   * country (variant)
   * language
   * country
   * variant
   * </pre>
   *
   * @return String version of this locale, suitable for display to the user
   */
  public String getDisplayName()
  {
    return getDisplayName(defaultLocale);
  }

  /**
   * Gets all local components suitable for display to the user, formatted
   * for a specified locale. For the language component,
   * getDisplayLanguage(Locale) is called. For the country component,
   * getDisplayCountry(Locale) is called. For the variant set component,
   * getDisplayVariant(Locale) is called.
   *
   * <p>The returned String will be one of the following forms:<br>
   * <pre>
   * language (country, variant)
   * language (country)
   * language (variant)
   * country (variant)
   * language
   * country
   * variant
   * </pre>
   *
   * @param locale locale to use for formatting
   * @return String version of this locale, suitable for display to the user
   */
  public String getDisplayName(Locale locale)
  {
    StringBuffer result = new StringBuffer();
    int count = 0;
    String[] delimiters = {"", " (", ","};
    if (language.length() != 0)
      {
        result.append(delimiters[count++]);
        result.append(getDisplayLanguage(locale));
      }
    if (country.length() != 0)
      {
        result.append(delimiters[count++]);
        result.append(getDisplayCountry(locale));
      }
    if (variant.length() != 0)
      {
        result.append(delimiters[count++]);
        result.append(getDisplayVariant(locale));
      }
    if (count > 1)
      result.append(")");
    return result.toString();
  }

  /**
   * Does the same as <code>Object.clone()</code> but does not throw
   * a <code>CloneNotSupportedException</code>. Why anyone would
   * use this method is a secret to me, since this class is immutable.
   *
   * @return the clone
   */
  public Object clone()
  {
    // This class is final, so no need to use native super.clone().
    return new Locale(language, country, variant);
  }

  /**
   * Return the hash code for this locale. The hashcode is the logical
   * xor of the hash codes of the language, the country and the variant.
   * The hash code is precomputed, since <code>Locale</code>s are often
   * used in hash tables.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return hashcode;
  }

  /**
   * Compares two locales. To be equal, obj must be a Locale with the same
   * language, country, and variant code.
   *
   * @param obj the other locale
   * @return true if obj is equal to this
   */
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (! (obj instanceof Locale))
      return false;
    Locale l = (Locale) obj;

    return (language == l.language 
            && country == l.country 
            && variant == l.variant);
  }

  /**
   * Write the locale to an object stream.
   *
   * @param s the stream to write to
   * @throws IOException if the write fails
   * @serialData The first three fields are Strings representing language,
   *             country, and variant. The fourth field is a placeholder for 
   *             the cached hashcode, but this is always written as -1, and 
   *             recomputed when reading it back.
   */
  private void writeObject(ObjectOutputStream s)
    throws IOException
  {
    ObjectOutputStream.PutField fields = s.putFields();
    fields.put("hashcode", -1);
    s.defaultWriteObject();
  }

  /**
   * Reads a locale from the input stream.
   *
   * @param s the stream to read from
   * @throws IOException if reading fails
   * @throws ClassNotFoundException if reading fails
   * @serialData the hashCode is always invalid and must be recomputed
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    language = language.intern();
    country = country.intern();
    variant = variant.intern();
    hashcode = language.hashCode() ^ country.hashCode() ^ variant.hashCode();
  }
} // class Locale
