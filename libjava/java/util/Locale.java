/* java.util.Locale
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.
 
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.
 
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

/**
 * Locales represent a specific country and culture.
 * <br><br>
 * Classes which can be passed a Locale object tailor their information 
 * for a given locale.  For instance, currency number formatting is 
 * handled differently for the USA and France.
 * <br><br>
 * Locales are made up of a language code, a country code, and an optional
 * set of variant strings.
 * <br><br>
 * Language codes are represented by
 * <a href="http://www.indigo.ie/egt/standards/iso639/iso639-1-en.html">ISO 639:1988</a>
 * w/ additions from ISO 639/RA Newsletter No. 1/1989
 * and a decision of the Advisory Committee of ISO/TC39 on
 * August 8, 1997.
 * <br><br>
 * Country codes are represented by 
 * <a href="ftp://ftp.ripe.net/iso3166-countrycodes">ISO 3166</a>.
 * <br><br>
 * Variant strings are vendor and browser specific.  Standard variant
 * strings include "POSIX" for POSIX, "WIN" for MS-Windows, and "MAC" for
 * Macintosh.  When there is more than one variant string, they must
 * be separated by an underscore (U+005F).
 * <br><br>
 * The default locale is determined by the values of the system properties
 * user.language, user.region, and user.variant.
 * @see ResourceBundle
 * @see java.text.Format
 * @see java.text.NumberFormat
 * @see java.text.Collator
 * @author Jochen Hoenicke
 * @author Paul Fisher
 */
public final class Locale implements java.io.Serializable, Cloneable
{
  /**
   * Locale which represents the English language.
   */
  public static final Locale ENGLISH = new Locale("en", "");
  /**
   * Locale which represents the English language.
   */
  public static final Locale FRENCH = new Locale("fr", "");
  /**
   * Locale which represents the German language.
   */
  public static final Locale GERMAN = new Locale("de", "");
  /**
   * Locale which represents the Italian language.
   */
  public static final Locale ITALIAN = new Locale("it", "");
  /**
   * Locale which represents the Japanese language.
   */
  public static final Locale JAPANESE = new Locale("ja", "");
  /**
   * Locale which represents the Korean language.
   */
  public static final Locale KOREAN = new Locale("ko", "");
  /**
   * Locale which represents the Chinese language.
   */
  public static final Locale CHINESE = new Locale("zh", "");
  /**
   * Locale which represents the Chinese language as used in China.
   */
  public static final Locale SIMPLIFIED_CHINESE = new Locale("zh", "CN");
  /**
   * Locale which represents the Chinese language as used in Taiwan.
   * Same as TAIWAN Locale.
   */
  public static final Locale TRADITIONAL_CHINESE = new Locale("zh", "TW");
  /**
   * Locale which represents France.
   */
  public static final Locale FRANCE = new Locale("fr", "FR");
  /**
   * Locale which represents Germany.
   */
  public static final Locale GERMANY = new Locale("de", "DE");
  /**
   * Locale which represents Italy.
   */
  public static final Locale ITALY = new Locale("it", "IT");
  /**
   * Locale which represents Japan.
   */
  public static final Locale JAPAN = new Locale("ja", "JP");
  /**
   * Locale which represents Korea.
   */
  public static final Locale KOREA = new Locale("ko", "KR");
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
  /**
   * Locale which represents the United Kingdom.
   */
  public static final Locale UK = new Locale("en", "GB");
  /**
   * Locale which represents the United States.
   */
  public static final Locale US = new Locale("en", "US");
  /**
   * Locale which represents the English speaking portion of Canada.
   */
  public static final Locale CANADA = new Locale("en", "CA");
  /**
   * Locale which represents the French speaking portion of Canada.
   */
  public static final Locale CANADA_FRENCH = new Locale("fr", "CA");

  /**
   * We are compatible to sun's Locale.
   */
  static final long serialVersionUID = 9149081749638150636L;

  /**
   * The language code, as returned by getLanguage().
   * @serial
   */
  private String language;
  /**
   * The country code, as returned by getCountry().
   * @serial
   */
  private String country;
  /**
   * The variant code, as returned by getVariant().
   * @serial
   */
  private String variant;
  /**
   * This is the cached hashcode.  When writing to stream, we write -1.
   * @serial
   */
  private int hashcode;

  /**
   * Convert old iso639 codes to the new ones.
   */
  private String convertLanguage(String language)
  {
    if (language.equals(""))
      return language;

    language = language.toLowerCase();
    int index = "iw,in,ji".indexOf(language);
    if (index != -1)
      return "he,id,yi".substring(index, index + 2);
    return language;
  }

  /**
   * Creates a new locale for the given language and country.
   * @param language lowercase two-letter ISO-639 A2 language code.
   * @param country uppercase two-letter ISO-3166 A2 contry code.
   * @param variant vendor and browser specific.
   */
  public Locale(String language, String country, String variant)
  {
    this.language = convertLanguage(language);
    this.country = country.toUpperCase();
    this.variant = variant.toUpperCase();
    this.hashcode = (this.language.hashCode() ^ this.country.hashCode()
		     ^ this.variant.hashCode());
  }

  /**
   * Creates a new locale for the given language and country.
   * @param language lowercase two-letter ISO-639 A2 language code.
   * @param country uppercase two-letter ISO-3166 A2 country code.
   */
  public Locale(String language, String country)
  {
    this(language, country, "");
  }

  private static Locale defaultLocale =
    new Locale(System.getProperty("user.language", ""),
	       System.getProperty("user.region", ""),
	       System.getProperty("user.variant", ""));

  /**
   * Returns the default Locale.  The default locale is generally
   * once set on start up and then never changed.  Normally you 
   * should use this locale for everywhere you need a locale.
   * The initial setting matches the default locale, the user has
   * chosen.
   */
  public static Locale getDefault()
  {
    return defaultLocale;
  }

  /**
   * Changes the default locale.  Normally only called on program
   * start up.  Note that this doesn't change the locale for other
   * programs.  
   */
  public static void setDefault(Locale newLocale)
  {
    defaultLocale = newLocale;
  }

  /**
   * Returns the list of available locales.
   */
  public static Locale[] getAvailableLocales()
  {
    /* I only return those for which localized language
     * or country information exists.
     * XXX - remove hard coded list.
     */
    return new Locale[]
    {
      ENGLISH, FRENCH, GERMAN, new Locale("ga", "")
    };
  }

  /**
   * Returns a list of all 2-letter uppercase country codes as defined
   * in ISO 3166
   */
  public static String[] getISOCountries()
  {
    return new String[]
    {
       "AF", "AL", "DZ", "AS", "AD", "AO", "AI", "AQ", "AG",
       "AR", "AM", "AW", "AU", "AT", "AZ", "BS", "BH", "BD",
       "BB", "BY", "BE", "BZ", "BJ", "BM", "BT", "BO", "BA",
       "BW", "BV", "BR", "IO", "BN", "BG", "BF", "BI", "KH",
       "CM", "CA", "CV", "KY", "CF", "TD", "CL", "CN", "CX",
       "CC", "CO", "KM", "CG", "CK", "CR", "CI", "HR", "CU",
       "CY", "CZ", "DK", "DJ", "DM", "DO", "TP", "EC", "EG",
       "SV", "GQ", "ER", "EE", "ET", "FK", "FO", "FJ", "FI",
       "FR", "FX", "GF", "PF", "TF", "GA", "GM", "GE", "DE",
       "GH", "GI", "GR", "GL", "GD", "GP", "GU", "GT", "GN",
       "GW", "GY", "HT", "HM", "HN", "HK", "HU", "IS", "IN",
       "ID", "IR", "IQ", "IE", "IL", "IT", "JM", "JP", "JO",
       "KZ", "KE", "KI", "KP", "KR", "KW", "KG", "LA", "LV",
       "LB", "LS", "LR", "LY", "LI", "LT", "LU", "MO", "MK",
       "MG", "MW", "MY", "MV", "ML", "MT", "MH", "MQ", "MR",
       "MU", "YT", "MX", "FM", "MD", "MC", "MN", "MS", "MA",
       "MZ", "MM", "NA", "NR", "NP", "NL", "AN", "NC", "NZ",
       "NI", "NE", "NG", "NU", "NF", "MP", "NO", "OM", "PK",
       "PW", "PA", "PG", "PY", "PE", "PH", "PN", "PL", "PT",
       "PR", "QA", "RE", "RO", "RU", "RW", "KN", "LC", "VC",
       "WS", "SM", "ST", "SA", "SN", "SC", "SL", "SG", "SK",
       "SI", "SB", "SO", "ZA", "GS", "ES", "LK", "SH", "PM",
       "SD", "SR", "SJ", "SZ", "SE", "CH", "SY", "TW", "TJ",
       "TZ", "TH", "TG", "TK", "TO", "TT", "TN", "TR", "TM",
       "TC", "TV", "UG", "UA", "AE", "GB", "US", "UM", "UY",
       "UZ", "VU", "VA", "VE", "VN", "VG", "VI", "WF", "EH",
       "YE", "YU", "ZR", "ZM", "ZW"};
  }

  /**
   * Returns a list of all 2-letter lowercase language codes as defined
   * in ISO 639 (both old and new variant).
   */
  public static String[] getISOLanguages()
  {
    return new String[]
    {
	"aa", "ab", "af", "am", "ar", "as", "ay", "az", "ba",
	"be", "bg", "bh", "bi", "bn", "bo", "br", "ca", "co",
	"cs", "cy", "da", "de", "dz", "el", "en", "eo", "es",
	"et", "eu", "fa", "fi", "fj", "fo", "fr", "fy", "ga",
	"gd", "gl", "gn", "gu", "ha", "iw", "he", "hi", "hr",
	"hu", "hy", "ia", "in", "id", "ie", "ik", "is", "it",
	"iu", "ja", "jw", "ka", "kk", "kl", "km", "kn", "ko",
	"ks", "ku", "ky", "la", "ln", "lo", "lt", "lv", "mg",
	"mi", "mk", "ml", "mn", "mo", "mr", "ms", "mt", "my",
	"na", "ne", "nl", "no", "oc", "om", "or", "pa", "pl",
	"ps", "pt", "qu", "rm", "rn", "ro", "ru", "rw", "sa",
	"sd", "sg", "sh", "si", "sk", "sl", "sm", "sn", "so",
	"sq", "sr", "ss", "st", "su", "sv", "sw", "ta", "te",
	"tg", "th", "ti", "tk", "tl", "tn", "to", "tr", "ts",
	"tt", "tw", "ug", "uk", "ur", "uz", "vi", "vo", "wo",
	"xh", "ji", "yi", "yo", "za", "zh", "zu"};
  }

  /**
   * Returns the language code of this locale.
   * @return language code portion of this locale, or an empty String if
   * none exists
   */
  public String getLanguage()
  {
    return language;
  }

  /**
   * Returns the country code of this locale.
   * @return country code portion of this locale, or an empty String if
   * none exists
   */
  public String getCountry()
  {
    return country;
  }

  /**
   * Returns the variant code of this locale.
   */
  public String getVariant()
  {
    return variant;
  }

  /**
   * Gets the string representation of the current locale.  This
   * consists of the language, the country, and the variant,
   * separated by an underscore.  If one of this three component is
   * missing the underscore will also disappear.  
   * @return the string representation of this Locale.
   */
  public final String toString()
  {
    StringBuffer result = new StringBuffer(language);
    String underscore = "";
    if (language.length() != 0)
      underscore = "_";
    if (country.length() != 0)
      {
	result.append(underscore);
	result.append(country);
	underscore = "_";
      }
    if (variant.length() != 0)
      {
	result.append(underscore);
	result.append(variant);
      }
    return result.toString();
  }

  /**
   * Returns the three-letter ISO language abbrevation of this locale.
   * @exception MissingResourceException if the three-letter code is not
   * known.  
   */
  public String getISO3Language() throws MissingResourceException
  {
    int index =
      ("aa,ab,af,am,ar,as,ay,az,ba,be,bg,bh,bi,bn,bo,br,ca,co,cs,cy," +
       "da,de,dz,el,en,eo,es,et,eu,fa,fi,fj,fo,fr,fy,ga,gd,gl,gn,gu," +
       "gv,ha,hi,hr,hu,hy,ia,ie,ik,id,is,it,iu,he,ja,yi,jw,ka,kk,kl," +
       "km,kn,ko,ks,ku,kw,ky,la,lb,ln,lo,lt,lv,mg,mi,mk,ml,mn,mo,mr," +
       "ms,mt,my,na,ne,nl,no,oc,om,or,pa,pl,ps,pt,qu,rm,rn,ro,ru,rw," +
       "sa,sd,se,sg,sh,si,sk,sl,sm,sn,so,sq,sr,ss,st,su,sv,sw,ta,te," +
       "tg,th,ti,tk,tl,tn,to,tr,ts,tt,tw,ug,uk,ur,uz,vi,vo,wo,xh,yo," +
       "za,zh,zu,").indexOf(language + ",");
       
    if (index == -1 || language.length() != 2)
      throw new MissingResourceException
	("Can't find ISO3 language for " + language,
	 "java.util.Locale", language);

    /* Don't read this aloud.  This are the three letter language codes
     */
    return
      ("aarabkaframharaasmaymazebakbelbulbihbisbenbodbrecatcoscescym" +
       "dandeudzoellengepospaesteusfasfinfijfaofrafrygaigdhglggrnguj" +
       "maxhauhinhrvhunhyeinaileipkindislitaikuhebjpnyidjawkatkazkal" +
       "khmkankorkaskurcorkirlatltzlinlaolitlavmlgmrimkdmalmonmolmar" +
       "msamltmyanaunepnldnorociormoripanpolpusporquerohrunronruskin" +
       "sansndsmisagsrpsinslkslvsmosnasomsqisrpsswsotsunsweswatamtel" +
       "tgkthatirtuktgltsntonturtsotattwiuigukrurduzbvievolwolxhoyor" +
       "zhazhozul").substring(index, index + 3);
  }

  /**
   * Returns the three-letter ISO country abbrevation of the locale.
   * @exception MissingResourceException if the three-letter code is not
   * known.
   */
  public String getISO3Country() throws MissingResourceException
  {
    int index =
      ("AF,AL,DZ,AS,AD,AO,AI,AQ,AG,AR,AM,AW,AU,AT,AZ,BS,BH,BD,BB,BY,BE," +
       "BZ,BJ,BM,BT,BO,BA,BW,BV,BR,IO,BN,BG,BF,BI,KH,CM,CA,CV,KY,CF,TD," +
       "CL,CN,CX,CC,CO,KM,CG,CD,CK,CR,CI,HR,CU,CY,CZ,DK,DJ,DM,DO,TP,EC," +
       "EG,SV,GQ,ER,EE,ET,FK,FO,FJ,FI,FR,FX,GF,PF,TF,GA,GM,GE,DE,GH,GI," +
       "GR,GL,GD,GP,GU,GT,GN,GW,GY,HT,HM,VA,HN,HK,HU,IS,IN,ID,IR,IQ,IE," +
       "IL,IT,JM,JP,JO,KZ,KE,KI,KP,KR,KW,KG,LA,LV,LB,LS,LR,LY,LI,LT,LU," +
       "MO,MK,MG,MW,MY,MV,ML,MT,MH,MQ,MR,MU,YT,MX,FM,MD,MC,MN,MS,MA,MZ," +
       "MM,NA,NR,NP,NL,AN,NC,NZ,NI,NE,NG,NU,NF,MP,NO,OM,PK,PW,PA,PG,PY," +
       "PE,PH,PN,PL,PT,PR,QA,RE,RO,RU,RW,KN,LC,VC,WS,SM,ST,SA,SN,SC,SL," +
       "SG,SK,SI,SB,SO,ZA,GS,ES,LK,SH,PM,SD,SR,SJ,SZ,SE,CH,SY,TW,TJ,TZ," +
       "TH,TG,TK,TO,TT,TN,TR,TM,TC,TV,UG,UA,AE,GB,US,UM,UY,UZ,VU,VE,VN," +
       "VG,VI,WF,EH,YE,YU,ZM,ZW,").indexOf(country + ",");
       
    if (index == -1 || language.length() != 2)
      throw new MissingResourceException
	("Can't find ISO3 country for " + country,
	 "java.util.Locale", country);

    /* Don't read this aloud.  This are the three letter country codes
     */
    return
      ("AFGALBDZAASMANDAGOAIAATAATGARGARMABWAUSAUTAZEBHSBHRBGDBRBBLRBEL" +
       "BLZBENBMUBTNBOLBIHBWABVTBRAIOTBRNBGRBFABDIKHMCMRCANCPVCYMCAFTCD" +
       "CHLCHNCXRCCKCOLCOMCOGCODCOKCRICIVHRVCUBCYPCZEDNKDJIDMADOMTMPECU" +
       "EGYSLVGNQERIESTETHFLKFROFJIFINFRAFXXGUFPYFATFGABGMBGEODEUGHAGIB" +
       "GRCGRLGRDGLPGUMGTMGINGNBGUYHTIHMDVATHNDHKGHUNISLINDIDNIRNIRQIRL" +
       "ISRITAJAMJPNJORKAZKENKIRPRKKORKWTKGZLAOLVALBNLSOLBRLBYLIELTULUX" +
       "MACMKDMDGMWIMYSMDVMLIMLTMHLMTQMRTMUSMYTMEXFSMMDAMCOMNGMSRMARMOZ" +
       "MMRNAMNRUNPLNLDANTNCLNZLNICNERNGANIUNFKMNPNOROMNPAKPLWPANPNGPRY" +
       "PERPHLPCNPOLPRTPRIQATREUROMRUSRWAKNALCAVCTWSMSMRSTPSAUSENSYCSLE" +
       "SGPSVKSVNSLBSOMZAFSGSESPLKASHNSPMSDNSURSJMSWZSWECHESYRTWNTJKTZA" +
       "THATGOTKLTONTTOTUNTURTKMTCATUVUGAUKRAREGBRUSAUMIURYUZBVUTVENVNM" +
       "VGBVIRWLFESHYEMYUGZMBZWE").substring(index, index + 3);
  }

  /** 
   * Gets the country name suitable for display to the user, formatted
   * for the default locale.  This has the same effect as
   * <pre>
   * getDisplayLanguage(Locale.getDefault());
   * </pre>
   *
   * @return the language name of this locale localized to the
   * default locale.  If the localized is not found, the ISO code
   * is returned.
   */
  public String getDisplayLanguage()
  {
    return getDisplayLanguage(getDefault());
  }

  /** 
   * Gets the language name suitable for display to the user, formatted
   * for a specified locale.
   * @param locale locale to use for formatting
   * @return the language name of this locale localized to the
   * given locale.  If the localized is not found, the ISO code
   * is returned.
   */
  public String getDisplayLanguage(Locale locale)
  {
    try
      {
	ResourceBundle bundle
	  = ResourceBundle.getBundle("gnu.java.locale.iso639", locale);
	return bundle.getString(language);
      }
    catch (MissingResourceException ex)
      {
	return language;
      }
  }

  /** 
   * Returns the country name of this locale localized to the
   * default locale.  If the localized is not found, the ISO code
   * is returned.  This has the same effect as
   * <pre>
   * getDisplayCountry(Locale.getDefault());
   * </pre>
   */
  public String getDisplayCountry()
  {
    return getDisplayCountry(getDefault());
  }

  /** 
   * Gets the country name suitable for display to the user, formatted
   * for a specified locale.
   *
   * @param locale locale to use for formatting
   * @return the country name of this locale localized to the given
   * locale.  If the localized is not found, the ISO country code is
   * returned.  */
  public String getDisplayCountry(Locale locale)
  {
    try
      {
	ResourceBundle bundle =
	  ResourceBundle.getBundle("gnu.java.locale.iso3166", locale);
	return bundle.getString(country);
      }
    catch (MissingResourceException ex)
      {
	return country;
      }
  }

  /** 
   * Returns the variant name of this locale localized to the
   * default locale.  If the localized is not found, the variant code
   * itself is returned.  This has the same effect as
   * <pre>
   * getDisplayVariant(Locale.getDefault());
   * </pre>
   */
  public String getDisplayVariant()
  {
    return getDisplayVariant(getDefault());
  }

  /** 
   * Returns the variant name of this locale localized to the
   * given locale.  If the localized is not found, the variant code
   * itself is returned.
   */
  public String getDisplayVariant(Locale locale)
  {
    /*XXX - load a bundle? */
    return variant;
  }

  /**
   * Gets all local components suitable for display to the user, formatted
   * for the default locale.  For the language component, getDisplayLanguage
   * is called.  For the country component, getDisplayCountry is called.
   * For the variant set component, getDisplayVariant is called.
   * <br><br>
   * The returned String will be one of the following forms:<br>
   * <pre>
   * language (country, variant)
   * language (country)
   * language (variant)
   * country (variant)
   * language
   * country
   * variant
   * </pre>
   * @return String version of this locale, suitable for display to the
   * user.
   */
  public String getDisplayName()
  {
    return getDisplayName(getDefault());
  }

  /**
   * Gets all local components suitable for display to the user, formatted
   * for a specified locale.  For the language component, 
   * getDisplayLanguage(Locale) is called.  For the country component, 
   * getDisplayCountry(Locale) is called.  For the variant set component, 
   * getDisplayVariant(Locale) is called.
   * <br><br>
   * The returned String will be one of the following forms:<br>
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
   *
   * @return String version of this locale, suitable for display to the
   * user.
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
   * an <code>CloneNotSupportedException</code>.  Why anyone would
   * use this method is a secret to me, since this class is
   * immutable.  
   */
  public Object clone()
  {
    try
      {
	return super.clone();
      }
    catch (CloneNotSupportedException ex)
      {
	return null;
      }
  }

  /**
   * Return the hash code for this locale.  The hashcode is the logical
   * xor of the hash codes of the language, the country and the variant.
   * The hash code is precomputed, since <code>Locale</code>s are often
   * used in hash tables.
   */
  public synchronized int hashCode()
  {
    // This method is synchronized because writeObject() might reset
    // the hashcode.
    return hashcode;
  }

  /**
   * Compares two locales.
   * @param obj the other locale.
   * @return true, if obj is a Locale with the same language, country, and
   * variant code as this locale, otherwise false.
   */
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof Locale))
      return false;
    Locale l = (Locale) obj;
    return (language.equals(l.language)
	    && country.equals(l.country)
	    && variant.equals(l.variant));
  }

  /**
   * @serialdata According to jdk1.2 the hashcode should always be 
   * written as -1; 
   */
  private synchronized void writeObject(java.io.ObjectOutputStream output)
    throws java.io.IOException
  {
    int tmpHashcode = hashcode;
    hashcode = -1;
    output.defaultWriteObject();
    hashcode = tmpHashcode;
  }

  /**
   * @serialdata  According to jdk1.2 the hashCode is always invalid
   * and must be recomputed.
   */
  private void readObject(java.io.ObjectInputStream input)
    throws java.io.IOException, ClassNotFoundException
  {
    input.defaultReadObject();
    hashcode = language.hashCode() ^ country.hashCode() ^ variant.hashCode();
  }
}
