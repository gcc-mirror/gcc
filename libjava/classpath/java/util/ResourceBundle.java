/* ResourceBundle -- aids in loading resource bundles
   Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

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

import gnu.classpath.VMStackWalker;

import java.io.IOException;
import java.io.InputStream;

/**
 * A resource bundle contains locale-specific data. If you need localized
 * data, you can load a resource bundle that matches the locale with
 * <code>getBundle</code>. Now you can get your object by calling
 * <code>getObject</code> or <code>getString</code> on that bundle.
 *
 * <p>When a bundle is demanded for a specific locale, the ResourceBundle
 * is searched in following order (<i>def. language</i> stands for the
 * two letter ISO language code of the default locale (see
 * <code>Locale.getDefault()</code>).
 *
<pre>baseName_<i>language code</i>_<i>country code</i>_<i>variant</i>
baseName_<i>language code</i>_<i>country code</i>
baseName_<i>language code</i>
baseName_<i>def. language</i>_<i>def. country</i>_<i>def. variant</i>
baseName_<i>def. language</i>_<i>def. country</i>
baseName_<i>def. language</i>
baseName</pre>
 *
 * <p>A bundle is backed up by less specific bundles (omitting variant, country
 * or language). But it is not backed up by the default language locale.
 *
 * <p>If you provide a bundle for a given locale, say
 * <code>Bundle_en_UK_POSIX</code>, you must also provide a bundle for
 * all sub locales, ie. <code>Bundle_en_UK</code>, <code>Bundle_en</code>, and
 * <code>Bundle</code>.
 *
 * <p>When a bundle is searched, we look first for a class with the given
 * name, then for a file with <code>.properties</code> extension in the
 * classpath. The name must be a fully qualified classname (with dots as
 * path separators).
 *
 * <p>(Note: This implementation always backs up the class with a properties
 * file if that is existing, but you shouldn't rely on this, if you want to
 * be compatible to the standard JDK.)
 *
 * @author Jochen Hoenicke
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Locale
 * @see ListResourceBundle
 * @see PropertyResourceBundle
 * @since 1.1
 * @status updated to 1.4
 */
public abstract class ResourceBundle
{
  /**
   * The parent bundle. This is consulted when you call getObject and there
   * is no such resource in the current bundle. This field may be null.
   */
  protected ResourceBundle parent;

  /**
   * The locale of this resource bundle. You can read this with
   * <code>getLocale</code> and it is automatically set in
   * <code>getBundle</code>.
   */
  private Locale locale;

  /**
   * The resource bundle cache.
   */
  private static Map bundleCache;

  /**
   * The last default Locale we saw. If this ever changes then we have to
   * reset our caches.
   */
  private static Locale lastDefaultLocale;

  /**
   * The `empty' locale is created once in order to optimize
   * tryBundle().
   */
  private static final Locale emptyLocale = new Locale("");

  /**
   * The constructor. It does nothing special.
   */
  public ResourceBundle()
  {
  }

  /**
   * Get a String from this resource bundle. Since most localized Objects
   * are Strings, this method provides a convenient way to get them without
   * casting.
   *
   * @param key the name of the resource
   * @throws MissingResourceException if the resource can't be found
   * @throws NullPointerException if key is null
   * @throws ClassCastException if resource is not a string
   */
  public final String getString(String key)
  {
    return (String) getObject(key);
  }

  /**
   * Get an array of Strings from this resource bundle. This method
   * provides a convenient way to get it without casting.
   *
   * @param key the name of the resource
   * @throws MissingResourceException if the resource can't be found
   * @throws NullPointerException if key is null
   * @throws ClassCastException if resource is not a string
   */
  public final String[] getStringArray(String key)
  {
    return (String[]) getObject(key);
  }

  /**
   * Get an object from this resource bundle. This will call
   * <code>handleGetObject</code> for this resource and all of its parents,
   * until it finds a non-null resource.
   *
   * @param key the name of the resource
   * @throws MissingResourceException if the resource can't be found
   * @throws NullPointerException if key is null
   */
  public final Object getObject(String key)
  {
    for (ResourceBundle bundle = this; bundle != null; bundle = bundle.parent)
      {
        Object o = bundle.handleGetObject(key);
        if (o != null)
          return o;
      }

    String className = getClass().getName();
    throw new MissingResourceException("Key '" + key
				       + "'not found in Bundle: "
				       + className, className, key);
  }

  /**
   * Return the actual locale of this bundle. You can use it after calling
   * getBundle, to know if the bundle for the desired locale was loaded or
   * if the fall back was used.
   *
   * @return the bundle's locale
   */
  public Locale getLocale()
  {
    return locale;
  }

  /**
   * Set the parent of this bundle. The parent is consulted when you call
   * getObject and there is no such resource in the current bundle.
   *
   * @param parent the parent of this bundle
   */
  protected void setParent(ResourceBundle parent)
  {
    this.parent = parent;
  }

  /**
   * Get the appropriate ResourceBundle for the default locale. This is like
   * calling <code>getBundle(baseName, Locale.getDefault(),
   * getClass().getClassLoader()</code>, except that any security check of
   * getClassLoader won't fail.
   *
   * @param baseName the name of the ResourceBundle
   * @return the desired resource bundle
   * @throws MissingResourceException if the resource bundle can't be found
   * @throws NullPointerException if baseName is null
   */
  public static ResourceBundle getBundle(String baseName)
  {
    ClassLoader cl = VMStackWalker.getCallingClassLoader();
    if (cl == null)
      cl = ClassLoader.getSystemClassLoader();
    return getBundle(baseName, Locale.getDefault(), cl);
  }

  /**
   * Get the appropriate ResourceBundle for the given locale. This is like
   * calling <code>getBundle(baseName, locale,
   * getClass().getClassLoader()</code>, except that any security check of
   * getClassLoader won't fail.
   *
   * @param baseName the name of the ResourceBundle
   * @param locale A locale
   * @return the desired resource bundle
   * @throws MissingResourceException if the resource bundle can't be found
   * @throws NullPointerException if baseName or locale is null
   */
  public static ResourceBundle getBundle(String baseName, Locale locale)
  {
    ClassLoader cl = VMStackWalker.getCallingClassLoader();
    if (cl == null)
      cl = ClassLoader.getSystemClassLoader();
    return getBundle(baseName, locale, cl);
  }

  /** Cache key for the ResourceBundle cache.  Resource bundles are keyed
      by the combination of bundle name, locale, and class loader. */
  private static class BundleKey
  {
    String baseName;
    Locale locale;
    ClassLoader classLoader;
    int hashcode;

    BundleKey() {}

    BundleKey(String s, Locale l, ClassLoader cl)
    {
      set(s, l, cl);
    }
    
    void set(String s, Locale l, ClassLoader cl)
    {
      baseName = s;
      locale = l;
      classLoader = cl;
      hashcode = baseName.hashCode() ^ locale.hashCode() ^
        classLoader.hashCode();
    }
    
    public int hashCode()
    {
      return hashcode;
    }
    
    public boolean equals(Object o)
    {
      if (! (o instanceof BundleKey))
        return false;
      BundleKey key = (BundleKey) o;
      return hashcode == key.hashcode &&
	baseName.equals(key.baseName) &&
        locale.equals(key.locale) &&
	classLoader.equals(key.classLoader);
    }    
  }
  
  /** A cache lookup key. This avoids having to a new one for every
   *  getBundle() call. */
  private static BundleKey lookupKey = new BundleKey();
  
  /** Singleton cache entry to represent previous failed lookups. */
  private static Object nullEntry = new Object();

  /**
   * Get the appropriate ResourceBundle for the given locale. The following
   * strategy is used:
   *
   * <p>A sequence of candidate bundle names are generated, and tested in
   * this order, where the suffix 1 means the string from the specified
   * locale, and the suffix 2 means the string from the default locale:</p>
   *
   * <ul>
   * <li>baseName + "_" + language1 + "_" + country1 + "_" + variant1</li>
   * <li>baseName + "_" + language1 + "_" + country1</li>
   * <li>baseName + "_" + language1</li>
   * <li>baseName + "_" + language2 + "_" + country2 + "_" + variant2</li>
   * <li>baseName + "_" + language2 + "_" + country2</li>
   * <li>baseName + "_" + language2</li>
   * <li>baseName</li>
   * </ul>
   *
   * <p>In the sequence, entries with an empty string are ignored. Next,
   * <code>getBundle</code> tries to instantiate the resource bundle:</p>
   *
   * <ul>
   * <li>First, an attempt is made to load a class in the specified classloader
   * which is a subclass of ResourceBundle, and which has a public constructor
   * with no arguments, via reflection.</li>
   * <li>Next, a search is made for a property resource file, by replacing
   * '.' with '/' and appending ".properties", and using
   * ClassLoader.getResource(). If a file is found, then a
   * PropertyResourceBundle is created from the file's contents.</li>
   * </ul>
   * If no resource bundle was found, a MissingResourceException is thrown.
   *
   * <p>Next, the parent chain is implemented. The remaining candidate names
   * in the above sequence are tested in a similar manner, and if any results
   * in a resource bundle, it is assigned as the parent of the first bundle
   * using the <code>setParent</code> method (unless the first bundle already
   * has a parent).</p>
   *
   * <p>For example, suppose the following class and property files are
   * provided: MyResources.class, MyResources_fr_CH.properties,
   * MyResources_fr_CH.class, MyResources_fr.properties,
   * MyResources_en.properties, and MyResources_es_ES.class. The contents of
   * all files are valid (that is, public non-abstract subclasses of
   * ResourceBundle with public nullary constructors for the ".class" files,
   * syntactically correct ".properties" files). The default locale is
   * Locale("en", "UK").</p>
   *
   * <p>Calling getBundle with the shown locale argument values instantiates
   * resource bundles from the following sources:</p>
   *
   * <ul>
   * <li>Locale("fr", "CH"): result MyResources_fr_CH.class, parent
   *   MyResources_fr.properties, parent MyResources.class</li>
   * <li>Locale("fr", "FR"): result MyResources_fr.properties, parent
   *   MyResources.class</li>
   * <li>Locale("de", "DE"): result MyResources_en.properties, parent
   *   MyResources.class</li>
   * <li>Locale("en", "US"): result MyResources_en.properties, parent
   *   MyResources.class</li>
   * <li>Locale("es", "ES"): result MyResources_es_ES.class, parent
   *   MyResources.class</li>
   * </ul>
   * 
   * <p>The file MyResources_fr_CH.properties is never used because it is hidden
   * by MyResources_fr_CH.class.</p>
   *
   * @param baseName the name of the ResourceBundle
   * @param locale A locale
   * @param classLoader a ClassLoader
   * @return the desired resource bundle
   * @throws MissingResourceException if the resource bundle can't be found
   * @throws NullPointerException if any argument is null
   * @since 1.2
   */
  // This method is synchronized so that the cache is properly
  // handled.
  public static synchronized ResourceBundle getBundle
    (String baseName, Locale locale, ClassLoader classLoader)
  {
    // If the default locale changed since the last time we were called,
    // all cache entries are invalidated.
    Locale defaultLocale = Locale.getDefault();
    if (defaultLocale != lastDefaultLocale)
      {
	bundleCache = new HashMap();
	lastDefaultLocale = defaultLocale;
      }

    // This will throw NullPointerException if any arguments are null.
    lookupKey.set(baseName, locale, classLoader);
    
    Object obj = bundleCache.get(lookupKey);
    ResourceBundle rb = null;
    
    if (obj instanceof ResourceBundle)
      {
        return (ResourceBundle) obj;
      }
    else if (obj == nullEntry)
      {
        // Lookup has failed previously. Fall through.
      }
    else
      {
	// First, look for a bundle for the specified locale. We don't want
	// the base bundle this time.
	boolean wantBase = locale.equals(defaultLocale);
	ResourceBundle bundle = tryBundle(baseName, locale, classLoader, 
					  wantBase);

        // Try the default locale if neccessary.
	if (bundle == null && !locale.equals(defaultLocale))
	  bundle = tryBundle(baseName, defaultLocale, classLoader, true);

	BundleKey key = new BundleKey(baseName, locale, classLoader);
        if (bundle == null)
	  {
	    // Cache the fact that this lookup has previously failed.
	    bundleCache.put(key, nullEntry);
	  }
	else
	  {
            // Cache the result and return it.
	    bundleCache.put(key, bundle);
	    return bundle;
	  }
      }

    throw new MissingResourceException("Bundle " + baseName 
				       + " not found for locale "
				       + locale
				       + " by classloader "
				       + classLoader,
				       baseName, "");
  }

  /**
   * Override this method to provide the resource for a keys. This gets
   * called by <code>getObject</code>. If you don't have a resource
   * for the given key, you should return null instead throwing a
   * MissingResourceException. You don't have to ask the parent, getObject()
   * already does this; nor should you throw a MissingResourceException.
   *
   * @param key the key of the resource
   * @return the resource for the key, or null if not in bundle
   * @throws NullPointerException if key is null
   */
  protected abstract Object handleGetObject(String key);

  /**
   * This method should return all keys for which a resource exists; you
   * should include the enumeration of any parent's keys, after filtering out
   * duplicates.
   *
   * @return an enumeration of the keys
   */
  public abstract Enumeration getKeys();

  /**
   * Tries to load a class or a property file with the specified name.
   *
   * @param localizedName the name
   * @param classloader the classloader
   * @return the resource bundle if it was loaded, otherwise the backup
   */
  private static ResourceBundle tryBundle(String localizedName,
                                          ClassLoader classloader)
  {
    ResourceBundle bundle = null;
    try
      {
        Class rbClass;
        if (classloader == null)
          rbClass = Class.forName(localizedName);
        else
          rbClass = classloader.loadClass(localizedName);
	// Note that we do the check up front instead of catching
	// ClassCastException.  The reason for this is that some crazy
	// programs (Eclipse) have classes that do not extend
	// ResourceBundle but that have the same name as a property
	// bundle; in fact Eclipse relies on ResourceBundle not
	// instantiating these classes.
	if (ResourceBundle.class.isAssignableFrom(rbClass))
	  bundle = (ResourceBundle) rbClass.newInstance();
      }
    catch (IllegalAccessException ex) {}
    catch (InstantiationException ex) {}
    catch (ClassNotFoundException ex) {}

    if (bundle == null)
      {
	try
	  {
	    InputStream is;
	    String resourceName
	      = localizedName.replace('.', '/') + ".properties";
	    if (classloader == null)
	      is = ClassLoader.getSystemResourceAsStream(resourceName);
	    else
	      is = classloader.getResourceAsStream(resourceName);
	    if (is != null)
	      bundle = new PropertyResourceBundle(is);
	  }
	catch (IOException ex)
	  {
	    MissingResourceException mre = new MissingResourceException
	      ("Failed to load bundle: " + localizedName, localizedName, "");
	    mre.initCause(ex);
	    throw mre;
	  }
      }

    return bundle;
  }

  /**
   * Tries to load a the bundle for a given locale, also loads the backup
   * locales with the same language.
   *
   * @param baseName the raw bundle name, without locale qualifiers
   * @param locale the locale
   * @param classLoader the classloader
   * @param wantBase whether a resource bundle made only from the base name
   *        (with no locale information attached) should be returned.
   * @return the resource bundle if it was loaded, otherwise the backup
   */
  private static ResourceBundle tryBundle(String baseName, Locale locale,
                                          ClassLoader classLoader, 
					  boolean wantBase)
  {
    String language = locale.getLanguage();
    String country = locale.getCountry();
    String variant = locale.getVariant();
    
    int baseLen = baseName.length();

    // Build up a StringBuffer containing the complete bundle name, fully
    // qualified by locale.
    StringBuffer sb = new StringBuffer(baseLen + variant.length() + 7);

    sb.append(baseName);
    
    if (language.length() > 0)
      {
	sb.append('_');
	sb.append(language);
	
	if (country.length() > 0)
	  {
	    sb.append('_');
	    sb.append(country);
	    
	    if (variant.length() > 0)
	      {
	        sb.append('_');
		sb.append(variant);
	      }
	  }
      }

    // Now try to load bundles, starting with the most specialized name.
    // Build up the parent chain as we go.
    String bundleName = sb.toString();
    ResourceBundle first = null; // The most specialized bundle.
    ResourceBundle last = null; // The least specialized bundle.
    
    while (true)
      {
        ResourceBundle foundBundle = tryBundle(bundleName, classLoader);
	if (foundBundle != null)
	  {
	    if (first == null)
	      first = foundBundle;
	    if (last != null)
	      last.parent = foundBundle;
	    foundBundle.locale = locale;
	    last = foundBundle;
	  }
	int idx = bundleName.lastIndexOf('_');
	// Try the non-localized base name only if we already have a
	// localized child bundle, or wantBase is true.
	if (idx > baseLen || (idx == baseLen && (first != null || wantBase)))
	  bundleName = bundleName.substring(0, idx);
	else
	  break;
      }
    
    return first;
  }
}
