/* ResourceBundle -- aids in loading resource bundles
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.io.InputStream;
import java.io.IOException;
import gnu.classpath.Configuration;

/**
 * A resource bundle contains locale-specific data. If you need localized
 * data, you can load a resource bundle that matches the locale with
 * <code>getBundle</code>. Now you can get your object by calling
 * <code>getObject</code> or <code>getString</code> on that bundle.
 *
 * <p>When a bundle is demanded for a specific locale, the ResourceBundle
 * is searched in following order (<i>def. language<i> stands for the
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

  private static native ClassLoader getCallingClassLoader();

  /**
   * The resource bundle cache. This is a two-level hash map: The key
   * is the class loader, the value is a new HashMap. The key of this
   * second hash map is the localized name, the value is a soft
   * references to the resource bundle.
   */
  private static Map resourceBundleCache;

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
      try
        {
          Object o = bundle.handleGetObject(key);
          if (o != null)
            return o;
        }
      catch (MissingResourceException ex)
        {
        }
    throw new MissingResourceException("Key not found",
                                       getClass().getName(), key);
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
  public static final ResourceBundle getBundle(String baseName)
  {
    return getBundle(baseName, Locale.getDefault(),
                     getCallingClassLoader());
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
  public static final ResourceBundle getBundle(String baseName,
                                               Locale locale)
  {
    return getBundle(baseName, locale, getCallingClassLoader());
  }

  /**
   * Get the appropriate ResourceBundle for the given locale. The following
   * strategy is used:
   *
   * <p>A sequence of candidate bundle names are generated, and tested in
   * this order, where the suffix 1 means the string from the specified
   * locale, and the suffix 2 means the string from the default locale:<ul>
   * <li>baseName + "_" + language1 + "_" + country1 + "_" + variant1</li>
   * <li>baseName + "_" + language1 + "_" + country1</li>
   * <li>baseName + "_" + language1</li>
   * <li>baseName + "_" + language2 + "_" + country2 + "_" + variant2</li>
   * <li>baseName + "_" + language2 + "_" + country2</li>
   * <li>baseName + "_" + language2<li>
   * <li>baseName</li>
   * </ul>
   *
   * <p>In the sequence, entries with an empty string are ignored. Next,
   * <code>getBundle</code> tries to instantiate the resource bundle:<ul>
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
   * has a parent).
   *
   * <p>For example, suppose the following class and property files are
   * provided: MyResources.class, MyResources_fr_CH.properties,
   * MyResources_fr_CH.class, MyResources_fr.properties,
   * MyResources_en.properties, and MyResources_es_ES.class. The contents of
   * all files are valid (that is, public non-abstract subclasses of
   * ResourceBundle with public nullary constructors for the ".class" files,
   * syntactically correct ".properties" files). The default locale is
   * Locale("en", "UK").
   *
   * <p>Calling getBundle with the shown locale argument values instantiates
   * resource bundles from the following sources:<ul>
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
   * The file MyResources_fr_CH.properties is never used because it is hidden
   * by MyResources_fr_CH.class.
   *
   * @param baseName the name of the ResourceBundle
   * @param locale A locale
   * @param classloader a ClassLoader
   * @return the desired resource bundle
   * @throws MissingResourceException if the resource bundle can't be found
   * @throws NullPointerException if any argument is null
   * @since 1.2
   */
  // This method is synchronized so that the cache is properly
  // handled.
  public static final synchronized ResourceBundle getBundle
    (String baseName, Locale locale, ClassLoader classLoader)
  {
    // This implementation searches the bundle in the reverse direction
    // and builds the parent chain on the fly.
    Locale defaultLocale = Locale.getDefault();
    if (defaultLocale != lastDefaultLocale)
      {
	resourceBundleCache = new HashMap();
	lastDefaultLocale = defaultLocale;
      }
    HashMap cache = (HashMap) resourceBundleCache.get(classLoader);
    StringBuffer sb = new StringBuffer(60);
    sb.append(baseName).append('_').append(locale);
    String name = sb.toString();

    if (cache == null)
      {
        cache = new HashMap();
        resourceBundleCache.put(classLoader, cache);
      }
    else if (cache.containsKey(name))
      {
	Reference ref = (Reference) cache.get(name);
	ResourceBundle result = null;
	// If REF is null, that means that we added a `null' value to
	// the hash map.  That means we failed to find the bundle
	// previously, and we cached that fact.  The JDK does this, so
	// it must be ok.
	if (ref == null)
	  throw new MissingResourceException("Bundle " + baseName
					     + " not found",
					     baseName, "");
	else
	  {
	    ResourceBundle rb = (ResourceBundle) ref.get();
	    if (rb != null)
	      {
		// RB should already have the right parent, except if
		// something very strange happened.
		return rb;
	      }
	    // If RB is null, then we previously found it but it was
	    // collected.  So we try again.
	  }
      }

    // It is ok if this returns null.  We aren't required to have the
    // base bundle.
    ResourceBundle baseBundle = tryBundle(baseName, emptyLocale,
                                          classLoader, null, cache);

    // Now use our locale, followed by the default locale.  We only
    // need to try the default locale if our locale is different, and
    // if our locale failed to yield a result other than the base
    // bundle.
    ResourceBundle bundle = tryLocalBundle(baseName, locale,
                                           classLoader, baseBundle, cache);
    if (bundle == baseBundle && !locale.equals(defaultLocale))
      {
	bundle = tryLocalBundle(baseName, defaultLocale,
				classLoader, baseBundle, cache);
	// We need to record that the argument locale maps to the
	// bundle we just found.  If we didn't find a bundle, record
	// that instead.
	if (bundle == null)
	  cache.put(name, null);
	else
	  cache.put(name, new SoftReference(bundle));
      }

    if (bundle == null)
      throw new MissingResourceException("Bundle " + baseName + " not found",
					 baseName, "");

    return bundle;
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
   * @param locale the locale, that must be used exactly
   * @param classloader the classloader
   * @param bundle the backup (parent) bundle
   * @return the resource bundle if it was loaded, otherwise the backup
   */
  private static final ResourceBundle tryBundle(String localizedName,
                                                Locale locale,
                                                ClassLoader classloader,
                                                ResourceBundle bundle,
                                                HashMap cache)
  {
    // First look into the cache.
    if (cache.containsKey(localizedName))
      {
	Reference ref = (Reference) cache.get(localizedName);
	ResourceBundle result = null;
	// If REF is null, that means that we added a `null' value to
	// the hash map.  That means we failed to find the bundle
	// previously, and we cached that fact.  The JDK does this, so
	// it must be ok.
	if (ref == null)
	  return null;
	else
	  {
	    ResourceBundle rb = (ResourceBundle) ref.get();
	    if (rb != null)
	      {
		// RB should already have the right parent, except if
		// something very strange happened.
		return rb;
	      }
	    // If RB is null, then we previously found it but it was
	    // collected.  So we try again.
	  }
      }

    // foundBundle holds exact matches for the localizedName resource
    // bundle, which may later be cached.
    ResourceBundle foundBundle = null;
    try
      {
        Class rbClass;
        if (classloader == null)
          rbClass = Class.forName(localizedName);
        else
          rbClass = classloader.loadClass(localizedName);
        foundBundle = (ResourceBundle) rbClass.newInstance();
        foundBundle.parent = bundle;
        foundBundle.locale = locale;
      }
    catch (Exception ex)
      {
        // ignore them all
      }
    if (foundBundle == null)
      {
	try
	  {
	    InputStream is;
	    final String resourceName
	      = localizedName.replace('.', '/') + ".properties";
	    if (classloader == null)
	      is = ClassLoader.getSystemResourceAsStream(resourceName);
	    else
	      is = classloader.getResourceAsStream(resourceName);
	    if (is != null)
	      {
		foundBundle = new PropertyResourceBundle(is);
		foundBundle.parent = bundle;
		foundBundle.locale = locale;
	      }
	  }
	catch (IOException ex)
	  {
	  }
      }

    // Put the result into the hash table.  If we didn't find anything
    // here, we record our parent bundle.  If we record `null' that means
    // nothing, not even the base, was found.
    if (foundBundle == null)
      foundBundle = bundle;
    if (foundBundle == null)
      cache.put(localizedName, null);
    else
      cache.put(localizedName, new SoftReference(foundBundle));
    return foundBundle;
  }

  /**
   * Tries to load a the bundle for a given locale, also loads the backup
   * locales with the same language.
   *
   * @param name the name
   * @param locale the locale
   * @param classloader the classloader
   * @param bundle the backup (parent) bundle
   * @return the resource bundle if it was loaded, otherwise the backup
   */
  private static final ResourceBundle tryLocalBundle(String baseName,
						     Locale locale,
                                                     ClassLoader classloader,
                                                     ResourceBundle bundle,
                                                     HashMap cache)
  {
    final String language = locale.getLanguage();
    final String country = locale.getCountry();
    final String variant = locale.getVariant();

    StringBuffer sb = new StringBuffer(60);
    sb.append(baseName);
    sb.append('_');

    if (language.length() > 0)
      {
	sb.append(language);
	bundle = tryBundle(sb.toString(), new Locale(language),
			   classloader, bundle, cache);
      }
    // If LANGUAGE was empty, we still need to try the other
    // components, and the `_' is required.
    sb.append('_');

    if (country.length() > 0)
      {
	sb.append(country);
	bundle = tryBundle(sb.toString(), new Locale(language, country),
			   classloader, bundle, cache);
      }
    sb.append('_');

    if (variant.length() > 0)
      {
	sb.append(variant);
	bundle = tryBundle(sb.toString(), locale,
			   classloader, bundle, cache);
      }

    return bundle;
  }
}
