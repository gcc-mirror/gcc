/* java.util.ResourceBundle
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
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.security.AccessController;
import java.security.PrivilegedAction;
import gnu.classpath.Configuration;

/**
 * A resource bundle contains locale-specific data.  If you need
 * localized data, you can load a resource bundle that matches the
 * locale with <code>getBundle</code>.  Now you can get your object by
 * calling <code>getObject</code> or <code>getString</code> on that
 * bundle.
 * <br>
 * When a bundle is demanded for a specific locale, the ResourceBundle
 * is searched in following order (<i>def. language code<i> stands for
 * the two letter ISO language code of the default locale (see
 * <code>Locale.getDefault()</code>).
 * <pre>
 * baseName_<i>language code</i>_<i>country code</i>_<i>variant</i>
 * baseName_<i>language code</i>_<i>country code</i>
 * baseName_<i>language code</i>
 * baseName_<i>def. language code</i>_<i>def. country code</i>_<i>def. variant</i>
 * baseName_<i>def. language code</i>_<i>def. country code</i>
 * baseName_<i>def. language code</i>
 * baseName
 * </pre>
 *
 * A bundle is backed up, by less specific bundle (omiting variant,
 * country or language). But it is not backed up by the default
 * language locale.
 * <br>
 * If you provide a bundle for a given locale, say
 * <code>Bundle_en_UK_POSIX</code>, you must also provide a bundle for
 * all sub locales, ie. <code>Bundle_en_UK</code>, <code>Bundle_en</code>, and
 * <code>Bundle</code>.
 * <br>
 * When a bundle is searched, we look first for a class with
 * the given name and if that is not found for a file with
 * <code>.properties</code> extension in the classpath.  The name
 * must be a fully qualified classname (with dots as path separators).  
 * <br>
 * (Note: This implementation always backs up the class with a
 * properties file if that is existing, but you shouldn't rely on
 * this, if you want to be compatible to the standard JDK.)
 *
 * @see Locale
 * @see PropertyResourceBundle
 * @author Jochen Hoenicke */
public abstract class ResourceBundle
{
  /**
   * The parent bundle.  This is consulted when you call getObject
   * and there is no such resource in the current bundle.  This
   * field may be null.  
   */
  protected ResourceBundle parent;

  /**
   * The locale of this resource bundle.  You can read this with
   * <code>getLocale</code> and it is automatically set in
   * <code>getBundle</code>.  
   */
  private Locale locale;

  /**
   * We override SecurityManager in order to access getClassContext(). 
   */
  static class Security extends SecurityManager
  {
    /** Return the ClassLoader of the class which called into this
        ResourceBundle, or null if it cannot be determined. */
    ClassLoader getCallingClassLoader()
    {
      Class[] stack = super.getClassContext();
      for (int i = 0; i < stack.length; i++)
        if (stack[i] != Security.class && stack[i] != ResourceBundle.class)
	  return stack[i].getClassLoader();
      return null;
    }
  }
  
  // This will always work since java.util classes have (all) system
  // permissions.
  static Security security = (Security) AccessController.doPrivileged
    (
      new PrivilegedAction()
      {
        public Object run()
        {
          return new Security();
        }
      }
    );

  /**
   * The constructor.  It does nothing special.
   */
  public ResourceBundle()
  {
  }

  /**
   * Get a String from this resource bundle.  Since most localized
   * Objects are Strings, this method provides a convenient way to get
   * them without casting.
   * @param key the name of the resource.
   * @exception MissingResourceException
   *   if that particular object could not be found in this bundle nor
   *   the parent bundle.
   */
  public final String getString(String key) throws MissingResourceException
  {
    return (String) getObject(key);
  }

  /**
   * Get an array of Strings from this resource bundle.  This method
   * provides a convenient way to get it without casting.
   * @param key the name of the resource.
   * @exception MissingResourceException
   *   if that particular object could not be found in this bundle nor
   *   the parent bundle.
   */
  public final String[] getStringArray(String key)
    throws MissingResourceException
  {
    return (String[]) getObject(key);
  }

  /**
   * Get an object from this resource bundle.
   * @param key the name of the resource.
   * @exception MissingResourceException
   *   if that particular object could not be found in this bundle nor
   *   the parent bundle.
   */
  public final Object getObject(String key) throws MissingResourceException
  {
    for (ResourceBundle bundle = this; bundle != null; bundle = bundle.parent)
      {
	try
	  {
	    Object o = bundle.handleGetObject(key);
	    if (o != null)
	      return o;
	  }
	catch (MissingResourceException ex)
	  {
	  }
      }
    throw new MissingResourceException
      ("Key not found", getClass().getName(), key);
  }

  /**
   * Get the appropriate ResourceBundle for the default locale.  
   * @param baseName the name of the ResourceBundle.  This should be
   * a name of a Class or a properties-File.  See the class
   * description for details.  
   * @return the desired resource bundle
   * @exception MissingResourceException 
   *    if the resource bundle couldn't be found.  
   */
  public static final ResourceBundle getBundle(String baseName)
    throws MissingResourceException
  {
    return getBundle(baseName, Locale.getDefault(),
		     security.getCallingClassLoader());
  }

  /**
   * Get the appropriate ResourceBundle for the given locale.  
   * @param baseName the name of the ResourceBundle.  This should be
   * a name of a Class or a properties-File.  See the class
   * description for details.  
   * @param locale A locale.
   * @return the desired resource bundle
   * @exception MissingResourceException 
   *    if the resource bundle couldn't be found.
   */
  public static final ResourceBundle getBundle(String baseName,
					       Locale locale)
    throws MissingResourceException
  {
    return getBundle(baseName, locale, security.getCallingClassLoader());
  }

  /**
   * The resource bundle cache.  This is a two-level hash map: The key
   * is the class loader, the value is a new HashMap.  The key of this
   * second hash map is the localized name, the value is a soft
   * references to the resource bundle.  */
  private static Map resourceBundleCache = new HashMap();

  /**
   * The `empty' locale is created once in order to optimize
   * tryBundle().  
   */
  private static final Locale emptyLocale = new Locale ("", "");

  /**
   * Tries to load a class or a property file with the specified name.
   * @param localizedName the name.
   * @param locale the locale, that must be used exactly.
   * @param classloader the classloader.
   * @param bundle the back up (parent) bundle
   * @return the resource bundle if that could be loaded, otherwise 
   * <code>bundle</code>.
   */
  private static final ResourceBundle tryBundle(String localizedName,
						Locale locale,
						ClassLoader classloader,
						ResourceBundle bundle,
						HashMap cache)
  {
    {
      // First look into the cache.
      // XXX We should remove cleared references from the cache.
      Reference ref = (Reference) cache.get(localizedName);
      if (ref != null)
	{
	  ResourceBundle rb = (ResourceBundle) ref.get();
	  if (rb != null)
	    // rb should already have the right parent, except if
	    // something very strange happened.
	    return rb;
	}
    }

    // foundBundle holds exact matches for the localizedName resource
    // bundle, which may later be cached.
    ResourceBundle foundBundle = null;

    try
      {
	java.io.InputStream is;
	final String resourceName =
	  localizedName.replace('.', '/') + ".properties";
	if (classloader == null)
	  is = ClassLoader.getSystemResourceAsStream (resourceName);
	else
	  is = classloader.getResourceAsStream (resourceName);
	if (is != null)
	  {
	    foundBundle = new PropertyResourceBundle(is);
	    foundBundle.parent = bundle;
	    foundBundle.locale = locale;
	  }
      }
    catch (java.io.IOException ex)
      {
      }

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
    catch (ClassNotFoundException ex)
      {
      }
    catch (IllegalAccessException ex)
      {
      }
    catch (InstantiationException ex)
      {
	// ignore them all
	// XXX should we also ignore ClassCastException?
      }

    if (foundBundle != null)
      cache.put(localizedName, new SoftReference(foundBundle));

    return foundBundle != null ? foundBundle : bundle;
  }

  /**
   * Tries to load a the bundle for a given locale, also loads the backup
   * locales with the same language.
   *
   * @param name the name.
   * @param locale the locale, that must be used exactly.
   * @param classloader the classloader.
   * @param bundle the back up (parent) bundle
   * @return the resource bundle if that could be loaded, otherwise 
   * <code>bundle</code>.
   */
  private static final ResourceBundle tryLocalBundle(String baseName,
						     Locale locale,
						     ClassLoader classloader,
						     ResourceBundle bundle,
						     HashMap cache)
  {
    final String language = locale.getLanguage();

    if (language.length() > 0)
      {
	final String country = locale.getCountry();
	String name = baseName + "_" + language;

	if (country.length() != 0)
	  {
	    bundle = tryBundle(name,
			       new Locale(language, ""),
			       classloader, bundle, cache);

	    name += "_" + country;

	    final String variant = locale.getVariant();

	    if (variant.length() != 0)
	      {
		bundle = tryBundle(name,
				   new Locale(language,
					      country),
				   classloader, bundle, cache);

		name += "_" + variant;
	      }
	  }
	bundle = tryBundle(name, locale, classloader, bundle, cache);
      }
    return bundle;
  }

  /**
   * Get the appropriate ResourceBundle for the given locale.  
   * @param baseName the name of the ResourceBundle.  This should be
   * a name of a Class or a properties file.  See the class
   * description for details.  
   * @param locale A locale.
   * @param classloader a ClassLoader.
   * @return the desired resource bundle
   * @exception MissingResourceException 
   *    if the resource bundle couldn't be found.
   */
  // This method is synchronized so that the cache is properly
  // handled.
  public static final synchronized ResourceBundle getBundle(String baseName,
							    Locale locale,
							    ClassLoader classLoader)
    throws MissingResourceException
  {
    // This implementation searches the bundle in the reverse direction
    // and builds the parent chain on the fly.

    HashMap cache = (HashMap) resourceBundleCache.get(classLoader);
    if (cache == null)
      {
	cache = new HashMap();
	resourceBundleCache.put(classLoader, cache);
      }
    else
      {
	// Fast path: If baseName + "_" + locale is in cache use it.
	String name = baseName + "_" + locale.toString();
	Reference ref = (Reference) cache.get(name);
	if (ref != null)
	  {
	    ResourceBundle rb = (ResourceBundle) ref.get();
	    if (rb != null)
	      // rb should already have the right parent, except if
	      // something very strange happened.
	      return rb;
	  }
      }

    ResourceBundle baseBundle = tryBundle(baseName, emptyLocale,
					  classLoader, null, cache);
    if (baseBundle == null)
      // JDK says, that if one provides a bundle base_en_UK, one
      // must also provide the bundles base_en and base.
      // This implies that if there is no bundle for base, there
      // is no bundle at all.
      throw new MissingResourceException("Bundle " + baseName + " not found", baseName, "");

    // Now use the default locale.
    ResourceBundle bundle = tryLocalBundle(baseName, locale,
					   classLoader, baseBundle, cache);
    if (bundle == baseBundle && !locale.equals(Locale.getDefault()))
      {
	bundle = tryLocalBundle(baseName, Locale.getDefault(),
				classLoader, baseBundle, cache);
      }
    return bundle;
  }

  /**
   * Return the actual locale of this bundle.  You can use it after
   * calling getBundle, to know if the bundle for the desired locale
   * was loaded or if the fall back was used.
   */
  public Locale getLocale()
  {
    return locale;
  }

  /**
   * Set the parent of this bundle. This is consulted when you call
   * getObject and there is no such resource in the current bundle.
   * @param parent the parent of this bundle.
   */
  protected void setParent(ResourceBundle parent)
  {
    // Shall we ignore the old parent?
    this.parent = parent;
  }

  /**
   * Override this method to provide the resource for a keys.  This gets
   * called by <code>getObject</code>.  If you don't have a resource
   * for the given key, you should return null instead throwing a
   * MissingResourceException.   You don't have to ask the parent, 
   * getObject() already does this.
   *
   * @param key The key of the resource.
   * @return The resource for the key, or null if not in bundle.
   * @exception MissingResourceException
   *   you shouldn't throw this.
   */
  protected abstract Object handleGetObject(String key)
    throws MissingResourceException;

  /**
   * This method should return all keys for which a resource exists.
   * @return An enumeration of the keys.
   */
  public abstract Enumeration getKeys();
}
