/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

import java.io.InputStream;

/**
 * @author Anthony Green <green@cygnus.com>
 * @date November 26, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3,
 * and "The Java Language Specification", ISBN 0-201-63451-1.  */

public abstract class ResourceBundle
{
  protected ResourceBundle parent;

  // This is used to cache resource bundles.
  private static Hashtable resource_cache = new Hashtable ();

  public ResourceBundle ()
    {
    }

  public final String getString (String key) throws MissingResourceException
    {
      return (String) getObject(key);
    }

  public final String[] getStringArray (String key) 
    throws MissingResourceException
    {
      return (String[]) getObject(key);
    }

  public final Object getObject(String key) throws MissingResourceException
    {
      Object result;

      try 
	{
	  return handleGetObject (key);
	}
      catch (MissingResourceException ex)
	{
	  if (parent != null)
	    return parent.getObject(key);
	  else 
	    throw ex;
	}
    }

  public static final ResourceBundle getBundle(String baseName) 
    throws MissingResourceException
    {
      return getBundle(baseName, Locale.getDefault());
    }

  // Start searching with the name bundleName.  Continue searching by
  // stripping off the '_' delimited tails until the search name is
  // the same as stopHere.
  private static final ResourceBundle trySomeGetBundle (String bundleName,
							String stopHere)
    {
      Class rbc;
      ResourceBundle needs_parent = null, r, result = null;

      while (true)
	{
	  try 
	    {
	      rbc = Class.forName(bundleName);
	      r = null;
	      try 
		{
		  r = (ResourceBundle) rbc.newInstance();
		}
	      catch (IllegalAccessException ex)
		{
		  // Fall through
		}
	      catch (InstantiationException ex)
		{
		  // Fall through
		}
	      if (r != null)
		{
		  if (result == null)
		    result = r;
		  if (needs_parent != null)
		    {
		      // We've been through the loop one or more times
		      // already.  Set the parent and keep going.
		      needs_parent.setParent(r);
		    }
		  needs_parent = r;
		}
	    }
	  catch (ClassNotFoundException ex)
	    {
	      // Fall through.
	    }

	  // Look for a properties file.
	  {
	    InputStream i = 
		ClassLoader.getSystemResourceAsStream (bundleName.replace ('.', '/') 
						       + ".properties");
	    if (i != null)
	      {
		try {
		  return new PropertyResourceBundle (i);
		} catch (java.io.IOException e) {
		  // The docs don't appear to define what happens in
		  // this case, but it seems like continuing the
		  // search is a reasonable thing to do.
		}
	      }
	  }

	  if (bundleName.equals(stopHere))
	    return result;
	  else
	    {
	      int last = bundleName.lastIndexOf('_');
		  
	      // No more underscores?
	      if (last == -1)
		return result;

	      // Loop around, testing this new shorter name.
	      bundleName = bundleName.substring(0, last);
	    }
	}
    }

  // Search for bundles, but stop at baseName_language (if required).
  // This is synchronized so that the cache works correctly.
  private static final synchronized ResourceBundle
    partialGetBundle (String baseName, Locale locale, boolean langStop)
    {
      ResourceBundle rb;

      // Explicitly invoke locale.toString() to force a
      // NullPointerException when required.
      String bundleName = baseName + "_" + locale.toString();

      // Check the cache.
      Object obj = resource_cache.get(bundleName);
      if (obj != null)
	return (ResourceBundle) obj;

      String stopHere = (baseName 
			 + (langStop ? ("_" + locale.getLanguage()) : ""));


      rb = trySomeGetBundle(bundleName, stopHere);
      if (rb != null)
	resource_cache.put(bundleName, rb);

      return rb;
    }

  public static final ResourceBundle getBundle (String baseName, 
						Locale locale)
    throws MissingResourceException
    {
      ResourceBundle rb;
      Class rbc;

      if (baseName == null)
	throw new NullPointerException ();

      rb = partialGetBundle(baseName, locale, false);
      if (rb != null)
	return rb;

      // Finally, try the default locale.
      if (! locale.equals(Locale.getDefault()))
	{
	  rb = partialGetBundle(baseName, Locale.getDefault(), true);
	  if (rb != null)
	    return rb;
	}

      throw new MissingResourceException("can't load bundle", 
					 baseName,
					 "bundle");
    }

  protected void setParent(ResourceBundle parent)
    {
      this.parent = parent;
    }

  protected abstract Object handleGetObject(String key) 
    throws MissingResourceException;

  public abstract Enumeration getKeys();
}
