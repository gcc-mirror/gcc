/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

/**
 * @author Anthony Green <green@cygnus.com>
 * @date November 26, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3,
 * and "The Java Language Specification", ISBN 0-201-63451-1.  */

public abstract class ResourceBundle
{
  protected ResourceBundle parent;

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

      while (true)
	{
	  try 
	    {
	      rbc = Class.forName(bundleName);
	      try 
		{
		  return (ResourceBundle) rbc.newInstance();
		}
	      catch (IllegalAccessException ex)
		{
		  // Fall through
		}
	      catch (InstantiationException ex)
		{
		  // Fall through
		}
	      return null;
	    }
	  catch (ClassNotFoundException ex)
	    {
	      if (bundleName.compareTo(stopHere) == 0)
		return null;
	      else
		{
		  int last = bundleName.lastIndexOf('_');
		  
		  // No more underscores?
		  if (last == -1)
		    return null;
		  
		  // Loop around, testing this new shorter name.
		  bundleName = bundleName.substring(0, last);
		}
	    }
	}
    }
  
  // Search for bundles, but stop at baseName_language.
  private static final ResourceBundle partialGetBundle (String baseName,
							Locale locale)
    {
      ResourceBundle rb;

      String bundleName = (baseName 
			   + "_" 
			   + locale.getLanguage() + "_"
			   + locale.getCountry() + "_"
			   + locale.getVariant());

      String stopHere = (baseName 
			 + "_" 
			 + locale.getLanguage());


      rb = trySomeGetBundle(bundleName, stopHere);

      return rb;
    }

  public static final ResourceBundle getBundle (String baseName, 
						Locale locale)
    throws MissingResourceException
    {
      ResourceBundle rb;
      Class rbc;

      // FIXME: We can't currently rely on NullPointerException being
      // thrown when we invoke a method on a null object.
      if (locale == null)
	throw new NullPointerException ();

      rb = partialGetBundle(baseName, locale);
      if (rb != null)
	return rb;

      if (! locale.equals(Locale.getDefault()))
	{
	  rb = partialGetBundle(baseName, Locale.getDefault());
	  if (rb != null)
	    return rb;
	}
			   
      // Try just the baseName.
      try
	{
	  rbc = Class.forName (baseName);
	  try 
	    {
	      return (ResourceBundle) rbc.newInstance();
	    }
	  catch (IllegalAccessException ex)
	    {
	      // Fall through.
	    }
	  catch (InstantiationException ex)
	    {
	      // Fall through.
	    }
	}
      catch (ClassNotFoundException ex)
	{
	  // Fall through.
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
