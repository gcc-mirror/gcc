/* Copyright (C) 1998, 1999  Free Software Foundation

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

public abstract class ListResourceBundle extends ResourceBundle 
{
  public final Object handleGetObject(String key)
    {
      Object a[][] = getContents();

      for (int i = 0; i < a.length; i++)
	{
	  if (key.compareTo((String) a[i][0]) == 0)
	    return a[i][1];
	}
      throw new MissingResourceException("can't find handle", 
					 getClass().getName(), 
					 key);
    }

  public Enumeration getKeys()
    {
      Object a[][] = getContents();

      Vector keys = new Vector(a.length);

      for (int i = 0; i < a.length; i++)
	keys.addElement(a[i][0]);

      return keys.elements();
    }

  protected abstract Object[][] getContents();

  public ListResourceBundle()
    {
    }
}
