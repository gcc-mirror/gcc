/* java.util.ListResourceBundle
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
 * A <code>ListResouceBundle</code> provides an easy way, to create
 * your own resource bundle.  It is an abstract class that you can
 * subclass.  You should then overwrite the getContents method, that
 * provides a key/value list.
 * <br>
 * The key/value list is a two dimensional list of Object.  The first
 * dimension ranges over the resources. The second dimension ranges
 * from zero (key) to one (value).  The keys must be of type String.
 * <br>
 * XXX Example!
 *
 * @see Locale
 * @see PropertyResourceBundle
 * @author Jochen Hoenicke */
public abstract class ListResourceBundle extends ResourceBundle
{
  /**
   * The constructor.  It does nothing special.
   */
  public ListResourceBundle()
  {
  }

  /**
   * Gets the key/value list.  You must override this method.
   * @return a two dimensional list of Objects.  The first dimension
   * ranges over the objects, and the second dimension ranges from
   * zero (key) to one (value).  
   */
  protected abstract Object[][] getContents();

  /**
   * Override this method to provide the resource for a keys.  This gets
   * called by <code>getObject</code>.
   * @param key The key of the resource.
   * @return The resource for the key or null if it doesn't exists.
   */
  public final Object handleGetObject(String key)
  {
    Object[][] contents = getContents();
    for (int i = 0; i < contents.length; i++)
      {
	if (key.equals(contents[i][0]))
	  return contents[i][1];
      }
    return null;
  }

  /**
   * This method should return all keys for which a resource exists.
   * @return An enumeration of the keys.
   */
  public Enumeration getKeys()
  {
    final Object[][] contents = getContents();
    
    return new Enumeration()
    {
      int i = 0;
      public boolean hasMoreElements()
      {
	return i < contents.length;
      }
      public Object nextElement()
      {
	return contents[i++][0];
      }
    };
  }
}
