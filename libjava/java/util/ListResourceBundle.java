/* ListResourceBundle -- a resource bundle build around a list
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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
 * A <code>ListResouceBundle</code> provides an easy way, to create your own
 * resource bundle. It is an abstract class that you can subclass. You should
 * then overwrite the getContents method, that provides a key/value list.
 *
 * <p>The key/value list is a two dimensional list of Object.  The first
 * dimension ranges over the resources. The second dimension ranges from
 * zero (key) to one (value). The keys must be of type String, and they are
 * case-sensitive. For example:
 *
<br><pre>public class MyResources
  extends ListResourceBundle
{
  public Object[][] getContents()
  {
    return contents;
  }

  static final Object[][] contents =
  {
    // LOCALIZED STRINGS
    {"s1", "The disk \"{1}\" contains {0}."},  // MessageFormat pattern
    {"s2", "1"},                       // location of {0} in pattern
    {"s3", "My Disk"},                 // sample disk name
    {"s4", "no files"},                // first ChoiceFormat choice
    {"s5", "one file"},                // second ChoiceFormat choice
    {"s6", "{0,number} files"}         // third ChoiceFormat choice
    {"s7", "3 Mar 96"},                // sample date
    {"s8", new Dimension(1,5)}         // real object, not just string
    // END OF LOCALIZED MATERIAL
  };
}</pre>
 *
 * @author Jochen Hoenicke
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Locale
 * @see PropertyResourceBundle
 * @since 1.1
 * @status updated to 1.4
 */
public abstract class ListResourceBundle extends ResourceBundle
{
  /**
   * The constructor. It does nothing special.
   */
  public ListResourceBundle()
  {
  }

  /**
   * Gets a resource for a given key. This is called by <code>getObject</code>.
   *
   * @param key the key of the resource
   * @return the resource for the key, or null if it doesn't exist
   */
  public final Object handleGetObject(String key)
  {
    Object[][] contents = getContents();
    int i = contents.length;
    while (--i >= 0)
      if (key.equals(contents[i][0]))
        return contents[i][1];
    return null;
  }

  /**
   * This method should return all keys for which a resource exists.
   *
   * @return an enumeration of the keys
   */
  public Enumeration getKeys()
  {
    // We make a new Set that holds all the keys, then return an enumeration
    // for that. This prevents modifications from ruining the enumeration,
    // as well as ignoring duplicates.
    final Object[][] contents = getContents();
    Set s = new HashSet();
    int i = contents.length;
    while (--i >= 0)
      s.add(contents[i][0]);
    ResourceBundle bundle = parent;
    // Eliminate tail recursion.
    while (bundle != null)
      {
        Enumeration e = bundle.getKeys();
        while (e.hasMoreElements())
          s.add(e.nextElement());
        bundle = bundle.parent;
      }
    return Collections.enumeration(s);
  }

  /**
   * Gets the key/value list. You must override this method, and should not
   * provide duplicate keys or null entries.
   *
   * @return a two dimensional list of String key / Object resouce pairs
   */
  protected abstract Object[][] getContents();
} // class ListResourceBundle
