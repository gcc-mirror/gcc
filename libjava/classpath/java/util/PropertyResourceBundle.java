/* PropertyResourceBundle -- a resource bundle built from a Property file
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

import java.io.IOException;
import java.io.InputStream;

/**
 * This class is a concrete <code>ResourceBundle</code> that gets it
 * resources from a property file. This implies that the resources are
 * strings. For more information about resource bundles see the class
 * <code>ResourceBundle</code>.
 *
 * You should not use this class directly, or subclass it, but you get
 * an object of this class automatically when you call
 * <code>ResourceBundle.getBundle()</code> and there is a properties
 * file.
 *
 * If there is also a class for this resource and the same locale, the
 * class will be chosen. The properties file should have the name of the
 * resource bundle, appended with the locale (e.g. <code>_de</code> and the
 * extension <code>.properties</code>. The file should have the same format
 * as for <code>Properties.load()</code>
 *
 * An example of a properties file for the german language is given
 * here. This extends the example given in ListResourceBundle.
 * Create a file MyResource_de.properties with the following contents
 * and put it in the CLASSPATH. (The char <code>\u00e4</code> is the
 * german umlaut)
 *
 *
<pre>
s1=3
s2=MeineDisk
s3=3. M\u00e4rz 96
s4=Die Diskette ''{1}'' enth\u00e4lt {0} in {2}.
s5=0
s6=keine Dateien
s7=1
s8=eine Datei
s9=2
s10={0,number} Dateien
s11=Die Formatierung warf eine Exception: {0}
s12=FEHLER
s13=Ergebnis
s14=Dialog
s15=Auswahlkriterium
s16=1,3
</pre>
 *
 * @author Jochen Hoenicke
 * @see ResourceBundle
 * @see ListResourceBundle
 * @see Properties#load(InputStream)
 * @since 1.1
 * @status updated to 1.4
 */
public class PropertyResourceBundle extends ResourceBundle
{
  /** The properties file this bundle is based on. */
  private Properties properties;

  /**
   * Creates a new property resource bundle.
   *
   * @param stream an input stream, where the resources are read from
   * @throws NullPointerException if stream is null
   * @throws IOException if reading the stream fails
   */
  public PropertyResourceBundle(InputStream stream) throws IOException
  {
    properties = new Properties();
    properties.load(stream);
  }

  /**
   * Called by <code>getObject</code> when a resource is needed. This
   * returns the resource given by the key.
   *
   * @param key the key of the resource
   * @return the resource for the key, or null if it doesn't exist
   */
  public Object handleGetObject(String key)
  {
    return properties.getProperty(key);
  }

  /**
   * This method should return all keys for which a resource exists.
   *
   * @return an enumeration of the keys
   */
  public Enumeration getKeys()
  {
    if (parent == null)
      return properties.propertyNames();
    // We make a new Set that holds all the keys, then return an enumeration
    // for that. This prevents modifications from ruining the enumeration,
    // as well as ignoring duplicates.
    Set s = new HashSet();
    Enumeration e = properties.propertyNames();
    while (e.hasMoreElements())
      s.add(e.nextElement());
    ResourceBundle bundle = parent;
    // Eliminate tail recursion.
    do
      {
        e = bundle.getKeys();
        while (e.hasMoreElements())
          s.add(e.nextElement());
        bundle = bundle.parent;
      }
    while (bundle != null);
    return Collections.enumeration(s);
  }
} // class PropertyResourceBundle
