/* IconvProvider.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package gnu.java.nio.charset.iconv;

import java.nio.charset.Charset;
import java.nio.charset.spi.CharsetProvider;
import java.util.Iterator;
import java.util.Vector;

/**
 * Charset provider wrapping iconv.
 *
 * Note: This class is a privileged class, because it can be instantiated without
 * requiring the RuntimePermission("charsetProvider"). There is a check in
 * java.nio.charset.spi.CharsetProvider to skip the security check if the provider
 * is an instance of this class.
 *
 * @author Sven de Marothy
 */
public final class IconvProvider extends CharsetProvider
{
  private static IconvProvider singleton;

  // Declaring the construtor public may violate the use of singleton.
  // But it must be public so that an instance of this class can be
  // created by Class.newInstance(), which is the case when this provider is
  // defined in META-INF/services/java.nio.charset.spi.CharsetProvider.
  public IconvProvider()
  {
    IconvMetaData.setup();
  }

  public Iterator charsets()
  {
    Vector names = IconvMetaData.charsets();
    Vector charsets = new Vector();
    for (int i = 0; i < names.size(); i++)
      {
	try
	  {
	    charsets.add(new IconvCharset((IconvMetaData) names.elementAt(i)));
	  }
	catch (IllegalArgumentException e)
	  {
	  }
      }
    return charsets.iterator();
  }

  public Charset charsetForName(String charsetName)
  {
    try
      {
	IconvMetaData info = IconvMetaData.get(charsetName);

	// Try anyway if the set isn't found.
	if (info == null)
	  info = new IconvMetaData(charsetName, 2.0f, 2.0f, 2.0f, 2.0f,
	                           new String[] {  }, charsetName);
	return new IconvCharset(info);
      }
    catch (IllegalArgumentException e)
      {
	return null;
      }
  }

  public static synchronized IconvProvider provider()
  {
    if (singleton == null)
      singleton = new IconvProvider();
    return singleton;
  }
}
