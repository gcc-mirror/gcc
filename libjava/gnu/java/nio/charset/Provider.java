/* Provider.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package gnu.java.nio.charset;

import java.nio.charset.Charset;
import java.nio.charset.spi.CharsetProvider;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Charset provider for the required charsets.  Used by
 * {@link Charset#charsetForName} and * {@link Charset#availableCharsets}.
 *
 * @author Jesse Rosenstock
 * @see Charset
 */
public final class Provider extends CharsetProvider
{
  private static Provider singleton;

  static
  {
    synchronized (Provider.class)
      {
        singleton = null;
      }
  }

  /**
   * Map from charset name to charset canonical name.
   */
  private final HashMap canonicalNames;

  /**
   * Map from canonical name to Charset.
   * TODO: We may want to use soft references.  We would then need to keep
   * track of the class name to regenerate the object.
   */
  private final HashMap charsets;

  private Provider ()
  {
    // FIXME: We might need to make the name comparison case insensitive.
    // Verify this with the Sun JDK.
    canonicalNames = new HashMap ();
    charsets = new HashMap ();

    // US-ASCII aka ISO646-US
    addCharset (new US_ASCII ());

    // ISO-8859-1 aka ISO-LATIN-1
    addCharset (new ISO_8859_1 ());

    // UTF-8
    addCharset (new UTF_8 ());

    // UTF-16BE
    addCharset (new UTF_16BE ());

    // UTF-16LE
    addCharset (new UTF_16LE ());

    // UTF-16
    addCharset (new UTF_16 ());
  }

  public Iterator charsets ()
  {
    return Collections.unmodifiableCollection (charsets.values ())
                      .iterator ();
  }

  public Charset charsetForName (String charsetName)
  {
    return (Charset) charsets.get (canonicalize (charsetName));
  }

  private Object canonicalize (String charsetName)
  {
    Object o = canonicalNames.get (charsetName);
    return o == null ? charsetName : o;
  }

  private void addCharset (Charset cs)
  {
    String canonicalName = cs.name ();
    charsets.put (canonicalName, cs);

    for (Iterator i = cs.aliases ().iterator (); i.hasNext (); )
      canonicalNames.put (i.next (), canonicalName);
  }

  public static synchronized Provider provider ()
  {
    if (singleton == null)
      singleton = new Provider ();
    return singleton;
  }
}
