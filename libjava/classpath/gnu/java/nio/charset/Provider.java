/* Provider.java --
   Copyright (C) 2002, 2005, 2006 Free Software Foundation, Inc.

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

package gnu.java.nio.charset;

import java.nio.charset.Charset;
import java.nio.charset.spi.CharsetProvider;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Charset provider for the required charsets.  Used by
 * {@link Charset#charsetForName} and * {@link Charset#availableCharsets}.
 *
 * Note: This class is a privileged class, because it can be instantiated without
 * requiring the RuntimePermission("charsetProvider"). There is a check in
 * java.nio.charset.spi.CharsetProvider to skip the security check if the provider
 * is an instance of this class.
 *
 * @author Jesse Rosenstock
 * @author Robert Schuster (thebohemian@gmx.net)
 * @see Charset
 */
public final class Provider extends CharsetProvider
{
  private static Provider singleton;

  /**
   * Map from charset name to charset canonical name. The strings
   * are all lower-case to allow case-insensitive retrieval of
   * Charset instances.
   */
  private final HashMap<String, String> canonicalNames;

  /**
   * Map from lower-case canonical name to Charset.
   * TODO: We may want to use soft references.  We would then need to keep
   * track of the class name to regenerate the object.
   */
  private final HashMap<String, Charset> charsets;

  /**
   * We don't load all available charsets at the start
   */
  private boolean extendedLoaded;

  // Package private to avoid an accessor method in PrivilegedAction below.
  Provider ()
  {
    extendedLoaded = false;
    canonicalNames = new HashMap<String,String> ();
    charsets = new HashMap<String,Charset> ();

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

    // UTF-16LE (marked)
    addCharset (new UnicodeLittle ());

    // Windows-1250 aka cp-1250 (East European)
    addCharset (new Windows1250 ());

    // Windows-1251 (Cyrillic)
    addCharset (new Windows1251 ());

    // Windows-1252 aka cp-1252 (Latin-1)
    addCharset (new Windows1252 ());

    // Windows-1253 (Greek)
    addCharset (new Windows1253 ());

    // Windows-1254 (Turkish)
    addCharset (new Windows1254 ());

    // Windows-1257 (Baltic)
    addCharset (new Windows1257 ());

    // ISO-8859-2 aka ISO-LATIN-2
    addCharset (new ISO_8859_2 ());

    // ISO-8859-4 aka ISO-LATIN-4
    addCharset (new ISO_8859_4 ());

    // ISO-8859-5 (Cyrillic)
    addCharset (new ISO_8859_5 ());

    // ISO-8859-7 (Greek)
    addCharset (new ISO_8859_7 ());

    // ISO-8859-9 aka ISO-LATIN-5
    addCharset (new ISO_8859_9 ());

    // ISO-8859-13 aka ISO-LATIN-7
    addCharset (new ISO_8859_13 ());

    // ISO-8859-15 aka ISO-LATIN-9
    addCharset (new ISO_8859_15 ());

    // KOI8 (Cyrillic)
    addCharset (new KOI_8 ());
  }

 /**
  * Load non-mandatory charsets.
  */
  private synchronized void loadExtended ()
  {
    if (extendedLoaded)
      return;

    addCharset (new ISO_8859_3 ());    // ISO-8859-3 aka ISO-LATIN-3
    addCharset (new ISO_8859_6 ());    // ISO-8859-6 (Arabic)
    addCharset (new ISO_8859_8 ());    // ISO-8859-8 (Hebrew)

    // Some more codepages
    addCharset (new Cp424());
    addCharset (new Cp437());
    addCharset (new Cp737());
    addCharset (new Cp775());
    addCharset (new Cp850());
    addCharset (new Cp852());
    addCharset (new Cp855()); // IBM Cyrillic
    addCharset (new Cp857()); // IBM Turkish
    addCharset (new Cp860()); // MSDOS Portugese
    addCharset (new Cp861()); // MSDOS Icelandic
    addCharset (new Cp862()); // PC Hebrew
    addCharset (new Cp863()); // MSDOS Can. French
    addCharset (new Cp864()); // PC Arabic
    addCharset (new Cp865()); // MSDOS Nordic
    addCharset (new Cp866()); // MSDOS Russian
    addCharset (new Cp869()); // IBM modern Greek
    addCharset (new Cp874()); // IBM Thai

    addCharset (new MacCentralEurope());
    addCharset (new MacCroatian());
    addCharset (new MacCyrillic());
    addCharset (new MacDingbat());
    addCharset (new MacGreek());
    addCharset (new MacIceland());
    addCharset (new MacRoman());
    addCharset (new MacRomania());
    addCharset (new MacSymbol());
    addCharset (new MacThai());
    addCharset (new MacTurkish());
    addCharset (new MS874());

    addCharset (new Windows1255());
    addCharset (new Windows1256());
    addCharset (new Windows1258());

    extendedLoaded = true;
  }

  public Iterator<Charset> charsets ()
  {
    loadExtended();
    return Collections.unmodifiableCollection (charsets.values ())
                      .iterator ();
  }

  /**
   * Returns a Charset instance by converting the given
   * name to lower-case, looking up the canonical charset
   * name and finally looking up the Charset with that name.
   *
   * <p>The lookup is therefore case-insensitive.</p>
   *
   *  @returns The Charset having <code>charsetName</code>
   *  as its alias or null if no such Charset exist.
   */
  public Charset charsetForName (String charsetName)
  {
    Charset cs = (Charset) charsets.get(canonicalNames.get(charsetName.toLowerCase()));
    if (cs == null)
     {
       loadExtended();
       cs = (Charset) charsets.get(canonicalNames.get(charsetName.toLowerCase()));
     }
    return cs;
  }

  /**
   * Puts a Charset under its canonical name into the 'charsets' map.
   * Then puts a mapping from all its alias names to the canonical name.
   *
   * <p>All names are converted to lower-case</p>.
   *
   * @param cs
   */
  private void addCharset (Charset cs)
  {
    String canonicalName = cs.name().toLowerCase();
    charsets.put (canonicalName, cs);

    /* Adds a mapping between the canonical name
     * itself making a lookup using that name
     * no special case.
     */
    canonicalNames.put(canonicalName, canonicalName);

    for (Iterator<String> i = cs.aliases ().iterator (); i.hasNext (); )
      canonicalNames.put (((String) i.next()).toLowerCase(), canonicalName);
  }

  public static synchronized Provider provider ()
  {
    // The default provider is safe to instantiate.
    if (singleton == null)
      singleton = AccessController.doPrivileged
        (new PrivilegedAction<Provider>()
          {
            public Provider run()
            {
              return new Provider();
            }
          });
    return singleton;
  }
}
