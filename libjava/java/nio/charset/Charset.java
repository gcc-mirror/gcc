/* Charset.java -- 
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

package java.nio.charset;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.spi.CharsetProvider;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import gnu.java.nio.charset.Provider;

/**
 * @author Jesse Rosenstock
 * @since 1.4
 */
public abstract class Charset implements Comparable
{
  private static CharsetEncoder cachedEncoder;
  private static CharsetDecoder cachedDecoder;
 
  static
  {
    synchronized (Charset.class)
      {
        cachedEncoder = null;
        cachedDecoder = null;
      }
  }

  private final String canonicalName;
  private final String[] aliases;
  
  protected Charset (String canonicalName, String[] aliases)
  {
    checkName (canonicalName);
    if (aliases != null)
      {
        int n = aliases.length;
        for (int i = 0; i < n; ++i)
            checkName (aliases[i]);
      }

    this.canonicalName = canonicalName;
    this.aliases = aliases;
  }

  /**
   * @throws IllegalCharsetNameException  if the name is illegal
   */
  private static void checkName (String name)
  {
    int n = name.length ();

    if (n == 0)
      throw new IllegalCharsetNameException (name);

    char ch = name.charAt (0);
    if (!(('A' <= ch && ch <= 'Z')
          || ('a' <= ch && ch <= 'z')
          || ('0' <= ch && ch <= '9')))
      throw new IllegalCharsetNameException (name);

    for (int i = 1; i < n; ++i)
      {
        ch = name.charAt (i);
        if (!(('A' <= ch && ch <= 'Z')
              || ('a' <= ch && ch <= 'z')
              || ('0' <= ch && ch <= '9')
              || ch == '-' || ch == '.' || ch == ':' || ch == '_'))
          throw new IllegalCharsetNameException (name);
      }
  }

  public static boolean isSupported (String charsetName)
  {
    return charsetForName (charsetName) != null;
  }
 
  public static Charset forName (String charsetName)
  {
    Charset cs = charsetForName (charsetName);
    if (cs == null)
      throw new UnsupportedCharsetException (charsetName);
    return cs;
  }

  /**
   * Retrieves a charset for the given charset name.
   *
   * @return A charset object for the charset with the specified name, or
   *   <code>null</code> if no such charset exists.
   *
   * @throws IllegalCharsetNameException  if the name is illegal
   */
  private static Charset charsetForName (String charsetName)
  {
    checkName (charsetName);
    return provider ().charsetForName (charsetName);
  }

  public static SortedMap availableCharsets ()
  {
    TreeMap charsets = new TreeMap (String.CASE_INSENSITIVE_ORDER);

    for (Iterator i = provider ().charsets (); i.hasNext (); )
      {
        Charset cs = (Charset) i.next ();
        charsets.put (cs.name (), cs);
      }

    return Collections.unmodifiableSortedMap (charsets);
  }

  // XXX: we need to support multiple providers, reading them from
  // java.nio.charset.spi.CharsetProvider in the resource directory
  // META-INF/services
  private static final CharsetProvider provider ()
  {
    return Provider.provider ();
  }

  public final String name ()
  {
    return canonicalName;
  }

  public final Set aliases ()
  {
    if (aliases == null)
      return Collections.EMPTY_SET;

    // should we cache the aliasSet instead?
    int n = aliases.length;
    HashSet aliasSet = new HashSet (n);
    for (int i = 0; i < n; ++i)
        aliasSet.add (aliases[i]);
    return Collections.unmodifiableSet (aliasSet);
  }

  public String displayName ()
  {
    return canonicalName;
  }

  public String displayName (Locale locale)
  {
    return canonicalName;
  }

  public final boolean isRegistered ()
  {
    return (!canonicalName.startsWith ("x-")
            && !canonicalName.startsWith ("X-"));
  }

  public abstract boolean contains (Charset cs);

  public abstract CharsetDecoder newDecoder ();

  public abstract CharsetEncoder newEncoder ();

  public boolean canEncode ()
  {
    return true;
  }

  public final ByteBuffer encode (CharBuffer cb)
  {
    try
      {
        // NB: This implementation serializes different threads calling
        // Charset.encode(), a potential performance problem.  It might
        // be better to remove the cache, or use ThreadLocal to cache on
        // a per-thread basis.
        synchronized (Charset.class)
          {
            if (cachedEncoder == null)
              {
                cachedEncoder = newEncoder ()
                  .onMalformedInput (CodingErrorAction.REPLACE)
                  .onUnmappableCharacter (CodingErrorAction.REPLACE);
              }

            return cachedEncoder.encode (cb);
          }
      }
    catch (CharacterCodingException e)
      {
        throw new AssertionError (e);
      }
  }
  
  public final ByteBuffer encode (String str)
  {
    return encode (CharBuffer.wrap (str));
  }

  public final CharBuffer decode (ByteBuffer bb)
  {
    try
      {
        // NB: This implementation serializes different threads calling
        // Charset.decode(), a potential performance problem.  It might
        // be better to remove the cache, or use ThreadLocal to cache on
        // a per-thread basis.
        synchronized (Charset.class)
          {
            if (cachedDecoder == null)
              {
                cachedDecoder = newDecoder ()
                  .onMalformedInput (CodingErrorAction.REPLACE)
                  .onUnmappableCharacter (CodingErrorAction.REPLACE);
              }

            return cachedDecoder.decode (bb);
          }
      }
    catch (CharacterCodingException e)
      {
        throw new AssertionError (e);
      }
  }

  public final int compareTo (Object ob)
  {
    return canonicalName.compareToIgnoreCase (((Charset) ob).canonicalName);
  }

  public final int hashCode ()
  {
    return canonicalName.hashCode ();
  }

  public final boolean equals (Object ob)
  {
    if (ob instanceof Charset)
      return canonicalName.equalsIgnoreCase (((Charset) ob).canonicalName);
    else
      return false;
  }

  public final String toString ()
  {
    return canonicalName;
  }
}
