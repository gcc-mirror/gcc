/* Charset.java -- 
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.nio.charset;

import gnu.classpath.SystemProperties;

import gnu.java.nio.charset.Provider;
import gnu.java.nio.charset.iconv.IconvProvider;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.spi.CharsetProvider;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * @author Jesse Rosenstock
 * @since 1.4
 * @status updated to 1.5
 */
public abstract class Charset implements Comparable
{
  private CharsetEncoder cachedEncoder;
  private CharsetDecoder cachedDecoder;
 
  /**
   * Charset providers.
   */
  private static CharsetProvider[] providers;
  
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

    cachedEncoder = null;
    cachedDecoder = null;
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

  /**
   * Returns the system default charset.
   *
   * This may be set by the user or VM with the file.encoding
   * property.
   */
  public static Charset defaultCharset()
  {
    String encoding;
    
    try 
      {
	encoding = SystemProperties.getProperty("file.encoding");
      }
    catch(SecurityException e)
      {
	// Use fallback.
	encoding = "ISO-8859-1";
      }
    catch(IllegalArgumentException e)
      {
	// Use fallback.
	encoding = "ISO-8859-1";
      }

    try
      {
	return forName(encoding);
      }
    catch(UnsupportedCharsetException e)
      {
	// Ignore.
      }
    catch(IllegalCharsetNameException e)
      {
	// Ignore.
      }
    catch(IllegalArgumentException e)
      {
	// Ignore.
      }
    
    throw new IllegalStateException("Can't get default charset!");
  }

  public static boolean isSupported (String charsetName)
  {
    return charsetForName (charsetName) != null;
  }

  /**
   * Returns the Charset instance for the charset of the given name.
   * 
   * @param charsetName
   * @return
   * @throws UnsupportedCharsetException if this VM does not support
   * the charset of the given name.
   * @throws IllegalCharsetNameException if the given charset name is
   * legal.
   * @throws IllegalArgumentException if <code>charsetName</code> is null.
   */
  public static Charset forName (String charsetName)
  {
    // Throws IllegalArgumentException as the JDK does.
    if(charsetName == null)
        throw new IllegalArgumentException("Charset name must not be null.");
    
    Charset cs = charsetForName (charsetName);
    if (cs == null)
      throw new UnsupportedCharsetException (charsetName);
    return cs;
  }

  /**
   * Retrieves a charset for the given charset name.
   *
   * @return A charset object for the charset with the specified name, or
   * <code>null</code> if no such charset exists.
   *
   * @throws IllegalCharsetNameException  if the name is illegal
   */
  private static Charset charsetForName(String charsetName)
  {
    checkName (charsetName);
    Charset cs = null;
    CharsetProvider[] providers = providers2();
    for (int i = 0; i < providers.length; i++)
      {
        cs = providers[i].charsetForName(charsetName);
        if (cs != null)
	  break;
      }
    return cs;
  }

  public static SortedMap availableCharsets()
  {
    TreeMap charsets = new TreeMap(String.CASE_INSENSITIVE_ORDER);

    CharsetProvider[] providers = providers2();
    for (int j = 0; j < providers.length; j++)
      {
        for (Iterator i = providers[j].charsets(); i.hasNext(); )
          {
            Charset cs = (Charset) i.next();
            charsets.put(cs.name(), cs);
          }
      }

    return Collections.unmodifiableSortedMap(charsets);
  }

  private static CharsetProvider provider()
  {
    String useIconv = SystemProperties.getProperty
      ("gnu.classpath.nio.charset.provider.iconv");

    if (useIconv != null)
      return IconvProvider.provider();

    return Provider.provider();
  }

  /**
   * We need to support multiple providers, reading them from
   * java.nio.charset.spi.CharsetProvider in the resource directory
   * META-INF/services.
   */
  private static CharsetProvider[] providers2()
  {
    if (providers == null)
      {
        try
          {
            Enumeration en = ClassLoader.getSystemResources
	      ("META-INF/services/java.nio.charset.spi.CharsetProvider");
            LinkedHashSet set = new LinkedHashSet();
            set.add(provider());
            while (en.hasMoreElements())
              {
                BufferedReader rdr = new BufferedReader(new InputStreamReader
                  (((URL) (en.nextElement())).openStream()));
                while (true)
                  {
                    String s = rdr.readLine();
                    if (s == null)
		      break;
                    CharsetProvider p =
		      (CharsetProvider) ((Class.forName(s)).newInstance());
                    set.add(p);
                  }
               }

            providers = new CharsetProvider[set.size()];
            set.toArray(providers);
          }
        catch (Exception e)
          {
            throw new RuntimeException(e);
          }
      }
    return providers;
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

  // NB: This implementation serializes different threads calling
  // Charset.encode(), a potential performance problem.  It might
  // be better to remove the cache, or use ThreadLocal to cache on
  // a per-thread basis.
  public final synchronized ByteBuffer encode (CharBuffer cb)
  {
    try
      {
	if (cachedEncoder == null)
	  {
	    cachedEncoder = newEncoder ()
	      .onMalformedInput (CodingErrorAction.REPLACE)
	      .onUnmappableCharacter (CodingErrorAction.REPLACE);
	  } else
	  cachedEncoder.reset();
	return cachedEncoder.encode (cb);
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

  // NB: This implementation serializes different threads calling
  // Charset.decode(), a potential performance problem.  It might
  // be better to remove the cache, or use ThreadLocal to cache on
  // a per-thread basis.
  public final synchronized CharBuffer decode (ByteBuffer bb)
  {
    try
      {
	if (cachedDecoder == null)
	  {
	    cachedDecoder = newDecoder ()
	      .onMalformedInput (CodingErrorAction.REPLACE)
	      .onUnmappableCharacter (CodingErrorAction.REPLACE);
	  } else
	  cachedDecoder.reset();

	return cachedDecoder.decode (bb);
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
