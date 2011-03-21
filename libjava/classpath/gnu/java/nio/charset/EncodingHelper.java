/* EncodingHelper.java -- Useful character encoding methods.
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import java.util.HashMap;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import java.nio.charset.Charset;
import java.io.UnsupportedEncodingException;

/**
 * This class provides some useful utility methods
 * for charset encoding for the java.lang and java.io methods.
 *
 * @author Sven de Marothy
 */
public class EncodingHelper
{

    /**
     * Contains the mapping from java.io canonical names
     * to java.nio canonical names.
     */
    private static final HashMap<String,String> canonicalNames;

    static {
        canonicalNames = new HashMap<String,String>();
        canonicalNames.put("US-ASCII", "ASCII");
        canonicalNames.put("windows-1250", "Cp1250");
        canonicalNames.put("windows-1251", "Cp1251");
        canonicalNames.put("windows-1252", "Cp1252");
        canonicalNames.put("windows-1253", "Cp1253");
        canonicalNames.put("windows-1254", "Cp1254");
        canonicalNames.put("windows-1257", "Cp1257");
        canonicalNames.put("ISO-8859-1", "ISO8859_1");
        canonicalNames.put("ISO-8859-2", "ISO8859_2");
        canonicalNames.put("ISO-8859-4", "ISO8859_4");
        canonicalNames.put("ISO-8859-5", "ISO8859_5");
        canonicalNames.put("ISO-8859-7", "ISO8859_7");
        canonicalNames.put("ISO-8859-9", "ISO8859_9");
        canonicalNames.put("ISO-8859-13", "ISO8859_13");
        canonicalNames.put("ISO-8859-15", "ISO8859_15");
        canonicalNames.put("KOI8-R", "KOI8_R");
        canonicalNames.put("UTF-8", "UTF8");
        canonicalNames.put("UTF-16BE", "UnicodeBigUnmarked");
        canonicalNames.put("UTF-16LE", "UnicodeLittleUnmarked");
        canonicalNames.put("windows-1255", "Cp1255");
        canonicalNames.put("windows-1256", "Cp1256");
        canonicalNames.put("windows-1258", "Cp1258");
        canonicalNames.put("ISO-8859-3", "ISO8859_3");
        canonicalNames.put("ISO-8859-6", "ISO8859_6");
        canonicalNames.put("ISO-8859-8", "ISO8859_8");
    }

    /**
     * Returns the name of the default encoding,
     * falls back on defaults to Latin-1 if there's a problem.
     */
    public static String getDefaultEncoding()
    {
        try
            {
                return System.getProperty("file.encoding");
            } catch(SecurityException e) {
            } catch(IllegalArgumentException e) {
            }
        // XXX - Throw an error here? For now, default to the 'safe' encoding.
        return "8859_1";
    }

    /**
     * Returns the java.io canonical name of a charset given with the
     * java.nio canonical name. If the charset does not have a java.io
     * canonical name, the input string is returned.
     */
    public static String getOldCanonical(String newCanonical)
    {
        String oldCanonical = (String) canonicalNames.get(newCanonical);
        return (oldCanonical != null)?oldCanonical : newCanonical;
    }

    public static boolean isISOLatin1(String s)
    {
        if(s.equals("ISO-8859-1") ||
           s.equals("8859_1") ||
           s.equals("ISO_8859-1") ||
           s.equals("latin1") ||
           s.equals("ISO8859_1") ||
           s.equals("ISO_8859_1"))
            return true;
        return false;
    }

   /**
    * Gets a charset, throwing the java.io exception and not
    * the java.nio exception if an error occurs.
    */
    public static Charset getCharset(String name)
        throws UnsupportedEncodingException
    {
     try
     {
       return Charset.forName(name);
     }
     catch(IllegalCharsetNameException e)
     {
       throw new UnsupportedEncodingException("Charset "+name+" not found.");
     }
     catch(UnsupportedCharsetException e)
     {
       throw new UnsupportedEncodingException("Charset "+name+" not found.");
     }
   }

  /**
   * Returns the default charset without throwing any exceptions. The default
   * charset is UTF8.
   *
   * @return the default charset
   */
  public static Charset getDefaultCharset()
  {
    return new UTF_8();
  }
}
