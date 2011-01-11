/* IntegerUtil.java -- JDK 5 Integer methods with 1.4 API
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.security.util;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Utility class which offers Integer related methods found in RI's version 5
 * but written with RI's 1.4 API.
 */
public abstract class IntegerUtil
{
  /** Maximum size of our cache of constructed Integers. */
  private static final int CACHE_SIZE = 100;
  /** LRU (Least Recently Used) cache, of the last accessed 100 Integers. */
  private static final Map cache = new LinkedHashMap(CACHE_SIZE + 1, 0.75F, true)
  {
    public boolean removeEldestEntry(Map.Entry entry)
    {
      return size() > CACHE_SIZE;
    }
  };

  /** Trivial private constructor to enforce Singleton usage. */
  private IntegerUtil()
  {
    super();
  }

  /**
   * Similar to {@link Integer#valueOf(String)} except it caches the result in
   * a local LRU cache of 100 elements, organized by access order.
   * <p>
   * This method MUST be used in the gnu.java.security and gnu.javax.crypto
   * packages to ensure they would work with a version 1.4 only of the Java
   * class library API.
   *
   * @param aString a string representation of an integer.
   * @return the {@link Integer} object representing the designated string.
   */
  public static final Integer valueOf(String aString)
  {
    Integer result;
    synchronized (cache)
    {
      result = (Integer) cache.get(aString);
      if (result == null)
        {
          result = Integer.valueOf(aString);
          cache.put(aString, result);
        }
    }
    return result;
  }

  /**
   * Simulates the <code>valueOf(int)</code> method found in {@link Integer} of
   * the RI's version 1.5 using a local LRU cache of 100 elements, organized by
   * access order.
   * <p>
   * This method MUST be used in the gnu.java.security and gnu.javax.crypto
   * packages to ensure they would work with a version 1.4 only of the Java
   * class library API.
   *
   * @param anInt a decimal integer.
   * @return the {@link Integer} object representing the designated primitive.
   */
  public static final Integer valueOf(int anInt)
  {
    return valueOf(Integer.toString(anInt, 10));
  }
}
