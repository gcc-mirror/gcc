/* CharsetProvider.java -- charset service provider interface
   Copyright (C) 2002 Free Software Foundation

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

package java.nio.charset.spi;

import java.nio.charset.Charset;
import java.util.Iterator;


/**
 * This class allows an implementor to provide additional character sets. The
 * subclass must have a nullary constructor, and be attached to charset
 * implementation classes. These extensions are loaded via the context class
 * loader. To provide the charset extension, all files named
 * <code>META-INF/services/java.nio.charset.spi.CharsetProvider</code> are
 * read from the classpath. Each one should be a UTF-8 encoded list of
 * fully-qualified names of concrete subclasses of this class; whitespace is
 * ignored, and '#' starts comments. Duplicates are ignored. The
 * implementations must be accessible to the classloader that requests them.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Charset
 * @since 1.4
 * @status updated to 1.4
 */
public abstract class CharsetProvider
{
  /**
   * Initialize a new charset provider. This performs a security check on
   * RuntimePermission("charsetProvider").
   *
   * @throws SecurityException if building a new set is not allowed
   */
  protected CharsetProvider()
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkPermission(new RuntimePermission("charsetProvider"));
  }

  /**
   * Returns an iterator over the charsets defined by this provider.
   *
   * @return the iterator
   * @see Charset#availableCharsets()
   */
  public abstract Iterator charsets();

  /**
   * Returns the named charset, by canonical name or alias.
   *
   * @param name the name of the character
   *
   * @return the charset, or null if not supported
   */
  public abstract Charset charsetForName(String name);
} // class CharsetProvider
