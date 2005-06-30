/* MissingResourceException.java -- thrown for a missing resource
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

/**
 * This exception is thrown when a resource is missing.
 *
 * @author Jochen Hoenicke
 * @author Warren Levy (warrenl@cygnus.com)
 * @see ResourceBundle
 * @since 1.1
 * @status updated to 1.4
 */
public class MissingResourceException extends RuntimeException
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -4876345176062000401L;

  /**
   * The name of the resource bundle requested by user.
   *
   * @serial the class name of the resource bundle
   */
  private final String className;

  /**
   * The key of the resource in the bundle requested by user.
   *
   * @serial the name of the resouce
   */
  private final String key;

  /**
   * Creates a new exception, with the specified parameters.
   *
   * @param s the detail message
   * @param className the name of the resource bundle
   * @param key the key of the missing resource
   */
  public MissingResourceException(String s, String className, String key)
  {
    super(s);
    this.className = className;
    this.key = key;
  }

  /**
   * Gets the name of the resource bundle, for which a resource is missing.
   *
   * @return the name of the resource bundle
   */
  public String getClassName()
  {
    return className;
  }

  /**
   * Gets the key of the resource that is missing bundle, this is an empty
   * string if the whole resource bundle is missing.
   *
   * @return the name of the resource bundle
   */
  public String getKey()
  {
    return key;
  }
}
