/* AccessibleBundle.java -- base class for accessibility "enumerations"
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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

package javax.accessibility;

import java.util.Locale;

/**
 * This serves as a base class for accessibility "enumerations".  These
 * objects are strongly typed; to make up for the lack of true enums in Java.
 * Display should be locale dependent.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see AccessibleRole
 * @see AccessibleState
 * @since 1.2
 * @status updated to 1.4, but missing Locale support
 */
public abstract class AccessibleBundle
{
  /**
   * The locale independent name of the object. This is for the computer, not
   * necessarily for humans; changing it in subclasses is frowned upon.
   *
   * @see #toDisplayString(String, Locale)
   */
  protected String key;

  /**
   * Default constructor.
   */
  public AccessibleBundle()
  {
  }

  /**
   * Obtains the key as a localized string, falling back to the
   * locale-independent version if necessary.
   *
   * @param resourceBundle the resource to use for lookup
   * @param locale the locale to translate to
   * @return the translated name
   * @throws NullPointerException if resourceBundle or locale is null
   * @XXX For now, no transformation is done.
   */
  protected String toDisplayString(String resourceBundle, Locale locale)
  {
    return key;
  }

  /**
   * Obtains the key as a localized string, falling back to the
   * locale-independent version if necessary.
   *
   * @param locale the locale to translate to
   * @return the translated name
   * @throws NullPointerException if locale is null
   * @XXX For now, no transformation is done.
   */
  public String toDisplayString(Locale locale)
  {
    return key;
  }

  /**
   * Obtains the key as a localized string, using the default locale.
   *
   * @return the translated name
   * @XXX For now, no transformation is done.
   */
  public String toDisplayString()
  {
    return toDisplayString(Locale.getDefault());
  }

  /**
   * Obtains the key as a localized string, using the default locale.
   *
   * @return the translated name
   * @XXX For now, no transformation is done.
   */
  public String toString()
  {
    return toDisplayString(Locale.getDefault());
  }
} // class AccessibleBundle
