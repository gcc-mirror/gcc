/* InputMethodDescriptor.java -- enables loading and use of an input method
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

package java.awt.im.spi;

import java.awt.AWTException;
import java.awt.Image;
import java.util.Locale;

/**
 * This interface provides information about an InputMethod before it is
 * loaded.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.3
 * @status updated to 1.4
 */
public interface InputMethodDescriptor
{
  /**
   * Returns the locales supported by the input method this describes. This
   * allows the selection of input methods by locale (by language only, or
   * also by country and variant), via
   * {@link InputContext#selectInputMethod(Locale)}. The returned list should
   * ignore pass-through locales, so it is usually a subset of locales for
   * which {@link InputMethod#setContext(Locale)} returns true. If
   * {@link #hasDynamicLocaleList()} returns true, this is called each time
   * information is needed, allowing dynamic addition or removal of supported
   * locales.
   *
   * @return the list of supported locales
   * @throws AWTException if the input method is not available
   */
  Locale[] getAvailableLocales() throws AWTException;

  /**
   * Test whether the input method this describes has a static or dynamic
   * locale list. For example, this would return true if the list of supported
   * locales depends on adapters currently loaded over a network.
   *
   * @return true if the locale list is dynamic
   */
  boolean hasDynamicLocaleList();

  /**
   * Returns a user visible name of the input locale, displayed in the
   * specified locale. The inputLocale parameter must be one obtained from
   * the list in {@link #getAvailableLocales()}, or null for a
   * locale-independent description of the input method. If a translation to
   * the desired display language is not available, another language may be
   * used.
   *
   * @param inputLocale the locale of the input method, or null
   * @param displayLanguage the language of the result
   * @return the name of the input method when using the given inputLocale
   */
  String getInputMethodDisplayName(Locale inputLocale,
                                   Locale displayLanguage);

  /**
   * Returns a 16x16 icon for the input locale. The inputLocale parameter
   * must be one obtained from the list in {@link #getAvailableLocales()}, or
   * null for a locale-independent icon for the input method.
   *
   * @param inputLocale the locale of the input method, or null
   * @return a 16x16 icon for the input method when using the given inputLocale
   */
  Image getInputMethodIcon(Locale inputLocale);

  /**
   * Creates a new instance of the input method.
   *
   * @return the newly created input method
   * @throws Exception if anything goes wrong
   */
  InputMethod createInputMethod() throws Exception;

} // interface InputMethodDescriptor

