/* InputMethod.java -- defines an interface for complex text input
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

import java.awt.AWTEvent;
import java.awt.Rectangle;
import java.util.Locale;

/**
 * This interface supports complex text input, often for situations where
 * the text is more complex than a keyboard will accomodate. For example,
 * this can be used for Chinese, Japanese, and Korean, where multiple
 * keystrokes are necessary to compose text. This could also support things
 * like phonetic English, or reordering Thai.
 *
 * <p>These contexts can be loaded by the input method framework, using
 * {@link InputContext#selectInputMethod(Locale)}.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.3
 * @status updated to 1.4
 */
public interface InputMethod
{
  /**
   * Set the input method context, which ties the input method to a client
   * component. This is called once automatically when creating the input
   * method.
   *
   * @param context the context for this input method
   * @throws NullPointerException if context is null
   */
  void setInputMethodContext(InputMethodContext context);

  /**
   * Sets the input locale. If the input method supports that locale, it
   * changes its behavior to be consistent with the locale and returns true.
   * Otherwise, it returns false. This is called by
   * {@link InputContext#selectInputMethod(Locale)} when the user specifies
   * a locale, or when the previously selected input method had a locale.
   *
   * @param locale the locale to use for input
   * @return true if the change is successful
   * @throws NullPointerException if locale is null
   */
  boolean setLocale(Locale locale);

  /**
   * Returns the current input locale, or null if none is defined. This is
   * called by {@link InputContext#getLocale()}, or before switching input
   * methods.
   *
   * @return the current input locale, or null
   */
  Locale getLocale();

  /**
   * Sets the allowed Unicode subsets that this input method can use. Null
   * indicates that all characters are allowed. This is called after creation,
   * or when switching to this input method, by
   * {@link InputContext#setCharacterSubsets(Character.Subset[])}.
   *
   * @param subsets the accepted subsets for this input method, or null for all
   */
  void setCharacterSubsets(Character.Subset[] subsets);

  /**
   * Changes the enabled status of this input method. An enabled input method
   * accepts incoming events for composition and control purposes, while a
   * disabled input method ignores events (except for control purposes). This
   * is called by {@link InputContext#setCompositionEnabled(boolean)} or when
   * switching from an input method if the previous input method returned
   * without exception on {@link #isCompositionEnabled()}.
   *
   * @param enable whether to enable this input method
   * @throws UnsupportedOperationException if enabling/disabling is unsupported
   * @see #isCompositionEnabled()
   */
  void setCompositionEnabled(boolean enable);

  /**
   * Find out if this input method is enabled. This is called by
   * {@link InputContext#isCompositionEnabled()}, or when switching input
   * methods via {@link InputContext#selectInputMethod(Locale)}.
   *
   * @return true if this input method is enabled
   * @throws UnsupportedOperationException if enabling/disabling is unsupported
   * @see #setCompositionEnabled(boolean)
   */
  boolean isCompositionEnabled();

  /**
   * Starts a reconversion operation. The input method gets its text from the
   * client, using {@link InputMethodRequests#getSelectedText(Attribute[])}.
   * Then the composed and committed text produced by the operation is sent
   * back to the client using a sequence of InputMethodEvents. This is called
   * by {@link InputContext#reconvert()}.
   *
   * @throws UnsupportedOperationException if reconversion is unsupported
   */
  void reconvert();

  /**
   * Dispatch an event to the input method. If input method support is enabled,
   * certain events are dispatched to the input method before the client
   * component or event listeners. The input method must either consume the
   * event or pass it on to the component. Instances of InputEvent, including
   * KeyEvent and MouseEvent, are given to this input method. This method is
   * called by {@link InputContext#dispatchEvent(AWTEvent)}.
   *
   * @param event the event to dispatch
   * @throws NullPointerException if event is null
   */
  void dispatchEvent(AWTEvent event);

  /**
   * Notify this input method of changes in the client window. This is called
   * when notifications are enabled (see {@link
   * InputMethodContext#enableClientWindowNotification(InputMethod, boolean)},
   * if {@link #removeNotify(Component)} has not been called. The following
   * situations trigger a notification:<ul>
   * <li>The client window changes in location, size, visibility,
   * iconification, or is closed.</li>
   * <li>When enabling client notification (or on the first activation after
   * enabling if no client existed at the time).</li>
   * <li>When activating a new client after <code>removeNotify</code> was
   * called on a previous client.</li>
   * </ul>
   *
   * @param bounds the client window's current bounds, or null
   */
  void notifyClientWindowChange(Rectangle bounds);

  /**
   * Activate this input method for input processing. If the input method
   * provides its own windows, it should make them open and visible at this
   * time. This method is called when a client component receives a
   * FOCUS_GAINED event, or when switching to this input method from another
   * one. It is only called when the input method is inactive, assuming that
   * new instances begin in an inactive state.
   */
  void activate();

  /**
   * Deactivate this input method, either temporarily or permanently for the
   * given client. If the input method provides its own windows, it should
   * only close those related to the current composition (such as a lookup
   * choice panel), while leaving more persistant windows (like a control
   * panel) open to avoid screen flicker. Before control is given to another
   * input method, {@link #hideWindows()} will be called on this instance.
   * This method is called when a client component receives a
   * FOCUS_LOST event, when switching to another input method, or before
   * {@link #removeNotify()} when the client is removed.
   *
   * @param isTemporary true if the focus change is temporary
   */
  void deactivate(boolean isTemporary);

  /**
   * Close or hide all windows opened by this input method. This is called
   * before activating a different input method, and before calling
   * {@link #dispose()} on this instance. It is only called when the input
   * method is inactive.
   */
  void hideWindows();

  /**
   * Notify the input method that a client component has been removed from its
   * hierarchy, or that input method support has been disabled. This is
   * called by {@link InputContext#removeNotify()}, and only when the input
   * method is inactive.
   */
  void removeNotify();

  /**
   * End any input composition currently taking place. Depending on the
   * platform and user preferences, this may commit or delete uncommitted text,
   * using input method events. This may be called for a variety of reasons,
   * such as when the user moves the insertion point in the client text outside
   * the range of the composed text, or when text is saved to file. This is
   * called by {@link InputContext#endComposition()}, when switching to a
   * new input method, or by {@link InputContext#selectInputMethod(Locale)}.
   */
  void endComposition();

  /**
   * Disposes the input method and release any resources it is using. In
   * particular, the input method should dispose windows and close files. This
   * is called by {@link InputContext#dispose()}, when the input method is
   * inactive; and nothing will be called on this instance afterwards.
   */
  void dispose();

  /**
   * Returns a control object from this input method, or null. A control object
   * provides method to control the behavior of this input method, as well as
   * query information about it. The object is implementation dependent, so
   * clients must compare the result against known input method control
   * object types. This is called by
   * {@link InputContext#getInputMethodControlObject()}.
   *
   * @return the control object, or null
   */
  Object getControlObject();
} // interface InputMethod
