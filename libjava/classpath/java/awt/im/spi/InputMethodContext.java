/* InputMethodContext.java -- communication between an input method and client
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


package java.awt.im.spi;

import java.awt.HeadlessException;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.font.TextHitInfo;
import java.awt.im.InputMethodRequests;
import java.text.AttributedCharacterIterator;

import javax.swing.JFrame;

/**
 * Provides methods for the communication context between an input method
 * and the client component. This should be passed to
 * {@link InputMethod#setInputMethodContext(InputMethodContext)}.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.3
 * @status updated to 1.4
 */
public interface InputMethodContext extends InputMethodRequests
{
  /**
   * Create an input method event and dispatch it to the client.
   *
   * @param id the event type
   * @param text an iterator over the text to be committed
   * @param count the count of characters to be committed
   * @param caret the insertion point of the commit, or null
   * @param visiblePosition the best location to make visible, or null
   */
  void dispatchInputMethodEvent(int id, AttributedCharacterIterator text,
                                int count, TextHitInfo caret,
                                TextHitInfo visiblePosition);

  /**
   * Creates a top-level window for use by the input method. This window should
   * float above all document windows and dialogs, not receive focus, and have
   * lightweight decorations (such as no title, reduced drag regions). But
   * this behavior may be modified to meet the platform style. The title may
   * or may not be displayed, depending on the platform.
   *
   * <p>If attachToInputContext is true, the new window will share the input
   * context of the input method, so that events in the new window are
   * dispatched to the input method. Also, this supresses deactivate and
   * activate calls to the input method caused by setVisible.
   *
   * @param title the window title, if one is displayed; null becomes ""
   * @param attachToInputContext true for the window to share context with
   *        the input method
   * @return the new window for use by the input method
   * @throws HeadlessException if GraphicsEnvironment.isHeadless is true
   */
  Window createInputMethodWindow(String title, boolean attachToInputContext);

  /**
   * Creates a top-level Swing JFrame for use by the input method. This frame
   * should float above all document windows and dialogs, not receive focus,
   * and have lightweight decorations (such as no title, reduced drag
   * regions). But this behavior may be modified to meet the platform style.
   * The title may or may not be displayed, depending on the platform.
   *
   * <p>If attachToInputContext is true, the new window will share the input
   * context of the input method, so that events in the new window are
   * dispatched to the input method. Also, this supresses deactivate and
   * activate calls to the input method caused by setVisible.
   *
   * @param title the window title, if one is displayed; null becomes ""
   * @param attachToInputContext true for the window to share context with
   *        the input method
   * @return the new window for use by the input method
   * @throws HeadlessException if GraphicsEnvironment.isHeadless is true
   * @since 1.4
   */
  JFrame createInputMethodJFrame(String title, boolean attachToInputContext);

  /**
   * Sets whether notification of the client window's location and state should
   * be enabled for the input method. When enabled, the input method's
   * {@link InputMethod#notifyClientWindowChange(Rectangle)} method is called.
   * Notification is automatically disabled when the input method is disposed.
   *
   * @param inputMethod the method to change status of
   * @param enable true to enable notification
   */
  void enableClientWindowNotification(InputMethod inputMethod, boolean enable);
} // interface InputMethodContext
