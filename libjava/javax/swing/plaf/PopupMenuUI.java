/* PopupMenuUI.java --
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

package javax.swing.plaf;

import java.awt.event.MouseEvent;
import javax.swing.JPopupMenu;
import javax.swing.Popup;
import javax.swing.PopupFactory;


/**
 * An abstract base class for delegates that implement the pluggable
 * look and feel for a <code>JPopupMenu</code>.
 *
 * @see javax.swing.JPopupMenu
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class PopupMenuUI
  extends ComponentUI
{
  /**
   * Constructs a new <code>PopupMenuUI</code>.
   */
  public PopupMenuUI()
  {
  }


  /**
   * Tests whether or not a mouse event triggers a popup menu.
   *
   * <p>The default implementation calls
   * <code>event.isPopupTrigger()</code>, which checks for the gesture
   * that is common for the platform on which the application runs. If
   * a look and feel wants to employ non-standard conventions for
   * triggering a popup menu, it can override this method.
   *
   * @param event the event to check.
   *
   * @return <code>true</code> if the event triggers a popup menu;
   *         <code>false</code> otherwise.
   *
   * @since 1.3
   */
  public boolean isPopupTrigger(MouseEvent event)
  {
    return event.isPopupTrigger();
  }


  /**
   * Creates a <code>Popup</code> for displaying the popup menu.  The
   * default implementation uses the {@link javax.swing.PopupFactory}
   * for retrieving a suitable <code>Popup</code>, but subclasses
   * might want to override this method if a LookAndFeel needs special
   * Popups.
   *
   * @param popup the <code>JPopupMenu</code> for whose display
   *        a <code>Popup</code> is needed.
   *
   * @param x the horizontal position where the popup will be
   *        displayed.
   *
   * @param y the vertical position where the popup will be
   *        displayed.
   *
   * @return a <code>Popup</code> for showing and hiding
   *         the menu.
   *
   * @since 1.4
   */
  public Popup getPopup(JPopupMenu popup, int x, int y)
  {
    return PopupFactory.getSharedInstance().getPopup(
      /* origin/owner of the popup */ popup.getInvoker(),
      /* contents */                  popup,
      x, y);
  }
}
