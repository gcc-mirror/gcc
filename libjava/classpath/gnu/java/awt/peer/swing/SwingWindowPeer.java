/* SwingWindowPeer.java -- An abstract base for Swing based window peers
   Copyright (C)  2006  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.swing;

import java.awt.Window;
import java.awt.peer.ComponentPeer;
import java.awt.peer.WindowPeer;

/**
 * An abstract base class for Swing based WindowPeer implementation. Concrete
 * implementations of WindowPeers should subclass this class in order to get
 * the correct behaviour.
 *
 * As a minimum, a subclass must implement all the remaining abstract methods
 * as well as the following methods:
 * <ul>
 * <li>{@link ComponentPeer#getLocationOnScreen()}</li>
 * <li>{@link ComponentPeer#getGraphics()}</li>
 * <li>{@link ComponentPeer#createImage(int, int)}</li>
 * </ul>
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public abstract class SwingWindowPeer
  extends SwingContainerPeer
  implements WindowPeer
{

  /**
   * Creates a new instance of WindowPeer.
   *
   * @param window the AWT window
   */
  public SwingWindowPeer(Window window)
  {
    super(window);
    init(window, null);
  }

  public void updateIconImages()
  {
    // TODO: Implement properly.
  }

  public void updateMinimumSize()
  {
    // TODO: Implement properly.
  }

  public void setModalBlocked(java.awt.Dialog d, boolean b)
  {
    // TODO: Implement properly.
  }

  public void updateFocusableWindowState()
  {
    // TODO: Implement properly.
  }

  public void setAlwaysOnTop(boolean b)
  {
    // TODO: Implement properly.
  }
}
