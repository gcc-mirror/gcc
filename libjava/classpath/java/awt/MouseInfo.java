/* MouseInfo.java -- utility methods for mice.
   Copyright (C) 2006 Free Software Foundation

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

package java.awt;

import gnu.java.awt.ClasspathToolkit;
import java.awt.peer.MouseInfoPeer;

/**
 * MouseInfo is a class containing utility functions for mouse information.
 *
 * @author Sven de Marothy
 * @since 1.5
 */
public class MouseInfo
{
  private static MouseInfoPeer peer;

  /**
   * Returns a PointerInfo object containing information about the current
   * location of the mouse pointer
   *
   * @throws HeadlessException if the current GraphicsEnvironment is headless.
   * @return a PointerInfo object.
   */
  public static PointerInfo getPointerInfo() throws HeadlessException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission( new AWTPermission("watchMousePointer") );

    if( GraphicsEnvironment.isHeadless() )
      throw new HeadlessException();

    if( peer == null )
      peer = Toolkit.getDefaultToolkit().getMouseInfoPeer();

    Point p = new Point();
    int screen = peer.fillPointWithCoords( p );

    GraphicsDevice[] gds = GraphicsEnvironment.getLocalGraphicsEnvironment().
      getScreenDevices();
    
    return new PointerInfo( gds[ screen ], p );
  }

  /**
   * Returns the number of mouse buttons, or -1 if no mouse is connected.
   * (mentioned in the 1.5 release notes)
   *
   * @throws HeadlessException if the current GraphicsEnvironment is headless.
   * @return an integer number of buttons.
   */
  public static int getNumberOfButtons() throws HeadlessException
  {
    if( GraphicsEnvironment.isHeadless() )
      throw new HeadlessException();
    return ((ClasspathToolkit)Toolkit.getDefaultToolkit()).
      getMouseNumberOfButtons();
  }
} 
