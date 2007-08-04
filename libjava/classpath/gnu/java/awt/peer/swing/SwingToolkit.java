/* SwingToolkit.java -- A base toolkit for Swing peers
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


package gnu.java.awt.peer.swing;

import java.awt.Button;
import java.awt.Canvas;
import java.awt.Dialog;
import java.awt.Label;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.peer.ButtonPeer;
import java.awt.peer.CanvasPeer;
import java.awt.peer.LabelPeer;
import java.awt.peer.MenuBarPeer;
import java.awt.peer.MenuItemPeer;
import java.awt.peer.MenuPeer;
import java.awt.peer.PanelPeer;
import java.awt.peer.TextFieldPeer;

import gnu.java.awt.ClasspathToolkit;

/**
 * A base implementation for {@link java.awt.Toolkit} that provides the
 * Swing based widgets. Concrete implementations still must provide the
 * remaining abstract methods.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public abstract class SwingToolkit extends ClasspathToolkit
{

  /**
   * Creates a SwingButtonPeer.
   *
   * @param button the AWT button
   *
   * @return the Swing button peer
   */
  protected ButtonPeer createButton(Button button)
  {
    return new SwingButtonPeer(button);
  }

  /**
   * Creates a SwingCanvasPeer.
   *
   * @param canvas the AWT canvas
   *
   * @return the Swing canvas peer
   */
  protected CanvasPeer createCanvas(Canvas canvas)
  {
    return new SwingCanvasPeer(canvas);
  }

  /**
   * Creates a SwingLabelPeer.
   *
   * @param label the AWT label
   *
   * @return the Swing label peer
   */
  protected LabelPeer createLabel(Label label)
  {
    return new SwingLabelPeer(label);
  }

  /**
   * Creates a SwingMenuPeer.
   *
   * @param menu the AWT menu
   *
   * @return the Swing menu peer
   */
  protected MenuPeer createMenu(Menu menu)
  {
    return new SwingMenuPeer(menu);
  }

  /**
   * Creates a SwingMenuBarPeer.
   *
   * @param menuBar the AWT menubar
   *
   * @return the Swing menu bar peer
   */
  protected MenuBarPeer createMenuBar(MenuBar menuBar)
  {
    return new SwingMenuBarPeer(menuBar);
  }

  /**
   * Creates a SwingMenuItemPeer.
   *
   * @param menuItem the AWT menu item
   *
   * @return the Swing menu item peer
   */
  protected MenuItemPeer createMenuItem(MenuItem menuItem)
  {
    return new SwingMenuItemPeer(menuItem);
  }

  /**
   * Creates a SwingPanelPeer.
   *
   * @param panel the AWT panel
   *
   * @return the Swing panel peer
   */
  protected PanelPeer createPanel(Panel panel)
  {
    return new SwingPanelPeer(panel);
  }

  /**
   * Creates a SwingTextFieldPeer.
   *
   * @param textField the AWT text field
   *
   * @return the Swing text field peer
   */
  protected TextFieldPeer createTextField(TextField textField)
  {
    return new SwingTextFieldPeer(textField);
  }

  @Override
  public boolean isModalExclusionTypeSupported
  (Dialog.ModalExclusionType modalExclusionType)
  {
    return false;
  }

  @Override
  public boolean isModalityTypeSupported(Dialog.ModalityType modalityType)
  {
    return false;
  }


}
