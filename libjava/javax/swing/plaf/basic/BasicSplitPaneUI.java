/* BasicSplitPaneUI.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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

package javax.swing.plaf.basic;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeListener;
import javax.swing.JComponent;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.SplitPaneUI;

/**
 * FIXME: Stubbed to allow compiling other classes,
 * no real implementation.
 */
public class BasicSplitPaneUI
  extends SplitPaneUI
{
  protected static final String NON_CONTINUOUS_DIVIDER
    = "nonContinuousDivider";

  protected static int KEYBOARD_DIVIDER_MOVE_OFFSET;

  protected JSplitPane splitPane;
  protected BasicSplitPaneDivider divider;
  protected PropertyChangeListener propertyChangeListener;
  protected FocusListener focusListener;
  protected int dividerSize;
  protected Component nonContinuousLayoutDivider;
  protected boolean draggingHW;
  protected int beginDragDividerLocation;
  protected KeyStroke upKey;
  protected KeyStroke downKey;
  protected KeyStroke leftKey;
  protected KeyStroke rightKey;
  protected KeyStroke homeKey;
  protected KeyStroke endKey;
  protected KeyStroke dividerResizeToggleKey;
  protected ActionListener keyboardUpLeftListener;
  protected ActionListener keyboardDownRightListener;
  protected ActionListener keyboardHomeListener;
  protected ActionListener keyboardEndListener;
  protected ActionListener keyboardResizeToggleListener;

  public static ComponentUI createUI(JComponent c)
  {
    BasicSplitPaneUI newUI;

    newUI = new BasicSplitPaneUI();
    newUI.installUI(c);
    return newUI;
  }

  public BasicSplitPaneUI()
  {
    propertyChangeListener = createPropertyChangeListener();
    focusListener = createFocusListener();
  }

  public void installUI(JComponent c)
  {
  }

  protected void installDefaults()
  {
  }

  protected void installListeners()
  {
  }

  protected void installKeyboardListeners()
  {
  }

  protected void installKeyboardActions()
  {
  }

  public void uninstallUI(JComponent c)
  {
  }

  protected void uninstallDefaults()
  {
  }

  protected void uninstallListeners()
  {
  }

  protected void uninstallKeyboardActions()
  {
  }

  protected PropertyChangeListener createPropertyChangeListener()
  {
    return null;
  }

  protected FocusListener createFocusListener()
  {
    return null;
  }

  protected ActionListener createKeyboardUpLeftListener()
  {
    return null;
  }

  protected ActionListener createKeyboardDownRightListener()
  {
    return null;
  }

  protected ActionListener createKeyboardHomeListener()
  {
    return null;
  }

  protected ActionListener createKeyboardEndListener()
  {
    return null;
  }

  protected ActionListener createKeyboardResizeToggleListener()
  {
    return null;
  }

  public int getOrientation()
  {
    return splitPane.getOrientation();
  }

  public void setOrientation(int orientation)
  {
  }


  public boolean isContinuousLayout()
  {
    return false;
  }

  public void setContinuousLayout(boolean b)
  {
  }

  public int getLastDragLocation()
  {
    return 0;
  }

  public void setLastDragLocation(int l)
  {
  }


  public BasicSplitPaneDivider getDivider()
  {
    return divider;
  }


  protected Component createDefaultNonContinuousLayoutDivider()
  {
    return null;
  }

  protected void setNonContinuousLayoutDivider(Component newDivider)
  {
    setNonContinuousLayoutDivider(newDivider, true /* false? */);
  }

  protected void setNonContinuousLayoutDivider(Component newDivider,
                                               boolean rememberSizes)
  {
    nonContinuousLayoutDivider = newDivider;
  }

  public Component getNonContinuousLayoutDivider()
  {
    return nonContinuousLayoutDivider;
  }

  public JSplitPane getSplitPane()
  {
    return splitPane;
  }

  public BasicSplitPaneDivider createDefaultDivider()
  {
    return null;
  }

  public void resetToPreferredSizes(JSplitPane jc)
  {
  }

  public void setDividerLocation(JSplitPane jc, int location)
  {
  }

  public int getDividerLocation(JSplitPane jc)
  {
    return 0;
  }

  public int getMinimumDividerLocation(JSplitPane jc)
  {
    return 0;
  }

  public int getMaximumDividerLocation(JSplitPane jc)
  {
    return 0;
  }

  public void finishedPaintingChildren(JSplitPane jc, Graphics g)
  {
  }

  public void paint(Graphics g, JComponent jc)
  {
  }

  public Dimension getPreferredSize(JComponent jc)
  {
    return null;
  }

  public Dimension getMinimumSize(JComponent jc)
  {
    return null;
  }

  public Dimension getMaximumSize(JComponent jc)
  {
    return null;
  }

  public Insets getInsets(JComponent jc)
  {
    return new Insets(0, 0, 0, 0);
  }

  protected void resetLayoutManager()
  {
  }

  protected void startDragging()
  {
  }

  protected void dragDividerTo(int location)
  {
  }

  protected void finishDraggingTo(int location)
  {
  }

  protected int getDividerBorderSize()
  {
    return 0;
  }
}
