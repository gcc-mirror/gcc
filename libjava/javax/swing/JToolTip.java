/* JToolTip.java --
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


package javax.swing;

import java.awt.AWTEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.ToolTipUI;

/**
 * This class is used to display ToolTips. ToolTips are small floating windows
 * that display text when the mouse comes to rest over a Component. ToolTips
 * are set for JComponents using JComponent.setToolTipText(String).
 */
public class JToolTip extends JComponent implements Accessible
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = -1138929898906751643L;

  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJToolTip extends AccessibleJComponent
  {
    /**
     * Creates a new AccessibleJToolTip object.
     */
    protected AccessibleJToolTip()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getAccessibleDescription()
    {
      return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleRole getAccessibleRole()
    {
      return null;
    }
  }

  /** The text to display in the JToolTip. */
  String text;

  /** The JComponent this JToolTip is used for. */
  JComponent component;

  /**
   * Creates a new JToolTip object.
   */
  public JToolTip()
  {
    disableEvents(AWTEvent.MOUSE_EVENT_MASK);
    updateUI();
  }

  /**
   * This method returns the text this JToolTip displays.
   *
   * @return The text that this JToolTip displays.
   */
  public String getTipText()
  {
    return text;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  /**
   * This method returns the JComponent this JToolTip displays for.
   *
   * @return The JComponent this JToolTip displays for.
   */
  public JComponent getComponent()
  {
    return component;
  }

  /**
   * This method returns the UI responsible for displaying this JToolTip.
   *
   * @return The UI responsible for displaying this JToolTip.
   */
  public ToolTipUI getUI()
  {
    return (ToolTipUI) ui;
  }

  /**
   * This method returns the String identifier for the UI class.
   *
   * @return The String identifier for the UI class.
   */
  public String getUIClassID()
  {
    return "ToolTipUI";
  }

  /**
   * This method returns a debugging String describing the JToolTip.
   *
   * @return A debugging String describing the JToolTip.
   */
  protected String paramString()
  {
    return "JToolTip";
  }

  /**
   * This method sets the JComponent that the JToolTip displays for.
   *
   * @param c The JComponent that the JToolTip displays for.
   */
  public void setComponent(JComponent c)
  {
    component = c;
  }

  /**
   * This method sets the text that the JToolTip displays.
   *
   * @param tipText The text that the JToolTip displays.
   */
  public void setTipText(String tipText)
  {
    text = tipText;
  }

  /**
   * This method resets the UI used to the Look and Feel default.
   */
  public void updateUI()
  {
    setUI((ToolTipUI) UIManager.getUI(this));
    revalidate();
    repaint();
  }
}
