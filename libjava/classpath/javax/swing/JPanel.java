/* JPanel.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.FlowLayout;
import java.awt.LayoutManager;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.PanelUI;

/**
 * An instance of JPanel can be added to a panel, frame etc
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JPanel extends JComponent implements Accessible
{
  /**
   * Provides accessibility support for <code>JPanel</code>.
   *
   * @author Roman Kennke (roman@kennke.org)
   */
  protected class AccessibleJPanel extends AccessibleJComponent
  {
    /**
     * Creates a new instance of <code>AccessibleJPanel</code>.
     */
    public AccessibleJPanel()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessible role for <code>JPanel</code>, which is
     * {@link AccessibleRole#PANEL}.
     *
     * @return the accessible role for <code>JPanel</code>
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PANEL;
    }
  }

  public JPanel()
  {
    this(new FlowLayout(), true);
  }

  public JPanel(boolean double_buffered)
  {
    this(new FlowLayout(), double_buffered);
  }

  public JPanel(LayoutManager layout)
  {
    this(layout, true);
  }

  public JPanel(LayoutManager layout, boolean isDoubleBuffered)
  {
    if (layout == null)
      {
        // TODO: Is this correct? Or should we throw a NPE?
        layout = new FlowLayout();
      }
    setLayout(layout); 
    setOpaque(true); 

    updateUI();	
  } 

  public String getUIClassID()
  {
    return "PanelUI";
  }

  public void setUI(PanelUI ui)
  {
    super.setUI(ui);
  }

  public PanelUI getUI()
  {
    return (PanelUI) ui;
  }

  public void updateUI()
  {
    setUI((PanelUI) UIManager.getUI(this));
  }

  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJPanel();
    return accessibleContext;
  }
    
  protected  String paramString()
  {
	return "JPanel";
  }
}
