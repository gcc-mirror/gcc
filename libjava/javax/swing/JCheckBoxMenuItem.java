/* JCheckBoxMenuItem.java --
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

import java.io.IOException;
import java.io.ObjectOutputStream;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;


/**
 * DOCUMENT ME!
 */
public class JCheckBoxMenuItem extends JMenuItem implements SwingConstants,
                                                            Accessible
{
  private static final String uiClassID = "CheckBoxMenuItemUI";
  private boolean state;
  private Object[] selectedObjects;

  /**
   * Creates a new JCheckBoxMenuItem object.
   */
  public JCheckBoxMenuItem()
  {
    this(null, null);
  }

  /**
   * Creates a new JCheckBoxMenuItem object.
   *
   * @param icon DOCUMENT ME!
   */
  public JCheckBoxMenuItem(Icon icon)
  {
    this(null, icon);
  }

  /**
   * Creates a new JCheckBoxMenuItem object.
   *
   * @param text DOCUMENT ME!
   */
  public JCheckBoxMenuItem(String text)
  {
    this(text, null);
  }

  /**
   * Creates a new JCheckBoxMenuItem object.
   *
   * @param action DOCUMENT ME!
   */
  public JCheckBoxMenuItem(Action action)
  {
    this();
    setAction(action);
  }

  /**
   * Creates a new JCheckBoxMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param icon DOCUMENT ME!
   */
  public JCheckBoxMenuItem(String text, Icon icon)
  {
    this(text, icon, false);
  }

  /**
   * Creates a new JCheckBoxMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param state DOCUMENT ME!
   */
  public JCheckBoxMenuItem(String text, boolean state)
  {
    this(text, null, state);
  }

  /**
   * Creates a new JCheckBoxMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param icon DOCUMENT ME!
   * @param state DOCUMENT ME!
   */
  public JCheckBoxMenuItem(String text, Icon icon, boolean state)
  {
    super(text, icon);
    setModel(new JToggleButton.ToggleButtonModel());
    this.state = state;
  }

  /**
   * DOCUMENT ME!
   *
   * @param stream DOCUMENT ME!
   *
   * @throws IOException DOCUMENT ME!
   */
  private void writeObject(ObjectOutputStream stream) throws IOException
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public String getUIClassID()
  {
    return uiClassID;
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public boolean getState()
  {
    return state;
  }

  /**
   * DOCUMENT ME!
   *
   * @param state DOCUMENT ME!
   */
  public synchronized void setState(boolean state)
  {
    this.state = state;
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public Object[] getSelectedObjects()
  {
    return selectedObjects;
  }

  /**
   * DOCUMENT ME!
   */
  public void requestFocus()
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  protected String paramString()
  {
    return "JCheckBoxMenuItem";
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJCheckBoxMenuItem(this);

    return accessibleContext;
  }

  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJCheckBoxMenuItem extends AccessibleJMenuItem
  {
    /**
     * Creates a new AccessibleJCheckBoxMenuItem object.
     *
     * @param component DOCUMENT ME!
     */
    protected AccessibleJCheckBoxMenuItem(JCheckBoxMenuItem component)
    {
      super(component);

      // TODO
    }

    /**
     * DOCUMENT ME!
     *
     * @return $returnType$ DOCUMENT ME!
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.CHECK_BOX;
    }
  }
}
