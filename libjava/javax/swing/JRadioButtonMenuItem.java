/* JRadioButtonMenuItem.java --
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
public class JRadioButtonMenuItem extends JMenuItem implements Accessible
{
  private static final String uiClassID = "RadioButtonMenuItemUI";

  /**
   * Creates a new JRadioButtonMenuItem object.
   */
  public JRadioButtonMenuItem()
  {
    this(null, null);
  }

  /**
   * Creates a new JRadioButtonMenuItem object.
   *
   * @param icon DOCUMENT ME!
   */
  public JRadioButtonMenuItem(Icon icon)
  {
    this(null, icon);
  }

  /**
   * Creates a new JRadioButtonMenuItem object.
   *
   * @param text DOCUMENT ME!
   */
  public JRadioButtonMenuItem(String text)
  {
    this(text, null);
  }

  /**
   * Creates a new JRadioButtonMenuItem object.
   *
   * @param action DOCUMENT ME!
   */
  public JRadioButtonMenuItem(Action action)
  {
    this();
    setAction(action);
  }

  /**
   * Creates a new JRadioButtonMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param icon DOCUMENT ME!
   */
  public JRadioButtonMenuItem(String text, Icon icon)
  {
    this(text, icon, false);
  }

  /**
   * Creates a new JRadioButtonMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param selected DOCUMENT ME!
   */
  public JRadioButtonMenuItem(String text, boolean selected)
  {
    this(text, null, selected);
  }

  /**
   * Creates a new JRadioButtonMenuItem object.
   *
   * @param icon DOCUMENT ME!
   * @param selected DOCUMENT ME!
   */
  public JRadioButtonMenuItem(Icon icon, boolean selected)
  {
    this(null, icon, selected);
  }

  /**
   * Creates a new JRadioButtonMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param icon DOCUMENT ME!
   * @param selected DOCUMENT ME!
   */
  public JRadioButtonMenuItem(String text, Icon icon, boolean selected)
  {
    super(text, icon);
    setModel(new JToggleButton.ToggleButtonModel());
    model.setSelected(selected);
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
    return "JRadioButtonMenuItem";
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJRadioButtonMenuItem(this);

    return accessibleContext;
  }

  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJRadioButtonMenuItem extends AccessibleJMenuItem
  {
    /**
     * Creates a new AccessibleJRadioButtonMenuItem object.
     *
     * @param component DOCUMENT ME!
     */
    protected AccessibleJRadioButtonMenuItem(JRadioButtonMenuItem component)
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
      return AccessibleRole.RADIO_BUTTON;
    }
  }
}
