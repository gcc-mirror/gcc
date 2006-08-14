/* BasicTextFieldUI.java
   Copyright (C) 2004, 2006,  Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.beans.PropertyChangeEvent;

import javax.swing.JComponent;
import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.Element;
import javax.swing.text.FieldView;
import javax.swing.text.View;

public class BasicTextFieldUI extends BasicTextUI
{
  public BasicTextFieldUI()
  {
    super();
  }

  public View create(Element elem)
  {
    return new FieldView(elem);
  }
  
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicTextFieldUI();
  }

  /**
   * Returns the prefix for entries in the {@link UIDefaults} table.
   *
   * @return "TextField"
   */
  protected String getPropertyPrefix()
  {
    return "TextField";
  }

  public void installUI(JComponent c)
  {
    super.installUI(c);
  }

  /**
   * Receives notification whenever one of the text component's bound
   * properties changes. Here we check for the editable and enabled
   * properties and adjust the background color accordingly.
   * 
   * <p>The colors are only changed if they are not a
   * <code>ColorUIResource</code>.</p>
   *
   * @param event the property change event
   */
  protected void propertyChange(PropertyChangeEvent event)
  {
    if (event.getPropertyName().equals("editable"))
      {
        // Changing the color only if the current background is an instance of
        // ColorUIResource is the behavior of the RI.
        if (textComponent.getBackground() instanceof ColorUIResource)
          {
            Color c = null;
            Color old = textComponent.getBackground();
            String prefix = getPropertyPrefix();
            if (! textComponent.isEnabled())
              c = SharedUIDefaults.getColor(prefix + ".disabledBackground");
            if (c == null && ! textComponent.isEditable())
              c = SharedUIDefaults.getColor(prefix + ".inactiveBackground");
            if (c == null)
              c = SharedUIDefaults.getColor(prefix + ".background");
            if (c != null && c != old)
              {
                textComponent.setBackground(c);
              }
          }
      }
  }
}
