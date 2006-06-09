/* BasicTextAreaUI.java -- 
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


import java.beans.PropertyChangeEvent;

import javax.swing.JComponent;
import javax.swing.JTextArea;
import javax.swing.UIDefaults;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.Element;
import javax.swing.text.PlainView;
import javax.swing.text.View;
import javax.swing.text.WrappedPlainView;

public class BasicTextAreaUI extends BasicTextUI
{
  public static ComponentUI createUI(JComponent comp)
  {
    return new BasicTextAreaUI();
  }

  public BasicTextAreaUI()
  {
    // Nothing to do here.
  }

  /**
   * Create the view.  Returns a WrappedPlainView if the text area
   * has lineWrap set to true, otherwise returns a PlainView.  If
   * lineWrap is true has to check whether the wrap style is word 
   * or character and return an appropriate WrappedPlainView.
   * 
   * @param elem the element to create a View for
   * @return an appropriate View for the element
   */
  public View create(Element elem)
  {
    JTextArea comp = (JTextArea) getComponent();
    if (comp.getLineWrap())
      {
        if (comp.getWrapStyleWord())
          return new WrappedPlainView(elem, true);
        else
          return new WrappedPlainView(elem, false);
      }
    else
      return new PlainView(elem);
  }

  /**
   * Returns the prefix for entries in the {@link UIDefaults} table.
   *
   * @return "TextArea"
   */
  protected String getPropertyPrefix()
  {
    return "TextArea";
  }
  
  /**
   * Receives notification whenever one of the text component's bound
   * properties changes. This changes the view to WrappedPlainView
   * if setLineWrap(true) is called, and back to PlainView if 
   * setLineWrap(false) is called.
   *
   * @param ev the property change event
   */
  protected void propertyChange(PropertyChangeEvent ev)
  {
    JTextArea comp = (JTextArea) getComponent();
    if (ev.getPropertyName() == "lineWrap"
        || ev.getPropertyName() == "wrapStyleWord")
      {
        // Changes the View (without modifying the document or it's listeners).
        setView(create(textComponent.getDocument().getDefaultRootElement()));
      }
  }
}
