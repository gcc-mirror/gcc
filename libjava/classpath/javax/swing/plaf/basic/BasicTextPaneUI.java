/* BasicTextPaneUI.java -- 
   Copyright (C) 2004  Free Software Foundation, Inc.

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

import javax.swing.JComponent;
import javax.swing.JTextPane;
import javax.swing.plaf.ColorUIResource;
import javax.swing.UIDefaults;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

public class BasicTextPaneUI extends BasicEditorPaneUI
{
  public BasicTextPaneUI()
  {
    // Do nothing here.
  }

  public static ComponentUI createUI(JComponent comp)
  {
    return new BasicTextPaneUI();
  }

  /**
   * Returns the prefix for entries in the {@link UIDefaults} table.
   *
   * @return "TextPane"
   */
  protected String getPropertyPrefix()
  {
    return "TextPane";
  }

  /**
   * Installs this UI on the specified <code>JTextPane</code>. This calls the
   * super implementation and then adds a default style to the text pane.
   *
   * @param c the text pane to install the UI to
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    JTextPane tp = (JTextPane) c;
    Style defaultStyle = tp.getStyle(StyleContext.DEFAULT_STYLE);
    defaultStyle.addAttribute(StyleConstants.Foreground,
                              new ColorUIResource(Color.BLACK));
    defaultStyle.addAttribute(StyleConstants.FontFamily, "Serif");
    defaultStyle.addAttribute(StyleConstants.Italic, Boolean.FALSE);
    defaultStyle.addAttribute(StyleConstants.Bold, Boolean.FALSE);
    defaultStyle.addAttribute(StyleConstants.FontSize, new Integer(12));
  }
}
