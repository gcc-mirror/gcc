/* MetalInternalFrameUI.java
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf.metal;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.ActionMap;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicInternalFrameUI;

/**
 * A UI delegate for the {@link JInternalFrame} component.
 */
public class MetalInternalFrameUI
  extends BasicInternalFrameUI
{
  /** 
   * The key (<code>JInternalFrame.isPalette</code>) for the client property 
   * that controls whether the internal frame is displayed using the palette 
   * style. 
   */
  protected static String IS_PALETTE = "JInternalFrame.isPalette";

  /**
   * Constructs a new instance of <code>MetalInternalFrameUI</code>.
   * 
   * @param frame  the frame.
   */
  public MetalInternalFrameUI(JInternalFrame frame)
  {
    super(frame);
  }

  /**
   * Returns an instance of <code>MetalInternalFrameUI</code>.
   *
   * @param component the internal frame.
   *
   * @return an instance of <code>MetalInternalFrameUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalInternalFrameUI((JInternalFrame) component);
  }
  
  /**
   * Sets the fields and properties for the component.
   * 
   * @param c  the component.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    JInternalFrame f = (JInternalFrame) c;
    boolean isPalette = false;
    Boolean p = (Boolean) f.getClientProperty(IS_PALETTE);
    if (p != null)
      isPalette = p.booleanValue();
    setPalette(isPalette);
  }

  /**
   * Creates and returns the component that will be used for the north pane
   * of the {@link JInternalFrame}.  
   * 
   * @param w  the internal frame.
   * 
   * @return A new instance of {@link MetalInternalFrameTitlePane}.
   */
  protected JComponent createNorthPane(JInternalFrame w)
  {
    titlePane = new MetalInternalFrameTitlePane(w);
    return titlePane;  
  }
  
  /**
   * Sets the state of the {@link JInternalFrame} to reflect whether or not
   * it is using the palette style.  When a frame is displayed as a palette,
   * it uses a different border and the title pane is drawn differently.
   * 
   * @param isPalette  use the palette style?
   */
  public void setPalette(boolean isPalette)
  {
    MetalInternalFrameTitlePane title = (MetalInternalFrameTitlePane) northPane;
    title.setPalette(isPalette);
    if (isPalette)
      frame.setBorder(new MetalBorders.PaletteBorder());
    else
      frame.setBorder(new MetalBorders.InternalFrameBorder());
  }
 
  /** A listener that is used to handle IS_PALETTE property changes. */
  private PropertyChangeListener paletteListener;
  
  /**
   * Adds the required listeners.
   */
  protected void installListeners()
  {
    super.installListeners(); 
    paletteListener = new PropertyChangeListener() 
    {
      public void propertyChange(PropertyChangeEvent e)
      {
        if (e.getPropertyName().equals(IS_PALETTE))
          {
            if (Boolean.TRUE.equals(e.getNewValue()))
              setPalette(true);
            else
              setPalette(false);
          }
      }
    };
    frame.addPropertyChangeListener(paletteListener);
  }
  
  /**
   * Removes the listeners used.
   */
  protected void uninstallListeners()
  {
    super.uninstallListeners();
    frame.removePropertyChangeListener(IS_PALETTE, paletteListener);
    paletteListener = null;
  }

  /**
   * Installs keyboard actions. This is overridden to remove the
   * <code>showSystemMenu</code> Action that is installed by the
   * <code>BasicInternalFrameUI</code>, since Metal JInternalFrames don't have
   * a system menu.
   */
  protected void installKeyboardActions()
  {
    super.installKeyboardActions();
    ActionMap am = SwingUtilities.getUIActionMap(frame);
    if (am != null)
      {
        am.remove("showSystemMenu");
      }
  }
}
