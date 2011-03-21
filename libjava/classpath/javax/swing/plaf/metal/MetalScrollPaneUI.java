/* MetalScrollPaneUI.java
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

import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollPaneUI;

/**
 * A UI delegate for the {@link JScrollPane} component.
 */
public class MetalScrollPaneUI
  extends BasicScrollPaneUI
{
  /**
   * Constructs a new instance of <code>MetalScrollPaneUI</code>.
   */
  public MetalScrollPaneUI()
  {
    super();
  }

  /**
   * Returns a shared instance of <code>MetalScrollPaneUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A shared instance of <code>MetalScrollPaneUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalScrollPaneUI();
  }

  /**
   * Configures the specified component appropriate for the look and feel.
   * This method is invoked when the ComponentUI instance is being installed
   * as the UI delegate on the specified component. This method should
   * completely configure the component for the look and feel,
   * including the following:
   * 1. Install any default property values for color, fonts, borders,
   * icons, opacity, etc. on the component. Whenever possible, property
   * values initialized by the client program should not be overridden.
   * 2. Install a LayoutManager on the component if necessary.
   * 3. Create/add any required sub-components to the component.
   * 4. Create/install event listeners on the component.
   * 5. Create/install a PropertyChangeListener on the component in order
   * to detect and respond to component property changes appropriately.
   * 6. Install keyboard UI (mnemonics, traversal, etc.) on the component.
   * 7. Initialize any appropriate instance data.
   *
   * @param c - the component to install the ui on
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    JScrollBar hsb = scrollpane.getHorizontalScrollBar();
    hsb.putClientProperty(MetalScrollBarUI.FREE_STANDING_PROP, Boolean.FALSE);
    JScrollBar vsb = scrollpane.getVerticalScrollBar();
    vsb.putClientProperty(MetalScrollBarUI.FREE_STANDING_PROP, Boolean.FALSE);
  }

  /**
   * Reverses configuration which was done on the specified component
   * during installUI. This method is invoked when this UIComponent
   * instance is being removed as the UI delegate for the specified
   * component. This method should undo the configuration performed in
   * installUI, being careful to leave the JComponent instance in a
   * clean state (no extraneous listeners, look-and-feel-specific property
   *  objects, etc.). This should include the following:
   *  1. Remove any UI-set borders from the component.
   *  2. Remove any UI-set layout managers on the component.
   *  3. Remove any UI-added sub-components from the component.
   *  4. Remove any UI-added event/property listeners from the component.
   *  5. Remove any UI-installed keyboard UI from the component.
   *  6. Nullify any allocated instance data objects to allow for GC.
   *
   *  @param c - the component to uninstall the ui on
   */
  public void uninstallUI(JComponent c)
  {
    JScrollBar hsb = scrollpane.getHorizontalScrollBar();
    hsb.putClientProperty(MetalScrollBarUI.FREE_STANDING_PROP, null);
    JScrollBar vsb = scrollpane.getVerticalScrollBar();
    vsb.putClientProperty(MetalScrollBarUI.FREE_STANDING_PROP, null);
    super.uninstallUI(c);
  }

  /**
   * Installs listeners on scrollPane
   *
   * @param scrollPane - the component to install the listeners on
   */
  public void installListeners(JScrollPane scrollPane)
  {
    super.installListeners(scrollPane);
  }

  /**
   * Uninstalls listeners on scrollPane
   *
   * @param scrollPane - the component to uninstall the listeners on
   */
  public void uninstallListeners(JScrollPane scrollPane)
  {
    super.uninstallListeners(scrollPane);
  }

  /**
   * TODO
   *
   * @return TODO
   */
  protected PropertyChangeListener createScrollBarSwapListener()
  {
    // FIXME: Anything else to do here?
    return super.createPropertyChangeListener();
  }
}
