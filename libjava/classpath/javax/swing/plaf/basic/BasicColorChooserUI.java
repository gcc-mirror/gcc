/* BasicColorChooserUI.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

import java.awt.BorderLayout;
import java.awt.Container;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JColorChooser;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import javax.swing.colorchooser.ColorChooserComponentFactory;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ColorChooserUI;
import javax.swing.plaf.ComponentUI;

/**
 * This is the UI Class for the JColorChooser in the Basic Look and Feel.
 */
public class BasicColorChooserUI extends ColorChooserUI
{
  /**
   * This helper class handles property changes from the JColorChooser.
   */
  public class PropertyHandler implements PropertyChangeListener
  {
    /**
     * This method is called when any of the properties of the JColorChooser
     * change.
     *
     * @param e The PropertyChangeEvent.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName() == JColorChooser.CHOOSER_PANELS_PROPERTY)
	makeTabs(chooser.getChooserPanels());
      else if (e.getPropertyName() == JColorChooser.PREVIEW_PANEL_PROPERTY)
	updatePreviewPanel(chooser.getPreviewPanel());
      else if (e.getPropertyName() == JColorChooser.SELECTION_MODEL_PROPERTY)
	((AbstractColorChooserPanel) pane.getSelectedComponent())
	.updateChooser();

      chooser.repaint();
    }
  }

  /**
   * This is a helper class that listens to the Model of the JColorChooser for
   * color change events so it can update the preview panel.
   */
  private class PreviewListener implements ChangeListener
  {
    /**
     * This method is called whenever the JColorChooser's color changes.
     *
     * @param e The ChangeEvent.
     */
    public void stateChanged(ChangeEvent e)
    {
      if (pane != null)
        {
	  AbstractColorChooserPanel panel = (AbstractColorChooserPanel) pane
	                                    .getSelectedComponent();
	  if (panel != null)
	    panel.updateChooser();
        }
      chooser.repaint();
    }
  }

  /**
   * This helper class listens to the JTabbedPane that is used for tab
   * changes.
   */
  private class TabPaneListener implements ChangeListener
  {
    /**
     * This method is called whenever a different tab is selected in the
     * JTabbedPane.
     *
     * @param e The ChangeEvent.
     */
    public void stateChanged(ChangeEvent e)
    {
      // Need to do this because we don't update all the tabs when they're not
      // visible, so they are not informed of new colors when they're hidden.
      AbstractColorChooserPanel comp = (AbstractColorChooserPanel) pane
                                       .getSelectedComponent();
      comp.updateChooser();
    }
  }

  /** An array of default choosers to use in the JColorChooser. */
  protected AbstractColorChooserPanel[] defaultChoosers;

  /** The listener for the preview panel. */
  protected ChangeListener previewListener;

  /** The PropertyChangeListener for the JColorChooser. */
  protected PropertyChangeListener propertyChangeListener;

  /**
   * The JColorChooser.
   * This is package-private to avoid an accessor method.
   */
  JColorChooser chooser;

  /** The JTabbedPane that is used. */
  JTabbedPane pane;

  /** The Container that holds the preview panel. */
  private Container prevContainer;

  /**
   * Creates a new BasicColorChooserUI object.
   */
  public BasicColorChooserUI()
  {
    super();
  }

  /**
   * This method creates a new UI Component for the given JComponent.
   *
   * @param c The JComponent to create an UI for.
   *
   * @return A new BasicColorChooserUI.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicColorChooserUI();
  }

  /**
   * This method creates the default chooser panels for the JColorChooser.
   *
   * @return The default chooser panels.
   */
  protected AbstractColorChooserPanel[] createDefaultChoosers()
  {
    return ColorChooserComponentFactory.getDefaultChooserPanels();
  }

  /**
   * This method installs the UI Component for the given JComponent.
   *
   * @param c The JComponent to install this UI for.
   */
  public void installUI(JComponent c)
  {
    if (c instanceof JColorChooser)
      {
	chooser = (JColorChooser) c;
	chooser.setLayout(new BorderLayout());

	// Do this first, so we avoid doing work for property change events.
	defaultChoosers = createDefaultChoosers();
	chooser.setChooserPanels(defaultChoosers);
	pane = new JTabbedPane();

	pane.addChangeListener(new ChangeListener()
	    {
	      public void stateChanged(ChangeEvent e)
	      {
		pane.repaint();
	      }
	    });

	makeTabs(defaultChoosers);

	chooser.add(pane, BorderLayout.NORTH);

	installPreviewPanel();

	installDefaults();
	installListeners();
      }
  }

  /**
   * This method adds tabs to the JTabbedPane for the chooserPanels defined in
   * the JColorChooser.
   * This is package-private to avoid an accessor method.
   *
   * @param panels The Panels that need tabs to be made for them.
   */
  void makeTabs(AbstractColorChooserPanel[] panels)
  {
    pane.removeAll();
    for (int i = 0; i < panels.length; i++)
      pane.addTab(panels[i].getDisplayName(), panels[i].getSmallDisplayIcon(),
                  panels[i]);
  }

  /**
   * This method uninstalls this UI for the given JComponent.
   *
   * @param c The JComponent that will have this UI removed.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallDefaults();

    pane = null;
    chooser = null;
  }

  /**
   * This method installs the preview panel for the JColorChooser.
   */
  protected void installPreviewPanel()
  {
    updatePreviewPanel(ColorChooserComponentFactory.getPreviewPanel());
  }

  /**
   * This is a helper method that swaps the existing preview panel with the
   * given panel.
   * This is package-private to avoid an accessor method.
   *
   * @param preview The new preview panel.
   */
  void updatePreviewPanel(JComponent preview)
  {
    if (prevContainer == null)
      {
	prevContainer = new JPanel();
	prevContainer.setLayout(new BorderLayout());
	chooser.add(prevContainer, BorderLayout.CENTER);
      }
    prevContainer.removeAll();
    prevContainer.add(preview, BorderLayout.CENTER);
  }

  /**
   * This method installs the default properties given by the Basic Look and
   * Feel.
   */
  protected void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    chooser.setFont(defaults.getFont("ColorChooser.font"));
    chooser.setForeground(defaults.getColor("ColorChooser.foreground"));
    chooser.setBackground(defaults.getColor("ColorChooser.background"));
  }

  /**
   * This method uninstalls the default properties given by the Basic Look and
   * Feel.
   */
  protected void uninstallDefaults()
  {
    chooser.setBackground(null);
    chooser.setForeground(null);
    chooser.setFont(null);
  }

  /**
   * This method installs any listeners required for this UI to function.
   */
  protected void installListeners()
  {
    propertyChangeListener = createPropertyChangeListener();
    previewListener = new PreviewListener();

    chooser.addPropertyChangeListener(propertyChangeListener);
    chooser.getSelectionModel().addChangeListener(previewListener);

    pane.addChangeListener(new TabPaneListener());
  }

  /**
   * This method creates the PropertyChangeListener used for listening to the
   * JColorChooser.
   *
   * @return A PropertyChangeListener.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyHandler();
  }

  /**
   * This method uninstalls any listeners that were previously installed by
   * the UI.
   */
  protected void uninstallListeners()
  {
    chooser.removePropertyChangeListener(propertyChangeListener);
    chooser.getSelectionModel().removeChangeListener(previewListener);

    previewListener = null;
    propertyChangeListener = null;
  }
}
