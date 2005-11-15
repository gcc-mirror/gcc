/* JColorChooser.java --
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

import java.awt.AWTError;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import javax.swing.colorchooser.ColorSelectionModel;
import javax.swing.colorchooser.DefaultColorSelectionModel;
import javax.swing.plaf.ColorChooserUI;


/**
 * A Swing widget that offers users different ways to
 * select a color. By default, three different panels are presented to the
 * user that are capable of changing the selected color. There are three ways
 * to utilize JColorChooser. The first is to build a JColorChooser and add it
 * to the content pane. The second is to use the createDialog method to
 * create a JDialog that holds a JColorChooser. The third is to show a
 * JColorChooser in a JDialog directly using the showDialog method.
 *
 * @author original author unknown
 */
public class JColorChooser extends JComponent implements Accessible
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = 9168066781620640889L;

  /**
   * Accessibility support for <code>JColorChooser</code>.
   */
  protected class AccessibleJColorChooser
    extends JComponent.AccessibleJComponent
  {
    /** DOCUMENT ME! */
    private static final long serialVersionUID = -2038297864782299082L;

    /**
     * Constructor AccessibleJColorChooser
     */
    protected AccessibleJColorChooser()
    {
      // Nothing to do here.
    }

    /**
     * getAccessibleRole
     *
     * @return AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.COLOR_CHOOSER;
    } // getAccessibleRole()
  } // AccessibleJColorChooser

  /** The model used with the JColorChooser. */
  private ColorSelectionModel selectionModel;

  /** The preview panel associated with the JColorChooser. */
  private JComponent previewPanel;

  /**
   * The set of AbstractColorChooserPanels associated with the JColorChooser.
   */
  private AbstractColorChooserPanel[] chooserPanels;

  /** A Drag and Drop property. */
  private boolean dragEnabled;

  /**
   * The property fired by the JColorChooser when the selectionModel property
   * changes.
   */
  public static final String SELECTION_MODEL_PROPERTY = "selectionModel";

  /**
   * The property fired by the JColorChooser when the previewPanel property
   * changes.
   */
  public static final String PREVIEW_PANEL_PROPERTY = "previewPanel";

  /**
   * The property fired by the JColorChooser when the chooserPanels property
   * changes.
   */
  public static final String CHOOSER_PANELS_PROPERTY = "chooserPanels";

  /** accessibleContext */
  protected AccessibleContext accessibleContext;

  /**
   * This method creates a new JColorChooser with the default initial color.
   */
  public JColorChooser()
  {
    this(new DefaultColorSelectionModel());
  } // JColorChooser()

  /**
   * This method creates a new JColorChooser with the given initial color.
   *
   * @param initial The initial color.
   */
  public JColorChooser(Color initial)
  {
    this(new DefaultColorSelectionModel(initial));
  } // JColorChooser()

  /**
   * This method creates a new JColorChooser with the given model. The model
   * will dictate what the initial color for the JColorChooser is.
   *
   * @param model The Model to use with the JColorChooser.
   */
  public JColorChooser(ColorSelectionModel model)
  {
    if (model == null)
      model = new DefaultColorSelectionModel();
    selectionModel = model;
    updateUI();
  } // JColorChooser()

  /**
   * This method sets the current color for the JColorChooser.
   *
   * @param color The new color for the JColorChooser.
   */
  public void setColor(Color color)
  {
    if (color != null)
      selectionModel.setSelectedColor(color);
  } // setColor()

  /**
   * This method sets the current color for the JColorChooser using RGB
   * values.
   *
   * @param r The red value.
   * @param g The green value.
   * @param b The blue value.
   */
  public void setColor(int r, int g, int b)
  {
    selectionModel.setSelectedColor(new Color(r, g, b));
  } // setColor()

  /**
   * This method sets the current color for the JColorChooser using the
   * integer value. Bits 0-7 represent the blue value. Bits 8-15 represent
   * the green value. Bits 16-23 represent the red value.
   *
   * @param color The new current color of the JColorChooser.
   */
  public void setColor(int color)
  {
    setColor(new Color(color, false));
  } // setColor()

  /**
   * This method shows a JColorChooser inside a JDialog. The JDialog will
   * block until it is hidden. The JDialog comes with three buttons: OK,
   * Cancel, and Reset. Pressing OK or Cancel hide the JDialog. Pressing
   * Reset will reset the JColorChooser to its initial value.
   *
   * @param component The Component that parents the JDialog.
   * @param title The title displayed in the JDialog.
   * @param initial The initial color.
   *
   * @return The selected color.
   */
  public static Color showDialog(Component component, String title,
                                 Color initial)
  {
    JColorChooser choose = new JColorChooser(initial);

    JDialog dialog = createDialog(component, title, true, choose, null, null);

    dialog.getContentPane().add(choose);
    dialog.pack();
    dialog.show();

    return choose.getColor();
  } // showDialog()

  /**
   * This is a helper method to make the given JDialog block until it is
   * hidden.  This is package-private to avoid an accessor method.
   *
   * @param dialog The JDialog to block.
   */
  static void makeModal(JDialog dialog)
  {
    try
      {
        synchronized (dialog)
          {
            while (dialog.isVisible())
              dialog.wait();
          }
      }
    catch (InterruptedException e)
      {
        // TODO: Should this be handled?
      }
  }

  /**
   * This is a helper method to find the first Frame or Dialog ancestor of the
   * given Component.
   *
   * @param c The Component to find ancestors for.
   *
   * @return A Frame or Dialog ancestor. Null if none are found.
   */
  private static Component findParent(Component c)
  {
    Component parent = SwingUtilities.getAncestorOfClass(Frame.class, c);
    if (parent != null)
      return parent;
    parent = SwingUtilities.getAncestorOfClass(Dialog.class, c);
    return parent;
  }

  /**
   * This method will take the given JColorChooser and place it in a JDialog
   * with the given modal property. Three buttons are displayed in the
   * JDialog: OK, Cancel and Reset. If OK or Cancel are pressed, the JDialog
   * is hidden. If Reset is pressed, then the JColorChooser will take on its
   * default color value. The given okListener will be registered to the OK
   * button and the cancelListener will be registered to the Cancel button.
   * If the modal property is set, then the JDialog will block until it is
   * hidden.
   *
   * @param component The Component that will parent the JDialog.
   * @param title The title displayed in the JDialog.
   * @param modal The modal property.
   * @param chooserPane The JColorChooser to place in the JDialog.
   * @param okListener The ActionListener to register to the OK button.
   * @param cancelListener The ActionListener to register to the Cancel
   *        button.
   *
   * @return A JDialog with the JColorChooser inside of it.
   *
   * @throws AWTError If the component is not a suitable parent.
   */
  public static JDialog createDialog(Component component, String title,
                                     boolean modal, JColorChooser chooserPane,
                                     ActionListener okListener,
                                     ActionListener cancelListener)
  {
    Component parent = findParent(component);
    if (parent == null)
      throw new AWTError("No suitable parent found for Component.");
    JDialog dialog;
    if (parent instanceof Frame)
      dialog = new ModalDialog((Frame) parent, title);
    else
      dialog = new ModalDialog((Dialog) parent, title);
    dialog.setModal(modal);

    dialog.getContentPane().setLayout(new BorderLayout());

    JPanel panel = new JPanel();
    panel.setLayout(new FlowLayout());

    ActionListener al = new DefaultOKCancelListener(dialog);

    JButton ok = new JButton("OK");
    ok.addActionListener(okListener);
    ok.addActionListener(al);

    JButton cancel = new JButton("Cancel");
    cancel.addActionListener(cancelListener);
    cancel.addActionListener(al);

    JButton reset = new JButton("Reset");
    reset.addActionListener(new DefaultResetListener(chooserPane));

    dialog.getContentPane().add(chooserPane, BorderLayout.NORTH);

    panel.add(ok);
    panel.add(cancel);
    panel.add(reset);

    dialog.getContentPane().add(panel, BorderLayout.SOUTH);

    return dialog;
  } // createDialog()

  /**
   * This method returns the UI Component used for this JColorChooser.
   *
   * @return The UI Component for this JColorChooser.
   */
  public ColorChooserUI getUI()
  {
    return (ColorChooserUI) ui;
  } // getUI()

  /**
   * This method sets the UI Component used for this JColorChooser.
   *
   * @param ui The UI Component to use with this JColorChooser.
   */
  public void setUI(ColorChooserUI ui)
  {
    super.setUI(ui);
  } // setUI()

  /**
   * This method resets the UI Component property to the Look and Feel
   * default.
   */
  public void updateUI()
  {
    setUI((ColorChooserUI) UIManager.getUI(this));
    revalidate();
  } // updateUI()

  /**
   * This method returns a String identifier for the UI Class to be used with
   * the JColorChooser.
   *
   * @return The String identifier for the UI Class.
   */
  public String getUIClassID()
  {
    return "ColorChooserUI";
  } // getUIClassID()

  /**
   * This method returns the current color for the JColorChooser.
   *
   * @return The current color for the JColorChooser.
   */
  public Color getColor()
  {
    return selectionModel.getSelectedColor(); // TODO
  } // getColor()

  /**
   * This method changes the previewPanel property for the JTabbedPane. The
   * previewPanel is responsible for indicating the current color of the
   * JColorChooser.
   *
   * @param component The Component that will act as the previewPanel.
   */
  public void setPreviewPanel(JComponent component)
  {
    if (component != previewPanel)
      {
        JComponent old = previewPanel;
        previewPanel = component;
        firePropertyChange(PREVIEW_PANEL_PROPERTY, old, previewPanel);
      }
  } // setPreviewPanel()

  /**
   * This method returns the current previewPanel used with this
   * JColorChooser.
   *
   * @return The current previewPanel.
   */
  public JComponent getPreviewPanel()
  {
    return previewPanel; // TODO
  } // getPreviewPanel()

  /**
   * This method adds the given AbstractColorChooserPanel to the list of the
   * JColorChooser's chooserPanels.
   *
   * @param panel The AbstractColorChooserPanel to add.
   */
  public void addChooserPanel(AbstractColorChooserPanel panel)
  {
    if (panel == null)
      return;
    AbstractColorChooserPanel[] old = chooserPanels;
    AbstractColorChooserPanel[] newPanels =
      new AbstractColorChooserPanel[(old == null) ? 1 : old.length + 1];
    if (old != null)
      System.arraycopy(old, 0, newPanels, 0, old.length);
    newPanels[newPanels.length - 1] = panel;
    chooserPanels = newPanels;
    panel.installChooserPanel(this);
    firePropertyChange(CHOOSER_PANELS_PROPERTY, old, newPanels);
  } // addChooserPanel()

  /**
   * This method removes the given AbstractColorChooserPanel from the
   * JColorChooser's list of chooserPanels.
   *
   * @param panel The AbstractColorChooserPanel to remove.
   *
   * @return The AbstractColorChooserPanel that was removed.
   */
  public AbstractColorChooserPanel removeChooserPanel(AbstractColorChooserPanel panel)
  {
    int index = -1;
    for (int i = 0; i < chooserPanels.length; i++)
      if (panel == chooserPanels[i])
        {
          index = i;
          break;
        }

    if (index == -1)
      return null;

    AbstractColorChooserPanel[] old = chooserPanels;
    if (chooserPanels.length == 1)
      chooserPanels = null;
    else
      {
        AbstractColorChooserPanel[] newPanels =
          new AbstractColorChooserPanel[chooserPanels.length - 1];
        System.arraycopy(chooserPanels, 0, newPanels, 0, index);
        System.arraycopy(chooserPanels, index, newPanels, index - 1,
                         chooserPanels.length - index);
        chooserPanels = newPanels;
      }
    panel.uninstallChooserPanel(this);
    firePropertyChange(CHOOSER_PANELS_PROPERTY, old, chooserPanels);
    return panel;
  }

  /**
   * This method sets the chooserPanels property for this JColorChooser.
   *
   * @param panels The new set of AbstractColorChooserPanels to use.
   */
  public void setChooserPanels(AbstractColorChooserPanel[] panels)
  {
    if (panels != chooserPanels)
      {
        if (chooserPanels != null)
          for (int i = 0; i < chooserPanels.length; i++)
            if (chooserPanels[i] != null)
              chooserPanels[i].uninstallChooserPanel(this);

        AbstractColorChooserPanel[] old = chooserPanels;
        chooserPanels = panels;

        if (panels != null)
          for (int i = 0; i < panels.length; i++)
            if (panels[i] != null)
              panels[i].installChooserPanel(this);

        firePropertyChange(CHOOSER_PANELS_PROPERTY, old, chooserPanels);
      }
  } // setChooserPanels()

  /**
   * This method returns the AbstractColorChooserPanels used with this
   * JColorChooser.
   *
   * @return The AbstractColorChooserPanels used with this JColorChooser.
   */
  public AbstractColorChooserPanel[] getChooserPanels()
  {
    return chooserPanels;
  } // getChooserPanels()

  /**
   * This method returns the ColorSelectionModel used with this JColorChooser.
   *
   * @return The ColorSelectionModel.
   */
  public ColorSelectionModel getSelectionModel()
  {
    return selectionModel;
  } // getSelectionModel()

  /**
   * This method sets the ColorSelectionModel to be used with this
   * JColorChooser.
   *
   * @param model The ColorSelectionModel to be used with this JColorChooser.
   *
   * @throws AWTError If the given model is null.
   */
  public void setSelectionModel(ColorSelectionModel model)
  {
    if (model == null)
      throw new AWTError("ColorSelectionModel is not allowed to be null.");
    selectionModel = model;
  } // setSelectionModel()

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean getDragEnabled()
  {
    return dragEnabled;
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
   */
  public void setDragEnabled(boolean b)
  {
    dragEnabled = b;
  }

  /**
   * This method returns a String describing the JColorChooser.
   *
   * @return A String describing the JColorChooser.
   */
  protected String paramString()
  {
    return "JColorChooser";
  } // paramString()

  /**
   * getAccessibleContext
   *
   * @return AccessibleContext
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJColorChooser();

    return accessibleContext;
  }

  /**
   * A helper class that hides a JDialog when the action is performed.
   */
  static class DefaultOKCancelListener implements ActionListener
  {
    /** The JDialog to hide. */
    private JDialog dialog;

    /**
     * Creates a new DefaultOKCancelListener with the given JDialog to hide.
     *
     * @param dialog The JDialog to hide.
     */
    public DefaultOKCancelListener(JDialog dialog)
    {
      super();
      this.dialog = dialog;
    }

    /**
     * This method hides the JDialog when called.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      dialog.hide();
    }
  }

  /**
   * This method resets the JColorChooser color to the initial color when the
   * action is performed.
   */
  static class DefaultResetListener implements ActionListener
  {
    /** The JColorChooser to reset. */
    private JColorChooser chooser;

    /** The initial color. */
    private Color init;

    /**
     * Creates a new DefaultResetListener with the given JColorChooser.
     *
     * @param chooser The JColorChooser to reset.
     */
    public DefaultResetListener(JColorChooser chooser)
    {
      super();
      this.chooser = chooser;
      init = chooser.getColor();
    }

    /**
     * This method resets the JColorChooser to its initial color.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      chooser.setColor(init);
    }
  }

  /**
   * This is a custom JDialog that will notify when it is hidden and the modal
   * property is set.
   */
  static class ModalDialog extends JDialog
  {
    /** The modal property. */
    private boolean modal;

    /**
     * Creates a new ModalDialog object with the given parent and title.
     *
     * @param parent The parent of the JDialog.
     * @param title The title of the JDialog.
     */
    public ModalDialog(Frame parent, String title)
    {
      super(parent, title);
    }

    /**
     * Creates a new ModalDialog object with the given parent and title.
     *
     * @param parent The parent of the JDialog.
     * @param title The title of the JDialog.
     */
    public ModalDialog(Dialog parent, String title)
    {
      super(parent, title);
    }

    /**
     * This method sets the modal property.
     *
     * @param modal The modal property.
     */
    public void setModal(boolean modal)
    {
      this.modal = modal;
    }

    /**
     * This method shows the ModalDialog.
     */
    public void show()
    {
      super.show();
      if (modal)
	makeModal(this);
    }

    /**
     * This method hides the ModalDialog.
     */
    public synchronized void hide()
    {
      super.hide();
      notifyAll();
    }
  }
}
