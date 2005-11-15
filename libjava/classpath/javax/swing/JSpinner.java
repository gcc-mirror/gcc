/* JSpinner.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.SpinnerUI;
import javax.swing.text.DateFormatter;

/**
 * A JSpinner is a component which typically contains a numeric value and a
 * way to manipulate the value.
 *
 * @author Ka-Hing Cheung
 * 
 * @since 1.4
 */
public class JSpinner extends JComponent
{
  /**
   * DOCUMENT ME!
   */
  public static class DefaultEditor extends JPanel implements ChangeListener,
                                                              PropertyChangeListener,
                                                              LayoutManager
  {
    private JSpinner spinner;

    /** The JFormattedTextField that backs the editor. */
    JFormattedTextField ftf;

    /**
     * For compatability with Sun's JDK 1.4.2 rev. 5
     */
    private static final long serialVersionUID = -5317788736173368172L;

    /**
     * Creates a new <code>DefaultEditor</code> object.
     *
     * @param spinner the <code>JSpinner</code> associated with this editor
     */
    public DefaultEditor(JSpinner spinner)
    {
      super();
      setLayout(this);
      this.spinner = spinner;
      ftf = new JFormattedTextField();
      add(ftf);
      ftf.setValue(spinner.getValue());
      spinner.addChangeListener(this);
    }

    /**
     * Returns the <code>JSpinner</code> object for this editor.
     */
    public JSpinner getSpinner()
    {
      return spinner;
    }
    
    /**
     * DOCUMENT ME!
     */
    public void commitEdit() throws ParseException
    {
      // TODO: Implement this properly.
    }

    /**
     * DOCUMENT ME!
     *
     * @param spinner DOCUMENT ME!
     */
    public void dismiss(JSpinner spinner)
    {
      spinner.removeChangeListener(this);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public JFormattedTextField getTextField()
    {
      return ftf;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param parent DOCUMENT ME!
     */
    public void layoutContainer(Container parent)
    {
      Insets insets = getInsets();
      Dimension size = getSize();
      ftf.setBounds(insets.left, insets.top,
                    size.width - insets.left - insets.right,
                    size.height - insets.top - insets.bottom);
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param parent DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      Insets insets = getInsets();
      Dimension minSize = ftf.getMinimumSize();
      return new Dimension(minSize.width + insets.left + insets.right,
                            minSize.height + insets.top + insets.bottom);
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param parent DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      Insets insets = getInsets();
      Dimension prefSize = ftf.getPreferredSize();
      return new Dimension(prefSize.width + insets.left + insets.right,
                            prefSize.height + insets.top + insets.bottom);
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void propertyChange(PropertyChangeEvent event)
    {
      // TODO: Implement this properly.
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void stateChanged(ChangeEvent event)
    {
      // TODO: Implement this properly.
    }
    
    public void removeLayoutComponent(Component child)
    {
      // Nothing to do here.
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param child DOCUMENT ME!
     */
    public void addLayoutComponent(String name, Component child)
    {
      // Nothing to do here.
    }
  }

  /**
   * DOCUMENT ME!
   */
  public static class NumberEditor extends DefaultEditor
  {
    /**
     * For compatability with Sun's JDK
     */
    private static final long serialVersionUID = 3791956183098282942L;

    /**
     * Creates a new NumberEditor object.
     *
     * @param spinner DOCUMENT ME!
     */
    public NumberEditor(JSpinner spinner)
    {
      super(spinner);
    }

    /**
     * Creates a new NumberEditor object.
     *
     * @param spinner DOCUMENT ME!
     */
    public NumberEditor(JSpinner spinner, String decimalFormatPattern)
    {
      super(spinner);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DecimalFormat getFormat()
    {
      return null;
    }

    public SpinnerNumberModel getModel()
    {
      return (SpinnerNumberModel) getSpinner().getModel();
    }
  }

  /**
   * A <code>JSpinner</code> editor used for the {@link SpinnerListModel}.
   * This editor uses a <code>JFormattedTextField</code> to edit the values
   * of the spinner.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  public static class ListEditor extends DefaultEditor
  {
    /**
     * Creates a new instance of <code>ListEditor</code>.
     *
     * @param spinner the spinner for which this editor is used
     */
    public ListEditor(JSpinner spinner)
    {
      super(spinner);
    }

    public SpinnerListModel getModel()
    {
      return (SpinnerListModel) getSpinner().getModel();
    }
  }

  /**
   * An editor class for a <code>JSpinner</code> that is used
   * for displaying and editing dates (e.g. that uses
   * <code>SpinnerDateModel</code> as model).
   *
   * The editor uses a {@link JTextField} with the value
   * displayed by a {@link DateFormatter} instance.
   */
  public static class DateEditor extends DefaultEditor
  {

    /** The serialVersionUID. */
    private static final long serialVersionUID = -4279356973770397815L;

    /** The DateFormat instance used to format the date. */
    SimpleDateFormat dateFormat;

    /**
     * Creates a new instance of DateEditor for the specified
     * <code>JSpinner</code>.
     *
     * @param spinner the <code>JSpinner</code> for which to
     *     create a <code>DateEditor</code> instance
     */
    public DateEditor(JSpinner spinner)
    {
      super(spinner);
      init(new SimpleDateFormat());
    }

    /**
     * Creates a new instance of DateEditor for the specified
     * <code>JSpinner</code> using the specified date format
     * pattern.
     *
     * @param spinner the <code>JSpinner</code> for which to
     *     create a <code>DateEditor</code> instance
     * @param dateFormatPattern the date format to use
     *
     * @see SimpleDateFormat#SimpleDateFormat(String)
     */
    public DateEditor(JSpinner spinner, String dateFormatPattern)
    {
      super(spinner);
      init(new SimpleDateFormat(dateFormatPattern));
    }

    /**
     * Initializes the JFormattedTextField for this editor.
     *
     * @param format the date format to use in the formatted text field
     */
    private void init(SimpleDateFormat format)
    {
      dateFormat = format;
      getTextField().setFormatterFactory(
        new JFormattedTextField.AbstractFormatterFactory()
        {
          public JFormattedTextField.AbstractFormatter
          getFormatter(JFormattedTextField ftf)
          {
            return new DateFormatter(dateFormat);
          }
        });
    }

    /**
     * Returns the <code>SimpleDateFormat</code> instance that is used to
     * format the date value.
     *
     * @return the <code>SimpleDateFormat</code> instance that is used to
     *     format the date value
     */
    public SimpleDateFormat getFormat()
    {
      return dateFormat;
    }

    /**
     * Returns the {@link SpinnerDateModel} that is edited by this editor.
     *
     * @return the <code>SpinnerDateModel</code> that is edited by this editor
     */
    public SpinnerDateModel getModel()
    {
      return (SpinnerDateModel) getSpinner().getModel();
    }
  }

  private static final long serialVersionUID = 3412663575706551720L;

  /** DOCUMENT ME! */
  private SpinnerModel model;

  /** DOCUMENT ME! */
  private JComponent editor;

  /** DOCUMENT ME! */
  private ChangeListener listener = new ChangeListener()
    {
      public void stateChanged(ChangeEvent evt)
      {
	fireStateChanged();
      }
    };

  /**
   * Creates a JSpinner with <code>SpinnerNumberModel</code>
   *
   * @see javax.swing.SpinnerNumberModel
   */
  public JSpinner()
  {
    this(new SpinnerNumberModel());
  }

  /**
   * Creates a JSpinner with the specific model and sets the default editor
   *
   * @param model DOCUMENT ME!
   */
  public JSpinner(SpinnerModel model)
  {
    this.model = model;
    model.addChangeListener(listener);
    setEditor(createEditor(model));
    updateUI();
  }

  /**
   * If the editor is <code>JSpinner.DefaultEditor</code>, then forwards the
   * call to it, otherwise do nothing.
   *
   * @throws ParseException DOCUMENT ME!
   */
  public void commitEdit() throws ParseException
  {
    if (editor instanceof DefaultEditor)
      ((DefaultEditor) editor).commitEdit();
  }

  /**
   * Gets the current editor
   *
   * @return the current editor
   *
   * @see #setEditor
   */
  public JComponent getEditor()
  {
    return editor;
  }

  /**
   * Changes the current editor to the new editor. This methods should remove
   * the old listeners (if any) and adds the new listeners (if any).
   *
   * @param editor the new editor
   *
   * @throws IllegalArgumentException DOCUMENT ME!
   *
   * @see #getEditor
   */
  public void setEditor(JComponent editor)
  {
    if (editor == null)
      throw new IllegalArgumentException("editor may not be null");

    if (this.editor instanceof DefaultEditor)
      ((DefaultEditor) editor).dismiss(this);
    else if (this.editor instanceof ChangeListener)
      removeChangeListener((ChangeListener) this.editor);

    if (editor instanceof ChangeListener)
      addChangeListener((ChangeListener) editor);

    this.editor = editor;
  }

  /**
   * Gets the underly model.
   *
   * @return the underly model
   */
  public SpinnerModel getModel()
  {
    return model;
  }

  /**
   * Sets a new underlying model.
   *
   * @param newModel the new model to set
   *
   * @exception IllegalArgumentException if newModel is <code>null</code>
   */
  public void setModel(SpinnerModel newModel)
  {
    if (newModel == null)
      throw new IllegalArgumentException();
    
    if (model == newModel)
      return;

    SpinnerModel oldModel = model;
    model = newModel;
    firePropertyChange("model", oldModel, newModel);

    if (editor == null)
      setEditor(createEditor(model));
  }

  /**
   * Gets the next value without changing the current value.
   *
   * @return the next value
   *
   * @see javax.swing.SpinnerModel#getNextValue
   */
  public Object getNextValue()
  {
    return model.getNextValue();
  }

  /**
   * Gets the previous value without changing the current value.
   *
   * @return the previous value
   *
   * @see javax.swing.SpinnerModel#getPreviousValue
   */
  public Object getPreviousValue()
  {
    return model.getPreviousValue();
  }

  /**
   * Gets the <code>SpinnerUI</code> that handles this spinner
   *
   * @return the <code>SpinnerUI</code>
   */
  public SpinnerUI getUI()
  {
    return (SpinnerUI) ui;
  }

  /**
   * Gets the current value of the spinner, according to the underly model,
   * not the UI.
   *
   * @return the current value
   *
   * @see javax.swing.SpinnerModel#getValue
   */
  public Object getValue()
  {
    return model.getValue();
  }

  /**
   * DOCUMENT ME!
   *
   * @param value DOCUMENT ME!
   */
  public void setValue(Object value)
  {
    model.setValue(value);
  }

  /**
   * This method returns a name to identify which look and feel class will be
   * the UI delegate for this spinner.
   *
   * @return The UIClass identifier. "SpinnerUI"
   */
  public String getUIClassID()
  {
    return "SpinnerUI";
  }

  /**
   * This method resets the spinner's UI delegate to the default UI for the
   * current look and feel.
   */
  public void updateUI()
  {
    setUI((SpinnerUI) UIManager.getUI(this));
  }

  /**
   * This method sets the spinner's UI delegate.
   *
   * @param ui The spinner's UI delegate.
   */
  public void setUI(SpinnerUI ui)
  {
    super.setUI(ui);
  }

  /**
   * Adds a <code>ChangeListener</code>
   *
   * @param listener the listener to add
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Remove a particular listener
   *
   * @param listener the listener to remove
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * Gets all the <code>ChangeListener</code>s
   *
   * @return all the <code>ChangeListener</code>s
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * Fires a <code>ChangeEvent</code> to all the <code>ChangeListener</code>s
   * added to this <code>JSpinner</code>
   */
  protected void fireStateChanged()
  {
    ChangeEvent evt = new ChangeEvent(this);
    ChangeListener[] listeners = getChangeListeners();

    for (int i = 0; i < listeners.length; ++i)
      listeners[i].stateChanged(evt);
  }

  /**
   * Creates an editor for this <code>JSpinner</code>. Really, it should be a
   * <code>JSpinner.DefaultEditor</code>, but since that should be
   * implemented by a JFormattedTextField, and one is not written, I am just
   * using a dummy one backed by a JLabel.
   *
   * @param model DOCUMENT ME!
   *
   * @return the default editor
   */
  protected JComponent createEditor(SpinnerModel model)
  {
    if (model instanceof SpinnerDateModel)
      return new DateEditor(this);
    else if (model instanceof SpinnerNumberModel)
      return new NumberEditor(this);
    else
      return new DefaultEditor(this);
  }
}
