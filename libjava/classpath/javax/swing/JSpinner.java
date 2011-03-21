/* JSpinner.java --
   Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.

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
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.SpinnerUI;
import javax.swing.text.DateFormatter;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;

/**
 * A <code>JSpinner</code> is a component that displays a single value from
 * a sequence of values, and provides a convenient means for selecting the
 * previous and next values in the sequence.  Typically the spinner displays
 * a numeric value, but it is possible to display dates or arbitrary items
 * from a list.
 *
 * @author Ka-Hing Cheung
 *
 * @since 1.4
 */
public class JSpinner extends JComponent
{
  /**
   * The base class for the editor used by the {@link JSpinner} component.
   * The editor is in fact a panel containing a {@link JFormattedTextField}
   * component.
   */
  public static class DefaultEditor
    extends JPanel
    implements ChangeListener, PropertyChangeListener, LayoutManager
  {
    /** The spinner that the editor is allocated to. */
    private JSpinner spinner;

    /** The JFormattedTextField that backs the editor. */
    JFormattedTextField ftf;

    /**
     * For compatability with Sun's JDK 1.4.2 rev. 5
     */
    private static final long serialVersionUID = -5317788736173368172L;

    /**
     * Creates a new <code>DefaultEditor</code> object.  The editor is
     * registered with the spinner as a {@link ChangeListener} here.
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
      ftf.addPropertyChangeListener(this);
      if (getComponentOrientation().isLeftToRight())
        ftf.setHorizontalAlignment(JTextField.RIGHT);
      else
        ftf.setHorizontalAlignment(JTextField.LEFT);
      spinner.addChangeListener(this);
    }

    /**
     * Returns the <code>JSpinner</code> component that the editor is assigned
     * to.
     *
     * @return The spinner that the editor is assigned to.
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
     * Removes the editor from the {@link ChangeListener} list maintained by
     * the specified <code>spinner</code>.
     *
     * @param spinner  the spinner (<code>null</code> not permitted).
     */
    public void dismiss(JSpinner spinner)
    {
      spinner.removeChangeListener(this);
    }

    /**
     * Returns the text field used to display and edit the current value in
     * the spinner.
     *
     * @return The text field.
     */
    public JFormattedTextField getTextField()
    {
      return ftf;
    }

    /**
     * Sets the bounds for the child components in this container.  In this
     * case, the text field is the only component to be laid out.
     *
     * @param parent the parent container.
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
     * Calculates the minimum size for this component.  In this case, the
     * text field is the only subcomponent, so the return value is the minimum
     * size of the text field plus the insets of this component.
     *
     * @param parent  the parent container.
     *
     * @return The minimum size.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      Insets insets = getInsets();
      Dimension minSize = ftf.getMinimumSize();
      return new Dimension(minSize.width + insets.left + insets.right,
                            minSize.height + insets.top + insets.bottom);
    }

    /**
     * Calculates the preferred size for this component.  In this case, the
     * text field is the only subcomponent, so the return value is the
     * preferred size of the text field plus the insets of this component.
     *
     * @param parent  the parent container.
     *
     * @return The preferred size.
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      Insets insets = getInsets();
      Dimension prefSize = ftf.getPreferredSize();
      return new Dimension(prefSize.width + insets.left + insets.right,
                            prefSize.height + insets.top + insets.bottom);
    }

    /**
     * Receives notification of property changes.  If the text field's 'value'
     * property changes, the spinner's model is updated accordingly.
     *
     * @param event the event.
     */
    public void propertyChange(PropertyChangeEvent event)
    {
      if (event.getSource() == ftf)
        {
          if (event.getPropertyName().equals("value"))
            spinner.getModel().setValue(event.getNewValue());
        }
    }

    /**
     * Receives notification of changes in the state of the {@link JSpinner}
     * that the editor belongs to - the content of the text field is updated
     * accordingly.
     *
     * @param event  the change event.
     */
    public void stateChanged(ChangeEvent event)
    {
      ftf.setValue(spinner.getValue());
    }

    /**
     * This method does nothing.  It is required by the {@link LayoutManager}
     * interface, but since this component has a single child, there is no
     * need to use this method.
     *
     * @param child  the child component to remove.
     */
    public void removeLayoutComponent(Component child)
    {
      // Nothing to do here.
    }

    /**
     * This method does nothing.  It is required by the {@link LayoutManager}
     * interface, but since this component has a single child, there is no
     * need to use this method.
     *
     * @param name  the name.
     * @param child  the child component to add.
     */
    public void addLayoutComponent(String name, Component child)
    {
      // Nothing to do here.
    }
  }

  /**
   * A panel containing a {@link JFormattedTextField} that is configured for
   * displaying and editing numbers.  The panel is used as a subcomponent of
   * a {@link JSpinner}.
   *
   * @see JSpinner#createEditor(SpinnerModel)
   */
  public static class NumberEditor extends DefaultEditor
  {
    /**
     * For compatability with Sun's JDK
     */
    private static final long serialVersionUID = 3791956183098282942L;

    /**
     * Creates a new <code>NumberEditor</code> object for the specified
     * <code>spinner</code>.  The editor is registered with the spinner as a
     * {@link ChangeListener}.
     *
     * @param spinner the component the editor will be used with.
     */
    public NumberEditor(JSpinner spinner)
    {
      super(spinner);
      NumberEditorFormatter nef = new NumberEditorFormatter();
      nef.setMinimum(getModel().getMinimum());
      nef.setMaximum(getModel().getMaximum());
      ftf.setFormatterFactory(new DefaultFormatterFactory(nef));
    }

    /**
     * Creates a new <code>NumberEditor</code> object.
     *
     * @param spinner  the spinner.
     * @param decimalFormatPattern  the number format pattern.
     */
    public NumberEditor(JSpinner spinner, String decimalFormatPattern)
    {
      super(spinner);
      NumberEditorFormatter nef
          = new NumberEditorFormatter(decimalFormatPattern);
      nef.setMinimum(getModel().getMinimum());
      nef.setMaximum(getModel().getMaximum());
      ftf.setFormatterFactory(new DefaultFormatterFactory(nef));
    }

    /**
     * Returns the format used by the text field.
     *
     * @return The format used by the text field.
     */
    public DecimalFormat getFormat()
    {
      NumberFormatter formatter = (NumberFormatter) ftf.getFormatter();
      return (DecimalFormat) formatter.getFormat();
    }

    /**
     * Returns the model used by the editor's {@link JSpinner} component,
     * cast to a {@link SpinnerNumberModel}.
     *
     * @return The model.
     */
    public SpinnerNumberModel getModel()
    {
      return (SpinnerNumberModel) getSpinner().getModel();
    }
  }

  static class NumberEditorFormatter
    extends NumberFormatter
  {
    public NumberEditorFormatter()
    {
      super(NumberFormat.getInstance());
    }
    public NumberEditorFormatter(String decimalFormatPattern)
    {
      super(new DecimalFormat(decimalFormatPattern));
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

    /**
     * Returns the spinner's model cast as a {@link SpinnerListModel}.
     *
     * @return The spinner's model.
     */
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
      DateEditorFormatter nef = new DateEditorFormatter();
      nef.setMinimum(getModel().getStart());
      nef.setMaximum(getModel().getEnd());
      ftf.setFormatterFactory(new DefaultFormatterFactory(nef));
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
      DateEditorFormatter nef = new DateEditorFormatter(dateFormatPattern);
      nef.setMinimum(getModel().getStart());
      nef.setMaximum(getModel().getEnd());
      ftf.setFormatterFactory(new DefaultFormatterFactory(nef));
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
      DateFormatter formatter = (DateFormatter) ftf.getFormatter();
      return (SimpleDateFormat) formatter.getFormat();
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

  static class DateEditorFormatter
    extends DateFormatter
  {
    public DateEditorFormatter()
    {
      super(DateFormat.getInstance());
    }
    public DateEditorFormatter(String dateFormatPattern)
    {
      super(new SimpleDateFormat(dateFormatPattern));
    }
  }

  /**
   * A listener that forwards {@link ChangeEvent} notifications from the model
   * to the {@link JSpinner}'s listeners.
   */
  class ModelListener implements ChangeListener
  {
    /**
     * Creates a new listener.
     */
    public ModelListener()
    {
      // nothing to do here
    }

    /**
     * Receives notification from the model that its state has changed.
     *
     * @param event  the event (ignored).
     */
    public void stateChanged(ChangeEvent event)
    {
      fireStateChanged();
    }
  }

  /**
   * The model that defines the current value and permitted values for the
   * spinner.
   */
  private SpinnerModel model;

  /** The current editor. */
  private JComponent editor;

  private static final long serialVersionUID = 3412663575706551720L;

  /**
   * Creates a new <code>JSpinner</code> with default instance of
   * {@link SpinnerNumberModel} (that is, a model with value 0, step size 1,
   * and no upper or lower limit).
   *
   * @see javax.swing.SpinnerNumberModel
   */
  public JSpinner()
  {
    this(new SpinnerNumberModel());
  }

  /**
   * Creates a new <code>JSpinner with the specified model.  The
   * {@link #createEditor(SpinnerModel)} method is used to create an editor
   * that is suitable for the model.
   *
   * @param model the model (<code>null</code> not permitted).
   *
   * @throws NullPointerException if <code>model</code> is <code>null</code>.
   */
  public JSpinner(SpinnerModel model)
  {
    this.model = model;
    this.editor = createEditor(model);
    model.addChangeListener(new ModelListener());
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
   * Changes the current editor to the new editor. The old editor is
   * removed from the spinner's {@link ChangeEvent} list.
   *
   * @param editor the new editor (<code>null</code> not permitted.
   *
   * @throws IllegalArgumentException if <code>editor</code> is
   *                                  <code>null</code>.
   *
   * @see #getEditor
   */
  public void setEditor(JComponent editor)
  {
    if (editor == null)
      throw new IllegalArgumentException("editor may not be null");

    JComponent oldEditor = this.editor;
    if (oldEditor instanceof DefaultEditor)
      ((DefaultEditor) oldEditor).dismiss(this);
    else if (oldEditor instanceof ChangeListener)
      removeChangeListener((ChangeListener) oldEditor);

    this.editor = editor;
    firePropertyChange("editor", oldEditor, editor);
  }

  /**
   * Returns the model used by the {@link JSpinner} component.
   *
   * @return The model.
   *
   * @see #setModel(SpinnerModel)
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
   * Sets the value in the model.
   *
   * @param value the new value.
   */
  public void setValue(Object value)
  {
    model.setValue(value);
  }

  /**
   * Returns the ID that identifies which look and feel class will be
   * the UI delegate for this spinner.
   *
   * @return <code>"SpinnerUI"</code>.
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
   * Sets the UI delegate for the component.
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
   * Creates an editor that is appropriate for the specified <code>model</code>.
   *
   * @param model the model.
   *
   * @return The editor.
   */
  protected JComponent createEditor(SpinnerModel model)
  {
    if (model instanceof SpinnerDateModel)
      return new DateEditor(this);
    else if (model instanceof SpinnerNumberModel)
      return new NumberEditor(this);
    else if (model instanceof SpinnerListModel)
      return new ListEditor(this);
    else
      return new DefaultEditor(this);
  }
}
