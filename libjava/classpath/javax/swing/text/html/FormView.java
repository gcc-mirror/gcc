/* FormView.java -- A view for a variety of HTML form elements
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.text.html;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.text.AttributeSet;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;

/**
 * A View that renders HTML form elements like buttons and input fields.
 * This is implemented as a {@link ComponentView} that creates different Swing
 * component depending on the type and setting of the different form elements.
 *
 * Namely, this view creates the following components:
 * <table>
 * <tr><th>Element type</th><th>Swing component</th></tr>
 * <tr><td>input, button</td><td>JButton</td></tr>
 * <tr><td>input, checkbox</td><td>JButton</td></tr>
 * <tr><td>input, image</td><td>JButton</td></tr>
 * <tr><td>input, password</td><td>JButton</td></tr>
 * <tr><td>input, radio</td><td>JButton</td></tr>
 * <tr><td>input, reset</td><td>JButton</td></tr>
 * <tr><td>input, submit</td><td>JButton</td></tr>
 * <tr><td>input, text</td><td>JButton</td></tr>
 * <tr><td>select, size > 1 or with multiple attribute</td>
 * <td>JList in JScrollPane</td></tr>
 * <tr><td>select, size unspecified or == 1</td><td>JComboBox</td></tr>
 * <tr><td>textarea, text</td><td>JTextArea in JScrollPane</td></tr>
 * <tr><td>input, file</td><td>JTextField</td></tr> 
 * </table>
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class FormView
  extends ComponentView
  implements ActionListener
{

  /**
   * If the value attribute of an <code>&lt;input type=&quot;submit&quot;&gt>
   * tag is not specified, then this string is used.
   * 
   * @deprecated As of JDK1.3 the value is fetched from the UIManager property
   *             <code>FormView.submitButtonText</code>.
   */
  public static final String SUBMIT =
    UIManager.getString("FormView.submitButtonText");

  /**
   * If the value attribute of an <code>&lt;input type=&quot;reset&quot;&gt>
   * tag is not specified, then this string is used.
   * 
   * @deprecated As of JDK1.3 the value is fetched from the UIManager property
   *             <code>FormView.resetButtonText</code>.
   */
  public static final String RESET =
    UIManager.getString("FormView.resetButtonText");

  /**
   * Creates a new <code>FormView</code>.
   *
   * @param el the element that is displayed by this view.
   */
  public FormView(Element el)
  {
    super(el);
  }

  /**
   * Creates the correct AWT component for rendering the form element.
   */
  protected Component createComponent()
  {
    Component comp = null;
    Element el = getElement();
    Object tag = el.getAttributes().getAttribute(StyleConstants.NameAttribute);
    if (tag.equals(HTML.Tag.INPUT))
      {
        AttributeSet atts = el.getAttributes();
        String type = (String) atts.getAttribute(HTML.Attribute.TYPE);
        String value = (String) atts.getAttribute(HTML.Attribute.VALUE);
        if (type.equals("button"))
          comp = new JButton(value);
        else if (type.equals("checkbox"))
          comp = new JCheckBox(value);
        else if (type.equals("image"))
          comp = new JButton(value); // FIXME: Find out how to fetch the image.
        else if (type.equals("password"))
          comp = new JPasswordField(value);
        else if (type.equals("radio"))
          comp = new JRadioButton(value);
        else if (type.equals("reset"))
          {
            if (value == null || value.equals(""))
              value = RESET;
            comp = new JButton(value);
          }
        else if (type.equals("submit"))
          {
            if (value == null || value.equals(""))
              value = SUBMIT;
            comp = new JButton(value);
          }
        else if (type.equals("text"))
          comp = new JTextField(value);
        
      }
    // FIXME: Implement the remaining components.
    return comp;
  }

  /**
   * Determines the maximum span for this view on the specified axis.
   *
   * @param axis the axis along which to determine the span
   *
   * @return the maximum span for this view on the specified axis
   *
   * @throws IllegalArgumentException if the axis is invalid
   */
  public float getMaximumSpan(int axis)
  {
    // FIXME: The specs say that for some components the maximum span == the
    // preferred span of the component. This should be figured out and
    // implemented accordingly.
    float span;
    if (axis == X_AXIS)
      span = getComponent().getMaximumSize().width;
    else if (axis == Y_AXIS)
      span = getComponent().getMaximumSize().height;
    else
      throw new IllegalArgumentException("Invalid axis parameter");
    return span;
  }

  /**
   * Processes an action from the Swing component.
   *
   * If the action comes from a submit button, the form is submitted by calling
   * {@link #submitData}. In the case of a reset button, the form is reset to
   * the original state. If the action comes from a password or text field,
   * then the input focus is transferred to the next input element in the form,
   * unless this text/password field is the last one, in which case the form
   * is submitted.
   *
   * @param ev the action event
   */
  public void actionPerformed(ActionEvent ev)
  {
    Element el = getElement();
    Object tag = el.getAttributes().getAttribute(StyleConstants.NameAttribute);
    if (tag.equals(HTML.Tag.INPUT))
      {
        AttributeSet atts = el.getAttributes();
        String type = (String) atts.getAttribute(HTML.Attribute.TYPE);
        if (type.equals("submit"))
          submitData(""); // FIXME: How to fetch the actual form data?
      }
    // FIXME: Implement the remaining actions.
  }

  /**
   * Submits the form data. A separate thread is created to do the
   * transmission.
   *
   * @param data the form data
   */
  protected void submitData(String data)
  {
    // FIXME: Implement this.
  }

  /**
   * Submits the form data in response to a click on a
   * <code>&lt;input type=&quot;image&quot;&gt;</code> element.
   *
   * @param imageData the mouse click coordinates
   */
  protected void imageSubmit(String imageData)
  {
    // FIXME: Implement this.
  }
}
