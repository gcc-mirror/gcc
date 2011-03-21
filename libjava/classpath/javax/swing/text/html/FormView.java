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
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;

import javax.swing.ButtonModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JEditorPane;
import javax.swing.JList;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.HyperlinkEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.ComponentView;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.ElementIterator;
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

  protected class MouseEventListener
    extends MouseAdapter
  {
    /**
     * Creates a new <code>MouseEventListener</code>.
     */
    protected MouseEventListener()
    {
      // Nothing to do here.
    }

    public void mouseReleased(MouseEvent ev)
    {
      String data = getImageData(ev.getPoint());
      imageSubmit(data);
    }
  }

  /**
   * Actually submits the form data.
   */
  private class SubmitThread
    extends Thread
  {
    /**
     * The submit data.
     */
    private String data;

    /**
     * Creates a new SubmitThread.
     *
     * @param d the submit data
     */
    SubmitThread(String d)
    {
      data = d;
    }

    /**
     * Actually performs the submit.
     */
    public void run()
    {
      if (data.length() > 0)
        {
          final String method = getMethod();
          final URL actionURL = getActionURL();
          final String target = getTarget();
          URLConnection conn;
          final JEditorPane editor = (JEditorPane) getContainer();
          final HTMLDocument doc = (HTMLDocument) editor.getDocument();
          HTMLEditorKit kit = (HTMLEditorKit) editor.getEditorKit();
          if (kit.isAutoFormSubmission())
            {
              try
                {
                  final URL url;
                  if (method != null && method.equals("post"))
                    {
                      // Perform POST.
                      url = actionURL;
                      conn = url.openConnection();
                      postData(conn, data);
                    }
                  else
                    {
                      // Default to GET.
                      url = new URL(actionURL + "?" + data);
                    }
                  Runnable loadDoc = new Runnable()
                  {
                    public void run()
                    {
                      if (doc.isFrameDocument())
                        {
                          editor.fireHyperlinkUpdate(createSubmitEvent(method,
                                                                     actionURL,
                                                                     target));
                        }
                      else
                        {
                          try
                          {
                            editor.setPage(url);
                          }
                          catch (IOException ex)
                          {
                            // Oh well.
                            ex.printStackTrace();
                          }
                        }
                    }
                  };
                  SwingUtilities.invokeLater(loadDoc);
                }
              catch (MalformedURLException ex)
                {
                  ex.printStackTrace();
                }
              catch (IOException ex)
                {
                  ex.printStackTrace();
                }
            }
          else
            {
              editor.fireHyperlinkUpdate(createSubmitEvent(method,actionURL,
                                                           target));
            }
        }
    }

    /**
     * Determines the submit method.
     *
     * @return the submit method
     */
    private String getMethod()
    {
      AttributeSet formAtts = getFormAttributes();
      String method = null;
      if (formAtts != null)
        {
          method = (String) formAtts.getAttribute(HTML.Attribute.METHOD);
        }
      return method;
    }

    /**
     * Determines the action URL.
     *
     * @return the action URL
     */
    private URL getActionURL()
    {
      AttributeSet formAtts = getFormAttributes();
      HTMLDocument doc = (HTMLDocument) getElement().getDocument();
      URL url = doc.getBase();
      if (formAtts != null)
        {
          String action =
            (String) formAtts.getAttribute(HTML.Attribute.ACTION);
          if (action != null)
            {
              try
                {
                  url = new URL(url, action);
                }
              catch (MalformedURLException ex)
                {
                  url = null;
                }
            }
        }
      return url;
    }

    /**
     * Fetches the target attribute.
     *
     * @return the target attribute or _self if none is present
     */
    private String getTarget()
    {
      AttributeSet formAtts = getFormAttributes();
      String target = null;
      if (formAtts != null)
        {
          target = (String) formAtts.getAttribute(HTML.Attribute.TARGET);
          if (target != null)
            target = target.toLowerCase();
        }
      if (target == null)
        target = "_self";
      return target;
    }

    /**
     * Posts the form data over the specified connection.
     *
     * @param conn the connection
     */
    private void postData(URLConnection conn, String data)
    {
      conn.setDoOutput(true);
      PrintWriter out = null;
      try
        {
          out = new PrintWriter(new OutputStreamWriter(conn.getOutputStream()));
          out.print(data);
          out.flush();
        }
      catch (IOException ex)
        {
          // Deal with this!
          ex.printStackTrace();
        }
      finally
        {
          if (out != null)
            out.close();
        }
    }

    /**
     * Determines the attributes from the relevant form tag.
     *
     * @return the attributes from the relevant form tag, <code>null</code>
     *         when there is no form tag
     */
    private AttributeSet getFormAttributes()
    {
      AttributeSet atts = null;
      Element form = getFormElement();
      if (form != null)
        atts = form.getAttributes();
      return atts;
    }

    /**
     * Creates the submit event that should be fired.
     *
     * This is package private to avoid accessor methods.
     *
     * @param method the submit method
     * @param actionURL the action URL
     * @param target the target
     *
     * @return the submit event
     */
    FormSubmitEvent createSubmitEvent(String method, URL actionURL,
                                      String target)
    {
      FormSubmitEvent.MethodType m = "post".equals(method)
                                     ? FormSubmitEvent.MethodType.POST
                                     : FormSubmitEvent.MethodType.GET;
      return new FormSubmitEvent(FormView.this,
                                 HyperlinkEvent.EventType.ACTIVATED,
                                 actionURL, getElement(), target, m, data);
    }
  }

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
   * If this is true, the maximum size is set to the preferred size.
   */
  private boolean maxIsPreferred;

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
    AttributeSet atts = el.getAttributes();
    Object tag = atts.getAttribute(StyleConstants.NameAttribute);
    Object model = atts.getAttribute(StyleConstants.ModelAttribute);
    if (tag.equals(HTML.Tag.INPUT))
      {
        String type = (String) atts.getAttribute(HTML.Attribute.TYPE);
        if (type.equals("button"))
          {
            String value = (String) atts.getAttribute(HTML.Attribute.VALUE);
            JButton b = new JButton(value);
            if (model != null)
              {
                b.setModel((ButtonModel) model);
                b.addActionListener(this);
              }
            comp = b;
            maxIsPreferred = true;
          }
        else if (type.equals("checkbox"))
          {
            if (model instanceof ResetableToggleButtonModel)
              {
                ResetableToggleButtonModel m =
                  (ResetableToggleButtonModel) model;
                JCheckBox c = new JCheckBox();
                c.setModel(m);
                comp = c;
                maxIsPreferred = true;
              }
          }
        else if (type.equals("image"))
          {
            String src = (String) atts.getAttribute(HTML.Attribute.SRC);
            JButton b;
            try
              {
                URL base = ((HTMLDocument) el.getDocument()).getBase();
                URL srcURL = new URL(base, src);
                ImageIcon icon = new ImageIcon(srcURL);
                b = new JButton(icon);
              }
            catch (MalformedURLException ex)
              {
                b = new JButton(src);
              }
            if (model != null)
              {
                b.setModel((ButtonModel) model);
                b.addActionListener(this);
              }
            comp = b;
            maxIsPreferred = true;
          }
        else if (type.equals("password"))
          {
            int size = HTML.getIntegerAttributeValue(atts, HTML.Attribute.SIZE,
                                                     -1);
            JTextField tf = new JPasswordField();
            if (size > 0)
              tf.setColumns(size);
            else
              tf.setColumns(20);
            if (model != null)
              tf.setDocument((Document) model);
            tf.addActionListener(this);
            comp = tf;
            maxIsPreferred = true;
          }
        else if (type.equals("radio"))
          {
            if (model instanceof ResetableToggleButtonModel)
              {
                ResetableToggleButtonModel m =
                  (ResetableToggleButtonModel) model;
                JRadioButton c = new JRadioButton();
                c.setModel(m);
                comp = c;
                maxIsPreferred = true;
              }
          }
        else if (type.equals("reset"))
          {
            String value = (String) atts.getAttribute(HTML.Attribute.VALUE);
            if (value == null)
              value = UIManager.getString("FormView.resetButtonText");
            JButton b = new JButton(value);
            if (model != null)
              {
                b.setModel((ButtonModel) model);
                b.addActionListener(this);
              }
            comp = b;
            maxIsPreferred = true;
          }
        else if (type.equals("submit"))
          {
            String value = (String) atts.getAttribute(HTML.Attribute.VALUE);
            if (value == null)
              value = UIManager.getString("FormView.submitButtonText");
            JButton b = new JButton(value);
            if (model != null)
              {
                b.setModel((ButtonModel) model);
                b.addActionListener(this);
              }
            comp = b;
            maxIsPreferred = true;
          }
        else if (type.equals("text"))
          {
            int size = HTML.getIntegerAttributeValue(atts, HTML.Attribute.SIZE,
                                                     -1);
            JTextField tf = new JTextField();
            if (size > 0)
              tf.setColumns(size);
            else
              tf.setColumns(20);
            if (model != null)
              tf.setDocument((Document) model);
            tf.addActionListener(this);
            comp = tf;
            maxIsPreferred = true;
          }
      }
    else if (tag == HTML.Tag.TEXTAREA)
      {
        JTextArea textArea = new JTextArea((Document) model);
        int rows = HTML.getIntegerAttributeValue(atts, HTML.Attribute.ROWS, 1);
        textArea.setRows(rows);
        int cols = HTML.getIntegerAttributeValue(atts, HTML.Attribute.COLS, 20);
        textArea.setColumns(cols);
        maxIsPreferred = true;
        comp = new JScrollPane(textArea,
                               JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                               JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
      }
    else if (tag == HTML.Tag.SELECT)
      {
        if (model instanceof SelectListModel)
          {
            SelectListModel slModel = (SelectListModel) model;
            JList list = new JList(slModel);
            int size = HTML.getIntegerAttributeValue(atts, HTML.Attribute.SIZE,
                                                     1);
            list.setVisibleRowCount(size);
            list.setSelectionModel(slModel.getSelectionModel());
            comp = new JScrollPane(list);
          }
        else if (model instanceof SelectComboBoxModel)
          {
            SelectComboBoxModel scbModel = (SelectComboBoxModel) model;
            comp = new JComboBox(scbModel);
          }
        maxIsPreferred = true;
      }
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
    float span;
    if (maxIsPreferred)
      span = getPreferredSpan(axis);
    else
      span = super.getMaximumSpan(axis);
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
          submitData(getFormData());
        else if (type.equals("reset"))
          resetForm();
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
    SubmitThread submitThread = new SubmitThread(data);
    submitThread.start();
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

  /**
   * Determines the image data that should be submitted in response to a
   * mouse click on a image. This is either 'x=<p.x>&y=<p.y>' if the name
   * attribute of the element is null or '' or
   * <name>.x=<p.x>&<name>.y=<p.y>' when the name attribute is not empty.
   *
   * @param p the coordinates of the mouseclick
   */
  String getImageData(Point p)
  {
    String name = (String) getElement().getAttributes()
                                            .getAttribute(HTML.Attribute.NAME);
    String data;
    if (name == null || name.equals(""))
      {
        data = "x=" + p.x + "&y=" + p.y;
      }
    else
      {
        data = name + ".x=" + p.x + "&" + name + ".y=" + p.y;
      }
    return data;
  }

  /**
   * Determines and returns the enclosing form element if there is any.
   *
   * This is package private to avoid accessor methods.
   *
   * @return the enclosing form element, or <code>null</code> if there is no
   *         enclosing form element
   */
  Element getFormElement()
  {
    Element form = null;
    Element el = getElement();
    while (el != null && form == null)
      {
        AttributeSet atts = el.getAttributes();
        if (atts.getAttribute(StyleConstants.NameAttribute) == HTML.Tag.FORM)
          form = el;
        else
          el = el.getParentElement();
      }
    return form;
  }

  /**
   * Determines the form data that is about to be submitted.
   *
   * @return the form data
   */
  private String getFormData()
  {
    Element form = getFormElement();
    StringBuilder b = new StringBuilder();
    if (form != null)
      {
        ElementIterator i = new ElementIterator(form);
        Element next;
        while ((next = i.next()) != null)
          {
            if (next.isLeaf())
              {
                AttributeSet atts = next.getAttributes();
                String type = (String) atts.getAttribute(HTML.Attribute.TYPE);
                if (type != null && type.equals("submit")
                    && next != getElement())
                  {
                    // Skip this. This is not the actual submit trigger.
                  }
                else if (type == null || ! type.equals("image"))
                  {
                    getElementFormData(next, b);
                  }
              }
          }
      }
    return b.toString();
  }

  /**
   * Fetches the form data from the specified element and appends it to
   * the data string.
   *
   * @param el the element from which to fetch form data
   * @param b the data string
   */
  private void getElementFormData(Element el, StringBuilder b)
  {
    AttributeSet atts = el.getAttributes();
    String name = (String) atts.getAttribute(HTML.Attribute.NAME);
    if (name != null)
      {
        String value = null;
        HTML.Tag tag = (HTML.Tag) atts.getAttribute(StyleConstants.NameAttribute);
        if (tag == HTML.Tag.SELECT)
          {
            getSelectData(atts, b);
          }
        else
          {
            if (tag == HTML.Tag.INPUT)
              value = getInputFormData(atts);
            else if (tag == HTML.Tag.TEXTAREA)
              value = getTextAreaData(atts);
            if (name != null && value != null)
              {
                addData(b, name, value);
              }
          }
      }
  }

  /**
   * Fetches form data from select boxes.
   *
   * @param atts the attributes of the element
   *
   * @param b the form data string to append to
   */
  private void getSelectData(AttributeSet atts, StringBuilder b)
  {
    String name = (String) atts.getAttribute(HTML.Attribute.NAME);
    if (name != null)
      {
        Object m = atts.getAttribute(StyleConstants.ModelAttribute);
        if (m instanceof SelectListModel)
          {
            SelectListModel sl = (SelectListModel) m;
            ListSelectionModel lsm = sl.getSelectionModel();
            for (int i = 0; i < sl.getSize(); i++)
              {
                if (lsm.isSelectedIndex(i))
                  {
                    Option o = (Option) sl.getElementAt(i);
                    addData(b, name, o.getValue());
                  }
              }
          }
        else if (m instanceof SelectComboBoxModel)
          {
            SelectComboBoxModel scb = (SelectComboBoxModel) m;
            Option o = (Option) scb.getSelectedItem();
            if (o != null)
              addData(b, name, o.getValue());
          }
      }
  }

  /**
   * Fetches form data from a textarea.
   *
   * @param atts the attributes
   *
   * @return the form data
   */
  private String getTextAreaData(AttributeSet atts)
  {
    Document doc = (Document) atts.getAttribute(StyleConstants.ModelAttribute);
    String data;
    try
      {
        data = doc.getText(0, doc.getLength());
      }
    catch (BadLocationException ex)
      {
        data = null;
      }
    return data;
  }

  /**
   * Fetches form data from an input tag.
   *
   * @param atts the attributes from which to fetch the data
   *
   * @return the field value
   */
  private String getInputFormData(AttributeSet atts)
  {
    String type = (String) atts.getAttribute(HTML.Attribute.TYPE);
    Object model = atts.getAttribute(StyleConstants.ModelAttribute);
    String value = null;
    if (type.equals("text") || type.equals("password"))
      {
        Document doc = (Document) model;
        try
          {
            value = doc.getText(0, doc.getLength());
          }
        catch (BadLocationException ex)
          {
            // Sigh.
            assert false;
          }
      }
    else if (type.equals("hidden") || type.equals("submit"))
      {
        value = (String) atts.getAttribute(HTML.Attribute.VALUE);
        if (value == null)
          value = "";
      }
    // TODO: Implement the others. radio, checkbox and file.
    return value;
  }

  /**
   * Actually adds the specified data to the string. It URL encodes
   * the name and value and handles separation of the fields.
   *
   * @param b the string at which the form data to be added
   * @param name the name of the field
   * @param value the value
   */
  private void addData(StringBuilder b, String name, String value)
  {
    if (b.length() > 0)
      b.append('&');
    String encName = URLEncoder.encode(name);
    b.append(encName);
    b.append('=');
    String encValue = URLEncoder.encode(value);
    b.append(encValue);
  }

  /**
   * Resets the form data to their initial state.
   */
  private void resetForm()
  {
    Element form = getFormElement();
    if (form != null)
      {
        ElementIterator iter = new ElementIterator(form);
        Element next;
        while ((next = iter.next()) != null)
          {
            if (next.isLeaf())
              {
                AttributeSet atts = next.getAttributes();
                Object m = atts.getAttribute(StyleConstants.ModelAttribute);
                if (m instanceof ResetableModel)
                  ((ResetableModel) m).reset();
              }
          }
      }
  }
}
