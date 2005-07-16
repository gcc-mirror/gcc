/*
 * Copyright (c) 2003 World Wide Web Consortium,
 * (Massachusetts Institute of Technology, Institut National de
 * Recherche en Informatique et en Automatique, Keio University). All
 * Rights Reserved. This program is distributed under the W3C's Software
 * Intellectual Property License. This program is distributed in the
 * hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.
 * See W3C License http://www.w3.org/Consortium/Legal/ for more details.
 */

package org.w3c.dom.html2;

/**
 * Multi-line text field. See the TEXTAREA element definition in HTML 4.01.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLTextAreaElement extends HTMLElement {
    /**
     * Represents the contents of the element. The value of this attribute 
     * does not change if the contents of the corresponding form control, in 
     * an interactive user agent, changes.
     * @version DOM Level 2
     */
    public String getDefaultValue();
    /**
     * Represents the contents of the element. The value of this attribute 
     * does not change if the contents of the corresponding form control, in 
     * an interactive user agent, changes.
     * @version DOM Level 2
     */
    public void setDefaultValue(String defaultValue);

    /**
     * Returns the <code>FORM</code> element containing this control. Returns 
     * <code>null</code> if this control is not within the context of a 
     * form. 
     */
    public HTMLFormElement getForm();

    /**
     * A single character access key to give access to the form control. See 
     * the accesskey attribute definition in HTML 4.01.
     */
    public String getAccessKey();
    /**
     * A single character access key to give access to the form control. See 
     * the accesskey attribute definition in HTML 4.01.
     */
    public void setAccessKey(String accessKey);

    /**
     * Width of control (in characters). See the cols attribute definition in 
     * HTML 4.01.
     */
    public int getCols();
    /**
     * Width of control (in characters). See the cols attribute definition in 
     * HTML 4.01.
     */
    public void setCols(int cols);

    /**
     * The control is unavailable in this context. See the disabled attribute 
     * definition in HTML 4.01.
     */
    public boolean getDisabled();
    /**
     * The control is unavailable in this context. See the disabled attribute 
     * definition in HTML 4.01.
     */
    public void setDisabled(boolean disabled);

    /**
     * Form control or object name when submitted with a form. See the name 
     * attribute definition in HTML 4.01.
     */
    public String getName();
    /**
     * Form control or object name when submitted with a form. See the name 
     * attribute definition in HTML 4.01.
     */
    public void setName(String name);

    /**
     * This control is read-only. See the readonly attribute definition in 
     * HTML 4.01.
     */
    public boolean getReadOnly();
    /**
     * This control is read-only. See the readonly attribute definition in 
     * HTML 4.01.
     */
    public void setReadOnly(boolean readOnly);

    /**
     * Number of text rows. See the rows attribute definition in HTML 4.01.
     */
    public int getRows();
    /**
     * Number of text rows. See the rows attribute definition in HTML 4.01.
     */
    public void setRows(int rows);

    /**
     * Index that represents the element's position in the tabbing order. See 
     * the tabindex attribute definition in HTML 4.01.
     */
    public int getTabIndex();
    /**
     * Index that represents the element's position in the tabbing order. See 
     * the tabindex attribute definition in HTML 4.01.
     */
    public void setTabIndex(int tabIndex);

    /**
     * The type of this form control. This the string "textarea".
     */
    public String getType();

    /**
     * Represents the current contents of the corresponding form control, in 
     * an interactive user agent. Changing this attribute changes the 
     * contents of the form control, but does not change the contents of the 
     * element. If the entirety of the data can not fit into a single 
     * <code>DOMString</code>, the implementation may truncate the data.
     */
    public String getValue();
    /**
     * Represents the current contents of the corresponding form control, in 
     * an interactive user agent. Changing this attribute changes the 
     * contents of the form control, but does not change the contents of the 
     * element. If the entirety of the data can not fit into a single 
     * <code>DOMString</code>, the implementation may truncate the data.
     */
    public void setValue(String value);

    /**
     * Removes keyboard focus from this element.
     */
    public void blur();

    /**
     * Gives keyboard focus to this element.
     */
    public void focus();

    /**
     * Select the contents of the <code>TEXTAREA</code>.
     */
    public void select();

}
