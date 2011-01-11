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
 * This element is used for single-line text input. See the ISINDEX element
 * definition in HTML 4.01. This element is deprecated in HTML 4.01.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLIsIndexElement extends HTMLElement {
    /**
     * Returns the <code>FORM</code> element containing this control. Returns
     * <code>null</code> if this control is not within the context of a
     * form.
     */
    public HTMLFormElement getForm();

    /**
     * The prompt message. See the prompt attribute definition in HTML 4.01.
     * This attribute is deprecated in HTML 4.01.
     */
    public String getPrompt();
    /**
     * The prompt message. See the prompt attribute definition in HTML 4.01.
     * This attribute is deprecated in HTML 4.01.
     */
    public void setPrompt(String prompt);

}
