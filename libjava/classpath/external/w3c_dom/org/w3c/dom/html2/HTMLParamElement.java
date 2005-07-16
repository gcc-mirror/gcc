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
 * Parameters fed to the <code>OBJECT</code> element. See the PARAM element 
 * definition in HTML 4.01.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLParamElement extends HTMLElement {
    /**
     * The name of a run-time parameter. See the name attribute definition in 
     * HTML 4.01.
     */
    public String getName();
    /**
     * The name of a run-time parameter. See the name attribute definition in 
     * HTML 4.01.
     */
    public void setName(String name);

    /**
     * Content type for the <code>value</code> attribute when 
     * <code>valuetype</code> has the value "ref". See the type attribute 
     * definition in HTML 4.01.
     */
    public String getType();
    /**
     * Content type for the <code>value</code> attribute when 
     * <code>valuetype</code> has the value "ref". See the type attribute 
     * definition in HTML 4.01.
     */
    public void setType(String type);

    /**
     * The value of a run-time parameter. See the value attribute definition 
     * in HTML 4.01.
     */
    public String getValue();
    /**
     * The value of a run-time parameter. See the value attribute definition 
     * in HTML 4.01.
     */
    public void setValue(String value);

    /**
     * Information about the meaning of the <code>value</code> attribute 
     * value. See the valuetype attribute definition in HTML 4.01.
     */
    public String getValueType();
    /**
     * Information about the meaning of the <code>value</code> attribute 
     * value. See the valuetype attribute definition in HTML 4.01.
     */
    public void setValueType(String valueType);

}
