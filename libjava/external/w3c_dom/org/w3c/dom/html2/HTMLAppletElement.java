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
 * An embedded Java applet. See the APPLET element definition in HTML 4.01. 
 * This element is deprecated in HTML 4.01.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLAppletElement extends HTMLElement {
    /**
     * Aligns this object (vertically or horizontally) with respect to its 
     * surrounding text. See the align attribute definition in HTML 4.01. 
     * This attribute is deprecated in HTML 4.01.
     */
    public String getAlign();
    /**
     * Aligns this object (vertically or horizontally) with respect to its 
     * surrounding text. See the align attribute definition in HTML 4.01. 
     * This attribute is deprecated in HTML 4.01.
     */
    public void setAlign(String align);

    /**
     * Alternate text for user agents not rendering the normal content of this 
     * element. See the alt attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public String getAlt();
    /**
     * Alternate text for user agents not rendering the normal content of this 
     * element. See the alt attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public void setAlt(String alt);

    /**
     * Comma-separated archive list. See the archive attribute definition in 
     * HTML 4.01. This attribute is deprecated in HTML 4.01.
     */
    public String getArchive();
    /**
     * Comma-separated archive list. See the archive attribute definition in 
     * HTML 4.01. This attribute is deprecated in HTML 4.01.
     */
    public void setArchive(String archive);

    /**
     * Applet class file. See the code attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public String getCode();
    /**
     * Applet class file. See the code attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public void setCode(String code);

    /**
     * Optional base URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] for applet. See the codebase attribute definition in 
     * HTML 4.01. This attribute is deprecated in HTML 4.01.
     */
    public String getCodeBase();
    /**
     * Optional base URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] for applet. See the codebase attribute definition in 
     * HTML 4.01. This attribute is deprecated in HTML 4.01.
     */
    public void setCodeBase(String codeBase);

    /**
     * Override height. See the height attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public String getHeight();
    /**
     * Override height. See the height attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public void setHeight(String height);

    /**
     * Horizontal space, in pixels, to the left and right of this image, 
     * applet, or object. See the hspace attribute definition in HTML 4.01. 
     * This attribute is deprecated in HTML 4.01.
     * @version DOM Level 2
     */
    public int getHspace();
    /**
     * Horizontal space, in pixels, to the left and right of this image, 
     * applet, or object. See the hspace attribute definition in HTML 4.01. 
     * This attribute is deprecated in HTML 4.01.
     * @version DOM Level 2
     */
    public void setHspace(int hspace);

    /**
     * The name of the applet. See the name attribute definition in HTML 4.01. 
     * This attribute is deprecated in HTML 4.01.
     */
    public String getName();
    /**
     * The name of the applet. See the name attribute definition in HTML 4.01. 
     * This attribute is deprecated in HTML 4.01.
     */
    public void setName(String name);

    /**
     * The value of the "object" attribute. See the object attribute definition
     *  in HTML 4.01. This attribute is deprecated in HTML 4.01. 
     * @version DOM Level 2
     */
    public String getObject();
    /**
     * The value of the "object" attribute. See the object attribute definition
     *  in HTML 4.01. This attribute is deprecated in HTML 4.01. 
     * @version DOM Level 2
     */
    public void setObject(String object);

    /**
     * Vertical space, in pixels, above and below this image, applet, or 
     * object. See the vspace attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     * @version DOM Level 2
     */
    public int getVspace();
    /**
     * Vertical space, in pixels, above and below this image, applet, or 
     * object. See the vspace attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     * @version DOM Level 2
     */
    public void setVspace(int vspace);

    /**
     * Override width. See the width attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public String getWidth();
    /**
     * Override width. See the width attribute definition in HTML 4.01. This 
     * attribute is deprecated in HTML 4.01.
     */
    public void setWidth(String width);

}
