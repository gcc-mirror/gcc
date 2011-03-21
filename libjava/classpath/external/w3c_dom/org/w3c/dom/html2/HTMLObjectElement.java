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

import org.w3c.dom.Document;

/**
 * Generic embedded object.In principle, all properties on the object element
 * are read-write but in some environments some properties may be read-only
 * once the underlying object is instantiated. See the OBJECT element
 * definition in [<a href='http://www.w3.org/TR/1999/REC-html401-19991224'>HTML 4.01</a>].
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLObjectElement extends HTMLElement {
    /**
     * Returns the <code>FORM</code> element containing this control. Returns
     * <code>null</code> if this control is not within the context of a
     * form.
     */
    public HTMLFormElement getForm();

    /**
     * Applet class file. See the <code>code</code> attribute for
     * HTMLAppletElement.
     */
    public String getCode();
    /**
     * Applet class file. See the <code>code</code> attribute for
     * HTMLAppletElement.
     */
    public void setCode(String code);

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
     * Space-separated list of archives. See the archive attribute definition
     * in HTML 4.01.
     */
    public String getArchive();
    /**
     * Space-separated list of archives. See the archive attribute definition
     * in HTML 4.01.
     */
    public void setArchive(String archive);

    /**
     * Width of border around the object. See the border attribute definition
     * in HTML 4.01. This attribute is deprecated in HTML 4.01.
     */
    public String getBorder();
    /**
     * Width of border around the object. See the border attribute definition
     * in HTML 4.01. This attribute is deprecated in HTML 4.01.
     */
    public void setBorder(String border);

    /**
     * Base URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] for <code>classid</code>, <code>data</code>, and
     * <code>archive</code> attributes. See the codebase attribute definition
     *  in HTML 4.01.
     */
    public String getCodeBase();
    /**
     * Base URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] for <code>classid</code>, <code>data</code>, and
     * <code>archive</code> attributes. See the codebase attribute definition
     *  in HTML 4.01.
     */
    public void setCodeBase(String codeBase);

    /**
     * Content type for data downloaded via <code>classid</code> attribute.
     * See the codetype attribute definition in HTML 4.01.
     */
    public String getCodeType();
    /**
     * Content type for data downloaded via <code>classid</code> attribute.
     * See the codetype attribute definition in HTML 4.01.
     */
    public void setCodeType(String codeType);

    /**
     * A URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] specifying the location of the object's data. See the data
     * attribute definition in HTML 4.01.
     */
    public String getData();
    /**
     * A URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] specifying the location of the object's data. See the data
     * attribute definition in HTML 4.01.
     */
    public void setData(String data);

    /**
     * Declare (for future reference), but do not instantiate, this object.
     * See the declare attribute definition in HTML 4.01.
     */
    public boolean getDeclare();
    /**
     * Declare (for future reference), but do not instantiate, this object.
     * See the declare attribute definition in HTML 4.01.
     */
    public void setDeclare(boolean declare);

    /**
     * Override height. See the height attribute definition in HTML 4.01.
     */
    public String getHeight();
    /**
     * Override height. See the height attribute definition in HTML 4.01.
     */
    public void setHeight(String height);

    /**
     * Horizontal space, in pixels, to the left and right of this image,
     * applet, or object. See the hspace attribute definition in HTML 4.01.
     * This attribute is deprecated in HTML 4.01.
     */
    public int getHspace();
    /**
     * Horizontal space, in pixels, to the left and right of this image,
     * applet, or object. See the hspace attribute definition in HTML 4.01.
     * This attribute is deprecated in HTML 4.01.
     */
    public void setHspace(int hspace);

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
     * Message to render while loading the object. See the standby attribute
     * definition in HTML 4.01.
     */
    public String getStandby();
    /**
     * Message to render while loading the object. See the standby attribute
     * definition in HTML 4.01.
     */
    public void setStandby(String standby);

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
     * Content type for data downloaded via <code>data</code> attribute. See
     * the type attribute definition in HTML 4.01.
     */
    public String getType();
    /**
     * Content type for data downloaded via <code>data</code> attribute. See
     * the type attribute definition in HTML 4.01.
     */
    public void setType(String type);

    /**
     * Use client-side image map. See the usemap attribute definition in HTML
     * 4.01.
     */
    public String getUseMap();
    /**
     * Use client-side image map. See the usemap attribute definition in HTML
     * 4.01.
     */
    public void setUseMap(String useMap);

    /**
     * Vertical space, in pixels, above and below this image, applet, or
     * object. See the vspace attribute definition in HTML 4.01. This
     * attribute is deprecated in HTML 4.01.
     */
    public int getVspace();
    /**
     * Vertical space, in pixels, above and below this image, applet, or
     * object. See the vspace attribute definition in HTML 4.01. This
     * attribute is deprecated in HTML 4.01.
     */
    public void setVspace(int vspace);

    /**
     * Override width. See the width attribute definition in HTML 4.01.
     */
    public String getWidth();
    /**
     * Override width. See the width attribute definition in HTML 4.01.
     */
    public void setWidth(String width);

    /**
     * The document this object contains, if there is any and it is available,
     * or <code>null</code> otherwise.
     * @since DOM Level 2
     */
    public Document getContentDocument();

}
