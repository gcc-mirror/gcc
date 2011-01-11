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
 *  Style information. See the STYLE element definition in HTML 4.01, the CSS
 * module [<a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Style-20001113'>DOM Level 2 Style Sheets and CSS</a>] and the <code>LinkStyle</code> interface in the StyleSheets
 * module [<a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Style-20001113'>DOM Level 2 Style Sheets and CSS</a>].
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLStyleElement extends HTMLElement {
    /**
     * Enables/disables the style sheet.
     */
    public boolean getDisabled();
    /**
     * Enables/disables the style sheet.
     */
    public void setDisabled(boolean disabled);

    /**
     * Designed for use with one or more target media. See the media attribute
     * definition in HTML 4.01.
     */
    public String getMedia();
    /**
     * Designed for use with one or more target media. See the media attribute
     * definition in HTML 4.01.
     */
    public void setMedia(String media);

    /**
     * The content type of the style sheet language. See the type attribute
     * definition in HTML 4.01.
     */
    public String getType();
    /**
     * The content type of the style sheet language. See the type attribute
     * definition in HTML 4.01.
     */
    public void setType(String type);

}
