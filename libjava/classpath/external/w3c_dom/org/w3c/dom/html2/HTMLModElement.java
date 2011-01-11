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
 * Notice of modification to part of a document. See the INS and DEL element
 * definitions in HTML 4.01.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLModElement extends HTMLElement {
    /**
     * A URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] designating a document that describes the reason for the change.
     * See the cite attribute definition in HTML 4.01.
     */
    public String getCite();
    /**
     * A URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] designating a document that describes the reason for the change.
     * See the cite attribute definition in HTML 4.01.
     */
    public void setCite(String cite);

    /**
     * The date and time of the change. See the datetime attribute definition
     * in HTML 4.01.
     */
    public String getDateTime();
    /**
     * The date and time of the change. See the datetime attribute definition
     * in HTML 4.01.
     */
    public void setDateTime(String dateTime);

}
