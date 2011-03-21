/*
 * Copyright (c) 2000 World Wide Web Consortium,
 * (Massachusetts Institute of Technology, Institut National de
 * Recherche en Informatique et en Automatique, Keio University). All
 * Rights Reserved. This program is distributed under the W3C's Software
 * Intellectual Property License. This program is distributed in the
 * hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.
 * See W3C License http://www.w3.org/Consortium/Legal/ for more details.
 */

package org.w3c.dom.css;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.DOMException;

/**
 *  This interface allows the DOM user to create a <code>CSSStyleSheet</code>
 * outside the context of a document. There is no way to associate the new
 * <code>CSSStyleSheet</code> with a document in DOM Level 2.
 * <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Style-20001113'>Document Object Model (DOM) Level 2 Style Specification</a>.
 * @since DOM   Level 2
 */
public interface DOMImplementationCSS extends DOMImplementation {
    /**
     * Creates a new <code>CSSStyleSheet</code>.
     * @param title  The advisory title. See also the  section.
     * @param media  The comma-separated list of media associated with the
     *   new style sheet. See also the  section.
     * @return A new CSS style sheet.
     * @exception DOMException
     *    SYNTAX_ERR: Raised if the specified media string value has a syntax
     *   error and is unparsable.
     */
    public CSSStyleSheet createCSSStyleSheet(String title,
                                             String media)
                                             throws DOMException;

}
