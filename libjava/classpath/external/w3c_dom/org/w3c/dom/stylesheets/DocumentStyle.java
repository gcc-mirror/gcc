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

package org.w3c.dom.stylesheets;

/**
 *  The <code>DocumentStyle</code> interface provides a mechanism by which the
 * style sheets embedded in a document can be retrieved. The expectation is
 * that an instance of the <code>DocumentStyle</code> interface can be
 * obtained by using binding-specific casting methods on an instance of the
 * <code>Document</code> interface.
 * <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Style-20001113'>Document Object Model (DOM) Level 2 Style Specification</a>.
 * @since DOM Level 2
 */
public interface DocumentStyle {
    /**
     *  A list containing all the style sheets explicitly linked into or
     * embedded in a document. For HTML documents, this includes external
     * style sheets, included via the HTML  LINK element, and inline  STYLE
     * elements. In XML, this includes external style sheets, included via
     * style sheet processing instructions (see [XML StyleSheet]).
     */
    public StyleSheetList getStyleSheets();

}
