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

import org.w3c.dom.DOMException;

/**
 *  The <code>CSSPageRule</code> interface represents a @page rule within a
 * CSS style sheet. The <code>@page</code> rule is used to specify the
 * dimensions, orientation, margins, etc. of a page box for paged media.
 * <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Style-20001113'>Document Object Model (DOM) Level 2 Style Specification</a>.
 * @since DOM Level 2
 */
public interface CSSPageRule extends CSSRule {
    /**
     *  The parsable textual representation of the page selector for the rule.
     */
    public String getSelectorText();
    /**
     *  The parsable textual representation of the page selector for the rule.
     * @exception DOMException
     *   SYNTAX_ERR: Raised if the specified CSS string value has a syntax
     *   error and is unparsable.
     *   <br>NO_MODIFICATION_ALLOWED_ERR: Raised if this rule is readonly.
     */
    public void setSelectorText(String selectorText)
                           throws DOMException;

    /**
     *  The declaration-block of this rule.
     */
    public CSSStyleDeclaration getStyle();

}
