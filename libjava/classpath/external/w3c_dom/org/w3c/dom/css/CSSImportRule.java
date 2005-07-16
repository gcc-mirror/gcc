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

import org.w3c.dom.stylesheets.MediaList;

/**
 *  The <code>CSSImportRule</code> interface represents a @import rule within 
 * a CSS style sheet. The <code>@import</code> rule is used to import style 
 * rules from other style sheets. 
 * <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Style-20001113'>Document Object Model (DOM) Level 2 Style Specification</a>.
 * @since DOM Level 2
 */
public interface CSSImportRule extends CSSRule {
    /**
     *  The location of the style sheet to be imported. The attribute will not 
     * contain the <code>"url(...)"</code> specifier around the URI. 
     */
    public String getHref();

    /**
     *  A list of media types for which this style sheet may be used. 
     */
    public MediaList getMedia();

    /**
     * The style sheet referred to by this rule, if it has been loaded. The 
     * value of this attribute is <code>null</code> if the style sheet has 
     * not yet been loaded or if it will not be loaded (e.g. if the style 
     * sheet is for a media type not supported by the user agent). 
     */
    public CSSStyleSheet getStyleSheet();

}
