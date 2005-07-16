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

/**
 *  The <code>CSSRuleList</code> interface provides the abstraction of an 
 * ordered collection of CSS rules. 
 * <p> The items in the <code>CSSRuleList</code> are accessible via an 
 * integral index, starting from 0. 
 * <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Style-20001113'>Document Object Model (DOM) Level 2 Style Specification</a>.
 * @since DOM Level 2
 */
public interface CSSRuleList {
    /**
     *  The number of <code>CSSRules</code> in the list. The range of valid 
     * child rule indices is <code>0</code> to <code>length-1</code> 
     * inclusive. 
     */
    public int getLength();

    /**
     *  Used to retrieve a CSS rule by ordinal index. The order in this 
     * collection represents the order of the rules in the CSS style sheet. 
     * If index is greater than or equal to the number of rules in the list, 
     * this returns <code>null</code>. 
     * @param index Index into the collection
     * @return The style rule at the <code>index</code> position in the 
     *   <code>CSSRuleList</code>, or <code>null</code> if that is not a 
     *   valid index. 
     */
    public CSSRule item(int index);

}
