/*
 * Copyright (c) 2004 World Wide Web Consortium,
 *
 * (Massachusetts Institute of Technology, European Research Consortium for
 * Informatics and Mathematics, Keio University). All Rights Reserved. This
 * work is distributed under the W3C(r) Software License [1] in the hope that
 * it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * [1] http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231
 */

package org.w3c.dom;

/**
 *  The <code>DOMImplementationList</code> interface provides the abstraction
 * of an ordered collection of DOM implementations, without defining or
 * constraining how this collection is implemented. The items in the
 * <code>DOMImplementationList</code> are accessible via an integral index,
 * starting from 0.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 * @since DOM Level 3
 */
public interface DOMImplementationList {
    /**
     *  Returns the <code>index</code>th item in the collection. If
     * <code>index</code> is greater than or equal to the number of
     * <code>DOMImplementation</code>s in the list, this returns
     * <code>null</code>.
     * @param index Index into the collection.
     * @return  The <code>DOMImplementation</code> at the <code>index</code>
     *   th position in the <code>DOMImplementationList</code>, or
     *   <code>null</code> if that is not a valid index.
     */
    public DOMImplementation item(int index);

    /**
     *  The number of <code>DOMImplementation</code>s in the list. The range
     * of valid child node indices is 0 to <code>length-1</code> inclusive.
     */
    public int getLength();

}
