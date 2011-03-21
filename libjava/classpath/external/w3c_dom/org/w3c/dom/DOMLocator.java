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
 * <code>DOMLocator</code> is an interface that describes a location (e.g.
 * where an error occurred).
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 * @since DOM Level 3
 */
public interface DOMLocator {
    /**
     * The line number this locator is pointing to, or <code>-1</code> if
     * there is no column number available.
     */
    public int getLineNumber();

    /**
     * The column number this locator is pointing to, or <code>-1</code> if
     * there is no column number available.
     */
    public int getColumnNumber();

    /**
     * The byte offset into the input source this locator is pointing to or
     * <code>-1</code> if there is no byte offset available.
     */
    public int getByteOffset();

    /**
     * The UTF-16, as defined in [Unicode] and Amendment 1 of [ISO/IEC 10646], offset into the input source this locator is pointing to or
     * <code>-1</code> if there is no UTF-16 offset available.
     */
    public int getUtf16Offset();

    /**
     * The node this locator is pointing to, or <code>null</code> if no node
     * is available.
     */
    public Node getRelatedNode();

    /**
     * The URI this locator is pointing to, or <code>null</code> if no URI is
     * available.
     */
    public String getUri();

}
