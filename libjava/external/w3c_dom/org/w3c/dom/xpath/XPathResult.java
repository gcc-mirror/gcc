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

package org.w3c.dom.xpath;

import org.w3c.dom.Node;
import org.w3c.dom.DOMException;

/**
 * The <code>XPathResult</code> interface represents the result of the 
 * evaluation of an XPath 1.0 expression within the context of a particular 
 * node. Since evaluation of an XPath expression can result in various 
 * result types, this object makes it possible to discover and manipulate 
 * the type and value of the result.
 * <p>See also the <a href='http://www.w3.org/TR/2004/NOTE-DOM-Level-3-XPath-20040226'>Document Object Model (DOM) Level 3 XPath Specification</a>.
 */
public interface XPathResult {
    // XPathResultType
    /**
     * This code does not represent a specific type. An evaluation of an XPath 
     * expression will never produce this type. If this type is requested, 
     * then the evaluation returns whatever type naturally results from 
     * evaluation of the expression. 
     * <br>If the natural result is a node set when <code>ANY_TYPE</code> was 
     * requested, then <code>UNORDERED_NODE_ITERATOR_TYPE</code> is always 
     * the resulting type. Any other representation of a node set must be 
     * explicitly requested.
     */
    public static final short ANY_TYPE                  = 0;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#numbers'>number</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>]. 
     * Document modification does not invalidate the number, but may mean 
     * that reevaluation would not yield the same number.
     */
    public static final short NUMBER_TYPE               = 1;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#strings'>string</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>]. 
     * Document modification does not invalidate the string, but may mean 
     * that the string no longer corresponds to the current document.
     */
    public static final short STRING_TYPE               = 2;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#booleans'>boolean</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>]. 
     * Document modification does not invalidate the boolean, but may mean 
     * that reevaluation would not yield the same boolean.
     */
    public static final short BOOLEAN_TYPE              = 3;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
     * will be accessed iteratively, which may not produce nodes in a 
     * particular order. Document modification invalidates the iteration.
     * <br>This is the default type returned if the result is a node set and 
     * <code>ANY_TYPE</code> is requested.
     */
    public static final short UNORDERED_NODE_ITERATOR_TYPE = 4;
    /**
     * The result is a node set as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
     * will be accessed iteratively, which will produce document-ordered 
     * nodes. Document modification invalidates the iteration.
     */
    public static final short ORDERED_NODE_ITERATOR_TYPE = 5;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
     * will be accessed as a snapshot list of nodes that may not be in a 
     * particular order. Document modification does not invalidate the 
     * snapshot but may mean that reevaluation would not yield the same 
     * snapshot and nodes in the snapshot may have been altered, moved, or 
     * removed from the document.
     */
    public static final short UNORDERED_NODE_SNAPSHOT_TYPE = 6;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
     * will be accessed as a snapshot list of nodes that will be in original 
     * document order. Document modification does not invalidate the 
     * snapshot but may mean that reevaluation would not yield the same 
     * snapshot and nodes in the snapshot may have been altered, moved, or 
     * removed from the document.
     */
    public static final short ORDERED_NODE_SNAPSHOT_TYPE = 7;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] and 
     * will be accessed as a single node, which may be <code>null</code>if 
     * the node set is empty. Document modification does not invalidate the 
     * node, but may mean that the result node no longer corresponds to the 
     * current document. This is a convenience that permits optimization 
     * since the implementation can stop once any node in the resulting set 
     * has been found.
     * <br>If there is more than one node in the actual result, the single 
     * node returned might not be the first in document order.
     */
    public static final short ANY_UNORDERED_NODE_TYPE   = 8;
    /**
     * The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] and 
     * will be accessed as a single node, which may be <code>null</code> if 
     * the node set is empty. Document modification does not invalidate the 
     * node, but may mean that the result node no longer corresponds to the 
     * current document. This is a convenience that permits optimization 
     * since the implementation can stop once the first node in document 
     * order of the resulting set has been found.
     * <br>If there are more than one node in the actual result, the single 
     * node returned will be the first in document order.
     */
    public static final short FIRST_ORDERED_NODE_TYPE   = 9;

    /**
     * A code representing the type of this result, as defined by the type 
     * constants.
     */
    public short getResultType();

    /**
     * The value of this number result. If the native double type of the DOM 
     * binding does not directly support the exact IEEE 754 result of the 
     * XPath expression, then it is up to the definition of the binding to 
     * specify how the XPath number is converted to the native binding 
     * number.
     * @exception XPathException
     *   TYPE_ERR: raised if <code>resultType</code> is not 
     *   <code>NUMBER_TYPE</code>.
     */
    public double getNumberValue()
                             throws XPathException;

    /**
     * The value of this string result.
     * @exception XPathException
     *   TYPE_ERR: raised if <code>resultType</code> is not 
     *   <code>STRING_TYPE</code>.
     */
    public String getStringValue()
                             throws XPathException;

    /**
     * The value of this boolean result.
     * @exception XPathException
     *   TYPE_ERR: raised if <code>resultType</code> is not 
     *   <code>BOOLEAN_TYPE</code>.
     */
    public boolean getBooleanValue()
                             throws XPathException;

    /**
     * The value of this single node result, which may be <code>null</code>.
     * @exception XPathException
     *   TYPE_ERR: raised if <code>resultType</code> is not 
     *   <code>ANY_UNORDERED_NODE_TYPE</code> or 
     *   <code>FIRST_ORDERED_NODE_TYPE</code>.
     */
    public Node getSingleNodeValue()
                             throws XPathException;

    /**
     * Signifies that the iterator has become invalid. True if 
     * <code>resultType</code> is <code>UNORDERED_NODE_ITERATOR_TYPE</code> 
     * or <code>ORDERED_NODE_ITERATOR_TYPE</code> and the document has been 
     * modified since this result was returned.
     */
    public boolean getInvalidIteratorState();

    /**
     * The number of nodes in the result snapshot. Valid values for 
     * snapshotItem indices are <code>0</code> to 
     * <code>snapshotLength-1</code> inclusive.
     * @exception XPathException
     *   TYPE_ERR: raised if <code>resultType</code> is not 
     *   <code>UNORDERED_NODE_SNAPSHOT_TYPE</code> or 
     *   <code>ORDERED_NODE_SNAPSHOT_TYPE</code>.
     */
    public int getSnapshotLength()
                             throws XPathException;

    /**
     * Iterates and returns the next node from the node set or 
     * <code>null</code>if there are no more nodes.
     * @return Returns the next node.
     * @exception XPathException
     *   TYPE_ERR: raised if <code>resultType</code> is not 
     *   <code>UNORDERED_NODE_ITERATOR_TYPE</code> or 
     *   <code>ORDERED_NODE_ITERATOR_TYPE</code>.
     * @exception DOMException
     *   INVALID_STATE_ERR: The document has been mutated since the result was 
     *   returned.
     */
    public Node iterateNext()
                            throws XPathException, DOMException;

    /**
     * Returns the <code>index</code>th item in the snapshot collection. If 
     * <code>index</code> is greater than or equal to the number of nodes in 
     * the list, this method returns <code>null</code>. Unlike the iterator 
     * result, the snapshot does not become invalid, but may not correspond 
     * to the current document if it is mutated.
     * @param index Index into the snapshot collection.
     * @return The node at the <code>index</code>th position in the 
     *   <code>NodeList</code>, or <code>null</code> if that is not a valid 
     *   index.
     * @exception XPathException
     *   TYPE_ERR: raised if <code>resultType</code> is not 
     *   <code>UNORDERED_NODE_SNAPSHOT_TYPE</code> or 
     *   <code>ORDERED_NODE_SNAPSHOT_TYPE</code>.
     */
    public Node snapshotItem(int index)
                             throws XPathException;

}
