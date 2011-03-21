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
 * The <code>XPathExpression</code> interface represents a parsed and resolved
 * XPath expression.
 * <p>See also the <a href='http://www.w3.org/TR/2004/NOTE-DOM-Level-3-XPath-20040226'>Document Object Model (DOM) Level 3 XPath Specification</a>.
 */
public interface XPathExpression {
    /**
     * Evaluates this XPath expression and returns a result.
     * @param contextNode The <code>context</code> is context node for the
     *   evaluation of this XPath expression.If the XPathEvaluator was
     *   obtained by casting the <code>Document</code> then this must be
     *   owned by the same document and must be a <code>Document</code>,
     *   <code>Element</code>, <code>Attribute</code>, <code>Text</code>,
     *   <code>CDATASection</code>, <code>Comment</code>,
     *   <code>ProcessingInstruction</code>, or <code>XPathNamespace</code>
     *   node.If the context node is a <code>Text</code> or a
     *   <code>CDATASection</code>, then the context is interpreted as the
     *   whole logical text node as seen by XPath, unless the node is empty
     *   in which case it may not serve as the XPath context.
     * @param type If a specific <code>type</code> is specified, then the
     *   result will be coerced to return the specified type relying on
     *   XPath conversions and fail if the desired coercion is not possible.
     *   This must be one of the type codes of <code>XPathResult</code>.
     * @param result The <code>result</code> specifies a specific result
     *   object which may be reused and returned by this method. If this is
     *   specified as <code>null</code>or the implementation does not reuse
     *   the specified result, a new result object will be constructed and
     *   returned.For XPath 1.0 results, this object will be of type
     *   <code>XPathResult</code>.
     * @return The result of the evaluation of the XPath expression.For XPath
     *   1.0 results, this object will be of type <code>XPathResult</code>.
     * @exception XPathException
     *   TYPE_ERR: Raised if the result cannot be converted to return the
     *   specified type.
     * @exception DOMException
     *   WRONG_DOCUMENT_ERR: The Node is from a document that is not supported
     *   by the XPathEvaluator that created this <code>XPathExpression</code>
     *   .
     *   <br>NOT_SUPPORTED_ERR: The Node is not a type permitted as an XPath
     *   context node or the request type is not permitted by this
     *   <code>XPathExpression</code>.
     */
    public Object evaluate(Node contextNode,
                           short type,
                           Object result)
                           throws XPathException, DOMException;

}
