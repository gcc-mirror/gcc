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

/**
 * A new exception has been created for exceptions specific to these XPath 
 * interfaces.
 * <p>See also the <a href='http://www.w3.org/TR/2004/NOTE-DOM-Level-3-XPath-20040226'>Document Object Model (DOM) Level 3 XPath Specification</a>.
 */
public class XPathException extends RuntimeException {
    public XPathException(short code, String message) {
       super(message);
       this.code = code;
    }
    public short   code;
    // XPathExceptionCode
    /**
     * If the expression has a syntax error or otherwise is not a legal 
     * expression according to the rules of the specific 
     * <code>XPathEvaluator</code> or contains specialized extension 
     * functions or variables not supported by this implementation.
     */
    public static final short INVALID_EXPRESSION_ERR    = 51;
    /**
     * If the expression cannot be converted to return the specified type.
     */
    public static final short TYPE_ERR                  = 52;

}
