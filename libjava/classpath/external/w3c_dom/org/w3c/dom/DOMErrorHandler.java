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
 *  <code>DOMErrorHandler</code> is a callback interface that the DOM
 * implementation can call when reporting errors that happens while
 * processing XML data, or when doing some other processing (e.g. validating
 * a document). A <code>DOMErrorHandler</code> object can be attached to a
 * <code>Document</code> using the "error-handler" on the
 * <code>DOMConfiguration</code> interface. If more than one error needs to
 * be reported during an operation, the sequence and numbers of the errors
 * passed to the error handler are implementation dependent.
 * <p> The application that is using the DOM implementation is expected to
 * implement this interface.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 * @since DOM Level 3
 */
public interface DOMErrorHandler {
    /**
     * This method is called on the error handler when an error occurs.
     * <br> If an exception is thrown from this method, it is considered to be
     * equivalent of returning <code>true</code>.
     * @param error  The error object that describes the error. This object
     *   may be reused by the DOM implementation across multiple calls to
     *   the <code>handleError</code> method.
     * @return  If the <code>handleError</code> method returns
     *   <code>false</code>, the DOM implementation should stop the current
     *   processing when possible. If the method returns <code>true</code>,
     *   the processing may continue depending on
     *   <code>DOMError.severity</code>.
     */
    public boolean handleError(DOMError error);

}
