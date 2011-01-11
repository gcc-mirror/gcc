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
 * This interface represents a notation declared in the DTD. A notation either
 * declares, by name, the format of an unparsed entity (see <a href='http://www.w3.org/TR/2004/REC-xml-20040204#Notations'>section 4.7</a> of the XML 1.0 specification [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>]), or is
 * used for formal declaration of processing instruction targets (see <a href='http://www.w3.org/TR/2004/REC-xml-20040204#sec-pi'>section 2.6</a> of the XML 1.0 specification [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>]). The
 * <code>nodeName</code> attribute inherited from <code>Node</code> is set
 * to the declared name of the notation.
 * <p>The DOM Core does not support editing <code>Notation</code> nodes; they
 * are therefore readonly.
 * <p>A <code>Notation</code> node does not have any parent.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 */
public interface Notation extends Node {
    /**
     * The public identifier of this notation. If the public identifier was
     * not specified, this is <code>null</code>.
     */
    public String getPublicId();

    /**
     * The system identifier of this notation. If the system identifier was
     * not specified, this is <code>null</code>. This may be an absolute URI
     * or not.
     */
    public String getSystemId();

}
