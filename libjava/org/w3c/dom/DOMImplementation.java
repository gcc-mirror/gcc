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

package org.w3c.dom;

/**
 * The <code>DOMImplementation</code> interface provides a number of methods 
 * for performing operations that are independent of any particular instance 
 * of the document object model.
 * <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113'>Document Object Model (DOM) Level 2 Core Specification</a>.
 */
public interface DOMImplementation {
    /**
     * Test if the DOM implementation implements a specific feature.
     * @param featureThe name of the feature to test (case-insensitive). The 
     *   values used by DOM features are defined throughout the DOM Level 2 
     *   specifications and listed in the  section. The name must be an XML 
     *   name. To avoid possible conflicts, as a convention, names referring 
     *   to features defined outside the DOM specification should be made 
     *   unique by reversing the name of the Internet domain name of the 
     *   person (or the organization that the person belongs to) who defines 
     *   the feature, component by component, and using this as a prefix. 
     *   For instance, the W3C SVG Working Group defines the feature 
     *   "org.w3c.dom.svg".
     * @param versionThis is the version number of the feature to test. In 
     *   Level 2, the string can be either "2.0" or "1.0". If the version is 
     *   not specified, supporting any version of the feature causes the 
     *   method to return <code>true</code>.
     * @return <code>true</code> if the feature is implemented in the 
     *   specified version, <code>false</code> otherwise.
     */
    public boolean hasFeature(String feature, 
                              String version);

    /**
     * Creates an empty <code>DocumentType</code> node. Entity declarations 
     * and notations are not made available. Entity reference expansions and 
     * default attribute additions do not occur. It is expected that a 
     * future version of the DOM will provide a way for populating a 
     * <code>DocumentType</code>.
     * <br>HTML-only DOM implementations do not need to implement this method.
     * @param qualifiedNameThe qualified name of the document type to be 
     *   created. 
     * @param publicIdThe external subset public identifier.
     * @param systemIdThe external subset system identifier.
     * @return A new <code>DocumentType</code> node with 
     *   <code>Node.ownerDocument</code> set to <code>null</code>.
     * @exception DOMException
     *   INVALID_CHARACTER_ERR: Raised if the specified qualified name 
     *   contains an illegal character.
     *   <br>NAMESPACE_ERR: Raised if the <code>qualifiedName</code> is 
     *   malformed.
     * @since DOM Level 2
     */
    public DocumentType createDocumentType(String qualifiedName, 
                                           String publicId, 
                                           String systemId)
                                           throws DOMException;

    /**
     * Creates an XML <code>Document</code> object of the specified type with 
     * its document element. HTML-only DOM implementations do not need to 
     * implement this method.
     * @param namespaceURIThe namespace URI of the document element to create.
     * @param qualifiedNameThe qualified name of the document element to be 
     *   created.
     * @param doctypeThe type of document to be created or <code>null</code>.
     *   When <code>doctype</code> is not <code>null</code>, its 
     *   <code>Node.ownerDocument</code> attribute is set to the document 
     *   being created.
     * @return A new <code>Document</code> object.
     * @exception DOMException
     *   INVALID_CHARACTER_ERR: Raised if the specified qualified name 
     *   contains an illegal character.
     *   <br>NAMESPACE_ERR: Raised if the <code>qualifiedName</code> is 
     *   malformed, if the <code>qualifiedName</code> has a prefix and the 
     *   <code>namespaceURI</code> is <code>null</code>, or if the 
     *   <code>qualifiedName</code> has a prefix that is "xml" and the 
     *   <code>namespaceURI</code> is different from "
     *   http://www.w3.org/XML/1998/namespace" .
     *   <br>WRONG_DOCUMENT_ERR: Raised if <code>doctype</code> has already 
     *   been used with a different document or was created from a different 
     *   implementation.
     * @since DOM Level 2
     */
    public Document createDocument(String namespaceURI, 
                                   String qualifiedName, 
                                   DocumentType doctype)
                                   throws DOMException;

}
