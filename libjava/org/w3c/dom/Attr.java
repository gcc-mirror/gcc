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
 *  The <code>Attr</code> interface represents an attribute in an 
 * <code>Element</code> object. Typically the allowable values for the 
 * attribute are defined in a document type definition.
 * <p><code>Attr</code> objects inherit the <code>Node</code> interface, but 
 * since they are not actually child nodes of the element they describe, the 
 * DOM does not consider them part of the document tree. Thus, the 
 * <code>Node</code> attributes <code>parentNode</code>, 
 * <code>previousSibling</code>, and <code>nextSibling</code> have a 
 * <code>null</code> value for <code>Attr</code> objects. The DOM takes the 
 * view that attributes are properties of elements rather than having a 
 * separate identity from the elements they are associated with; this should 
 * make it more efficient to implement such features as default attributes 
 * associated with all elements of a given type. Furthermore, 
 * <code>Attr</code> nodes may not be immediate children of a 
 * <code>DocumentFragment</code>. However, they can be associated with 
 * <code>Element</code> nodes contained within a 
 * <code>DocumentFragment</code>. In short, users and implementors of the 
 * DOM need to be aware that <code>Attr</code> nodes have some things in 
 * common with other objects inheriting the <code>Node</code> interface, but 
 * they also are quite distinct.
 * <p> The attribute's effective value is determined as follows: if this 
 * attribute has been explicitly assigned any value, that value is the 
 * attribute's effective value; otherwise, if there is a declaration for 
 * this attribute, and that declaration includes a default value, then that 
 * default value is the attribute's effective value; otherwise, the 
 * attribute does not exist on this element in the structure model until it 
 * has been explicitly added. Note that the <code>nodeValue</code> attribute 
 * on the <code>Attr</code> instance can also be used to retrieve the string 
 * version of the attribute's value(s). 
 * <p>In XML, where the value of an attribute can contain entity references, 
 * the child nodes of the <code>Attr</code> node may be either 
 * <code>Text</code> or <code>EntityReference</code> nodes (when these are 
 * in use; see the description of <code>EntityReference</code> for 
 * discussion). Because the DOM Core is not aware of attribute types, it 
 * treats all attribute values as simple strings, even if the DTD or schema 
 * declares them as having tokenized types. 
 * <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113'>Document Object Model (DOM) Level 2 Core Specification</a>.
 */
public interface Attr extends Node {
    /**
     * Returns the name of this attribute. 
     */
    public String getName();

    /**
     * If this attribute was explicitly given a value in the original 
     * document, this is <code>true</code>; otherwise, it is 
     * <code>false</code>. Note that the implementation is in charge of this 
     * attribute, not the user. If the user changes the value of the 
     * attribute (even if it ends up having the same value as the default 
     * value) then the <code>specified</code> flag is automatically flipped 
     * to <code>true</code>. To re-specify the attribute as the default 
     * value from the DTD, the user must delete the attribute. The 
     * implementation will then make a new attribute available with 
     * <code>specified</code> set to <code>false</code> and the default 
     * value (if one exists).
     * <br>In summary:  If the attribute has an assigned value in the document 
     * then <code>specified</code> is <code>true</code>, and the value is 
     * the assigned value.  If the attribute has no assigned value in the 
     * document and has a default value in the DTD, then 
     * <code>specified</code> is <code>false</code>, and the value is the 
     * default value in the DTD. If the attribute has no assigned value in 
     * the document and has a value of #IMPLIED in the DTD, then the 
     * attribute does not appear in the structure model of the document. If 
     * the <code>ownerElement</code> attribute is <code>null</code> (i.e. 
     * because it was just created or was set to <code>null</code> by the 
     * various removal and cloning operations) <code>specified</code> is 
     * <code>true</code>. 
     */
    public boolean getSpecified();

    /**
     * On retrieval, the value of the attribute is returned as a string. 
     * Character and general entity references are replaced with their 
     * values. See also the method <code>getAttribute</code> on the 
     * <code>Element</code> interface.
     * <br>On setting, this creates a <code>Text</code> node with the unparsed 
     * contents of the string. I.e. any characters that an XML processor 
     * would recognize as markup are instead treated as literal text. See 
     * also the method <code>setAttribute</code> on the <code>Element</code> 
     * interface.
     * @exception DOMException
     *   NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
     */
    public String getValue();
    public void setValue(String value)
                            throws DOMException;

    /**
     * The <code>Element</code> node this attribute is attached to or 
     * <code>null</code> if this attribute is not in use.
     * @since DOM Level 2
     */
    public Element getOwnerElement();

}
