// Attributes.java - attribute list with Namespace support
// http://www.saxproject.org
// Written by David Megginson
// NO WARRANTY!  This class is in the public domain.

// $Id: Attributes.java,v 1.5.2.4 2002/01/29 21:34:14 dbrownell Exp $


package org.xml.sax;


/**
 * Interface for a list of XML attributes.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>This interface allows access to a list of attributes in
 * three different ways:</p>
 *
 * <ol>
 * <li>by attribute index;</li>
 * <li>by Namespace-qualified name; or</li>
 * <li>by qualified (prefixed) name.</li>
 * </ol>
 *
 * <p>The list will not contain attributes that were declared
 * #IMPLIED but not specified in the start tag.  It will also not
 * contain attributes used as Namespace declarations (xmlns*) unless
 * the <code>http://xml.org/sax/features/namespace-prefixes</code> 
 * feature is set to <var>true</var> (it is <var>false</var> by 
 * default).
 * Because SAX2 conforms to the "Namespaces in XML" specification,
 * it does not give namespace declaration attributes a namespace URI.
 * Some other W3C specifications are in conflict with that, expecting
 * these declarations to be in a namespace.
 * Handler code may need to resolve that conflict.
 * </p>
 *
 * <p>If the namespace-prefixes feature (see above) is <var>false</var>, 
 * access by qualified name may not be available; if the 
 * <code>http://xml.org/sax/features/namespaces</code>
 * feature is <var>false</var>, access by Namespace-qualified names 
 * may not be available.</p>
 *
 * <p>This interface replaces the now-deprecated SAX1 {@link
 * org.xml.sax.AttributeList AttributeList} interface, which does not 
 * contain Namespace support.  In addition to Namespace support, it 
 * adds the <var>getIndex</var> methods (below).</p>
 *
 * <p>The order of attributes in the list is unspecified, and will
 * vary from implementation to implementation.</p>
 *
 * @since SAX 2.0
 * @author David Megginson
 * @version 2.0.1 (sax2r2)
 * @see org.xml.sax.helpers.AttributesImpl
 * @see org.xml.sax.ext.DeclHandler#attributeDecl
 */
public interface Attributes
{


    ////////////////////////////////////////////////////////////////////
    // Indexed access.
    ////////////////////////////////////////////////////////////////////


    /**
     * Return the number of attributes in the list.
     *
     * <p>Once you know the number of attributes, you can iterate
     * through the list.</p>
     *
     * @return The number of attributes in the list.
     * @see #getURI(int)
     * @see #getLocalName(int)
     * @see #getQName(int)
     * @see #getType(int)
     * @see #getValue(int)
     */
    public abstract int getLength ();


    /**
     * Look up an attribute's Namespace URI by index.
     *
     * @param index The attribute index (zero-based).
     * @return The Namespace URI, or the empty string if none
     *         is available, or null if the index is out of
     *         range.
     * @see #getLength
     */
    public abstract String getURI (int index);


    /**
     * Look up an attribute's local name by index.
     *
     * @param index The attribute index (zero-based).
     * @return The local name, or the empty string if Namespace
     *         processing is not being performed, or null
     *         if the index is out of range.
     * @see #getLength
     */
    public abstract String getLocalName (int index);


    /**
     * Look up an attribute's XML 1.0 qualified name by index.
     *
     * @param index The attribute index (zero-based).
     * @return The XML 1.0 qualified name, or the empty string
     *         if none is available, or null if the index
     *         is out of range.
     * @see #getLength
     */
    public abstract String getQName (int index);


    /**
     * Look up an attribute's type by index.
     *
     * <p>The attribute type is one of the strings "CDATA", "ID",
     * "IDREF", "IDREFS", "NMTOKEN", "NMTOKENS", "ENTITY", "ENTITIES",
     * or "NOTATION" (always in upper case).</p>
     *
     * <p>If the parser has not read a declaration for the attribute,
     * or if the parser does not report attribute types, then it must
     * return the value "CDATA" as stated in the XML 1.0 Recommentation
     * (clause 3.3.3, "Attribute-Value Normalization").</p>
     *
     * <p>For an enumerated attribute that is not a notation, the
     * parser will report the type as "NMTOKEN".</p>
     *
     * @param index The attribute index (zero-based).
     * @return The attribute's type as a string, or null if the
     *         index is out of range.
     * @see #getLength
     */
    public abstract String getType (int index);


    /**
     * Look up an attribute's value by index.
     *
     * <p>If the attribute value is a list of tokens (IDREFS,
     * ENTITIES, or NMTOKENS), the tokens will be concatenated
     * into a single string with each token separated by a
     * single space.</p>
     *
     * @param index The attribute index (zero-based).
     * @return The attribute's value as a string, or null if the
     *         index is out of range.
     * @see #getLength
     */
    public abstract String getValue (int index);



    ////////////////////////////////////////////////////////////////////
    // Name-based query.
    ////////////////////////////////////////////////////////////////////


    /**
     * Look up the index of an attribute by Namespace name.
     *
     * @param uri The Namespace URI, or the empty string if
     *        the name has no Namespace URI.
     * @param localName The attribute's local name.
     * @return The index of the attribute, or -1 if it does not
     *         appear in the list.
     */
    public int getIndex (String uri, String localName);


    /**
     * Look up the index of an attribute by XML 1.0 qualified name.
     *
     * @param qName The qualified (prefixed) name.
     * @return The index of the attribute, or -1 if it does not
     *         appear in the list.
     */
    public int getIndex (String qName);


    /**
     * Look up an attribute's type by Namespace name.
     *
     * <p>See {@link #getType(int) getType(int)} for a description
     * of the possible types.</p>
     *
     * @param uri The Namespace URI, or the empty String if the
     *        name has no Namespace URI.
     * @param localName The local name of the attribute.
     * @return The attribute type as a string, or null if the
     *         attribute is not in the list or if Namespace
     *         processing is not being performed.
     */
    public abstract String getType (String uri, String localName);


    /**
     * Look up an attribute's type by XML 1.0 qualified name.
     *
     * <p>See {@link #getType(int) getType(int)} for a description
     * of the possible types.</p>
     *
     * @param qName The XML 1.0 qualified name.
     * @return The attribute type as a string, or null if the
     *         attribute is not in the list or if qualified names
     *         are not available.
     */
    public abstract String getType (String qName);


    /**
     * Look up an attribute's value by Namespace name.
     *
     * <p>See {@link #getValue(int) getValue(int)} for a description
     * of the possible values.</p>
     *
     * @param uri The Namespace URI, or the empty String if the
     *        name has no Namespace URI.
     * @param localName The local name of the attribute.
     * @return The attribute value as a string, or null if the
     *         attribute is not in the list.
     */
    public abstract String getValue (String uri, String localName);


    /**
     * Look up an attribute's value by XML 1.0 qualified name.
     *
     * <p>See {@link #getValue(int) getValue(int)} for a description
     * of the possible values.</p>
     *
     * @param qName The XML 1.0 qualified name.
     * @return The attribute value as a string, or null if the
     *         attribute is not in the list or if qualified names
     *         are not available.
     */
    public abstract String getValue (String qName);

}

// end of Attributes.java
