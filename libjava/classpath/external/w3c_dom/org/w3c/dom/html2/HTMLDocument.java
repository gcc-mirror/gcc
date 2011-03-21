/*
 * Copyright (c) 2003 World Wide Web Consortium,
 * (Massachusetts Institute of Technology, Institut National de
 * Recherche en Informatique et en Automatique, Keio University). All
 * Rights Reserved. This program is distributed under the W3C's Software
 * Intellectual Property License. This program is distributed in the
 * hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.
 * See W3C License http://www.w3.org/Consortium/Legal/ for more details.
 */

package org.w3c.dom.html2;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.DOMException;

/**
 * An <code>HTMLDocument</code> is the root of the HTML hierarchy and holds
 * the entire content. Besides providing access to the hierarchy, it also
 * provides some convenience methods for accessing certain sets of
 * information from the document.
 * <p>The following properties have been deprecated in favor of the
 * corresponding ones for the <code>BODY</code> element:alinkColorbackground
 * bgColorfgColorlinkColorvlinkColorIn DOM Level 2, the method
 * <code>getElementById</code> is inherited from the <code>Document</code>
 * interface where it was moved to.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLDocument extends Document {
    /**
     * The title of a document as specified by the <code>TITLE</code> element
     * in the head of the document.
     */
    public String getTitle();
    /**
     * The title of a document as specified by the <code>TITLE</code> element
     * in the head of the document.
     */
    public void setTitle(String title);

    /**
     * Returns the URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] of the page that linked to this page. The value is an
     * empty string if the user navigated to the page directly (not through
     * a link, but, for example, via a bookmark).
     */
    public String getReferrer();

    /**
     * The domain name of the server that served the document, or
     * <code>null</code> if the server cannot be identified by a domain
     * name.
     */
    public String getDomain();

    /**
     * The absolute URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] of the document.
     */
    public String getURL();

    /**
     * The element that contains the content for the document. In documents
     * with <code>BODY</code> contents, returns the <code>BODY</code>
     * element. In frameset documents, this returns the outermost
     * <code>FRAMESET</code> element.
     */
    public HTMLElement getBody();
    /**
     * The element that contains the content for the document. In documents
     * with <code>BODY</code> contents, returns the <code>BODY</code>
     * element. In frameset documents, this returns the outermost
     * <code>FRAMESET</code> element.
     */
    public void setBody(HTMLElement body);

    /**
     * A collection of all the <code>IMG</code> elements in a document. The
     * behavior is limited to <code>IMG</code> elements for backwards
     * compatibility. As suggested by [<a href='http://www.w3.org/TR/1999/REC-html401-19991224'>HTML 4.01</a>], to include images, authors may use
     * the <code>OBJECT</code> element or the <code>IMG</code> element.
     * Therefore, it is recommended not to use this attribute to find the
     * images in the document but <code>getElementsByTagName</code> with
     * HTML 4.01 or <code>getElementsByTagNameNS</code> with XHTML 1.0.
     */
    public HTMLCollection getImages();

    /**
     * A collection of all the <code>OBJECT</code> elements that include
     * applets and <code>APPLET</code> (deprecated) elements in a document.
     */
    public HTMLCollection getApplets();

    /**
     * A collection of all <code>AREA</code> elements and anchor (
     * <code>A</code>) elements in a document with a value for the
     * <code>href</code> attribute.
     */
    public HTMLCollection getLinks();

    /**
     * A collection of all the forms of a document.
     */
    public HTMLCollection getForms();

    /**
     *  A collection of all the anchor (<code>A</code>) elements in a document
     * with a value for the <code>name</code> attribute. For reasons of
     * backward compatibility, the returned set of anchors only contains
     * those anchors created with the <code>name</code> attribute, not those
     * created with the <code>id</code> attribute. Note that in [<a href='http://www.w3.org/TR/2002/REC-xhtml1-20020801'>XHTML 1.0</a>], the
     * <code>name</code> attribute (see section 4.10) has no semantics and
     * is only present for legacy user agents: the <code>id</code> attribute
     * is used instead. Users should prefer the iterator mechanisms provided
     * by [<a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Traversal-Range-20001113'>DOM Level 2 Traversal</a>] instead.
     */
    public HTMLCollection getAnchors();

    /**
     *  This mutable string attribute denotes persistent state information
     * that (1) is associated with the current frame or document and (2) is
     * composed of information described by the <code>cookies</code>
     * non-terminal of [<a href='http://www.ietf.org/rfc/rfc2965.txt'>IETF RFC 2965</a>], Section 4.2.2.
     * <br> If no persistent state information is available for the current
     * frame or document document, then this property's value is an empty
     * string.
     * <br> When this attribute is read, all cookies are returned as a single
     * string, with each cookie's name-value pair concatenated into a list
     * of name-value pairs, each list item being separated by a ';'
     * (semicolon).
     * <br> When this attribute is set, the value it is set to should be a
     * string that adheres to the <code>cookie</code> non-terminal of [<a href='http://www.ietf.org/rfc/rfc2965.txt'>IETF RFC 2965</a>]; that
     * is, it should be a single name-value pair followed by zero or more
     * cookie attribute values. If no domain attribute is specified, then
     * the domain attribute for the new value defaults to the host portion
     * of an absolute URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] of the current frame or document. If no path
     * attribute is specified, then the path attribute for the new value
     * defaults to the absolute path portion of the URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] of the current
     * frame or document. If no max-age attribute is specified, then the
     * max-age attribute for the new value defaults to a user agent defined
     * value. If a cookie with the specified name is already associated with
     * the current frame or document, then the new value as well as the new
     * attributes replace the old value and attributes. If a max-age
     * attribute of 0 is specified for the new value, then any existing
     * cookies of the specified name are removed from the cookie storage.
     * See [<a href='http://www.ietf.org/rfc/rfc2965.txt'>IETF RFC 2965</a>] for the semantics of persistent state item attribute value
     * pairs.  The precise nature of a user agent session is not defined by
     * this specification.
     */
    public String getCookie();
    /**
     *  This mutable string attribute denotes persistent state information
     * that (1) is associated with the current frame or document and (2) is
     * composed of information described by the <code>cookies</code>
     * non-terminal of [<a href='http://www.ietf.org/rfc/rfc2965.txt'>IETF RFC 2965</a>], Section 4.2.2.
     * <br> If no persistent state information is available for the current
     * frame or document document, then this property's value is an empty
     * string.
     * <br> When this attribute is read, all cookies are returned as a single
     * string, with each cookie's name-value pair concatenated into a list
     * of name-value pairs, each list item being separated by a ';'
     * (semicolon).
     * <br> When this attribute is set, the value it is set to should be a
     * string that adheres to the <code>cookie</code> non-terminal of [<a href='http://www.ietf.org/rfc/rfc2965.txt'>IETF RFC 2965</a>]; that
     * is, it should be a single name-value pair followed by zero or more
     * cookie attribute values. If no domain attribute is specified, then
     * the domain attribute for the new value defaults to the host portion
     * of an absolute URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] of the current frame or document. If no path
     * attribute is specified, then the path attribute for the new value
     * defaults to the absolute path portion of the URI [<a href='http://www.ietf.org/rfc/rfc2396.txt'>IETF RFC 2396</a>] of the current
     * frame or document. If no max-age attribute is specified, then the
     * max-age attribute for the new value defaults to a user agent defined
     * value. If a cookie with the specified name is already associated with
     * the current frame or document, then the new value as well as the new
     * attributes replace the old value and attributes. If a max-age
     * attribute of 0 is specified for the new value, then any existing
     * cookies of the specified name are removed from the cookie storage.
     * See [<a href='http://www.ietf.org/rfc/rfc2965.txt'>IETF RFC 2965</a>] for the semantics of persistent state item attribute value
     * pairs.  The precise nature of a user agent session is not defined by
     * this specification.
     * @exception DOMException
     *    SYNTAX_ERR: If the new value does not adhere to the cookie syntax
     *   specified by [<a href='http://www.ietf.org/rfc/rfc2965.txt'>IETF RFC 2965</a>].
     */
    public void setCookie(String cookie)
                                      throws DOMException;

    /**
     * Open a document stream for writing. If a document exists in the target,
     * this method clears it. This method and the ones following allow a
     * user to add to or replace the structure model of a document using
     * strings of unparsed HTML. At the time of writing alternate methods
     * for providing similar functionality for both HTML and XML documents
     * were being considered (see [<a href='http://www.w3.org/TR/2002/WD-DOM-Level-3-LS-20020725'>DOM Level 3 Load and Save</a>]).
     */
    public void open();

    /**
     * Closes a document stream opened by <code>open()</code> and forces
     * rendering.
     */
    public void close();

    /**
     * Write a string of text to a document stream opened by
     * <code>open()</code>. Note that the function will produce a document
     * which is not necessarily driven by a DTD and therefore might be
     * produce an invalid result in the context of the document.
     * @param text The string to be parsed into some structure in the
     *   document structure model.
     */
    public void write(String text);

    /**
     * Write a string of text followed by a newline character to a document
     * stream opened by <code>open()</code>. Note that the function will
     * produce a document which is not necessarily driven by a DTD and
     * therefore might be produce an invalid result in the context of the
     * document
     * @param text The string to be parsed into some structure in the
     *   document structure model.
     */
    public void writeln(String text);

    /**
     *  With [<a href='http://www.w3.org/TR/1999/REC-html401-19991224'>HTML 4.01</a>] documents, this method returns the (possibly empty) collection
     * of elements whose <code>name</code> value is given by
     * <code>elementName</code>. In [<a href='http://www.w3.org/TR/2002/REC-xhtml1-20020801'>XHTML 1.0</a>] documents, this methods only return the
     * (possibly empty) collection of form controls with matching name. This
     * method is case sensitive.
     * @param elementName The <code>name</code> attribute value for an
     *   element.
     * @return The matching elements.
     */
    public NodeList getElementsByName(String elementName);

}
