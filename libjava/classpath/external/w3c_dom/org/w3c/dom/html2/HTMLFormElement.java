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

/**
 * The <code>FORM</code> element encompasses behavior similar to a collection
 * and an element. It provides direct access to the contained form controls
 * as well as the attributes of the form element. See the FORM element
 * definition in HTML 4.01.
 * <p>See also the <a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>Document Object Model (DOM) Level 2 HTML Specification</a>.
 */
public interface HTMLFormElement extends HTMLElement {
    /**
     * Returns a collection of all form control elements in the form.
     */
    public HTMLCollection getElements();

    /**
     * The number of form controls in the form.
     */
    public int getLength();

    /**
     * Names the form.
     */
    public String getName();
    /**
     * Names the form.
     */
    public void setName(String name);

    /**
     * List of character sets supported by the server. See the accept-charset
     * attribute definition in HTML 4.01.
     */
    public String getAcceptCharset();
    /**
     * List of character sets supported by the server. See the accept-charset
     * attribute definition in HTML 4.01.
     */
    public void setAcceptCharset(String acceptCharset);

    /**
     * Server-side form handler. See the action attribute definition in HTML
     * 4.01.
     */
    public String getAction();
    /**
     * Server-side form handler. See the action attribute definition in HTML
     * 4.01.
     */
    public void setAction(String action);

    /**
     * The content type of the submitted form, generally
     * "application/x-www-form-urlencoded". See the enctype attribute
     * definition in HTML 4.01. The onsubmit even handler is not guaranteed
     * to be triggered when invoking this method. The behavior is
     * inconsistent for historical reasons and authors should not rely on a
     * particular one.
     */
    public String getEnctype();
    /**
     * The content type of the submitted form, generally
     * "application/x-www-form-urlencoded". See the enctype attribute
     * definition in HTML 4.01. The onsubmit even handler is not guaranteed
     * to be triggered when invoking this method. The behavior is
     * inconsistent for historical reasons and authors should not rely on a
     * particular one.
     */
    public void setEnctype(String enctype);

    /**
     * HTTP method [<a href='http://www.ietf.org/rfc/rfc2616.txt'>IETF RFC 2616</a>] used to submit form. See the method attribute definition
     * in HTML 4.01.
     */
    public String getMethod();
    /**
     * HTTP method [<a href='http://www.ietf.org/rfc/rfc2616.txt'>IETF RFC 2616</a>] used to submit form. See the method attribute definition
     * in HTML 4.01.
     */
    public void setMethod(String method);

    /**
     * Frame to render the resource in. See the target attribute definition in
     * HTML 4.01.
     */
    public String getTarget();
    /**
     * Frame to render the resource in. See the target attribute definition in
     * HTML 4.01.
     */
    public void setTarget(String target);

    /**
     * Submits the form. It performs the same action as a submit button.
     */
    public void submit();

    /**
     * Restores a form element's default values. It performs the same action
     * as a reset button.
     */
    public void reset();

}
