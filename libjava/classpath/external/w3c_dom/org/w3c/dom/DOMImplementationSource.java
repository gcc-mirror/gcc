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
 * This interface permits a DOM implementer to supply one or more
 * implementations, based upon requested features and versions, as specified
 * in . Each implemented <code>DOMImplementationSource</code> object is
 * listed in the binding-specific list of available sources so that its
 * <code>DOMImplementation</code> objects are made available.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 * @since DOM Level 3
 */
public interface DOMImplementationSource {
    /**
     *  A method to request the first DOM implementation that supports the
     * specified features.
     * @param features  A string that specifies which features and versions
     *   are required. This is a space separated list in which each feature
     *   is specified by its name optionally followed by a space and a
     *   version number.  This method returns the first item of the list
     *   returned by <code>getDOMImplementationList</code>.  As an example,
     *   the string <code>"XML 3.0 Traversal +Events 2.0"</code> will
     *   request a DOM implementation that supports the module "XML" for its
     *   3.0 version, a module that support of the "Traversal" module for
     *   any version, and the module "Events" for its 2.0 version. The
     *   module "Events" must be accessible using the method
     *   <code>Node.getFeature()</code> and
     *   <code>DOMImplementation.getFeature()</code>.
     * @return The first DOM implementation that support the desired
     *   features, or <code>null</code> if this source has none.
     */
    public DOMImplementation getDOMImplementation(String features);

    /**
     * A method to request a list of DOM implementations that support the
     * specified features and versions, as specified in .
     * @param features A string that specifies which features and versions
     *   are required. This is a space separated list in which each feature
     *   is specified by its name optionally followed by a space and a
     *   version number. This is something like: "XML 3.0 Traversal +Events
     *   2.0"
     * @return A list of DOM implementations that support the desired
     *   features.
     */
    public DOMImplementationList getDOMImplementationList(String features);

}
