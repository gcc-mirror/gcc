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
 * When associating an object to a key on a node using
 * <code>Node.setUserData()</code> the application can provide a handler
 * that gets called when the node the object is associated to is being
 * cloned, imported, or renamed. This can be used by the application to
 * implement various behaviors regarding the data it associates to the DOM
 * nodes. This interface defines that handler.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 * @since DOM Level 3
 */
public interface UserDataHandler {
    // OperationType
    /**
     * The node is cloned, using <code>Node.cloneNode()</code>.
     */
    public static final short NODE_CLONED               = 1;
    /**
     * The node is imported, using <code>Document.importNode()</code>.
     */
    public static final short NODE_IMPORTED             = 2;
    /**
     * The node is deleted.
     * <p ><b>Note:</b> This may not be supported or may not be reliable in
     * certain environments, such as Java, where the implementation has no
     * real control over when objects are actually deleted.
     */
    public static final short NODE_DELETED              = 3;
    /**
     * The node is renamed, using <code>Document.renameNode()</code>.
     */
    public static final short NODE_RENAMED              = 4;
    /**
     * The node is adopted, using <code>Document.adoptNode()</code>.
     */
    public static final short NODE_ADOPTED              = 5;

    /**
     * This method is called whenever the node for which this handler is
     * registered is imported or cloned.
     * <br> DOM applications must not raise exceptions in a
     * <code>UserDataHandler</code>. The effect of throwing exceptions from
     * the handler is DOM implementation dependent.
     * @param operation Specifies the type of operation that is being
     *   performed on the node.
     * @param key Specifies the key for which this handler is being called.
     * @param data Specifies the data for which this handler is being called.
     * @param src Specifies the node being cloned, adopted, imported, or
     *   renamed. This is <code>null</code> when the node is being deleted.
     * @param dst Specifies the node newly created if any, or
     *   <code>null</code>.
     */
    public void handle(short operation,
                       String key,
                       Object data,
                       Node src,
                       Node dst);

}
