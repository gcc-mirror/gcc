/* DOMResult.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package javax.xml.transform.dom;

import javax.xml.transform.Result;
import org.w3c.dom.Node;

/**
 * Output result specified as a W3C DOM object graph.
 * The result tree may be appended to an existing tree.
 * If no target node is specified, the result tree will be made available by
 * the {@link #getNode} method after the transformation.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class DOMResult
  implements Result
{

  /**
   * Factory feature indicating that DOM results are supported.
   */
  public static final String FEATURE =
    "http://javax.xml.transform.dom.DOMResult/feature";

  private Node node;
  private Node nextSibling;
  private String systemId;

  /**
   * Default constructor.
   */
  public DOMResult()
  {
    this(null, null, null);
  }

  /**
   * Constructor with the node to append to.
   */
  public DOMResult(Node node)
  {
    this(node, null, null);
  }

  /**
   * Constructor with the node to append to and the child node before which
   * the result should be appended.
   * @since 1.3
   */
  public DOMResult(Node node, Node nextSibling)
  {
    this(node, nextSibling, null);
  }
  
  /**
   * Constructor with the node to append to and the system ID.
   */
  public DOMResult(Node node, String systemID)
  {
    this(node, null, systemID);
  }
  
  /**
   * Constructor with the node to append to, the child node before which
   * the result should be appended, and the system ID.
   * @since 1.3
   */
  public DOMResult(Node node, Node nextSibling, String systemID)
  {
    this.node = node;
    this.nextSibling = nextSibling;
    this.systemId = systemID;
  }

  /**
   * Sets the node to which the result tree will be appended.
   * @param node the node
   */
  public void setNode(Node node)
  {
    this.node = node;
  }

  /**
   * Returns the node to which the result tree is to be appended,
   * or the result tree after transformation if no node was previously set.
   */
  public Node getNode()
  {
    return node;
  }

  /**
   * Sets the child node before which the result nodes will be inserted.
   * @since 1.3
   */
  public void setNextSibling(Node nextSibling)
  {
    this.nextSibling = nextSibling;
  }

  /**
   * Returns the child node before which the result nodes will be inserted.
   * @since 1.3
   */
  public Node getNextSibling()
  {
    return nextSibling;
  }

  /**
   * Sets the system ID used to resolve external entities.
   */
  public void setSystemId(String systemId)
  {
    this.systemId = systemId;
  }

  /**
   * Returns the system ID used to resolve external entities.
   */
  public String getSystemId()
  {
    return systemId;
  }

}
