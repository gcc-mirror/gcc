/* PolicyNode.java -- a single node in a policy tree
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package java.security.cert;

public interface PolicyNode
{

  /**
   * Get the iterator of the child nodes of this node. The returned
   * iterator is (naturally) unmodifiable.
   *
   * @return An iterator over the child nodes.
   */
  java.util.Iterator getChildren();

  /**
   * Get the depth of this node within the tree, starting at 0 for the
   * root node.
   *
   * @return The depth of this node.
   */
  int getDepth();

  /**
   * Returns a set of policies (string OIDs) that will satisfy this
   * node's policy. The root node should always return the singleton set
   * with the element "any-policy".
   *
   * @return The set of expected policies.
   */
  java.util.Set getExpectedPolicies();

  /**
   * Returns the parent node of this node, or null if this is the root
   * node.
   *
   * @return The parent node, or null.
   */
  PolicyNode getParent();

  /**
   * Returns a set of {@link PolicyQualifierInfo} objects that qualify
   * the valid policy of this node. The root node should always return
   * the empty set.
   *
   * @return The set of {@link PolicyQualifierInfo} objects.
   */
  java.util.Set getPolicyQualifiers();

  /**
   * Get the policy OID this node represents. The root node should return
   * the special value "any-policy".
   *
   * @return The policy of this node.
   */
  String getValidPolicy();

  /**
   * Return the criticality flag of this policy node. Nodes who return
   * true for this method should be considered critical. The root node
   * is never critical.
   *
   * @return The criticality flag.
   */
  boolean isCritical();
}
