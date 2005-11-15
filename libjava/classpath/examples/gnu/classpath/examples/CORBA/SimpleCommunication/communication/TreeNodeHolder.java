/* TreeNodeHolder.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

/**
 * The TreeNode holder is a wrapper about the TreeNode data structure. It
 * can be used where the TreeNode must be passed both to and from
 * the method being called. The same structure holds the tree,
 * as it can be represented as a root TreeNode with children.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class TreeNodeHolder
  implements Streamable
{
  /**
   * Stores the TreeNode value.
   */
  public TreeNode value;

  /**
   * Creates the TreeNode holder with the null initial value.
   */
  public TreeNodeHolder()
  {
  }

  /**
   * Creates the TreeNode holder with the given initial value.
   */
  public TreeNodeHolder(TreeNode initialValue)
  {
    value = initialValue;
  }

  /**
   * Reads the TreeNode value from the common data representation (CDR)
   * stream.
   */
  public void _read(InputStream in)
  {
    value = TreeNodeHelper.read(in);
  }

  /**
   * Writes the TreeNode value into common data representation (CDR)
   * stream.
   * @return
   */
  public TypeCode _type()
  {
    return TreeNodeHelper.type();
  }

  public void _write(OutputStream out)
  {
    TreeNodeHelper.write(out, value);
  }
}
