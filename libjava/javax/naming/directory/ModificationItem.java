/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;
import java.io.Serializable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 13, 2001
 */

public class ModificationItem implements Serializable
{
  // Serialized fields.
  private int mod_op;
  private Attribute attr;

  public ModificationItem(int mod_op, Attribute attr)
  {
    if (attr == null)
      throw new IllegalArgumentException("attr is null");
    if (mod_op != DirContext.ADD_ATTRIBUTE &&
	mod_op != DirContext.REPLACE_ATTRIBUTE &&
	mod_op != DirContext.REMOVE_ATTRIBUTE)
      throw new IllegalArgumentException("mod_op is invalid");
    this.mod_op = mod_op;
    this.attr = attr;
  }

  public int getModificationOp()
  {
    return mod_op;
  }

  public Attribute getAttribute()
  {
    return attr;
  }

  public String toString()
  {
    return "mod_op=" + mod_op + ":" + "attr=" + attr.toString();
  }
}
