/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;

import javax.naming.NamingException;

/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 14, 2001
 */

public class AttributeModificationException extends NamingException
{
  // Serialized fields.
  private ModificationItem[] unexecs;

  public AttributeModificationException ()
  {
    super ();
  }

  public AttributeModificationException (String msg)
  {
    super (msg);
  }

  public void setUnexecutedModifications(ModificationItem[] e)
  {
    unexecs = e;
  }

  public ModificationItem[] getUnexecutedModifications()
  {
    return unexecs;
  }

  public String toString()
  {
    return super.toString () + ": " + unexecs[0].toString ();
  }
}
