/* Copyright (C) 2001 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming;

import java.io.Serializable;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date May 16, 2001
 */
public class LinkRef extends Reference
{
  public LinkRef (Name name)
  {
    this (name.toString ());
  }

  public LinkRef (String name)
  {
    // FIXME: javax.naming.LinkRef?
    super ("LinkRef", new StringRefAddr ("LinkAddress", name));
  }

  public String getLinkName ()
  {
    StringRefAddr sra = (StringRefAddr) get (0);
    return (String) sra.getContent ();
  }
}
