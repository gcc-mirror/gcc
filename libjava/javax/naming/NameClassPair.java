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
public class NameClassPair implements Serializable
{
  public NameClassPair (String name, String className)
  {
    this (name, className, true);
  }

  public NameClassPair (String name, String className, boolean isRelative)
  {
    this.name = name;
    this.className = className;
    this.isRel = isRelative;
  }

  public String getClassName ()
  {
    return className;
  }

  public String getName ()
  {
    return name;
  }

  public boolean isRelative ()
  {
    return isRel;
  }

  public void setClassName (String name)
  {
    this.className = name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public void setRelative (boolean r)
  {
    this.isRel = r;
  }

  public String toString ()
  {
    // Specified by class documentation.
    return name + ":" + className;
  }

  // These field names are fixed by the serialization spec.
  private String name;
  private String className;
  private boolean isRel;
}
