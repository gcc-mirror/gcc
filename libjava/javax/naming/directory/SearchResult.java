/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;
import javax.naming.*;
import java.io.Serializable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 13, 2001
 */

public class SearchResult extends Binding
{
  // Serialized fields.
  private Attributes attrs;

  public SearchResult(String name, Object obj, Attributes attrs)
  {
    super(name, obj);
    this.attrs = attrs;
  }

  public SearchResult(String name, Object obj, Attributes attrs,
  		      boolean isRelative)
  {
    super(name, obj, isRelative);
    this.attrs = attrs;
  }

  public SearchResult(String name, String className, Object obj,
  		      Attributes attrs)
  {
    super(name, className, obj);
    this.attrs = attrs;
  }

  public SearchResult(String name, String className, Object obj,
  		      Attributes attrs, boolean isRelative)
  {
    super(name, className, obj, isRelative);
    this.attrs = attrs;
  }

  public Attributes getAttributes()
  {
    return attrs;
  }

  public void setAttributes(Attributes attrs)
  {
    this.attrs = attrs;
  }

  public String toString()
  {
    return super.toString() + ":" + attrs.toString();
  }
}
