/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming;

import java.lang.Exception;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 14, 2001
 */

public class LinkException extends NamingException
{
  // Serialized fields.
  protected Name linkResolvedName;
  protected Object linkResolvedObj;
  protected Name linkRemainingName;
  protected String linkExplanation;

  public LinkException ()
  {
    super ();
  }

  public LinkException (String msg)
  {
    super (msg);
  }

  public Name getLinkResolvedName()
  {
    return linkResolvedName;
  }

  public Name getLinkRemainingName()
  {
    return linkRemainingName;
  }

  public Object getLinkResolvedObj()
  {
    return linkResolvedObj;
  }

  public String getLinkExplanation()
  {
    return linkExplanation;
  }

  public void setLinkExplanation(String msg)
  {
    linkExplanation = msg;
  }

  public void setLinkResolvedName(Name name)
  {
    linkResolvedName = (Name) name.clone();
  }

  public void setLinkRemainingName(Name name)
  {
    linkRemainingName = (Name) name.clone();
  }

  public void setLinkResolvedObj(Object obj)
  {
    linkResolvedObj = obj;
  }

  public String toString ()
  {
    return super.toString () + "; " + linkRemainingName.toString ();
  }

  public String toString (boolean detail)
  {
    String r = super.toString (detail) + "; " + linkRemainingName.toString ();
    if (detail)
      r += "; " + linkResolvedObj.toString ();
    return r;
  }
}
