/* LinkException.java --
   Copyright (C) 2001, 2004, 2005, 2006  Free Software Foundation, Inc.

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

 
package javax.naming;

 
/**
 * @author Warren Levy (warrenl@redhat.com)
 * @date June 14, 2001
 */

public class LinkException extends NamingException
{
  private static final long serialVersionUID = - 7967662604076777712L;

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
