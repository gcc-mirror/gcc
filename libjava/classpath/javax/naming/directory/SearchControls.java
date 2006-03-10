/* SearchControls.java --
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


package javax.naming.directory;

import java.io.Serializable;
 
/**
 * @author Warren Levy (warrenl@redhat.com)
 * @date June 5, 2001
 */

public class SearchControls implements Serializable
{
  private static final long serialVersionUID = - 2480540967773454797L;
  public static final int OBJECT_SCOPE = 0;
  public static final int ONELEVEL_SCOPE = 1;
  public static final int SUBTREE_SCOPE = 2;

  // Serialized fields.
  private int searchScope;
  private int timeLimit;
  private boolean derefLink;
  private boolean returnObj;
  private long countLimit;
  private String[] attributesToReturn;

  public SearchControls()
  {
    this(ONELEVEL_SCOPE, 0L, 0, null, false, false);
  }

  public SearchControls(int scope, long countlim, int timelim, String[] attrs,
  			boolean retobj, boolean deref)
  {
    searchScope = scope;
    timeLimit = timelim;
    derefLink = deref;
    returnObj = retobj;
    countLimit = countlim;
    attributesToReturn = attrs;
  }

  public int getSearchScope()
  {
    return searchScope;
  }

  public int getTimeLimit()
  {
    return timeLimit;
  }

  public boolean getDerefLinkFlag()
  {
    return derefLink;
  }

  public boolean getReturningObjFlag()
  {
    return returnObj;
  }

  public long getCountLimit()
  {
    return countLimit;
  }

  public String[] getReturningAttributes()
  {
    return attributesToReturn;
  }

  public void setSearchScope(int scope)
  {
    searchScope = scope;
  }

  public void setTimeLimit(int ms)
  {
    timeLimit = ms;
  }

  public void setDerefLinkFlag(boolean on)
  {
    derefLink = on;
  }

  public void setReturningObjFlag(boolean on)
  {
    returnObj = on;
  }

  public void setCountLimit(long limit)
  {
    countLimit = limit;
  }

  public void setReturningAttributes(String[] attrs)
  {
    attributesToReturn = attrs;
  }
}
