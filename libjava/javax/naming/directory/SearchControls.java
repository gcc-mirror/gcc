/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;
import java.io.Serializable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 5, 2001
 */

public class SearchControls extends Object implements Serializable
{
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
