/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming;

import java.lang.Exception;
import java.util.Hashtable;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 14, 2001
 */

public class CannotProceedException extends NamingException
{
  // Serialized fields.
  protected Name remainingNewName;
  protected Hashtable environment;
  protected Name altName;
  protected Context altNameCtx;

  public CannotProceedException ()
  {
    super ();
  }

  public CannotProceedException (String msg)
  {
    super (msg);
  }

  public Hashtable getEnvironment()
  {
    return environment;
  }

  public void setEnvironment(Hashtable environment)
  {
    this.environment = environment;
  }

  public Name getRemainingNewName()
  {
    return remainingNewName;
  }

  public void setRemainingNewName(Name newName)
  {
    remainingNewName = (Name) newName.clone();
  }

  public Name getAltName()
  {
    return altName;
  }

  public void setAltName(Name altName)
  {
    this.altName = altName;
  }

  public Context getAltNameCtx()
  {
    return altNameCtx;
  }

  public void setAltNameCtx(Context altNameCtx)
  {
    this.altNameCtx = altNameCtx;
  }
}
