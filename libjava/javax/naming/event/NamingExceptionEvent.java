/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.event;
import javax.naming.*;
import java.util.EventObject;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 5, 2001
 */

public class NamingExceptionEvent extends EventObject
{
  // Serialized fields.
  private NamingException exception;

  public NamingExceptionEvent(EventContext source, NamingException exc)
  {
    super(source);
    exception = exc;
  }

  public NamingException getException()
  {
    return exception;
  }

  public EventContext getEventContext()
  {
    return (EventContext) getSource();
  }

  public void dispatch(NamingListener listener)
  {
    listener.namingExceptionThrown(this);
  }
}
