/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.ldap;
import java.util.EventObject;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 5, 2001
 */

public class UnsolicitedNotificationEvent extends EventObject
{
  // Serialized fields.
  private UnsolicitedNotification notice;

  public UnsolicitedNotificationEvent(Object src,
  				      UnsolicitedNotification notice)
  {
    super(src);
    this.notice = notice;
  }

  public UnsolicitedNotification getNotification()
  {
    return notice;
  }

  public void dispatch(UnsolicitedNotificationListener listener)
  {
    listener.notificationReceived(this);
  }
}
