/* CommandInfo.java -- Description of the result of a command request.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package javax.activation;

import java.beans.Beans;
import java.io.Externalizable;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectInputStream;

/**
 * Description of the result of a command request.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public class CommandInfo
{

  private final String verb;
  private final String className;

  /**
   * Constructor.
   * @param verb the command verb
   * @param className the command class name
   */
  public CommandInfo(String verb, String className)
  {
    this.verb = verb;
    this.className = className;
  }

  /**
   * Returns the command verb.
   */
  public String getCommandName()
  {
    return verb;
  }

  /**
   * Returns the command class name.
   */
  public String getCommandClass()
  {
    return className;
  }

  /**
   * Returns the instantiated bean.
   * If the bean implements <code>CommandObject</code>, its
   * <code>setCommandContext</code> method will be called.
   * @param dh the data handler describing the command data
   * @param loader the class loader used to instantiate the bean
   */
  public Object getCommandObject(DataHandler dh, ClassLoader loader)
    throws IOException, ClassNotFoundException
  {
    Object object = Beans.instantiate(loader, className);
    if (object != null)
      {
        if (object instanceof CommandObject)
          {
            CommandObject command = (CommandObject)object;
            command.setCommandContext(verb, dh);
          }
        else if (dh != null && (object instanceof Externalizable))
          {
            InputStream in = dh.getInputStream();
            if (in != null)
              {
                Externalizable externalizable = (Externalizable)object;
                externalizable.readExternal(new ObjectInputStream(in));
              }
          }
      }
    return object;
  }

}
