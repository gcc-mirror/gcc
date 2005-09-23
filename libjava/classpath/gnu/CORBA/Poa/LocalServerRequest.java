/* LocalServerRequest.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.Poa;

import gnu.CORBA.gnuNamedValue;

import org.omg.CORBA.ARG_INOUT;
import org.omg.CORBA.ARG_OUT;
import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.Context;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ServerRequest;
import org.omg.CORBA.UnknownUserException;

/**
 * Used to make local invocations via LocalRequest.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class LocalServerRequest
  extends ServerRequest
{
  /**
   * The local request, on the base of that this instance is created.
   */
  final LocalRequest request;

  /**
   * Create a new instance.
   */
  public LocalServerRequest(LocalRequest _request)
  {
    request = _request;
  }

  /**
   * Get the argument list that can be modified.
   */
  public void params(NVList args)
  {
    arguments(args);
  }

  /**
   * Get contexts.
   */
  public Context ctx()
  {
    return request.ctx();
  }

  /**
   * Get the operatin being performed.
   */
  public String operation()
  {
    return request.operation();
  }

  /**
   * Get the argument list that can be modified.
   * The direction depends on the size of the passed list.
   * The empty list is filled with the request arguments.
   * The non-empty list is used to set the request arguments.
   */
  public void arguments(NVList args)
  {
    NVList l = request.arguments();
    NamedValue a;

    try
      {
        if (args.count() == 0)
          {
            // Transfer to the passed parameter.
            for (int i = 0; i < l.count(); i++)
              {
                a = l.item(i);
                args.add_value(a.name(), a.value(), a.flags());
              }
          }
        else
          {
            // Transfer from the passed parameter.
            if (l.count() != args.count())
              throw new BAD_PARAM("Argument number mismatch, current " +
                                  l.count() + ", passed " + args.count()
                                 );
            try
              {
                for (int i = 0; i < l.count(); i++)
                  {
                    a = l.item(i);
                    if (a.flags() == ARG_INOUT.value ||
                        a.flags() == ARG_OUT.value
                       )
                      {
                        ((gnuNamedValue) a).setValue(args.item(i).value());
                      }
                  }
              }
            catch (ClassCastException cex)
              {
                InternalError ierr = new InternalError();
                ierr.initCause(cex);
                throw ierr;
              }
          }
      }
    catch (Bounds ex)
      {
        InternalError ierr = new InternalError();
        ierr.initCause(ex);
        throw ierr;
      }
  }

  /**
   * Set the result.
   */
  public void set_result(Any result)
  {
    gnuNamedValue g = new gnuNamedValue();
    g.setValue(result);
    g.setFlags(ARG_OUT.value);
    request.set_result(g);
  }

  /**
   * Get the name of the method being called.
   */
  public String op_name()
  {
    return request.operation();
  }

  /**
   * Set the exception that has been thrown.
   */
  public void set_exception(Any exc)
  {
    request.env().exception(new UnknownUserException(exc));
  }

  /**
   * Set the result.
   */
  public void result(Any r)
  {
    set_result(r);
  }

  /**
   * Set the exception.
   */
  public void except(Any exc)
  {
    set_exception(exc);
  }
}