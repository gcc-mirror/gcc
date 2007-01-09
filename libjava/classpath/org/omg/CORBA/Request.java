/* Request.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.CORBA;


/**
 * An object, containing the information, needed to invoke the method of
 * the local or remote CORBA object. The Request is used in 
 * Dynamic Invocation Interface (DII) which allows dynamic creation of
 * requests. 
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class Request
{
  /**
   * Add the named input parameter that passes value to
   * the method being invoked. This is similar to the "passing by value"
   * conception.
   *
   * The created parameter is returned allowing to set the value.
   *
   * @return the created parameter.
   */
  public abstract Any add_in_arg();

  /**
   * Add the input/output parameter that passes value both to and from
   * the method being invoked. This is similar to the "passing by reference"
   * conception.
   *
   * The created parameter is returned allowing to set the value.
   *
   * @return the created parameter.
   */
  public abstract Any add_inout_arg();

  /**
   * Add the named input parameter that passes value to
   * the method being invoked. This is similar to the "passing by value"
   * conception.
   *
   * The created parameter is returned allowing to set the value.
   *
   * @param name the parameter name.
   *
   * @return the created parameter.
   */
  public abstract Any add_named_in_arg(String name);

  /**
   * Add the named input/output parameter that passes value both to and from
   * the method being invoked. This is similar to the "passing by reference"
   * conception.
   *
   * The created parameter is returned allowing to set the value.
   *
   * @param name the parameter name.
   *
   * @return the created parameter.
   */
  public abstract Any add_named_inout_arg(String name);

  /**
   * Add the named output parameter that passes value from
   * the method being invoked. Differently from the java
   * language, the CORBA IDL method can return multiple values.
   *
   * The created parameter is returned allowing to set the value.
   *
   * @param name the parameter name.
   *
   * @return the created parameter.
   */
  public abstract Any add_named_out_arg(String name);

  /**
   * Add the output parameter that passes value from
   * the method being invoked. Differently from the java
   * language, the CORBA IDL method can return multiple values.
   *
   * The created parameter is returned allowing to set the value.
   *
   * @return the created parameter.
   */
  public abstract Any add_out_arg();

  /**
   * Return the list of all previously added parameters.
   *
   * @return the list of parameters.
   */
  public abstract NVList arguments();
  
  /**
   * Get the context list object for this request.
   * 
   * @return a list of strings that must be resolved and sent with the 
   * invocation.
   */
  public abstract ContextList contexts();

  /**
   * Get the context, previously set using {@link #ctx(Context)}.
   * The context contains the details about this request.
   */
  public abstract Context ctx();

  /**
   * Set the context that shuld be later returned by {@link #ctx()}.
   * This context contains the details about this request.
   *
   * @param a_context a context to set.
   */
  public abstract void ctx(Context a_context);

  /**
   * Returns the container, eclosing an exception that the invoked method
   * has thrown.
   *
   * @return the Environment object, containng the exception.
   */
  public abstract Environment env();

  /**
   * List the exceptions that may be thrown by the CORBA object method being
   * invoked.
   *
   * @return the list of exceptions.
   */
  public abstract ExceptionList exceptions();

  /**
   * Allow to access the response that has been previously sent using
   * {@link #send_deferred()}.
   *
   * @throws WrongTransaction if the transaction scope mismatches.
   */
  public abstract void get_response()
                             throws WrongTransaction;

  /**
   * Submit the request, suspending the current thread until the
   * answer is received.
   */
  public abstract void invoke();

  /**
   * Get the name of the method being invoked.
   *
   * @return the name of the method being invoked.
   */
  public abstract String operation();

  /**
   * Check if the response is received to the request that was
   * previously send using {@link #send_deferred()}.
   *
   * @return true if the response has been already received, false otherwise.
   */
  public abstract boolean poll_response();

  /**
   * Get the value, returned by the method, together with its name.
   *
   * @return the value, returned by the method.
   */
  public abstract NamedValue result();

  /**
   * Get the value, returned by the method.
   *
   * @return the value, returned by the method.
   */
  public abstract Any return_value();

  /**
   * Send a request without suspending the current thread.
   *
   * Allow later check of the request status by {@link #poll_response()} and
   * retrieving the results by {@link #get_response()}.
   */
  public abstract void send_deferred();

  /**
   * Send a request and forget about it, not waiting for a response.
   * This can be done also for methods that normally are expected
   * to return some values.
   */
  public abstract void send_oneway();

  /**
   * Set the return type.
   *
   * @param returns the type of the value, returned in response to this
   * request.
   */
  public abstract void set_return_type(TypeCode returns);

  /**
   * Return the CORBA object on that the method would be invoked.
   *
   * @return the invocation target.
   */
  public abstract org.omg.CORBA.Object target();
}
