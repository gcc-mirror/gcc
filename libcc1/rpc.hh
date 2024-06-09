/* RPC call and callback templates
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef CC1_PLUGIN_RPC_HH
#define CC1_PLUGIN_RPC_HH

#include "status.hh"
#include "connection.hh"
#include "deleter.hh"

namespace cc1_plugin
{
  // The plugin API may contain some "const" method parameters.
  // However, when unmarshalling we cannot unmarshall into a const
  // object; and furthermore we want to be able to deallocate pointers
  // when finished with them.  This wrapper class lets us properly
  // remove the "const" and handle deallocation from pointer types.

  template<typename T>
  class argument_wrapper
  {
  public:

    argument_wrapper () { }
    ~argument_wrapper () { }

    argument_wrapper (const argument_wrapper &) = delete;
    argument_wrapper &operator= (const argument_wrapper &) = delete;

    T get () const { return m_object; }

    status unmarshall (connection *conn)
    {
      return ::cc1_plugin::unmarshall (conn, &m_object);
    }

  private:

    T m_object;
  };

  // Specialization for any kind of pointer.
  template<typename T>
  class argument_wrapper<T *>
  {
  public:
    argument_wrapper () = default;
    ~argument_wrapper () = default;

    argument_wrapper (const argument_wrapper &) = delete;
    argument_wrapper &operator= (const argument_wrapper &) = delete;

    typedef typename std::remove_const<T>::type type;

    const type *get () const
    {
      return m_object.get ();
    }

    status unmarshall (connection *conn)
    {
      type *ptr;
      if (!::cc1_plugin::unmarshall (conn, &ptr))
	return FAIL;
      m_object.reset (ptr);
      return OK;
    }

  private:

    unique_ptr<type> m_object;
  };

  // There are two kinds of template functions here: "call" and
  // "invoker".

  // The "call" template is used for making a remote procedure call.
  // It starts a query ('Q') packet, marshalls its arguments, waits
  // for a result, and finally reads and returns the result via an
  // "out" parameter.

  // The "invoker" template is used when receiving a remote procedure
  // call.  This template function is suitable for use with the
  // "callbacks" and "connection" classes.  It decodes incoming
  // arguments, passes them to the wrapped function, and finally
  // marshalls a reply packet.

  template<typename R, typename... Arg>
  status
  call (connection *conn, const char *method, R *result, Arg... args)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, (int) sizeof... (Arg)))
      return FAIL;
    if (!marshall (conn, args...))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  // The base case -- just return OK.
  template<int I, typename... T>
  typename std::enable_if<I == sizeof... (T), status>::type
  unmarshall (connection *, std::tuple<T...> &)
  {
    return OK;
  }

  // Unmarshall this argument, then unmarshall all subsequent args.
  template<int I, typename... T>
  typename std::enable_if<I < sizeof... (T), status>::type
  unmarshall (connection *conn, std::tuple<T...> &value)
  {
    if (!std::get<I> (value).unmarshall (conn))
      return FAIL;
    return unmarshall<I + 1, T...> (conn, value);
  }

  // Wrap a static function that is suitable for use as a callback.
  // This is a template function inside a template class to work
  // around limitations with multiple variadic packs.
  template<typename R, typename... Arg>
  class invoker
  {
    // Base case -- we can call the function.
    template<int I, R func (connection *, Arg...), typename... T>
    static typename std::enable_if<I == sizeof... (Arg), R>::type
    call (connection *conn, const std::tuple<argument_wrapper<Arg>...> &,
	  T... args)
    {
      return func (conn, args...);
    }

    // Unpack one argument and continue the recursion.
    template<int I, R func (connection *, Arg...), typename... T>
    static typename std::enable_if<I < sizeof... (Arg), R>::type
    call (connection *conn, const std::tuple<argument_wrapper<Arg>...> &value,
	  T... args)
    {
      return call<I + 1, func> (conn, value, args...,
				std::get<I> (value).get ());
    }

  public:

    // A callback function that reads arguments from the connection,
    // calls the wrapped function, and then sends the result back on
    // the connection.
    template<R func (connection *, Arg...)>
    static status
    invoke (connection *conn)
    {
      if (!unmarshall_check (conn, sizeof... (Arg)))
	return FAIL;
      std::tuple<argument_wrapper<Arg>...> wrapped;
      if (!unmarshall<0> (conn, wrapped))
	return FAIL;

      R result = call<0, func> (conn, wrapped);

      if (!conn->send ('R'))
	return FAIL;
      return marshall (conn, result);
    }
  };
};

#endif // CC1_PLUGIN_RPC_HH
