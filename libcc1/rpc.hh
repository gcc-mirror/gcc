/* RPC call and callback templates
   Copyright (C) 2014-2020 Free Software Foundation, Inc.

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

    operator T () const { return m_object; }

    status unmarshall (connection *conn)
    {
      return ::cc1_plugin::unmarshall (conn, &m_object);
    }

  private:

    T m_object;

    // No copying or assignment allowed.
    argument_wrapper (const argument_wrapper &);
    argument_wrapper &operator= (const argument_wrapper &);
  };

  // Specialization for any kind of pointer.  This is declared but not
  // defined to avoid bugs if a new pointer type is introduced into
  // the API.  Instead you will just get a compilation error.
  template<typename T>
  class argument_wrapper<const T *>;

  // Specialization for string types.
  template<>
  class argument_wrapper<const char *>
  {
  public:
    argument_wrapper () : m_object (NULL) { }
    ~argument_wrapper ()
    {
      delete[] m_object;
    }

    operator const char * () const
    {
      return m_object;
    }

    status unmarshall (connection *conn)
    {
      return ::cc1_plugin::unmarshall (conn, &m_object);
    }

  private:

    char *m_object;

    // No copying or assignment allowed.
    argument_wrapper (const argument_wrapper &);
    argument_wrapper &operator= (const argument_wrapper &);
  };

  // Specialization for gcc_type_array.
  template<>
  class argument_wrapper<const gcc_type_array *>
  {
  public:
    argument_wrapper () : m_object (NULL) { }
    ~argument_wrapper ()
    {
      // It would be nicer if gcc_type_array could have a destructor.
      // But, it is in code shared with gdb and cannot.
      if (m_object != NULL)
	delete[] m_object->elements;
      delete m_object;
    }

    operator const gcc_type_array * () const
    {
      return m_object;
    }

    status unmarshall (connection *conn)
    {
      return ::cc1_plugin::unmarshall (conn, &m_object);
    }

  private:

    gcc_type_array *m_object;

    // No copying or assignment allowed.
    argument_wrapper (const argument_wrapper &);
    argument_wrapper &operator= (const argument_wrapper &);
  };

#ifdef GCC_CP_INTERFACE_H
  // Specialization for gcc_vbase_array.
  template<>
  class argument_wrapper<const gcc_vbase_array *>
  {
  public:
    argument_wrapper () : m_object (NULL) { }
    ~argument_wrapper ()
    {
      // It would be nicer if gcc_type_array could have a destructor.
      // But, it is in code shared with gdb and cannot.
      if (m_object != NULL)
	{
	  delete[] m_object->flags;
	  delete[] m_object->elements;
	}
      delete m_object;
    }

    operator const gcc_vbase_array * () const
    {
      return m_object;
    }

    status unmarshall (connection *conn)
    {
      return ::cc1_plugin::unmarshall (conn, &m_object);
    }

  private:

    gcc_vbase_array *m_object;

    // No copying or assignment allowed.
    argument_wrapper (const argument_wrapper &);
    argument_wrapper &operator= (const argument_wrapper &);
  };

  // Specialization for gcc_cp_template_args.
  template<>
  class argument_wrapper<const gcc_cp_template_args *>
  {
  public:
    argument_wrapper () : m_object (NULL) { }
    ~argument_wrapper ()
    {
      // It would be nicer if gcc_type_array could have a destructor.
      // But, it is in code shared with gdb and cannot.
      if (m_object != NULL)
	{
	  delete[] m_object->elements;
	  delete[] m_object->kinds;
	}
      delete m_object;
    }

    operator const gcc_cp_template_args * () const
    {
      return m_object;
    }

    status unmarshall (connection *conn)
    {
      return ::cc1_plugin::unmarshall (conn, &m_object);
    }

  private:

    gcc_cp_template_args *m_object;

    // No copying or assignment allowed.
    argument_wrapper (const argument_wrapper &);
    argument_wrapper &operator= (const argument_wrapper &);
  };

  // Specialization for gcc_cp_function_args.
  template<>
  class argument_wrapper<const gcc_cp_function_args *>
  {
  public:
    argument_wrapper () : m_object (NULL) { }
    ~argument_wrapper ()
    {
      // It would be nicer if gcc_type_array could have a destructor.
      // But, it is in code shared with gdb and cannot.
      if (m_object != NULL)
	{
	  delete[] m_object->elements;
	}
      delete m_object;
    }

    operator const gcc_cp_function_args * () const
    {
      return m_object;
    }

    status unmarshall (connection *conn)
    {
      return ::cc1_plugin::unmarshall (conn, &m_object);
    }

  private:

    gcc_cp_function_args *m_object;

    // No copying or assignment allowed.
    argument_wrapper (const argument_wrapper &);
    argument_wrapper &operator= (const argument_wrapper &);
  };
#endif /* GCC_CP_INTERFACE_H */

  // There are two kinds of template functions here: "call" and
  // "callback".  They are each repeated multiple times to handle
  // different numbers of arguments.  (This would be improved with
  // C++11, though applying a call is still tricky until C++14 can be
  // used.)

  // The "call" template is used for making a remote procedure call.
  // It starts a query ('Q') packet, marshalls its arguments, waits
  // for a result, and finally reads and returns the result via an
  // "out" parameter.

  // The "callback" template is used when receiving a remote procedure
  // call.  This template function is suitable for use with the
  // "callbacks" and "connection" classes.  It decodes incoming
  // arguments, passes them to the wrapped function, and finally
  // marshalls a reply packet.

  template<typename R>
  status
  call (connection *conn, const char *method, R *result)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, 0))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  template<typename R, R (*func) (connection *)>
  status
  callback (connection *conn)
  {
    R result;

    if (!unmarshall_check (conn, 0))
      return FAIL;
    result = func (conn);
    if (!conn->send ('R'))
      return FAIL;
    return marshall (conn, result);
  }

  template<typename R, typename A>
  status
  call (connection *conn, const char *method, R *result, A arg)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, 1))
      return FAIL;
    if (!marshall (conn, arg))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  template<typename R, typename A, R (*func) (connection *, A)>
  status
  callback (connection *conn)
  {
    argument_wrapper<A> arg;
    R result;

    if (!unmarshall_check (conn, 1))
      return FAIL;
    if (!arg.unmarshall (conn))
      return FAIL;
    result = func (conn, arg);
    if (!conn->send ('R'))
      return FAIL;
    return marshall (conn, result);
  }

  template<typename R, typename A1, typename A2>
  status
  call (connection *conn, const char *method, R *result, A1 arg1, A2 arg2)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, 2))
      return FAIL;
    if (!marshall (conn, arg1))
      return FAIL;
    if (!marshall (conn, arg2))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  template<typename R, typename A1, typename A2, R (*func) (connection *,
							    A1, A2)>
  status
  callback (connection *conn)
  {
    argument_wrapper<A1> arg1;
    argument_wrapper<A2> arg2;
    R result;

    if (!unmarshall_check (conn, 2))
      return FAIL;
    if (!arg1.unmarshall (conn))
      return FAIL;
    if (!arg2.unmarshall (conn))
      return FAIL;
    result = func (conn, arg1, arg2);
    if (!conn->send ('R'))
      return FAIL;
    return marshall (conn, result);
  }

  template<typename R, typename A1, typename A2, typename A3>
  status
  call (connection *conn, const char *method, R *result, A1 arg1, A2 arg2,
	A3 arg3)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, 3))
      return FAIL;
    if (!marshall (conn, arg1))
      return FAIL;
    if (!marshall (conn, arg2))
      return FAIL;
    if (!marshall (conn, arg3))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  template<typename R, typename A1, typename A2, typename A3,
	   R (*func) (connection *, A1, A2, A3)>
  status
  callback (connection *conn)
  {
    argument_wrapper<A1> arg1;
    argument_wrapper<A2> arg2;
    argument_wrapper<A3> arg3;
    R result;

    if (!unmarshall_check (conn, 3))
      return FAIL;
    if (!arg1.unmarshall (conn))
      return FAIL;
    if (!arg2.unmarshall (conn))
      return FAIL;
    if (!arg3.unmarshall (conn))
      return FAIL;
    result = func (conn, arg1, arg2, arg3);
    if (!conn->send ('R'))
      return FAIL;
    return marshall (conn, result);
  }

  template<typename R, typename A1, typename A2, typename A3, typename A4>
  status
  call (connection *conn, const char *method, R *result, A1 arg1, A2 arg2,
	A3 arg3, A4 arg4)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, 4))
      return FAIL;
    if (!marshall (conn, arg1))
      return FAIL;
    if (!marshall (conn, arg2))
      return FAIL;
    if (!marshall (conn, arg3))
      return FAIL;
    if (!marshall (conn, arg4))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  template<typename R, typename A1, typename A2, typename A3, typename A4,
	   R (*func) (connection *, A1, A2, A3, A4)>
  status
  callback (connection *conn)
  {
    argument_wrapper<A1> arg1;
    argument_wrapper<A2> arg2;
    argument_wrapper<A3> arg3;
    argument_wrapper<A4> arg4;
    R result;

    if (!unmarshall_check (conn, 4))
      return FAIL;
    if (!arg1.unmarshall (conn))
      return FAIL;
    if (!arg2.unmarshall (conn))
      return FAIL;
    if (!arg3.unmarshall (conn))
      return FAIL;
    if (!arg4.unmarshall (conn))
      return FAIL;
    result = func (conn, arg1, arg2, arg3, arg4);
    if (!conn->send ('R'))
      return FAIL;
    return marshall (conn, result);
  }

  template<typename R, typename A1, typename A2, typename A3, typename A4,
	   typename A5>
  status
  call (connection *conn, const char *method, R *result, A1 arg1, A2 arg2,
	A3 arg3, A4 arg4, A5 arg5)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, 5))
      return FAIL;
    if (!marshall (conn, arg1))
      return FAIL;
    if (!marshall (conn, arg2))
      return FAIL;
    if (!marshall (conn, arg3))
      return FAIL;
    if (!marshall (conn, arg4))
      return FAIL;
    if (!marshall (conn, arg5))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  template<typename R, typename A1, typename A2, typename A3, typename A4,
	   typename A5, R (*func) (connection *, A1, A2, A3, A4, A5)>
  status
  callback (connection *conn)
  {
    argument_wrapper<A1> arg1;
    argument_wrapper<A2> arg2;
    argument_wrapper<A3> arg3;
    argument_wrapper<A4> arg4;
    argument_wrapper<A5> arg5;
    R result;

    if (!unmarshall_check (conn, 5))
      return FAIL;
    if (!arg1.unmarshall (conn))
      return FAIL;
    if (!arg2.unmarshall (conn))
      return FAIL;
    if (!arg3.unmarshall (conn))
      return FAIL;
    if (!arg4.unmarshall (conn))
      return FAIL;
    if (!arg5.unmarshall (conn))
      return FAIL;
    result = func (conn, arg1, arg2, arg3, arg4, arg5);
    if (!conn->send ('R'))
      return FAIL;
    return marshall (conn, result);
  }

  template<typename R, typename A1, typename A2, typename A3, typename A4,
	   typename A5, typename A6, typename A7>
  status
  call (connection *conn, const char *method, R *result, A1 arg1, A2 arg2,
	A3 arg3, A4 arg4, A5 arg5, A6 arg6, A7 arg7)
  {
    if (!conn->send ('Q'))
      return FAIL;
    if (!marshall (conn, method))
      return FAIL;
    if (!marshall (conn, 7))
      return FAIL;
    if (!marshall (conn, arg1))
      return FAIL;
    if (!marshall (conn, arg2))
      return FAIL;
    if (!marshall (conn, arg3))
      return FAIL;
    if (!marshall (conn, arg4))
      return FAIL;
    if (!marshall (conn, arg5))
      return FAIL;
    if (!marshall (conn, arg6))
      return FAIL;
    if (!marshall (conn, arg7))
      return FAIL;
    if (!conn->wait_for_result ())
      return FAIL;
    if (!unmarshall (conn, result))
      return FAIL;
    return OK;
  }

  template<typename R, typename A1, typename A2, typename A3, typename A4,
	   typename A5, typename A6, typename A7,
	   R (*func) (connection *, A1, A2, A3, A4, A5, A6, A7)>
  status
  callback (connection *conn)
  {
    argument_wrapper<A1> arg1;
    argument_wrapper<A2> arg2;
    argument_wrapper<A3> arg3;
    argument_wrapper<A4> arg4;
    argument_wrapper<A5> arg5;
    argument_wrapper<A6> arg6;
    argument_wrapper<A7> arg7;
    R result;

    if (!unmarshall_check (conn, 7))
      return FAIL;
    if (!arg1.unmarshall (conn))
      return FAIL;
    if (!arg2.unmarshall (conn))
      return FAIL;
    if (!arg3.unmarshall (conn))
      return FAIL;
    if (!arg4.unmarshall (conn))
      return FAIL;
    if (!arg5.unmarshall (conn))
      return FAIL;
    if (!arg6.unmarshall (conn))
      return FAIL;
    if (!arg7.unmarshall (conn))
      return FAIL;
    result = func (conn, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    if (!conn->send ('R'))
      return FAIL;
    return marshall (conn, result);
  }
};

#endif // CC1_PLUGIN_RPC_HH
