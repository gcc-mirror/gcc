// { dg-do compile { target c++11 } }

// Copyright (C) 2007-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <system_error>

#define TEST_ERRC(x) errc x(errc::x); (void)x

void test01()
{
  using std::errc;

  TEST_ERRC(address_family_not_supported);
  TEST_ERRC(address_in_use);
  TEST_ERRC(address_not_available);
  TEST_ERRC(already_connected);
  TEST_ERRC(argument_list_too_long);
  TEST_ERRC(argument_out_of_domain);
  TEST_ERRC(bad_address);
  TEST_ERRC(bad_file_descriptor);

#ifdef EBADMSG
  TEST_ERRC(bad_message);
#endif

  TEST_ERRC(broken_pipe);
  TEST_ERRC(connection_aborted);
  TEST_ERRC(connection_already_in_progress);
  TEST_ERRC(connection_refused);
  TEST_ERRC(connection_reset);
  TEST_ERRC(cross_device_link);
  TEST_ERRC(destination_address_required);
  TEST_ERRC(device_or_resource_busy);
  TEST_ERRC(directory_not_empty);
  TEST_ERRC(executable_format_error);
  TEST_ERRC(file_exists);
  TEST_ERRC(file_too_large);
  TEST_ERRC(filename_too_long);
  TEST_ERRC(function_not_supported);
  TEST_ERRC(host_unreachable);

#ifdef EIDRM
  TEST_ERRC(identifier_removed);
#endif

  TEST_ERRC(illegal_byte_sequence);
  TEST_ERRC(inappropriate_io_control_operation);
  TEST_ERRC(interrupted);
  TEST_ERRC(invalid_argument);
  TEST_ERRC(invalid_seek);
  TEST_ERRC(io_error);
  TEST_ERRC(is_a_directory);
  TEST_ERRC(message_size);
  TEST_ERRC(network_down);
  TEST_ERRC(network_reset);
  TEST_ERRC(network_unreachable);
  TEST_ERRC(no_buffer_space);
  TEST_ERRC(no_child_process);

#ifdef ENOLINK
  TEST_ERRC(no_link);
#endif

  TEST_ERRC(no_lock_available);

#ifdef ENODATA
  TEST_ERRC(no_message_available);
#endif

#ifdef ENOMSG
  TEST_ERRC(no_message);
#endif
  TEST_ERRC(no_protocol_option);
  TEST_ERRC(no_space_on_device);

#ifdef ENOSR
  TEST_ERRC(no_stream_resources);
#endif

  TEST_ERRC(no_such_device_or_address);
  TEST_ERRC(no_such_device);
  TEST_ERRC(no_such_file_or_directory);
  TEST_ERRC(no_such_process);
  TEST_ERRC(not_a_directory);
  TEST_ERRC(not_a_socket);

#ifdef ENOSTR
  TEST_ERRC(not_a_stream);
#endif

  TEST_ERRC(not_connected);
  TEST_ERRC(not_enough_memory);
  TEST_ERRC(not_supported);

#ifdef ECANCELED
  TEST_ERRC(operation_canceled);
#endif

  TEST_ERRC(operation_in_progress);
  TEST_ERRC(operation_not_permitted);
  TEST_ERRC(operation_not_supported);
  TEST_ERRC(operation_would_block);

#ifdef EOWNERDEAD
  TEST_ERRC(owner_dead);
#endif

  TEST_ERRC(permission_denied);

#ifdef EPROTO
  TEST_ERRC(protocol_error);
#endif

  TEST_ERRC(protocol_not_supported);
  TEST_ERRC(read_only_file_system);
  TEST_ERRC(resource_deadlock_would_occur);
  TEST_ERRC(resource_unavailable_try_again);
  TEST_ERRC(result_out_of_range);

#ifdef ENOTRECOVERABLE
  TEST_ERRC(state_not_recoverable);
#endif

#ifdef ETIME
  TEST_ERRC(stream_timeout);
#endif

#ifdef ETXTBSY
  TEST_ERRC(text_file_busy);
#endif

  TEST_ERRC(timed_out);
  TEST_ERRC(too_many_files_open_in_system);
  TEST_ERRC(too_many_files_open);
  TEST_ERRC(too_many_links);
  TEST_ERRC(too_many_symbolic_link_levels);

#ifdef EOVERFLOW
  TEST_ERRC(value_too_large);
#endif

  TEST_ERRC(wrong_protocol_type);
}
