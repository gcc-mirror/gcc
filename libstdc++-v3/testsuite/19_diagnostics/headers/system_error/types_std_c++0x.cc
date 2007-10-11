// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <system_error>

namespace gnu
{
  using std::system_error;
  using std::error_code;
  using std::error_category;
  using std::system_category;

  using std::posix_errno;
  using std::address_family_not_supported;
  using std::address_in_use;
  using std::address_not_available;
  using std::already_connected;
  using std::argument_list_too_long;
  using std::argument_out_of_domain;
  using std:: bad_address;
  using std::bad_file_descriptor;

#ifdef _GLIBCXX_HAVE_EBADMSG
  using std::bad_message;
#endif

  using std::broken_pipe;
  using std::connection_aborted;
  using std::connection_already_in_progress; 
  using std::connection_refused; 
  using std::connection_reset; 
  using std::cross_device_link; 
  using std::destination_address_required;
  using std::device_or_resource_busy;
  using std::directory_not_empty; 
  using std::executable_format_error;
  using std::file_exists;
  using std::file_too_large; 	
  using std::filename_too_long;
  using std::function_not_supported; 
  using std::host_unreachable; 
  using std::identifier_removed;
  using std::illegal_byte_sequence; 
  using std::inappropriate_io_control_operation; 
  using std::interrupted; 
  using std::invalid_argument;
  using std::invalid_seek; 
  using std::io_error; 
  using std::is_a_directory; 
  using std::message_size; 
  using std::network_down; 
  using std::network_reset;
  using std::network_unreachable; 
  using std::no_buffer_space; 
  using std::no_child_process;

#ifdef _GLIBCXX_HAVE_ENOLINK
  using std::no_link; 
#endif

  using std::no_lock_available; 

#ifdef _GLIBCXX_HAVE_ENODATA
  using std::no_message_available; 
#endif

  using std::no_message; 
  using std::no_posix_equivalent; 
  using std::no_protocol_option; 
  using std::no_space_on_device;

#ifdef _GLIBCXX_HAVE_ENOSR
  using std::no_stream_resources; 
#endif

  using std::no_such_device_or_address; 
  using std::no_such_device; 	
  using std::no_such_file_or_directory; 
  using std::no_such_process; 	
  using std::not_a_directory; 
  using std::not_a_socket; 

#ifdef _GLIBCXX_HAVE_ENOSTR
  using std::not_a_stream; 
#endif

  using std::not_connected; 
  using std::not_enough_memory;
  using std::not_supported;

#ifdef _GLIBCXX_HAVE_ECANCELED
  using std::operation_canceled;
#endif

  using std::operation_in_progress;
  using std::operation_not_permitted;
  using std::operation_not_supported;
  using std::operation_would_block;

#ifdef _GLIBCXX_HAVE_EOWNERDEAD
  using std::owner_dead; 
#endif

  using std::permission_denied;

#ifdef _GLIBCXX_HAVE_EPROTO
  using std::protocol_error; 
#endif

  using std::protocol_not_supported;
  using std::read_only_file_system; 
  using std::resource_deadlock_would_occur;
  using std::resource_unavailable_try_again; 
  using std::result_out_of_range;

#ifdef _GLIBCXX_HAVE_ENOTRECOVERABLE
  using std::state_not_recoverable; 
#endif

#ifdef _GLIBCXX_HAVE_ETIME
  using std::stream_timeout; 
#endif

  using std::text_file_busy; 
  using std::timed_out; 
  using std::too_many_files_open_in_system; 
  using std::too_many_files_open; 
  using std::too_many_links; 	
  using std::too_many_synbolic_link_levels; 

#ifdef _GLIBCXX_HAVE_EOVERFLOW
  using std::value_too_large; 
#endif

  using std::wrong_protocol_type;
}
