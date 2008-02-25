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

  using std::posix_error::posix_errno;
  using std::posix_error::address_family_not_supported;
  using std::posix_error::address_in_use;
  using std::posix_error::address_not_available;
  using std::posix_error::already_connected;
  using std::posix_error::argument_list_too_long;
  using std::posix_error::argument_out_of_domain;
  using std::posix_error::bad_address;
  using std::posix_error::bad_file_descriptor;

#ifdef _GLIBCXX_HAVE_EBADMSG
  using std::posix_error::bad_message;
#endif

  using std::posix_error::broken_pipe;
  using std::posix_error::connection_aborted;
  using std::posix_error::connection_already_in_progress; 
  using std::posix_error::connection_refused; 
  using std::posix_error::connection_reset; 
  using std::posix_error::cross_device_link; 
  using std::posix_error::destination_address_required;
  using std::posix_error::device_or_resource_busy;
  using std::posix_error::directory_not_empty; 
  using std::posix_error::executable_format_error;
  using std::posix_error::file_exists;
  using std::posix_error::file_too_large; 	
  using std::posix_error::filename_too_long;
  using std::posix_error::function_not_supported; 
  using std::posix_error::host_unreachable; 

#ifdef _GLIBCXX_HAVE_EIDRM
  using std::posix_error::identifier_removed;
#endif

  using std::posix_error::illegal_byte_sequence; 
  using std::posix_error::inappropriate_io_control_operation; 
  using std::posix_error::interrupted; 
  using std::posix_error::invalid_argument;
  using std::posix_error::invalid_seek; 
  using std::posix_error::io_error; 
  using std::posix_error::is_a_directory; 
  using std::posix_error::message_size; 
  using std::posix_error::network_down; 
  using std::posix_error::network_reset;
  using std::posix_error::network_unreachable; 
  using std::posix_error::no_buffer_space; 
  using std::posix_error::no_child_process;

#ifdef _GLIBCXX_HAVE_ENOLINK
  using std::posix_error::no_link; 
#endif

  using std::posix_error::no_lock_available; 

#ifdef _GLIBCXX_HAVE_ENODATA
  using std::posix_error::no_message_available; 
#endif

  using std::posix_error::no_message; 
  using std::posix_error::no_posix_equivalent; 
  using std::posix_error::no_protocol_option; 
  using std::posix_error::no_space_on_device;

#ifdef _GLIBCXX_HAVE_ENOSR
  using std::posix_error::no_stream_resources; 
#endif

  using std::posix_error::no_such_device_or_address; 
  using std::posix_error::no_such_device; 	
  using std::posix_error::no_such_file_or_directory; 
  using std::posix_error::no_such_process; 	
  using std::posix_error::not_a_directory; 
  using std::posix_error::not_a_socket; 

#ifdef _GLIBCXX_HAVE_ENOSTR
  using std::posix_error::not_a_stream; 
#endif

  using std::posix_error::not_connected; 
  using std::posix_error::not_enough_memory;
  using std::posix_error::not_supported;

#ifdef _GLIBCXX_HAVE_ECANCELED
  using std::posix_error::operation_canceled;
#endif

  using std::posix_error::operation_in_progress;
  using std::posix_error::operation_not_permitted;
  using std::posix_error::operation_not_supported;
  using std::posix_error::operation_would_block;

#ifdef _GLIBCXX_HAVE_EOWNERDEAD
  using std::posix_error::owner_dead; 
#endif

  using std::posix_error::permission_denied;

#ifdef _GLIBCXX_HAVE_EPROTO
  using std::posix_error::protocol_error; 
#endif

  using std::posix_error::protocol_not_supported;
  using std::posix_error::read_only_file_system; 
  using std::posix_error::resource_deadlock_would_occur;
  using std::posix_error::resource_unavailable_try_again; 
  using std::posix_error::result_out_of_range;

#ifdef _GLIBCXX_HAVE_ENOTRECOVERABLE
  using std::posix_error::state_not_recoverable; 
#endif

#ifdef _GLIBCXX_HAVE_ETIME
  using std::posix_error::stream_timeout; 
#endif

#ifdef _GLIBCXX_HAVE_ETXTBSY
  using std::posix_error::text_file_busy; 
#endif

  using std::posix_error::timed_out; 
  using std::posix_error::too_many_files_open_in_system; 
  using std::posix_error::too_many_files_open; 
  using std::posix_error::too_many_links; 	
  using std::posix_error::too_many_synbolic_link_levels; 

#ifdef _GLIBCXX_HAVE_EOVERFLOW
  using std::posix_error::value_too_large; 
#endif

  using std::posix_error::wrong_protocol_type;
}
