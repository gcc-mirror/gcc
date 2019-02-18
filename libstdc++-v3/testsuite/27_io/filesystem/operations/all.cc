// { dg-options "-std=gnu++17 -fno-inline" }
// { dg-do link { target c++17 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2019 Free Software Foundation, Inc.
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

// C++17 30.10.15 Filesystem operation functions [fs.op.funcs]

#include <filesystem>

// Link-only test to ensure all operation functions are exported from the lib.

int
main()
{
  const std::filesystem::path p;
  std::filesystem::path p2;
  const std::filesystem::copy_options copyopts{};
  const std::filesystem::file_status st{};
  std::filesystem::file_status st2;
  const std::filesystem::file_time_type t;
  std::filesystem::file_time_type t2;
  const std::filesystem::perms perms{};
  const std::filesystem::perm_options permopts{};
  std::filesystem::space_info sp;
  std::error_code ec;
  bool b;
  std::uintmax_t size;

  std::filesystem::absolute(p);
  std::filesystem::absolute(p, ec);

  std::filesystem::canonical(p);
  std::filesystem::canonical(p, ec);

  std::filesystem::copy(p, p);
  std::filesystem::copy(p, p, ec);
  std::filesystem::copy(p, p, copyopts);
  std::filesystem::copy(p, p, copyopts, ec);

  std::filesystem::copy_file(p, p);
  std::filesystem::copy_file(p, p, ec);
  std::filesystem::copy_file(p, p, copyopts);
  std::filesystem::copy_file(p, p, copyopts, ec);

  std::filesystem::copy_symlink(p, p);
  std::filesystem::copy_symlink(p, p, ec);

  std::filesystem::create_directories(p);
  std::filesystem::create_directories(p, ec);

  std::filesystem::create_directory(p);
  std::filesystem::create_directory(p, ec);

  std::filesystem::create_directory(p, p);
  std::filesystem::create_directory(p, p, ec);

  std::filesystem::create_directory_symlink(p, p);
  std::filesystem::create_directory_symlink(p, p, ec);

  std::filesystem::create_hard_link(p, p);
  std::filesystem::create_hard_link(p, p, ec);

  std::filesystem::create_symlink(p, p);
  std::filesystem::create_symlink(p, p, ec);

  p2 = std::filesystem::current_path();
  p2 = std::filesystem::current_path(ec);
  std::filesystem::current_path(p);
  std::filesystem::current_path(p, ec);

  b = std::filesystem::equivalent(p, p);
  b = std::filesystem::equivalent(p, p, ec);

  b = std::filesystem::exists(st);
  b = std::filesystem::exists(p);
  b = std::filesystem::exists(p, ec);

  size = std::filesystem::file_size(p);
  size = std::filesystem::file_size(p, ec);

  size = std::filesystem::hard_link_count(p);
  size = std::filesystem::hard_link_count(p, ec);

  b = std::filesystem::is_block_file(st);
  b = std::filesystem::is_block_file(p);
  b = std::filesystem::is_block_file(p, ec);

  b = std::filesystem::is_character_file(st);
  b = std::filesystem::is_character_file(p);
  b = std::filesystem::is_character_file(p, ec);

  b = std::filesystem::is_directory(st);
  b = std::filesystem::is_directory(p);
  b = std::filesystem::is_directory(p, ec);

  b = std::filesystem::is_empty(p);
  b = std::filesystem::is_empty(p, ec);

  b = std::filesystem::is_fifo(st);
  b = std::filesystem::is_fifo(p);
  b = std::filesystem::is_fifo(p, ec);

  b = std::filesystem::is_other(st);
  b = std::filesystem::is_other(p);
  b = std::filesystem::is_other(p, ec);

  b = std::filesystem::is_regular_file(st);
  b = std::filesystem::is_regular_file(p);
  b = std::filesystem::is_regular_file(p, ec);

  b = std::filesystem::is_socket(st);
  b = std::filesystem::is_socket(p);
  b = std::filesystem::is_socket(p, ec);

  b = std::filesystem::is_symlink(st);
  b = std::filesystem::is_symlink(p);
  b = std::filesystem::is_symlink(p, ec);

  t2 = std::filesystem::last_write_time(p);
  t2 = std::filesystem::last_write_time(p, ec);
  std::filesystem::last_write_time(p, t);
  std::filesystem::last_write_time(p, t, ec);

  std::filesystem::permissions(p, perms);
  std::filesystem::permissions(p, perms, permopts);
  std::filesystem::permissions(p, perms, ec);
  std::filesystem::permissions(p, perms, permopts, ec);

  p2 = std::filesystem::proximate(p, ec);
  p2 = std::filesystem::proximate(p);
  p2 = std::filesystem::proximate(p, p);
  p2 = std::filesystem::proximate(p, p, ec);

  p2 = std::filesystem::read_symlink(p);
  p2 = std::filesystem::read_symlink(p, ec);

  p2 = std::filesystem::relative(p, ec);
  p2 = std::filesystem::relative(p);
  p2 = std::filesystem::relative(p, p);
  p2 = std::filesystem::relative(p, p, ec);

  b = std::filesystem::remove(p);
  b = std::filesystem::remove(p, ec);

  size = std::filesystem::remove_all(p);
  size = std::filesystem::remove_all(p, ec);

  std::filesystem::rename(p, p);
  std::filesystem::rename(p, p, ec);

  std::filesystem::resize_file(p, size);
  std::filesystem::resize_file(p, size, ec);

  sp = std::filesystem::space(p);
  sp = std::filesystem::space(p, ec);

  st2 = std::filesystem::status(p);
  st2 = std::filesystem::status(p, ec);

  b = std::filesystem::status_known(st);

  st2 = std::filesystem::symlink_status(p);
  st2 = std::filesystem::symlink_status(p, ec);

  p2 = std::filesystem::temp_directory_path();
  p2 = std::filesystem::temp_directory_path(ec);

  p2 = std::filesystem::weakly_canonical(p);
  p2 = std::filesystem::weakly_canonical(p, ec);
}
