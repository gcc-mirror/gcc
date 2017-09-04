// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11" }
// { dg-do compile }

#include <memory>

const std::shared_ptr<int> si;
const std::weak_ptr<int> wi;
const std::owner_less<std::shared_ptr<int>> osi;
static_assert( noexcept(osi(si, si)), "" );
static_assert( noexcept(osi(si, wi)), "" );
static_assert( noexcept(osi(wi, si)), "" );
const std::owner_less<std::weak_ptr<int>> owi;
static_assert( noexcept(owi(wi, wi)), "" );
static_assert( noexcept(owi(si, wi)), "" );
static_assert( noexcept(owi(wi, si)), "" );
