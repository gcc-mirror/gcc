// Copyright (C) 2020-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <tuple>
#include <utility>
#include <cstdio>

template <typename T>
  struct SincosReference
  {
    T x, s, c;

    std::tuple<const T &, const T &, const T &>
    as_tuple() const
    { return std::tie(x, s, c); }
  };

template <typename T>
  struct Reference {
    T x, ref;

    std::tuple<const T &, const T &>
    as_tuple() const
    { return std::tie(x, ref); }
  };

template <typename T>
  struct Array
  {
    std::size_t size_;
    const T *data_;

    Array()
    : size_(0), data_(nullptr) {}

    Array(size_t s, const T *p)
    : size_(s), data_(p) {}

    const T*
    begin() const
    { return data_; }

    const T*
    end() const
    { return data_ + size_; }

    std::size_t
    size() const
    { return size_; }
  };

namespace function {
  struct sincos{ static constexpr const char *const str = "sincos"; };
  struct atan  { static constexpr const char *const str = "atan"; };
  struct asin  { static constexpr const char *const str = "asin"; };
  struct acos  { static constexpr const char *const str = "acos"; };
  struct log   { static constexpr const char *const str = "ln"; };
  struct log2  { static constexpr const char *const str = "log2"; };
  struct log10 { static constexpr const char *const str = "log10"; };
}

template <class F>
  struct testdatatype_for_function
  {
    template <class T>
      using type = Reference<T>;
  };

template <>
  struct testdatatype_for_function<function::sincos>
  {
    template <class T>
      using type = SincosReference<T>;
  };

template <class F, class T>
  using testdatatype_for_function_t
    = typename testdatatype_for_function<F>::template type<T>;

template<typename T>
  struct StaticDeleter
  {
    const T *ptr;

    StaticDeleter(const T *p)
    : ptr(p) {}

    ~StaticDeleter()
    { delete[] ptr; }
  };

template <class F, class T>
  inline std::string filename()
  {
    static_assert(std::is_floating_point<T>::value, "");
    static const auto cache
      = std::string("reference-") + F::str
      + (sizeof(T) == 4 && std::__digits_v<T> == 24
	 && std::__max_exponent_v<T> == 128
	 ? "-sp"
	 : (sizeof(T) == 8
	    && std::__digits_v<T> == 53
	    && std::__max_exponent_v<T> == 1024
	    ? "-dp"
	    : (sizeof(T) == 16 && std::__digits_v<T> == 64
	       && std::__max_exponent_v<T> == 16384
	       ? "-ep"
	       : (sizeof(T) == 16 && std::__digits_v<T> == 113
		  && std::__max_exponent_v<T> == 16384
		  ? "-qp"
		  : "-unknown"))))
      + ".dat";
    return cache;
  }

template <class Fun, class T, class Ref = testdatatype_for_function_t<Fun, T>>
  Array<Ref>
  referenceData()
  {
    static Array<Ref> data;
    if (data.data_ == nullptr)
      {
	FILE* file = std::fopen(filename<Fun, T>().c_str(), "rb");
	if (file)
	  {
	    std::fseek(file, 0, SEEK_END);
	    const size_t size = std::ftell(file) / sizeof(Ref);
	    std::rewind(file);
	    auto                      mem = new Ref[size];
	    static StaticDeleter<Ref> _cleanup(data.data_);
	    data.size_ = std::fread(mem, sizeof(Ref), size, file);
	    data.data_ = mem;
	    std::fclose(file);
	  }
	else
	  {
	    __builtin_fprintf(
		stderr,
		"%s:%d: the reference data %s does not exist in the current "
		"working directory.\n",
		__FILE__, __LINE__, filename<Fun, T>().c_str());
	    __builtin_abort();
	  }
      }
    return data;
  }
