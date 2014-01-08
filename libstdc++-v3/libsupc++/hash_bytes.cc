// Definition of _Hash_bytes. -*- C++ -*-

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// This file defines Hash_bytes, a primitive used for defining hash
// functions. Based on public domain MurmurHashUnaligned2, by Austin
// Appleby.  http://murmurhash.googlepages.com/

// This file also defines _Fnv_hash_bytes, another primitive with
// exactly the same interface but using a different hash algorithm,
// Fowler / Noll / Vo (FNV) Hash (type FNV-1a). The Murmur hash
// function apears to be better in both speed and hash quality, and
// FNV is provided primarily for backward compatibility.

#include <bits/hash_bytes.h>

namespace
{
  inline std::size_t
  unaligned_load(const char* p)
  {
    std::size_t result;
    __builtin_memcpy(&result, p, sizeof(result));
    return result;
  }

#if __SIZEOF_SIZE_T__ == 8
  // Loads n bytes, where 1 <= n < 8.
  inline std::size_t
  load_bytes(const char* p, int n)
  {
    std::size_t result = 0;
    --n;
    do
      result = (result << 8) + static_cast<unsigned char>(p[n]);
    while (--n >= 0);
    return result;
  }

  inline std::size_t
  shift_mix(std::size_t v)
  { return v ^ (v >> 47);}
#endif
}

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#if __SIZEOF_SIZE_T__ == 4

  // Implementation of Murmur hash for 32-bit size_t.
  size_t
  _Hash_bytes(const void* ptr, size_t len, size_t seed)
  {
    const size_t m = 0x5bd1e995;
    size_t hash = seed ^ len;
    const char* buf = static_cast<const char*>(ptr);

    // Mix 4 bytes at a time into the hash.
    while(len >= 4)
      {
	size_t k = unaligned_load(buf);
	k *= m;
	k ^= k >> 24;
	k *= m;
	hash *= m;
	hash ^= k;
	buf += 4;
	len -= 4;
      }

    // Handle the last few bytes of the input array.
    switch(len)
      {
      case 3:
	hash ^= static_cast<unsigned char>(buf[2]) << 16;
      case 2:
	hash ^= static_cast<unsigned char>(buf[1]) << 8;
      case 1:
	hash ^= static_cast<unsigned char>(buf[0]);
	hash *= m;
      };

    // Do a few final mixes of the hash.
    hash ^= hash >> 13;
    hash *= m;
    hash ^= hash >> 15;
    return hash;
  }

  // Implementation of FNV hash for 32-bit size_t.
  size_t
  _Fnv_hash_bytes(const void* ptr, size_t len, size_t hash)
  {
    const char* cptr = static_cast<const char*>(ptr);
    for (; len; --len)
      {
	hash ^= static_cast<size_t>(*cptr++);
	hash *= static_cast<size_t>(16777619UL);
      }
    return hash;
  }

#elif __SIZEOF_SIZE_T__ == 8

  // Implementation of Murmur hash for 64-bit size_t.
  size_t
  _Hash_bytes(const void* ptr, size_t len, size_t seed)
  {
    static const size_t mul = (((size_t) 0xc6a4a793UL) << 32UL)
			      + (size_t) 0x5bd1e995UL;
    const char* const buf = static_cast<const char*>(ptr);

    // Remove the bytes not divisible by the sizeof(size_t).  This
    // allows the main loop to process the data as 64-bit integers.
    const int len_aligned = len & ~0x7;
    const char* const end = buf + len_aligned;
    size_t hash = seed ^ (len * mul);
    for (const char* p = buf; p != end; p += 8)
      {
	const size_t data = shift_mix(unaligned_load(p) * mul) * mul;
	hash ^= data;
	hash *= mul;
      }
    if ((len & 0x7) != 0)
      {
	const size_t data = load_bytes(end, len & 0x7);
	hash ^= data;
	hash *= mul;
      }
    hash = shift_mix(hash) * mul;
    hash = shift_mix(hash);
    return hash;
  }

  // Implementation of FNV hash for 64-bit size_t.
  size_t
  _Fnv_hash_bytes(const void* ptr, size_t len, size_t hash)
  {
    const char* cptr = static_cast<const char*>(ptr);
    for (; len; --len)
      {
	hash ^= static_cast<size_t>(*cptr++);
	hash *= static_cast<size_t>(1099511628211ULL);
      }
    return hash;
  }

#else

  // Dummy hash implementation for unusual sizeof(size_t).
  size_t
  _Hash_bytes(const void* ptr, size_t len, size_t seed)
  {
    size_t hash = seed;
    const char* cptr = reinterpret_cast<const char*>(ptr);
    for (; len; --len)
      hash = (hash * 131) + *cptr++;
    return hash;
  }

  size_t
  _Fnv_hash_bytes(const void* ptr, size_t len, size_t seed)
  { return _Hash_bytes(ptr, len, seed); }

#endif /* __SIZEOF_SIZE_T__ */

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
