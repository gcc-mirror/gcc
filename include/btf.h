/* Declarations and definitions relating to the BPF Type Format (BTF).
   Copyright (C) 2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This file is derived from the BTF specification described in the
   Linux kernel source tree (linux/Documentation/bpf/btf.rst).  */

#ifndef _BTF_H_
#define _BTF_H_

#include <stdint.h>

#ifdef	__cplusplus
extern "C"
{
#endif

/* BTF magic number to identify header, endianness.  */
#define BTF_MAGIC	0xeb9f
/* Data format version number.  */
#define BTF_VERSION 	1

struct btf_header
{
  uint16_t magic;	/* Magic number (BTF_MAGIC).  */
  uint8_t  version;	/* Data format version (BTF_VERSION).  */
  uint8_t  flags;	/* Flags. Currently unused.  */
  uint32_t hdr_len;	/* Length of this header (sizeof (struct btf_header)).  */

  /* Following offsets are relative to the end of this header.  */
  uint32_t type_off;	/* Offset of type section, in bytes.  */
  uint32_t type_len;	/* Length of type section, in bytes.  */
  uint32_t str_off;	/* Offset of string section, in bytes.  */
  uint32_t str_len;	/* Length of string section, in bytes.  */
};

/* Maximum type identifier.  */
#define BTF_MAX_TYPE	0x000fffff
/* Maximum offset into the string section.  */
#define BTF_MAX_NAME_OFFSET	0x00ffffff
/* Maximum number of struct, union, enum members or func args.  */
#define BTF_MAX_VLEN	0xffff

struct btf_type
{
  uint32_t name_off; 	/* Offset in string section of type name.  */
  uint32_t info;	/* Encoded kind, variant length, kind flag:
			   - bits  0-15: vlen
			   - bits 16-23: unused
			   - bits 24-28: kind
			   - bits 29-30: unused
			   - bit     31: kind_flag
			   See accessor macros below.  */

  /* SIZE is used by INT, ENUM, STRUCT, UNION, DATASEC kinds.
     TYPE is used by PTR, TYPEDEF, VOLATILE, CONST, RESTRICT, FUNC,
     FUNC_PROTO and VAR kinds.  */
  union
  {
    uint32_t size;	/* Size of the entire type, in bytes.  */
    uint32_t type;	/* A type_id referring to another type.  */
  };
};

/* The folloing macros access the information encoded in btf_type.info.  */
/* Type kind. See below.  */
#define BTF_INFO_KIND(info)	(((info) >> 24) & 0x1f)
/* Number of entries of variable length data following certain type kinds.
   For example, number of structure members, number of function parameters.  */
#define BTF_INFO_VLEN(info)	((info) & 0xffff)
/* For BTF_KIND_FWD, 1 if forward to union, 0 if forward to struct.
   For BTF_KIND_STRUCT and BTF_KIND_UNION, 1 if the struct/union contains
   a bitfield.  */
#define BTF_INFO_KFLAG(info)	((info) >> 31)

/* Encoding for struct btf_type.info.  */
#define BTF_TYPE_INFO(kind, kflag, vlen) \
  ((((kflag) ? 1 : 0 ) << 31) | ((kind) << 24) | ((vlen) & 0xffff))

#define BTF_KIND_UNKN		0	/* Unknown or invalid.  */
#define BTF_KIND_INT		1	/* Integer.  */
#define BTF_KIND_PTR		2	/* Pointer.  */
#define BTF_KIND_ARRAY		3	/* Array.  */
#define BTF_KIND_STRUCT		4	/* Struct.  */
#define BTF_KIND_UNION		5	/* Union.  */
#define BTF_KIND_ENUM		6	/* Enumeration.  */
#define BTF_KIND_FWD		7	/* Forward.  */
#define BTF_KIND_TYPEDEF	8	/* Typedef.  */
#define BTF_KIND_VOLATILE	9	/* Referenced type is volatile.  */
#define BTF_KIND_CONST		10	/* Referenced type is const.  */
#define BTF_KIND_RESTRICT	11	/* Restrict.  */
#define BTF_KIND_FUNC		12	/* Subprogram.  */
#define BTF_KIND_FUNC_PROTO	13	/* Function Prototype.  */
#define BTF_KIND_VAR		14	/* Variable.  */
#define BTF_KIND_DATASEC	15	/* Section such as .bss or .data.  */
#define BTF_KIND_FLOAT		16	/* Floating point.  */
#define BTF_KIND_MAX		BTF_KIND_FLOAT
#define NR_BTF_KINDS		(BTF_KIND_MAX + 1)

/* For some BTF_KINDs, struct btf_type is immediately followed by
   additional data describing the type.  */

/* BTF_KIND_INT is followed by a 32-bit word, with the following
   bit arrangement.  */
#define BTF_INT_ENCODING(VAL)	(((VAL) & 0x0f000000) >> 24)
#define BTF_INT_OFFSET(VAL)	(((VAL) & 0x00ff0000) >> 16)
#define BTF_INT_BITS(VAL)	((VAL)  & 0x000000ff)

#define BTF_INT_DATA(encoding, offset, bits) \
  ((((encoding) & 0x0f) << 24) | (((offset) & 0xff) << 16) | ((bits) & 0xff))

/* BTF_INT_ENCODING holds the following attribute flags.  */
#define BTF_INT_SIGNED 	(1 << 0)
#define BTF_INT_CHAR 	(1 << 1)
#define BTF_INT_BOOL	(1 << 2)

/* BTF_KIND_ENUM is followed by VLEN struct btf_enum entries,
   which describe the enumerators. Note that BTF currently only
   supports signed 32-bit enumerator values.  */
struct btf_enum
{
  uint32_t name_off;	/* Offset in string section of enumerator name.  */
  int32_t  val;		/* Enumerator value.  */
};

/* BTF_KIND_ARRAY is followed by a single struct btf_array.  */
struct btf_array
{
  uint32_t type;	/* Type of array elements.  */
  uint32_t index_type;	/* Type of array index.  */
  uint32_t nelems;	/* Number of elements. 0 for unsized/variable length.  */
};

/* BTF_KIND_STRUCT and BTF_KIND_UNION are followed by VLEN
   struct btf_member.  */
struct btf_member
{
  uint32_t name_off;	/* Offset in string section of member name.  */
  uint32_t type;	/* Type of member.  */
  uint32_t offset;	/* If the type info kind_flag is set, this contains
			   both the member bitfield size and bit offset,
			   according to the macros below. If kind_flag is not
			   set, offset contains only the bit offset (from the
			   beginning of the struct).  */
};

/* If struct or union type info kind_flag is set, used to access member
   bitfield size from btf_member.offset.  */
#define BTF_MEMBER_BITFIELD_SIZE (val) 	((val) >> 24)
/* If struct or union type info kind_flag is set, used to access member
   bit offset from btf_member.offset.  */
#define BTF_MEMBER_BIT_OFFSET (val)	((val) & 0x00ffffff)

/* BTF_KIND_FUNC_PROTO is followed by VLEN struct btf_param entries, which
   describe the types of the function parameters.  */
struct btf_param
{
  uint32_t name_off;	/* Offset in string section of parameter name.  */
  uint32_t type;	/* Type of parameter.  */
};

/* BTF_KIND_VAR is followed by a single struct btf_var, which describes
   information about the variable.  */
struct btf_var
{
  uint32_t linkage;	/* Currently only 0=static or 1=global.  */
};

/* BTF_KIND_DATASEC is followed by VLEN struct btf_var_secinfo entries,
   which describe all BTF_KIND_VAR types contained in the section.  */
struct btf_var_secinfo
{
  uint32_t type;	/* Type of variable.  */
  uint32_t offset;	/* In-section offset of variable (in bytes).  */
  uint32_t size;	/* Size (in bytes) of variable.  */
};

#ifdef	__cplusplus
}
#endif

#endif /* _BTF_H_ */
