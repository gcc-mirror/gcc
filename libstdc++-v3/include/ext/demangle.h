// C++ IA64 / g++ v3 demangler  -*- C++ -*-

// Copyright (C) 2003, 2004 Free Software Foundation, Inc.
// Written by Carlo Wood <carlo@alinoe.com>
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// This file implements demangling of "C++ ABI for Itanium"-mangled symbol
// and type names as described in Revision 1.73 of the C++ ABI as can be found
// at http://www.codesourcery.com/cxx-abi/abi.html#mangling

#ifndef _DEMANGLER_H
#define _DEMANGLER_H 1

#include <vector>
#include <string>
#include <ext/new_allocator.h>

#ifndef _GLIBCXX_DEMANGLER_DEBUG
#define _GLIBCXX_DEMANGLER_CWDEBUG 0
#define _GLIBCXX_DEMANGLER_DEBUG(x)
#define _GLIBCXX_DEMANGLER_DOUT(cntrl, data)
#define _GLIBCXX_DEMANGLER_DOUT_ENTERING(x)
#define _GLIBCXX_DEMANGLER_DOUT_ENTERING2(x)
#define _GLIBCXX_DEMANGLER_DOUT_ENTERING3(x)
#define _GLIBCXX_DEMANGLER_RETURN return M_result
#define _GLIBCXX_DEMANGLER_RETURN2 return M_result
#define _GLIBCXX_DEMANGLER_RETURN3
#define _GLIBCXX_DEMANGLER_FAILURE \
    do { M_result = false; return false; } while(0)
#else
#define _GLIBCXX_DEMANGLER_CWDEBUG 1
#endif

namespace __gnu_cxx
{
  namespace demangler
  {
    enum substitution_nt
    {
      type,
      template_template_param,
      nested_name_prefix,
      nested_name_template_prefix,
      unscoped_template_name
    };

    struct substitution_st
    {
      int M_start_pos;
      substitution_nt M_type;
      int M_number_of_prefixes;

      substitution_st(int start_pos,
		      substitution_nt type,
		      int number_of_prefixes)
      : M_start_pos(start_pos), M_type(type),
	M_number_of_prefixes(number_of_prefixes)
      { }
    };

    enum simple_qualifier_nt
    {
      complex_or_imaginary = 'G',
      pointer = 'P',
      reference = 'R'
    };

    enum cv_qualifier_nt
    {
      cv_qualifier = 'K'
    };

    enum param_qualifier_nt
    {
      vendor_extension = 'U',
      array = 'A',
      pointer_to_member = 'M'
    };

    template<typename Tp, typename Allocator = __gnu_cxx::new_allocator<Tp> >
      class qualifier;

    template<typename Tp, typename Allocator = __gnu_cxx::new_allocator<Tp> >
      class qualifier_list;

    template<typename Tp, typename Allocator = __gnu_cxx::new_allocator<Tp> >
      class session;

    template<typename Tp, typename Allocator>
      class qualifier
      {
	typedef typename Allocator::template rebind<char>::other
	        char_Allocator;
	typedef std::basic_string<char, std::char_traits<char>, char_Allocator>
	    string_type;

      private:
	char M_qualifier1;
	char M_qualifier2;
	char M_qualifier3;
	mutable unsigned char M_cnt;
	string_type M_optional_type;
	int M_start_pos;
	bool M_part_of_substitution;

      public:
	qualifier(int start_pos,
	          simple_qualifier_nt simple_qualifier,
		  int inside_substitution)
	: M_qualifier1(simple_qualifier),
	  M_start_pos(start_pos),
	  M_part_of_substitution(inside_substitution)
	{ }

	qualifier(int start_pos,
	          cv_qualifier_nt,
		  char const* start,
		  int count,
		  int inside_substitution)
	: M_qualifier1(start[0]),
	  M_qualifier2((count > 1) ? start[1] : '\0'),
	  M_qualifier3((count > 2) ? start[2] : '\0'),
	  M_start_pos(start_pos),
	  M_part_of_substitution(inside_substitution)
	{ }

	qualifier(int start_pos,
	          param_qualifier_nt param_qualifier,
		  string_type optional_type,
		  int inside_substitution)
	: M_qualifier1(param_qualifier),
	  M_optional_type(optional_type),
	  M_start_pos(start_pos),
	  M_part_of_substitution(inside_substitution)
	{ }

	int
	get_start_pos(void) const
	{ return M_start_pos; }

	char
	first_qualifier(void) const
	{ M_cnt = 1; return M_qualifier1; }

	char
	next_qualifier(void) const
	{
	  return (++M_cnt == 2) ? M_qualifier2
	                        : ((M_cnt == 3) ? M_qualifier3 : 0);
	}

	string_type const&
	get_optional_type(void) const
	{ return M_optional_type; }

	bool
	part_of_substitution(void) const
	{ return M_part_of_substitution; }

#if _GLIBCXX_DEMANGLER_CWDEBUG
	friend std::ostream& operator<<(std::ostream& os, qualifier const& qual)
	{
	  os << (char)qual.M_qualifier1;
	  if (qual.M_qualifier1 == vendor_extension ||
	      qual.M_qualifier1 == array ||
	      qual.M_qualifier1 == pointer_to_member)
	    os << " [" << qual.M_optional_type << ']';
	  else if (qual.M_qualifier1 == 'K' ||
		   qual.M_qualifier1 == 'V' ||
		   qual.M_qualifier1 == 'r')
	  {
	    if (qual.M_qualifier2)
	    {
	      os << (char)qual.M_qualifier2;
	      if (qual.M_qualifier3)
		os << (char)qual.M_qualifier3;
	    }
	  }
	  return os;
	}
#endif
      };

    template<typename Tp, typename Allocator>
      class qualifier_list
      {
	typedef typename Allocator::template rebind<char>::other
	  char_Allocator;
	typedef std::basic_string<char, std::char_traits<char>, char_Allocator>
	  string_type;

      private:
	mutable bool M_printing_suppressed;
	typedef qualifier<Tp, Allocator> qual;
        typedef typename Allocator::template rebind<qual>::other qual_Allocator;
	typedef std::vector<qual, qual_Allocator> qual_vector;
	qual_vector M_qualifier_starts;
	session<Tp, Allocator>& M_demangler;

	void decode_KVrA(string_type& prefix, string_type& postfix, int cvq,
			 typename qual_vector::
			   const_reverse_iterator const& iter_array) const;

      public:
	qualifier_list(session<Tp, Allocator>& demangler_obj)
	: M_printing_suppressed(false), M_demangler(demangler_obj)
	{ }

	void
	add_qualifier_start(simple_qualifier_nt simple_qualifier,
			    int start_pos,
			    int inside_substitution)
	{ M_qualifier_starts.
	      push_back(qualifier<Tp, Allocator>(start_pos,
		  simple_qualifier, inside_substitution)); }

	void
	add_qualifier_start(cv_qualifier_nt cv_qualifier,
			    int start_pos,
			    int count,
			    int inside_substitution)
	{ M_qualifier_starts.
	      push_back(qualifier<Tp, Allocator>(start_pos,
		    cv_qualifier, &M_demangler.M_str[start_pos],
		    count, inside_substitution)); }

	void
	add_qualifier_start(param_qualifier_nt param_qualifier,
			    int start_pos,
			    string_type optional_type,
			    int inside_substitution)
	{ M_qualifier_starts.
	      push_back(qualifier<Tp, Allocator>(start_pos,
		    param_qualifier, optional_type, inside_substitution)); }

	void
	decode_qualifiers(string_type& prefix,
			  string_type& postfix,
			  bool member_function_pointer_qualifiers) const;

	bool
	suppressed(void) const
	{ return M_printing_suppressed; }

	void
	printing_suppressed(void)
	{ M_printing_suppressed = true; }

	size_t
	size(void) const
	{ return M_qualifier_starts.size(); }

#if _GLIBCXX_DEMANGLER_CWDEBUG
	friend std::ostream& operator<<(std::ostream& os, qualifier_list const& list)
	{
	  typename qual_vector::const_iterator
	      iter = list.M_qualifier_starts.begin();
	  if (iter != list.M_qualifier_starts.end())
	  {
	    os << "{ " << *iter;
	    while (++iter != list.M_qualifier_starts.end())
	      os << ", " << *iter;
	    os << " }";
	  }
	  else
	    os << "{ }";
	  return os;
	}
#endif
      };

    struct implementation_details
    {
      private:
        unsigned int M_style;

      public:
	// The following flags change the behaviour of the demangler.  The
	// default behaviour is that none of these flags is set.

        static unsigned int const style_void = 1;
	// Default behaviour:				int f()
	// Use (void) instead of ():			int f(void)

        static unsigned int const style_literal = 2;
	// Default behaviour:				(long)13,
	//						(unsigned long long)19
	// Use extensions 'u', 'l' and 'll' for integral
	// literals (as in template arguments):		13l, 19ull

        static unsigned int const style_literal_int = 4;
	// Default behaviour:				4
	// Use also an explicit
	//   cast for int in literals:			(int)4

        static unsigned int const style_compact_expr_ops = 8;
	// Default behaviour:				(i) < (3), sizeof (int)
	// Don't output spaces around
	//   operators in expressions:			(i)<(3), sizeof(int)

        static unsigned int const style_sizeof_typename = 16;
	// Default behaviour:				sizeof (X::t)
	// Put 'typename' infront of <nested-name>
	// types inside a 'sizeof':			sizeof (typename X::t)

      public:
	implementation_details(unsigned int style_flags = 0) :
	    M_style(style_flags) { }
	virtual ~implementation_details() { }
	bool get_style_void(void) const
	    { return (M_style & style_void); }
	bool get_style_literal(void) const
	    { return (M_style & style_literal); }
	bool get_style_literal_int(void) const
	    { return (M_style & style_literal_int); }
	bool get_style_compact_expr_ops(void) const
	    { return (M_style & style_compact_expr_ops); }
	bool get_style_sizeof_typename(void) const
	    { return (M_style & style_sizeof_typename); }
        // This can be overridden by user implementations.
        virtual bool decode_real(char* /* output */, unsigned long* /* input */,
				 size_t /* size_of_real */) const
            { return false; }
    };

    template<typename Tp, typename Allocator>
      class session
      {
      public:
	friend class qualifier_list<Tp, Allocator>;
	typedef typename Allocator::template rebind<char>::other
	    char_Allocator;
	typedef std::basic_string<char, std::char_traits<char>, char_Allocator>
	    string_type;

      private:
	char const* M_str;
	int M_pos;
	int M_maxpos;
	bool M_result;
	int M_inside_template_args;
	int M_inside_type;
	int M_inside_substitution;
	bool M_saw_destructor;
	bool M_name_is_cdtor;
	bool M_name_is_template;
	bool M_name_is_conversion_operator;
	bool M_template_args_need_space;
	string_type M_function_name;
        typedef typename Allocator::template rebind<int>::other
                int_Allocator;
        typedef typename Allocator::template rebind<substitution_st>::other
                subst_Allocator;
	std::vector<int, int_Allocator> M_template_arg_pos;
	int M_template_arg_pos_offset;
	std::vector<substitution_st, subst_Allocator> M_substitutions_pos;
	implementation_details const& M_implementation_details;
	typedef typename Allocator::template
	    rebind<qualifier_list<Allocator> >::other qualifier_list_Allocator;
	qualifier_list_Allocator M_qualifier_list_alloc;
#if _GLIBCXX_DEMANGLER_CWDEBUG
	bool M_inside_add_substitution;
#endif

      public:
	explicit session(char const* in, int len,
	    implementation_details const& id = implementation_details())
	: M_str(in), M_pos(0), M_maxpos(len - 1), M_result(true),
	  M_inside_template_args(0), M_inside_type(0),
	  M_inside_substitution(0), M_saw_destructor(false),
	  M_name_is_cdtor(false), M_name_is_template(false),
	  M_name_is_conversion_operator(false),
	  M_template_args_need_space(false), M_template_arg_pos_offset(0),
	  M_implementation_details(id)
#if _GLIBCXX_DEMANGLER_CWDEBUG
	  , M_inside_add_substitution(false)
#endif
	{ }

	static int
	decode_encoding(string_type& output, char const* input, int len,
	  implementation_details const& id = implementation_details());

	bool
	decode_type(string_type& output,
	            qualifier_list<Tp, Allocator>* qualifiers = NULL)
	{
	  string_type postfix;
	  bool res = decode_type_with_postfix(output, postfix, qualifiers);
	  output += postfix;
	  return res;
	}

	bool
	remaining_input_characters(void) const
	{ return current() != 0; }

      private:
	char
	current(void) const
	{ return (M_pos > M_maxpos) ? 0 : M_str[M_pos]; }

	char
	next_peek(void) const
	{ return (M_pos >= M_maxpos) ? 0 : M_str[M_pos + 1]; }

	char
	next(void)
	{ return (M_pos >= M_maxpos) ? 0 : M_str[++M_pos]; }

	char
	eat_current(void)
	{ return (M_pos > M_maxpos) ? 0 : M_str[M_pos++]; }

	void
	store(int& saved_pos)
	{ saved_pos = M_pos; }

	void
	restore(int saved_pos)
	{ M_pos = saved_pos; M_result = true; }

	void
	add_substitution(int start_pos,
	                 substitution_nt sub_type,
			 int number_of_prefixes);

	bool decode_type_with_postfix(string_type& prefix,
	    string_type& postfix, qualifier_list<Tp, Allocator>* qualifiers = NULL);
	bool decode_bare_function_type(string_type& output);
	bool decode_builtin_type(string_type& output);
	bool decode_call_offset(string_type& output);
	bool decode_class_enum_type(string_type& output);
	bool decode_expression(string_type& output);
	bool decode_literal(string_type& output);
	bool decode_local_name(string_type& output);
	bool decode_name(string_type& output,
	    string_type& nested_name_qualifiers);
	bool decode_nested_name(string_type& output,
	    string_type& qualifiers);
	bool decode_number(string_type& output);
	bool decode_operator_name(string_type& output);
	bool decode_source_name(string_type& output);
	bool decode_substitution(string_type& output,
	    qualifier_list<Tp, Allocator>* qualifiers = NULL);
	bool decode_template_args(string_type& output);
	bool decode_template_param(string_type& output,
	    qualifier_list<Tp, Allocator>* qualifiers = NULL);
	bool decode_unqualified_name(string_type& output);
	bool decode_unscoped_name(string_type& output);
	bool decode_non_negative_decimal_integer(string_type& output);
	bool decode_special_name(string_type& output);
        bool decode_real(string_type& output, size_t size_of_real);
      };

    template<typename Tp, typename Allocator>
#if !_GLIBCXX_DEMANGLER_CWDEBUG
      inline
#endif
      void
      session<Tp, Allocator>::add_substitution(int start_pos,
					   substitution_nt sub_type,
					   int number_of_prefixes = 0)
      {
	if (!M_inside_substitution)
	{
#if _GLIBCXX_DEMANGLER_CWDEBUG
	  if (M_inside_add_substitution)
	    return;
#endif
	  M_substitutions_pos.
	      push_back(substitution_st(start_pos,
		  sub_type, number_of_prefixes));
#if _GLIBCXX_DEMANGLER_CWDEBUG
	  if (!DEBUGCHANNELS::dc::demangler.is_on())
	    return;
	  string_type substitution_name("S");
	  int n = M_substitutions_pos.size() - 1;
	  if (n > 0)
	    substitution_name += (n <= 10) ? (char)(n + '0' - 1)
					   : (char)(n + 'A' - 11);
	  substitution_name += '_';
	  string_type subst;
	  int saved_pos = M_pos;
	  M_pos = start_pos;
	  M_inside_add_substitution = true;
	  _GLIBCXX_DEMANGLER_DEBUG( dc::demangler.off() );
	  switch(sub_type)
	  {
	    case type:
	      decode_type(subst);
	      break;
	    case template_template_param:
	      decode_template_param(subst);
	      break;
	    case nested_name_prefix:
	    case nested_name_template_prefix:
	      for (int cnt = number_of_prefixes; cnt > 0; --cnt)
	      {
		if (current() == 'I')
		{
		  subst += ' ';
		  decode_template_args(subst);
		}
		else
		{
		  if (cnt < number_of_prefixes)
		    subst += "::";
		  if (current() == 'S')
		    decode_substitution(subst);
		  else if (current() == 'T')
		    decode_template_param(subst);
		  else
		    decode_unqualified_name(subst);
		}
	      }
	      break;
	    case unscoped_template_name:
	      decode_unscoped_name(subst);
	      break;
	  }
	  M_pos = saved_pos;
	  _GLIBCXX_DEMANGLER_DEBUG( dc::demangler.on() );
	  _GLIBCXX_DEMANGLER_DOUT(dc::demangler,
	      "Adding substitution " << substitution_name
	      << " : " << subst
	      << " (from " << location_ct((char*)__builtin_return_address(0)
		                          + builtin_return_address_offset)
	      << " <- " << location_ct((char*)__builtin_return_address(1)
		                       + builtin_return_address_offset)
	      << " <- " << location_ct((char*)__builtin_return_address(2)
		                       + builtin_return_address_offset)
	      << ").");
	  M_inside_add_substitution = false;
#endif
	}
      }

    // We don't want to depend on locale (or include <cctype> for that matter).
    // We also don't want to use "safe-ctype.h" because that headerfile is not
    // available to the users.
    inline bool isdigit(char c) { return c >= '0' && c <= '9'; }
    inline bool islower(char c) { return c >= 'a' && c <= 'z'; }
    inline bool isupper(char c) { return c >= 'A' && c <= 'Z'; }
    inline char tolower(char c) { return isupper(c) ? c - 'A' + 'a' : c; }

    //
    // <non-negative decimal integer> ::= 0
    //                                ::= 1|2|3|4|5|6|7|8|9 [<digit>+]
    // <digit>                        ::= 0|1|2|3|4|5|6|7|8|9
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::
	  decode_non_negative_decimal_integer(string_type& output)
      {
	char c = current();
	if (c == '0')
	{
	  output += '0';
	  eat_current();
	}
	else if (!isdigit(c))
	  M_result = false;
	else
	{
	  do
	  {
	    output += c;
	  }
	  while (isdigit((c = next())));
	}
	return M_result;
      }

    // <number> ::= [n] <non-negative decimal integer>
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_number(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_number");
	if (current() != 'n')
	  decode_non_negative_decimal_integer(output);
	else
	{
	  output += '-';
	  eat_current();
	  decode_non_negative_decimal_integer(output);
	}
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <builtin-type> ::= v  # void
    //                ::= w  # wchar_t
    //                ::= b  # bool
    //                ::= c  # char
    //                ::= a  # signed char
    //                ::= h  # unsigned char
    //                ::= s  # short
    //                ::= t  # unsigned short
    //                ::= i  # int
    //                ::= j  # unsigned int
    //                ::= l  # long
    //                ::= m  # unsigned long
    //                ::= x  # long long, __int64
    //                ::= y  # unsigned long long, __int64
    //                ::= n  # __int128
    //                ::= o  # unsigned __int128
    //                ::= f  # float
    //                ::= d  # double
    //                ::= e  # long double, __float80
    //                ::= g  # __float128
    //                ::= z  # ellipsis
    //                ::= u <source-name>    # vendor extended type
    //
    char const* const builtin_type_c[26] =
    {
      "signed char",	// a
      "bool",		// b
      "char",		// c
      "double",		// d
      "long double",	// e
      "float",		// f
      "__float128",		// g
      "unsigned char",	// h
      "int",		// i
      "unsigned int",	// j
      NULL,			// k
      "long",		// l
      "unsigned long",	// m
      "__int128",		// n
      "unsigned __int128",	// o
      NULL,			// p
      NULL,			// q
      NULL,			// r
      "short",		// s
      "unsigned short",	// t
      NULL,			// u
      "void",		// v
      "wchar_t",		// w
      "long long",		// x
      "unsigned long long",	// y
      "..."			// z
    };

    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_builtin_type(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_builtin_type");
	char const* bt;
	if (!islower(current()) || !(bt = builtin_type_c[current() - 'a']))
	  _GLIBCXX_DEMANGLER_FAILURE;
	output += bt;
	eat_current();
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <class-enum-type> ::= <name>
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_class_enum_type(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_class_enum_type");
	string_type nested_name_qualifiers;
	if (!decode_name(output, nested_name_qualifiers))
	  _GLIBCXX_DEMANGLER_FAILURE;
	output += nested_name_qualifiers;
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <substitution> ::=
    //   S <seq-id> _
    //   S_
    //   St # ::std::
    //   Sa # ::std::allocator
    //   Sb # ::std::basic_string
    //   Ss # ::std::basic_string<char, std::char_traits<char>,
    //                            std::allocator<char> >
    //   Si # ::std::basic_istream<char,  std::char_traits<char> >
    //   So # ::std::basic_ostream<char,  std::char_traits<char> >
    //   Sd # ::std::basic_iostream<char, std::char_traits<char> >
    //
    // <seq-id> ::=
    //   0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    //       [<seq-id>]	# Base 36 number
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_substitution(string_type& output,
	  qualifier_list<Tp, Allocator>* qualifiers)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_substitution");
	unsigned int value = 0;
	char c = next();
	if (c != '_')
	{
	  switch(c)
	  {
	    case 'a':
	    {
	      output += "std::allocator";
	      if (!M_inside_template_args)
	      {
		M_function_name = "allocator";
		M_name_is_template = true;
		M_name_is_cdtor = false;
		M_name_is_conversion_operator = false;
	      }
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCXX_DEMANGLER_RETURN;
	    }
	    case 'b':
	    {
	      output += "std::basic_string";
	      if (!M_inside_template_args)
	      {
		M_function_name = "basic_string";
		M_name_is_template = true;
		M_name_is_cdtor = false;
		M_name_is_conversion_operator = false;
	      }
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCXX_DEMANGLER_RETURN;
	    }
	    case 'd':
	      output += "std::iostream";
	      if (!M_inside_template_args)
	      {
		M_function_name = "iostream";
		M_name_is_template = true;
		M_name_is_cdtor = false;
		M_name_is_conversion_operator = false;
	      }
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCXX_DEMANGLER_RETURN;
	    case 'i':
	      output += "std::istream";
	      if (!M_inside_template_args)
	      {
		M_function_name = "istream";
		M_name_is_template = true;
		M_name_is_cdtor = false;
		M_name_is_conversion_operator = false;
	      }
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCXX_DEMANGLER_RETURN;
	    case 'o':
	      output += "std::ostream";
	      if (!M_inside_template_args)
	      {
		M_function_name = "ostream";
		M_name_is_template = true;
		M_name_is_cdtor = false;
		M_name_is_conversion_operator = false;
	      }
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCXX_DEMANGLER_RETURN;
	    case 's':
	      output += "std::string";
	      if (!M_inside_template_args)
	      {
		M_function_name = "string";
		M_name_is_template = true;
		M_name_is_cdtor = false;
		M_name_is_conversion_operator = false;
	      }
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCXX_DEMANGLER_RETURN;
	    case 't':
	      output += "std";
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCXX_DEMANGLER_RETURN;
	    default:
	      for(;; c = next())
	      {
		if (isdigit(c))
		  value = value * 36 + c - '0';
		else if (isupper(c))
		  value = value * 36 + c - 'A' + 10;
		else if (c == '_')
		  break;
		else
		  _GLIBCXX_DEMANGLER_FAILURE;
	      }
	      ++value;
	      break;
	  }
	}
	eat_current();
	if (value >= M_substitutions_pos.size() ||
	    M_inside_type > 20)			// Rather than core dump.
	  _GLIBCXX_DEMANGLER_FAILURE;
	++M_inside_substitution;
	int saved_pos = M_pos;
	substitution_st& substitution(M_substitutions_pos[value]);
	M_pos = substitution.M_start_pos;
	switch(substitution.M_type)
	{
	  case type:
	    decode_type(output, qualifiers);
	    break;
	  case template_template_param:
	    decode_template_param(output, qualifiers);
	    break;
	  case nested_name_prefix:
	  case nested_name_template_prefix:
	    for (int cnt = substitution.M_number_of_prefixes; cnt > 0; --cnt)
	    {
	      if (current() == 'I')
	      {
		if (M_template_args_need_space)
		  output += ' ';
		M_template_args_need_space = false;
		if (!decode_template_args(output))
		  _GLIBCXX_DEMANGLER_FAILURE;
	      }
	      else
	      {
		if (cnt < substitution.M_number_of_prefixes)
		  output += "::";
		if (current() == 'S')
		{
		  if (!decode_substitution(output))
		    _GLIBCXX_DEMANGLER_FAILURE;
		}
		else if (!decode_unqualified_name(output))
		  _GLIBCXX_DEMANGLER_FAILURE;
	      }
	    }
	    if (qualifiers)
	      qualifiers->printing_suppressed();
	    break;
	  case unscoped_template_name:
	    decode_unscoped_name(output);
	    if (qualifiers)
	      qualifiers->printing_suppressed();
	    break;
	}
	M_pos = saved_pos;
	--M_inside_substitution;
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <template-param> ::= T_			# first template parameter
    //                  ::= T <parameter-2 non-negative number> _
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_template_param(string_type& output,
	  qualifier_list<Tp, Allocator>* qualifiers)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_template_parameter");
	if (current() != 'T')
	  _GLIBCXX_DEMANGLER_FAILURE;
	unsigned int value = 0;
	char c;
	if ((c = next()) != '_')
	{
	  while(isdigit(c))
	  {
	    value = value * 10 + c - '0';
	    c = next();
	  }
	  ++value;
	}
	if (eat_current() != '_')
	  _GLIBCXX_DEMANGLER_FAILURE;
	value += M_template_arg_pos_offset;
	if (value >= M_template_arg_pos.size())
	  _GLIBCXX_DEMANGLER_FAILURE;
	int saved_pos = M_pos;
	M_pos = M_template_arg_pos[value];
	if (M_inside_type > 20)		// Rather than core dump.
	  _GLIBCXX_DEMANGLER_FAILURE;
	++M_inside_substitution;
	if (current() == 'X')
	{
	  eat_current();
	  decode_expression(output);
	}
	else if (current() == 'L')
	  decode_literal(output);
	else
	  decode_type(output, qualifiers);
	--M_inside_substitution;
	M_pos = saved_pos;
	_GLIBCXX_DEMANGLER_RETURN;
      }

    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_real(string_type& output, size_t size_of_real)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_real");

	unsigned long words[4];	// 32 bit per long, maximum of 128 bits.
	unsigned long* word = &words[0];

	int saved_pos;
	store(saved_pos);

	// The following assumes that leading zeroes are also included in the
	// mangled name, I am not sure that is conforming to the C++-ABI, but
	// it is what g++ does.
	unsigned char nibble, c = current();
	for(size_t word_cnt = size_of_real / 4; word_cnt > 0; --word_cnt)
	{
	  for (int nibble_cnt = 0; nibble_cnt < 8; ++nibble_cnt)
	  {
	    // Translate character into nibble.
	    if (c < '0' || c > 'f')
	      _GLIBCXX_DEMANGLER_FAILURE;
	    if (c <= '9')
	      nibble = c - '0';
	    else if (c >= 'a')
	      nibble = c - 'a' + 10;
	    else
	      _GLIBCXX_DEMANGLER_FAILURE;
	    // Write nibble into word array.
	    if (nibble_cnt == 0)
	      *word = nibble << 28;
	    else
	      *word |= (nibble << (28 - 4 * nibble_cnt));
	    c = next();
	  }
	  ++word;
	}
	char buf[24];
	if (M_implementation_details.decode_real(buf, words, size_of_real))
	{
	  output += buf;
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	restore(saved_pos);

	output += '[';
	c = current();
	for(size_t nibble_cnt = 0; nibble_cnt < 2 * size_of_real; ++nibble_cnt)
	{
	  if (c < '0' || c > 'f' || (c > '9' && c < 'a'))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  output += c;
	  c = next();
	}
	output += ']';

	_GLIBCXX_DEMANGLER_RETURN;
      }

    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_literal(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_literal");
	eat_current();	// Eat the 'L'.
	if (current() == '_')
	{
	  if (next() != 'Z')
	    _GLIBCXX_DEMANGLER_FAILURE;
	  eat_current();
	  if ((M_pos += decode_encoding(output, M_str + M_pos,
		  M_maxpos - M_pos + 1, M_implementation_details)) < 0)
	    _GLIBCXX_DEMANGLER_FAILURE;
	}
	else
	{
	  // Special cases
	  if (current() == 'b')
	  {
	    if (next() == '0')
	      output += "false";
	    else
	      output += "true";
	    eat_current();
	    _GLIBCXX_DEMANGLER_RETURN;
	  }
	  char c = current();
	  if ((c == 'i' || c == 'j' || c == 'l' ||
	       c == 'm' || c == 'x' || c == 'y') &&
              M_implementation_details.get_style_literal())
	    eat_current();
	  else if (c == 'i' &&
	      !M_implementation_details.get_style_literal_int())
	    eat_current();
	  else
	  {
	    output += '(';
	    if (!decode_type(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    output += ')';
	  }
	  if (c >= 'd' && c <= 'g')
	  {
	    size_t size_of_real = (c == 'd') ? sizeof(double) :
	        ((c == 'f') ? sizeof(float) :
		(c == 'e') ?  sizeof(long double) : 16);
	    if (!decode_real(output, size_of_real))
		_GLIBCXX_DEMANGLER_FAILURE;
	  }
	  else if (!decode_number(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
          if (M_implementation_details.get_style_literal())
	  {
	    if (c == 'j' || c == 'm' || c == 'y')
	      output += 'u';
	    if (c == 'l' || c == 'm')
	      output += 'l';
	    if (c == 'x' || c == 'y')
	      output += "ll";
	  }
	}
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <operator-name> ::=
    //   nw				# new
    //   na				# new[]
    //   dl				# delete
    //   da				# delete[]
    //   ps				# + (unary)
    //   ng				# - (unary)
    //   ad				# & (unary)
    //   de				# * (unary)
    //   co				# ~
    //   pl				# +
    //   mi				# -
    //   ml				# *
    //   dv				# /
    //   rm				# %
    //   an				# &
    //   or				# |
    //   eo				# ^
    //   aS				# =
    //   pL				# +=
    //   mI				# -=
    //   mL				# *=
    //   dV				# /=
    //   rM				# %=
    //   aN				# &=
    //   oR				# |=
    //   eO				# ^=
    //   ls				# <<
    //   rs				# >>
    //   lS				# <<=
    //   rS				# >>=
    //   eq				# ==
    //   ne				# !=
    //   lt				# <
    //   gt				# >
    //   le				# <=
    //   ge				# >=
    //   nt				# !
    //   aa				# &&
    //   oo				# ||
    //   pp				# ++
    //   mm				# --
    //   cm				# ,
    //   pm				# ->*
    //   pt				# ->
    //   cl				# ()
    //   ix				# []
    //   qu				# ?
    //   st				# sizeof (a type)
    //   sz				# sizeof (an expression)
    //   cv <type>			# (cast)
    //   v <digit> <source-name>	# vendor extended operator
    //
    // Symbol operator codes exist of two characters, we need to find a
    // quick hash so that their names can be looked up in a table.
    //
    // The puzzle :)
    // Shift the rows so that there is at most one character per column.
    //
    // A perfect solution (Oh no, it's THE MATRIX!):
    //                                              horizontal
    //    .......................................   offset + 'a'
    // a, a||d|||||||||n||||s||||||||||||||||||||       0
    // c,  || |||||||lm o||| ||||||||||||||||||||       0
    // d,  || a|||e||    l|| ||||||v|||||||||||||       4
    // e,  ||  ||| ||     || |||o|q |||||||||||||       8
    // g,  ||  ||| ||     || e|| |  ||||||||t||||      15
    // i,  ||  ||| ||     ||  || |  |||||||| |||x      15
    // l,  |e  ||| ||     st  || |  |||||||| |||       -2
    // m,  |   |i| lm         || |  |||||||| |||       -2
    // n,  a   e g            t| w  |||||||| |||        1
    // o,                      |    ||||o||r |||       16
    // p,                      |    ||lm |p  st|       17
    // q,                      |    u|   |     |        6
    // r,                      m     s   |     |        9
    // s,                                t     z       12
    //    .......................................
    // ^            ^__ second character
    // |___ first character
    //

    // Putting that solution in tables:

    char const offset_table_c [1 + CHAR_MAX - CHAR_MIN ] =
    {
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
#if (CHAR_MIN < 0)
      // Add -CHAR_MIN extra zeroes (128):
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      //   a    b    c    d    e    f    g    h    i    j    k
      0, -97,   0, -97, -93, -89,   0, -82,   0, -82,   0,   0,
      //   l    m    n    o    p    q    r    s    t    u    v
	 -99, -99, -96, -81, -80, -91, -88, -85,   0,   0,   0,
#else
      //   a    b    c    d    e    f    g    h    i    j    k
      0, 159,   0, 159, 163, 167,   0, 174,   0, 174,   0,   0,
      //   l    m    n    o    p    q    r    s    t    u    v
	 157, 157, 160, 175, 176, 165, 168, 171,   0,   0,   0,
#endif
      // ... more zeros
    };

    enum xary_nt {
      unary,
      binary,
      trinary
    };

    struct entry_st
    {
      char const* opcode;
      char const* symbol_name;
      xary_nt type;
    };

    entry_st const symbol_name_table_c[39] = {
      { "aa",  "operator&&", binary },
      { "na",  "operator new[]", unary },
      { "le",  "operator<=", binary },
      { "ad",  "operator&", unary },
      { "da",  "operator delete[]", unary },
      { "ne",  "operator!=", binary },
      { "mi=", "operator-", binary },
      { "ng",  "operator-", unary },
      { "de",  "operator*", unary },
      { "ml=", "operator*", binary },
      { "mm",  "operator--", unary },
      { "cl",  "operator()", unary },
      { "cm",  "operator,", binary },
      { "an=", "operator&", binary },
      { "co",  "operator~", binary },
      { "dl",  "operator delete", unary },
      { "ls=", "operator<<", binary },
      { "lt",  "operator<", binary },
      { "as=", "operator", binary },
      { "ge",  "operator>=", binary },
      { "nt",  "operator!", unary },
      { "rm=", "operator%", binary },
      { "eo=", "operator^", binary },
      { "nw",  "operator new", unary },
      { "eq",  "operator==", binary },
      { "dv=", "operator/", binary },
      { "qu",  "operator?", trinary },
      { "rs=", "operator>>", binary },
      { "pl=", "operator+", binary },
      { "pm",  "operator->*", binary },
      { "oo",  "operator||", binary },
      { "st",  "sizeof", unary },
      { "pp",  "operator++", unary },
      { "or=", "operator|", binary },
      { "gt",  "operator>", binary },
      { "ps",  "operator+", unary },
      { "pt",  "operator->", binary },
      { "sz",  "sizeof", unary },
      { "ix",  "operator[]", unary }
    };

    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_operator_name(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_operator_name");

	char opcode0 = current();
	char opcode1 = tolower(next());

	register char hash;
	if ((hash = offset_table_c[opcode0 - CHAR_MIN]))
	{
	  hash += opcode1;
	  if (
#if (CHAR_MIN < 0)
	      hash >= 0 &&
#endif
	      hash < 39)
	  {
	    int index = static_cast<int>(static_cast<unsigned char>(hash));
	    entry_st entry = symbol_name_table_c[index];
	    if (entry.opcode[0] == opcode0 && entry.opcode[1] == opcode1
		&& (opcode1 == current() || entry.opcode[2] == '='))
	    {
	      output += entry.symbol_name;
	      if (opcode1 != current())
		output += '=';
	      eat_current();
	      if (hash == 16 || hash == 17)
		M_template_args_need_space = true;
	      _GLIBCXX_DEMANGLER_RETURN;
	    }
	    else if (opcode0 == 'c' && opcode1 == 'v')	// casting operator
	    {
	      eat_current();
	      output += "operator ";
	      if (current() == 'T')
	      {
		// This is a templated cast operator.
		// It must be of the form "cvT_I...E".
		// Let M_template_arg_pos already point
		// to the template argument.
		M_template_arg_pos_offset = M_template_arg_pos.size();
		M_template_arg_pos.push_back(M_pos + 3);
	      }
	      if (!decode_type(output))
		_GLIBCXX_DEMANGLER_FAILURE;
	      if (!M_inside_template_args)
		M_name_is_conversion_operator = true;
	      _GLIBCXX_DEMANGLER_RETURN;
	    }
	  }
	}
	_GLIBCXX_DEMANGLER_FAILURE;
      }

    //
    // <expression> ::= <unary operator-name> <expression>
    //              ::= <binary operator-name> <expression> <expression>
    //              ::= <trinary operator-name> <expression> <expression> <expression>
    //              ::= st <type>
    //              ::= <template-param>
    //              ::= sr <type> <unqualified-name>                   # dependent name
    //              ::= sr <type> <unqualified-name> <template-args>   # dependent template-id
    //              ::= <expr-primary>
    //
    // <expr-primary> ::= L <type> <value number> E     # integer literal
    //                ::= L <type> <value float> E	# floating literal
    //                ::= L <mangled-name> E		# external name
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_expression(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_expression");
	if (current() == 'T')
	{
	  if (!decode_template_param(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	else if (current() == 'L')
	{
	  if (!decode_literal(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  if (current() != 'E')
	    _GLIBCXX_DEMANGLER_FAILURE;
	  eat_current();
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	else if (current() == 's')
	{
	  char opcode1 = next();
	  if (opcode1 == 't' || opcode1 == 'z')
	  {
	    eat_current();
	    if (M_implementation_details.get_style_compact_expr_ops())
	      output += "sizeof(";
	    else
	      output += "sizeof (";
	    if (opcode1 == 't')
	    {
	      // I cannot think of a mangled name that is valid for both cases
	      // when just replacing the 't' by a 'z' or vica versa, which
	      // indicates that there is no ambiguity that dictates the need
	      // for a seperate "st" case, except to be able catch invalid
	      // mangled names.  However there CAN be ambiguity in the demangled
	      // name when there are both a type and a symbol of the same name,
	      // which then leads to different encoding (of course) with
	      // sizeof (type) or sizeof (expression) respectively, but that
	      // ambiguity is not per se related to "sizeof" except that that
	      // is the only place where both a type AND an expression are valid
	      // in as part of a (template function) type.
	      //
	      // Example:
	      //
	      // struct B { typedef int t; };
	      // struct A : public B { static int t[2]; };
	      // template<int i, int j> struct C { typedef int q; };
	      // template<int i, typename T>
	      //   void f(typename C<sizeof (typename T::t),
	      //                     sizeof (T::t)>::q) { }
	      // void instantiate() { f<5, A>(0); }
	      //
	      // Leads to _Z1fILi5E1AEvN1CIXstN1T1tEEXszsrS2_1tEE1qE which
	      // demangles as
	      // void f<5, A>(C<sizeof (T::t), sizeof (T::t)>::q)
	      //
	      // This is ambiguity is very unlikely to happen and it is kind
	      // of fuzzy to detect when adding a 'typename' makes sense.
	      //
	      if (M_implementation_details.get_style_sizeof_typename())
	      {
		// We can only get here inside a template parameter,
		// so this is syntactically correct if the given type is
		// a typedef.  The only disadvantage is that it is inconsistent
		// with all other places where the 'typename' keyword should be
		// used and we don't.
		// With this, the above example will demangle as
		// void f<5, A>(C<sizeof (typename T::t), sizeof (T::t)>::q)
		if (current() == 'N' ||	// <nested-name>
					  // This should be a safe bet.
		    (current() == 'S' &&
		     next_peek() == 't'))	// std::something, guess that
					  // this involves a typedef.
		  output += "typename ";
	      }
	      if (!decode_type(output))
		_GLIBCXX_DEMANGLER_FAILURE;
	    }
	    else
	    {
	      if (!decode_expression(output))
		_GLIBCXX_DEMANGLER_FAILURE;
	    }
	    output += ')';
	    _GLIBCXX_DEMANGLER_RETURN;
	  }
	  else if (current() == 'r')
	  {
	    eat_current();
	    if (!decode_type(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    output += "::";
	    if (!decode_unqualified_name(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    if (current() != 'I' || decode_template_args(output))
	      _GLIBCXX_DEMANGLER_RETURN;
	  }
	}
	else
	{
	  char opcode0 = current();
	  char opcode1 = tolower(next());

	  register char hash;
	  if ((hash = offset_table_c[opcode0 - CHAR_MIN]))
	  {
	    hash += opcode1;
	    if (
#if (CHAR_MIN < 0)
		hash >= 0 &&
#endif
		hash < 39)
	    {
	      int index = static_cast<int>(static_cast<unsigned char>(hash));
	      entry_st entry = symbol_name_table_c[index];
	      if (entry.opcode[0] == opcode0 && entry.opcode[1] == opcode1
		  && (opcode1 == current() || entry.opcode[2] == '='))
	      {
		char const* op = entry.symbol_name + 8;	// Skip "operator".
		if (*op == ' ')				// operator new and delete.
		  ++op;
		if (entry.type == unary)
		  output += op;
		bool is_eq = (opcode1 != current());
		eat_current();
		if (index == 34 && M_inside_template_args)	// operator>
		  output += '(';
		output += '(';
		if (!decode_expression(output))
		  _GLIBCXX_DEMANGLER_FAILURE;
		output += ')';
		if (entry.type != unary)
		{
	          if (!M_implementation_details.get_style_compact_expr_ops())
		    output += ' ';
		  output += op;
		  if (is_eq)
		    output += '=';
	          if (!M_implementation_details.get_style_compact_expr_ops())
		    output += ' ';
		  output += '(';
		  if (!decode_expression(output))
		    _GLIBCXX_DEMANGLER_FAILURE;
		  output += ')';
		  if (index == 34 && M_inside_template_args)
		    output += ')';
		  if (entry.type == trinary)
		  {
		    if (M_implementation_details.get_style_compact_expr_ops())
		      output += ":(";
		    else
		      output += " : (";
		    if (!decode_expression(output))
		      _GLIBCXX_DEMANGLER_FAILURE;
		    output += ')';
		  }
		}
		_GLIBCXX_DEMANGLER_RETURN;
	      }
	      else if (opcode0 == 'c' &&
	               opcode1 == 'v')		// casting operator.
	      {
		eat_current();
		output += '(';
		if (!decode_type(output))
		  _GLIBCXX_DEMANGLER_FAILURE;
		output += ")(";
		if (!decode_expression(output))
		  _GLIBCXX_DEMANGLER_FAILURE;
		output += ')';
		_GLIBCXX_DEMANGLER_RETURN;
	      }
	    }
	  }
	}
	_GLIBCXX_DEMANGLER_FAILURE;
      }

    //
    // <template-args> ::= I <template-arg>+ E
    // <template-arg> ::= <type>			# type or template
    //                ::= L <type> <value number> E	# integer literal
    //                ::= L <type> <value float> E	# floating literal
    //                ::= L <mangled-name> E		# external name
    //                ::= X <expression> E		# expression
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_template_args(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_template_args");
	if (eat_current() != 'I')
	  _GLIBCXX_DEMANGLER_FAILURE;
	int prev_size = M_template_arg_pos.size();
	++M_inside_template_args;
	if (M_template_args_need_space)
	{
	  output += ' ';
	  M_template_args_need_space = false;
	}
	output += '<';
	for(;;)
	{
	  if (M_inside_template_args == 1 && !M_inside_type)
	    M_template_arg_pos.push_back(M_pos);
	  if (current() == 'X')
	  {
	    eat_current();
	    if (!decode_expression(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    if (current() != 'E')
	      _GLIBCXX_DEMANGLER_FAILURE;
	    eat_current();
	  }
	  else if (current() == 'L')
	  {
	    if (!decode_literal(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    if (current() != 'E')
	      _GLIBCXX_DEMANGLER_FAILURE;
	    eat_current();
	  }
	  else if (!decode_type(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  if (current() == 'E')
	    break;
	  output += ", ";
	}
	eat_current();
	if (*(output.rbegin()) == '>')
	  output += ' ';
	output += '>';
	--M_inside_template_args;
	if (!M_inside_template_args && !M_inside_type)
	{
	  M_name_is_template = true;
	  M_template_arg_pos_offset = prev_size;
	}
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <bare-function-type> ::=
    //   <signature type>+		# Types are parameter types.
    //
    // Note that the possible return type of the <bare-function-type>
    // has already been eaten before we call this function.  This makes
    // our <bare-function-type> slightly different from the one in
    // the C++-ABI description.
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_bare_function_type(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_bare_function_type");
	if (M_saw_destructor)
	{
	  if (eat_current() != 'v' || (current() != 'E' && current() != 0))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  output += "()";
	  M_saw_destructor = false;
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	if (current() == 'v' && !M_implementation_details.get_style_void())
	{
	  eat_current();
	  if (current() != 'E' && current() != 0)
	    _GLIBCXX_DEMANGLER_FAILURE;
	  output += "()";
	  M_saw_destructor = false;
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	output += '(';
	M_template_args_need_space = false;
	if (!decode_type(output))	// Must have at least one parameter.
	  _GLIBCXX_DEMANGLER_FAILURE;
	while (current() != 'E' && current() != 0)
	{
	  output += ", ";
	  if (!decode_type(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
	}
	output += ')';
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <type> ::=
    //   <builtin-type>		# Starts with a lower case character != r.
    //   <function-type>	# Starts with F
    //   <class-enum-type>	# Starts with N, S, C, D, Z, a digit or a lower
    //				# case character.  Since a lower case character
    //				# would be an operator name, that would be an
    //				# error.  The S is a substitution or St
    //				# (::std::).  A 'C' would be a constructor and
    //				# thus also an error.
    //   <template-param>	# Starts with T
    //   <substitution>         # Starts with S
    //   <template-template-param> <template-args>  # Starts with T or S,
    //						    # equivalent with the above.
    //
    //   <array-type>			# Starts with A
    //   <pointer-to-member-type>	# Starts with M
    //   <CV-qualifiers> <type>		# Starts with r, V or K
    //   P <type>   # pointer-to	# Starts with P
    //   R <type>   # reference-to	# Starts with R
    //   C <type>   # complex (C 2000)	# Starts with C
    //   G <type>   # imaginary (C 2000)# Starts with G
    //   U <source-name> <type>		# vendor extended type qualifier,
    //					# starts with U
    //
    // <template-template-param> ::= <template-param>
    //                           ::= <substitution>

    // My own analysis of how to decode qualifiers:
    //
    // F is a <function-type>, <T> is a <builtin-type>, <class-enum-type>,
    //   <template-param> or <template-template-param> <template-args>.
    // <Q> represents a series of qualifiers (not G or C).
    // <C> is an unqualified type.
    // <R> is a qualified type.
    // <B> is the bare-function-type without return type.
    // <I> is the array index.
    //						Substitutions:
    // <Q>M<C><Q2>F<R><B>E  ==> R (C::*Q)B Q2	"<C>", "F<R><B>E"
    //						    (<R> and <B> recursive),
    //						    "M<C><Q2>F<R><B>E".
    // <Q>F<R><B>E	    ==> R (Q)B		"<R>", "<B>" (<B> recursive)
    //                                              and "F<R><B>E".
    //
    // Note that if <R> has postfix qualifiers (an array or function), then
    // those are added AFTER the (member) function type.  For example:
    // <Q>FPA<R><B>E ==> R (*(Q)B) [], where the PA added the prefix
    // "(*" and the postfix ") []".
    //
    // <Q>G<T>		    ==> imaginary T Q	"<T>", "G<T>" (<T> recursive).
    // <Q>C<T>		    ==> complex T Q	"<T>", "C<T>" (<T> recursive).
    // <Q><T>		    ==> T Q		"<T>" (<T> recursive).
    //
    // where <Q> is any of:
    //
    // <Q>P		==> *Q				"P..."
    // <Q>R		==> &Q				"R..."
    // <Q>[K|V|r]+	==> [ const| volatile| restrict]+Q	"KVr..."
    // <Q>U<S>		==>  SQ				"U<S>..."
    // <Q>M<C>		==> C::*Q			"M<C>..." (<C> recurs.)
    // A<I>		==>  [I]			"A<I>..." (<I> recurs.)
    // <Q>A<I>		==>  (Q) [I]			"A<I>..." (<I> recurs.)
    //   Note that when <Q> ends on an A<I2> then the brackets are omitted
    //   and no space is written between the two:
    //   A<I2>A<I>	==>  [I2][I]
    //   If <Q> ends on [KVr]+, which can happen in combination with
    //   substitutions only, then special handling is required, see below.
    //
    // A <substitution> is handled with an input position switch during which
    // new substitutions are turned off.  Because recursive handling of types
    // (and therefore the order in which substitutions must be generated) must
    // be done left to right, but the generation of Q needs processing right to
    // left, substitutions per <type> are generated by reading the input left
    // to right and marking the starts of all substitutions only - implicitly
    // finishing them at the end of the type.  Then the output and real
    // substitutions are generated.
    //
    // The following comment was for the demangling of g++ version 3.0.x.  The
    // mangling (and I believe even the ABI description) have been fixed now
    // (as of g++ version 3.1).
    //
    // g++ 3.0.x only:
    // The ABI specifies for pointer-to-member function types the format
    // <Q>M<T>F<R><B>E.  In other words, the qualifier <Q2> (see above) is
    // implicitely contained in <T> instead of explicitly part of the M format.
    // I am convinced that this is a bug in the ABI.  Unfortunately, this is
    // how we have to demangle things as it has a direct impact on the order
    // in which substitutions are stored.  This ill-formed design results in
    // rather ill-formed demangler code too however :/
    //
    // <Q2> is now explicitely part of the M format.
    // For some weird reason, g++ (3.2.1) does not add substitutions for
    // qualified member function pointers.  I think that is another bug.
    //

    // In the case of
    // <Q>A<I>
    // where <Q> ends on [K|V|r]+ then that part should be processed as
    // if it was behind the A<I> instead of in front of it.  This is
    // because a constant array of ints is normally always mangled as
    // an array of constant ints.  KVr qualifiers can end up in front
    // of an array when the array is part of a substitution or template
    // parameter, but the demangling should still result in the same
    // syntax; thus KA2_i (const array of ints) must result in the same
    // demangling as A2_Ki (array of const ints).  As a result we must
    // demangle ...[...[[KVr]+A<I0>][KVr]+A<I1>]...[KVr]+A<In>[KVr]+
    // as A<I0>A<I1>...A<In>[KVr]+ where each K, V and r in the series
    // collapses to a single character at the right of the string.
    // For example:
    // VA9_KrA6_KVi --> A9_A6_KVri --> int volatile const restrict [9][6]
    // Note that substitutions are still added as usual (the translation
    // to A9_A6_KVri does not really happen).
    //
    // This decoding is achieved by delaying the decoding of any sequence
    // of [KVrA]'s and processing them together in the order: first the
    // short-circuited KVr part and then the arrays.
    static int const cvq_K = 1;		// Saw at least one K
    static int const cvq_V = 2;		// Saw at least one V
    static int const cvq_r = 4;		// Saw at least one r
    static int const cvq_A = 8;		// Saw at least one A
    static int const cvq_last = 16;	// No remaining qualifiers.
    static int const cvq_A_cnt = 32;	// Bit 5 and higher represent the
					//   number of A's in the series.
    // In the function below, iter_array points to the first (right most)
    // A in the series, if any.
    template<typename Tp, typename Allocator>
      void
      qualifier_list<Tp, Allocator>::decode_KVrA(
          string_type& prefix, string_type& postfix, int cvq,
          typename qual_vector::const_reverse_iterator const& iter_array) const
	{
	  _GLIBCXX_DEMANGLER_DOUT_ENTERING3("decode_KVrA");
	  if ((cvq & cvq_K))
	    prefix += " const";
	  if ((cvq & cvq_V))
	    prefix += " volatile";
	  if ((cvq & cvq_r))
	    prefix += " restrict";
	  if ((cvq & cvq_A))
	  {
	    int n = cvq >> 5;
	    for (typename qual_vector::
	        const_reverse_iterator iter = iter_array;
		iter != M_qualifier_starts.rend(); ++iter)
	    {
	      switch((*iter).first_qualifier())
	      {
		case 'K':
		case 'V':
		case 'r':
		  break;
		case 'A':
		{
		  string_type index = (*iter).get_optional_type();
		  if (--n == 0 && (cvq & cvq_last))
		    postfix = " [" + index + "]" + postfix;
		  else if (n > 0)
		    postfix = "[" + index + "]" + postfix;
		  else
		  {
		    prefix += " (";
		    postfix = ") [" + index + "]" + postfix;
		  }
		  break;
		}
		default:
		  _GLIBCXX_DEMANGLER_RETURN3;
	      }
	    }
	  }
	  _GLIBCXX_DEMANGLER_RETURN3;
	}

    template<typename Tp, typename Allocator>
      void
      qualifier_list<Tp, Allocator>::decode_qualifiers(
	  string_type& prefix,
	  string_type& postfix,
	  bool member_function_pointer_qualifiers = false) const
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING3("decode_qualifiers");
	int cvq = 0;
	typename qual_vector::const_reverse_iterator iter_array;
	for(typename qual_vector::
	    const_reverse_iterator iter = M_qualifier_starts.rbegin();
	    iter != M_qualifier_starts.rend(); ++iter)
	{
	  if (!member_function_pointer_qualifiers
	      && !(*iter).part_of_substitution())
	  {
	    int saved_inside_substitution = M_demangler.M_inside_substitution;
	    M_demangler.M_inside_substitution = 0;
	    M_demangler.add_substitution((*iter).get_start_pos(), type);
	    M_demangler.M_inside_substitution = saved_inside_substitution;
	  }
	  char qualifier_char = (*iter).first_qualifier();
	  for(; qualifier_char; qualifier_char = (*iter).next_qualifier())
	  {
	    switch(qualifier_char)
	    {
	      case 'P':
		if (cvq)
		{
		  decode_KVrA(prefix, postfix, cvq, iter_array);
		  cvq = 0;
		}
		prefix += "*";
		break;
	      case 'R':
		if (cvq)
		{
		  decode_KVrA(prefix, postfix, cvq, iter_array);
		  cvq = 0;
		}
		prefix += "&";
		break;
	      case 'K':
		cvq |= cvq_K;
		continue;
	      case 'V':
		cvq |= cvq_V;
		continue;
	      case 'r':
		cvq |= cvq_r;
		continue;
	      case 'A':
	        if (!(cvq & cvq_A))
		{
		  cvq |= cvq_A;
		  iter_array = iter;
		}
		cvq += cvq_A_cnt;
		break;
	      case 'M':
	        if (cvq)
		{
		  decode_KVrA(prefix, postfix, cvq, iter_array);
		  cvq = 0;
		}
		prefix += " ";
		prefix += (*iter).get_optional_type();
		prefix += "::*";
		break;
	      case 'U':
	        if (cvq)
		{
		  decode_KVrA(prefix, postfix, cvq, iter_array);
		  cvq = 0;
		}
		prefix += " ";
		prefix += (*iter).get_optional_type();
		break;
	      case 'G':	// Only here so we added a substitution.
		break;
	    }
	    break;
	  }
	}
	if (cvq)
	  decode_KVrA(prefix, postfix, cvq|cvq_last, iter_array);
	M_printing_suppressed = false;
	_GLIBCXX_DEMANGLER_RETURN3;
      }

    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_type_with_postfix(
	  string_type& prefix, string_type& postfix,
	  qualifier_list<Tp, Allocator>* qualifiers)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING2("decode_type");
	++M_inside_type;
	bool recursive_template_param_or_substitution_call;
	if (!(recursive_template_param_or_substitution_call = qualifiers))
	{
          qualifier_list<Allocator>* raw_qualifiers = M_qualifier_list_alloc.allocate(1);
	  qualifiers = new (raw_qualifiers) qualifier_list<Allocator>(*this);
	}
	// First eat all qualifiers.
	bool failure = false;
	for(;;)		// So we can use 'continue' to eat the next qualifier.
	{
	  int start_pos = M_pos;
	  switch(current())
	  {
	    case 'P':
	      qualifiers->add_qualifier_start(pointer, start_pos,
		  M_inside_substitution);
	      eat_current();
	      continue;
	    case 'R':
	      qualifiers->add_qualifier_start(reference, start_pos,
		  M_inside_substitution);
	      eat_current();
	      continue;
	    case 'K':
	    case 'V':
	    case 'r':
	    {
	      char c;
	      int count = 0;
	      do
	      {
		++count;
		c = next();
	      }
	      while(c == 'K' || c == 'V' || c == 'r');
	      qualifiers->add_qualifier_start(cv_qualifier, start_pos, count,
		  M_inside_substitution);
	      continue;
	    }
	    case 'U':
	    {
	      eat_current();
	      string_type source_name;
	      if (!decode_source_name(source_name))
	      {
		failure = true;
		break;
	      }
	      qualifiers->add_qualifier_start(vendor_extension, start_pos,
		  source_name, M_inside_substitution);
	      continue;
	    }
	    case 'A':
	    {
	      // <array-type> ::= A <positive dimension number> _ <element type>
	      //              ::= A [<dimension expression>] _ <element type>
	      //
	      string_type index;
	      int saved_pos;
	      store(saved_pos);
	      if (next() == 'n' || !decode_number(index))
	      {
		restore(saved_pos);
		if (next() != '_' && !decode_expression(index))
		{
		  failure = true;
		  break;
		}
	      }
	      if (eat_current() != '_')
	      {
		failure = true;
		break;
	      }
	      qualifiers->add_qualifier_start(array, start_pos, index,
		  M_inside_substitution);
	      continue;
	    }
	    case 'M':
	    {
	      // <pointer-to-member-type> ::= M <class type> <member type>
	      // <Q>M<C> or <Q>M<C><Q2>F<R><B>E
	      eat_current();
	      string_type class_type;
	      if (!decode_type(class_type))		// Substitution: "<C>".
	      {
		failure = true;
		break;
	      }
	      char c = current();
	      if (c == 'F' || c == 'K' || c == 'V' || c == 'r')
		  // Must be CV-qualifiers and a member function pointer.
	      {
		// <Q>M<C><Q2>F<R><B>E	==> R (C::*Q)B Q2
		//     substitutions: "<C>", "F<R><B>E" (<R> and <B>
		//                    recursive), "M<C><Q2>F<R><B>E".
		int count = 0;
		int Q2_start_pos = M_pos;
		while(c == 'K' || c == 'V' || c == 'r')		// Decode <Q2>.
		{
		  ++count;
		  c = next();
		}
		qualifier_list<Tp, Allocator> class_type_qualifiers(*this);
		if (count)
		  class_type_qualifiers.
		      add_qualifier_start(cv_qualifier, Q2_start_pos,
			  count, M_inside_substitution);
		string_type member_function_qualifiers;
		// It is unclear why g++ doesn't add a substitution for
		// "<Q2>F<R><B>E" as it should I think.
		string_type member_function_qualifiers_postfix;
		class_type_qualifiers.
		    decode_qualifiers(member_function_qualifiers,
			member_function_qualifiers_postfix, true);
		member_function_qualifiers +=
		    member_function_qualifiers_postfix;
		// I don't think this substitution is actually ever used.
		int function_pos = M_pos;
		if (eat_current() != 'F')
		{
		  failure = true;
		  break;
		}
		// Return type.
		// Constructors, destructors and conversion operators don't
		// have a return type, but seem to never get here.
		string_type return_type_postfix;
		if (!decode_type_with_postfix(prefix, return_type_postfix))
		    // substitution: <R> recursive
		{
		  failure = true;
		  break;
		}
		prefix += " (";
		prefix += class_type;
		prefix += "::*";
		string_type bare_function_type;
		if (!decode_bare_function_type(bare_function_type)
		    || eat_current() != 'E')	// Substitution: <B> recursive.
		{
		  failure = true;
		  break;
		}
		// substitution: "F<R><B>E".
		add_substitution(function_pos, type);
		// substitution: "M<C><Q2>F<R><B>E".
		add_substitution(start_pos, type);
		// substitution: all qualified types if any.
		qualifiers->decode_qualifiers(prefix, postfix);
		postfix += ")";
		postfix += bare_function_type;
		postfix += member_function_qualifiers;
		postfix += return_type_postfix;
		goto decode_type_exit;
	      }
	      qualifiers->add_qualifier_start(pointer_to_member, start_pos,
		  class_type, M_inside_substitution);
	      continue;
	    }
	    default:
	      break;
	  }
	  break;
	}
	if (!failure)
	{
	  // <Q>G<T>			==> imaginary T Q
	  //     substitutions: "<T>", "G<T>" (<T> recursive).
	  // <Q>C<T>			==> complex T Q
	  //     substitutions: "<T>", "C<T>" (<T> recursive).
	  if (current() == 'C' || current() == 'G')
	  {
	    prefix += current() == 'C' ? "complex " : "imaginary ";
	    qualifiers->add_qualifier_start(complex_or_imaginary, M_pos,
		M_inside_substitution);
	    eat_current();
	  }
	  int start_pos = M_pos;
	  switch(current())
	  {
	    case 'F':
	    {
	      // <function-type> ::= F [Y] <bare-function-type> E
	      //
	      // Note that g++ never generates the 'Y', but we try to
	      // demangle it anyway.
	      bool extern_C = (next() == 'Y');
	      if (extern_C)
		eat_current();

	      // <Q>F<R><B>E		==> R (Q)B
	      //     substitution: "<R>", "<B>" (<B> recursive) and "F<R><B>E".

	      // Return type.
	      string_type return_type_postfix;
	      if (!decode_type_with_postfix(prefix, return_type_postfix))
		  // Substitution: "<R>".
	      {
		failure = true;
		break;
	      }
	      // Only array and function (pointer) types have a postfix.
	      // In that case we don't want the space but expect something
	      // like prefix is "int (*" and postfix is ") [1]".
	      // We do want the space if this pointer is qualified.
	      if (return_type_postfix.size() == 0 ||
	          (prefix.size() > 0 && *prefix.rbegin() != '*'))
		prefix += ' ';
	      prefix += '(';
	      string_type bare_function_type;
	      if (!decode_bare_function_type(bare_function_type)
		  // substitution: "<B>" (<B> recursive).
		  || eat_current() != 'E')
	      {
		failure = true;
		break;
	      }
	      add_substitution(start_pos, type);  // Substitution: "F<R><B>E".
	      qualifiers->decode_qualifiers(prefix, postfix);
		  // substitution: all qualified types, if any.
	      postfix += ")";
	      if (extern_C)
	        postfix += " [extern \"C\"] ";
	      postfix += bare_function_type;
	      postfix += return_type_postfix;
	      break;
	    }
	    case 'T':
	      if (!decode_template_param(prefix, qualifiers))
	      {
		failure = true;
		break;
	      }
	      if (current() == 'I')
	      {
		add_substitution(start_pos, template_template_param);
		    // substitution: "<template-template-param>".
		if (!decode_template_args(prefix))
		{
		  failure = true;
		  break;
		}
	      }
	      if (!recursive_template_param_or_substitution_call
		  && qualifiers->suppressed())
	      {
		add_substitution(start_pos, type);
		    // substitution: "<template-param>" or
		    // "<template-template-param> <template-args>".
		qualifiers->decode_qualifiers(prefix, postfix);
		    // substitution: all qualified types, if any.
	      }
	      break;
	    case 'S':
	      if (M_pos >= M_maxpos)
	      {
		failure = true;
		break;
	      }
	      if (M_str[M_pos + 1] != 't')
	      {
		if (!decode_substitution(prefix, qualifiers))
		{
		  failure = true;
		  break;
		}
		if (current() == 'I')
		{
		  if (!decode_template_args(prefix))
		  {
		    failure = true;
		    break;
		  }
		  if (!recursive_template_param_or_substitution_call
		      && qualifiers->suppressed())
		    add_substitution(start_pos, type);
			// Substitution:
			//   "<template-template-param> <template-args>".
		}
		if (!recursive_template_param_or_substitution_call
		    && qualifiers->suppressed())
		  qualifiers->decode_qualifiers(prefix, postfix);
		      // Substitution: all qualified types, if any.
		break;
	      }
	      /* Fall-through for St */
	    case 'N':
	    case 'Z':
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	      // <Q><T>			==> T Q
	      //     substitutions: "<T>" (<T> recursive).
	      if (!decode_class_enum_type(prefix))
	      {
		failure = true;
		break;
	      }
	      if (!recursive_template_param_or_substitution_call)
	      {
		add_substitution(start_pos, type);
		    // substitution: "<class-enum-type>".
		qualifiers->decode_qualifiers(prefix, postfix);
		    // substitution: all qualified types, if any.
	      }
	      else
		qualifiers->printing_suppressed();
	      break;
	    default:
	      // <Q><T>			==> T Q
	      //     substitutions: "<T>" (<T> recursive).
	      if (!decode_builtin_type(prefix))
	      {
		failure = true;
		break;
	      }
	      // If decode_type was called from decode_template_param then we
	      // need to suppress calling qualifiers here in order to get a
	      // substitution added anyway (for the <template-param>).
	      if (!recursive_template_param_or_substitution_call)
		qualifiers->decode_qualifiers(prefix, postfix);
	      else
		qualifiers->printing_suppressed();
	      break;
	  }
	}
    decode_type_exit:
	--M_inside_type;
	if (!recursive_template_param_or_substitution_call)
	{
	  qualifiers->~qualifier_list<Allocator>();
	  M_qualifier_list_alloc.deallocate(qualifiers, 1);
	}
	if (failure)
	  _GLIBCXX_DEMANGLER_FAILURE;
	_GLIBCXX_DEMANGLER_RETURN2;
      }

    // <nested-name> ::= N [<CV-qualifiers>] <prefix> <unqualified-name> E
    //               ::= N [<CV-qualifiers>] <template-prefix> <template-args> E
    //
    // <prefix> ::= <prefix> <unqualified-name>
    //          ::= <template-prefix> <template-args>
    //          ::= <template-param>
    //          ::= # empty
    //          ::= <substitution>
    //
    // <template-prefix> ::= <prefix> <template unqualified-name>
    //                   ::= <template-param>
    //                   ::= <substitution>
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_nested_name(string_type& output,
					     string_type& qualifiers)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_nested_name");

	if (current() != 'N' || M_pos >= M_maxpos)
	  _GLIBCXX_DEMANGLER_FAILURE;

	// <CV-qualifiers> ::= [r] [V] [K]  # restrict (C99), volatile, const
	char const* qualifiers_start = &M_str[M_pos + 1];
	for (char c = next(); c == 'K' || c == 'V' || c == 'r'; c = next());
	for (char const* qualifier_ptr = &M_str[M_pos - 1];
	     qualifier_ptr >= qualifiers_start; --qualifier_ptr)
	  switch(*qualifier_ptr)
	  {
	    case 'K':
	      qualifiers += " const";
	      break;
	    case 'V':
	      qualifiers += " volatile";
	      break;
	    case 'r':
	      qualifiers += " restrict";
	      break;
	  }

	int number_of_prefixes = 0;
	int substitution_start = M_pos;
	for(;;)
	{
	  ++number_of_prefixes;
	  if (current() == 'S')
	  {
	    if (!decode_substitution(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	  }
	  else if (current() == 'I')
	  {
	    if (!decode_template_args(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    if (current() != 'E')
	    {
	      // substitution: "<template-prefix> <template-args>".
	      add_substitution(substitution_start, nested_name_prefix,
			       number_of_prefixes);
	    }
	  }
	  else
	  {
	    if (current() == 'T')
	    {
	      if (!decode_template_param(output))
		_GLIBCXX_DEMANGLER_FAILURE;
	    }
	    else if (!decode_unqualified_name(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    if (current() != 'E')
	    {
	      // substitution: "<prefix> <unqualified-name>" or
	      // "<prefix> <template unqualified-name>".
	      add_substitution(substitution_start,
		  (current() == 'I') ?  nested_name_template_prefix
		                     : nested_name_prefix,
		  number_of_prefixes);
	    }
	  }
	  if (current() == 'E')
	  {
	    eat_current();
	    _GLIBCXX_DEMANGLER_RETURN;
	  }
	  if (current() != 'I')
	    output += "::";
	  else if (M_template_args_need_space)
	    output += ' ';
	  M_template_args_need_space = false;
	}
	_GLIBCXX_DEMANGLER_FAILURE;
      }

    // <local-name> := Z <function encoding> E <entity name> [<discriminator>]
    //              := Z <function encoding> E s [<discriminator>]
    // <discriminator> := _ <non-negative number>
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_local_name(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_local_name");
	if (current() != 'Z' || M_pos >= M_maxpos)
	  _GLIBCXX_DEMANGLER_FAILURE;
	if ((M_pos += decode_encoding(output, M_str + M_pos + 1,
		M_maxpos - M_pos, M_implementation_details) + 1) < 0 ||
		eat_current() != 'E')
	  _GLIBCXX_DEMANGLER_FAILURE;
	output += "::";
	if (current() == 's')
	{
	  eat_current();
	  output += "string literal";
	}
	else
	{
	  string_type nested_name_qualifiers;
	  if (!decode_name(output, nested_name_qualifiers))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  output += nested_name_qualifiers;
	}
	string_type discriminator;
	if (current() == '_' && next() != 'n' && !decode_number(discriminator))
	  _GLIBCXX_DEMANGLER_FAILURE;
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <source-name> ::= <positive length number> <identifier>
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_source_name(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_source_name");
	int length = current() - '0';
	if (length < 1 || length > 9)
	  _GLIBCXX_DEMANGLER_FAILURE;
	while(isdigit(next()))
	  length = 10 * length + current() - '0';
	char const* ptr = &M_str[M_pos];
	if (length > 11 && !strncmp(ptr, "_GLOBAL_", 8) && ptr[9] == 'N'
	    && ptr[8] == ptr[10])
	{
	  output += "(anonymous namespace)";
	  if ((M_pos += length) > M_maxpos + 1)
	    _GLIBCXX_DEMANGLER_FAILURE;
	}
	else
	  while(length--)
	  {
	    if (current() == 0)
	      _GLIBCXX_DEMANGLER_FAILURE;
	    output += eat_current();
	  }
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <unqualified-name> ::= <operator-name>	# Starts with lower case.
    //                    ::= <ctor-dtor-name>  # Starts with 'C' or 'D'.
    //                    ::= <source-name>	# Starts with a digit.
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_unqualified_name(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_unqualified_name");
	if (M_inside_template_args)
	{
	  if (!decode_source_name(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
	}
	else if (isdigit(current()))
	{
	  bool recursive_unqualified_name = (&M_function_name == &output);
	  // This can be a recursive call when we are decoding
	  // an <operator-name> that is a cast operator for a some
	  // <unqualified-name>; for example "operator Foo()".
	  // In that case this is thus not a ctor or dtor and we
	  // are not interested in updating M_function_name.
	  if (!recursive_unqualified_name)
	    M_function_name.clear();
	  M_name_is_template = false;
	  M_name_is_cdtor = false;
	  M_name_is_conversion_operator = false;
	  if (!decode_source_name(M_function_name))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  if (!recursive_unqualified_name)
	    output += M_function_name;
	}
	else if (islower(current()))
	{
	  M_function_name.clear();
	  M_name_is_template = false;
	  M_name_is_cdtor = false;
	  M_name_is_conversion_operator = false;
	  if (!decode_operator_name(M_function_name))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  output += M_function_name;
	}
	else if (current() == 'C' || current() == 'D')
	{
	  // <ctor-dtor-name> ::=
	  //   C1	# complete object (in-charge) constructor
	  //   C2	# base object (not-in-charge) constructor
	  //   C3	# complete object (in-charge) allocating constructor
	  //   D0	# deleting (in-charge) destructor
	  //   D1	# complete object (in-charge) destructor
	  //   D2	# base object (not-in-charge) destructor
	  //
	  if (current() == 'C')
	  {
	    char c = next();
	    if (c < '1' || c > '3')
	      _GLIBCXX_DEMANGLER_FAILURE;
	  }
	  else
	  {
	    char c = next();
	    if (c < '0' || c > '2')
	      _GLIBCXX_DEMANGLER_FAILURE;
	    output += '~';
	    M_saw_destructor = true;
	  }
	  M_name_is_cdtor = true;
	  eat_current();
	  output += M_function_name;
	}
	else
	  _GLIBCXX_DEMANGLER_FAILURE;
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <unscoped-name> ::=
    //   <unqualified-name>		# Starts not with an 'S'
    //   St <unqualified-name>		# ::std::
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_unscoped_name(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_unscoped_name");
	if (current() == 'S')
	{
	  if (next() != 't')
	    _GLIBCXX_DEMANGLER_FAILURE;
	  eat_current();
	  output += "std::";
	}
	decode_unqualified_name(output);
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <name> ::=
    //   <nested-name>				# Starts with 'N'
    //   <unscoped-template-name> <template-args> # idem
    //   <local-name>				# Starts with 'Z'
    //   <unscoped-name>			# Starts with 'S', 'C', 'D',
    //						# a digit or a lower case
    //						# character.
    //
    // <unscoped-template-name> ::= <unscoped-name>
    //                          ::= <substitution>
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_name(string_type& output,
				      string_type& nested_name_qualifiers)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_name");
	int substitution_start = M_pos;
	if (current() == 'S' && (M_pos >= M_maxpos || M_str[M_pos + 1] != 't'))
	{
	  if (!decode_substitution(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
	}
	else if (current() == 'N')
	{
	  decode_nested_name(output, nested_name_qualifiers);
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	else if (current() == 'Z')
	{
	  decode_local_name(output);
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	else if (!decode_unscoped_name(output))
	  _GLIBCXX_DEMANGLER_FAILURE;
	if (current() == 'I')
	{
	  // Must have been an <unscoped-template-name>.
	  add_substitution(substitution_start, unscoped_template_name);
	  if (!decode_template_args(output))
	    _GLIBCXX_DEMANGLER_FAILURE;
	}
	M_template_args_need_space = false;
	_GLIBCXX_DEMANGLER_RETURN;
      }

    // <call-offset> ::= h <nv-offset> _
    //               ::= v <v-offset> _
    // <nv-offset>   ::= <offset number>
    //     non-virtual base override
    //
    // <v-offset>    ::= <offset number> _ <virtual offset number>
    //     virtual base override, with vcall offset
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_call_offset(string_type&
#if _GLIBCXX_DEMANGLER_CWDEBUG
	  output
#endif
	  )
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_call_offset");
	if (current() == 'h')
	{
	  string_type dummy;
	  eat_current();
	  if (decode_number(dummy) && current() == '_')
	  {
	    eat_current();
	    _GLIBCXX_DEMANGLER_RETURN;
	  }
	}
	else if (current() == 'v')
	{
	  string_type dummy;
	  eat_current();
	  if (decode_number(dummy) && current() == '_')
	  {
	    eat_current();
	    if (decode_number(dummy) && current() == '_')
	    {
	      eat_current();
	      _GLIBCXX_DEMANGLER_RETURN;
	    }
	  }
	}
	_GLIBCXX_DEMANGLER_FAILURE;
      }

    //
    // <special-name> ::=
    //   TV <type>			# virtual table
    //   TT <type>			# VTT structure (construction
    //                                    vtable index).
    //   TI <type>			# typeinfo structure
    //   TS <type>			# typeinfo name (null-terminated
    //                                    byte string).
    //   GV <object name>		# Guard variable for one-time
    //					  initialization of static objects in
    //					  a local scope.
    //   T <call-offset> <base encoding># base is the nominal target function
    //					  of thunk.
    //   Tc <call-offset> <call-offset> <base encoding> # base is the nominal
    //                                    target function of thunk; first
    //                                    call-offset is 'this' adjustment;
    //					  second call-offset is result
    //					  adjustment
    //
    template<typename Tp, typename Allocator>
      bool
      session<Tp, Allocator>::decode_special_name(string_type& output)
      {
	_GLIBCXX_DEMANGLER_DOUT_ENTERING("decode_special_name");
	if (current() == 'G')
	{
	  if (next() != 'V')
	    _GLIBCXX_DEMANGLER_FAILURE;
	  output += "guard variable for ";
	  string_type nested_name_qualifiers;
	  eat_current();
	  if (!decode_name(output, nested_name_qualifiers))
	    _GLIBCXX_DEMANGLER_FAILURE;
	  output += nested_name_qualifiers;
	  _GLIBCXX_DEMANGLER_RETURN;
	}
	else if (current() != 'T')
	  _GLIBCXX_DEMANGLER_FAILURE;
	switch(next())
	{
	  case 'V':
	    output += "vtable for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCXX_DEMANGLER_RETURN;
	  case 'T':
	    output += "VTT for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCXX_DEMANGLER_RETURN;
	  case 'I':
	    output += "typeinfo for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCXX_DEMANGLER_RETURN;
	  case 'S':
	    output += "typeinfo name for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCXX_DEMANGLER_RETURN;
	  case 'c':
	    output += "covariant return thunk to ";
	    if (!decode_call_offset(output)
		|| !decode_call_offset(output)
		|| (M_pos += decode_encoding(output, M_str + M_pos,
		    M_maxpos - M_pos + 1, M_implementation_details)) < 0)
	      _GLIBCXX_DEMANGLER_FAILURE;
	    _GLIBCXX_DEMANGLER_RETURN;
	  case 'C':		// GNU extension?
	  {
	    string_type first;
	    output += "construction vtable for ";
	    eat_current();
	    if (!decode_type(first))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    while(isdigit(current()))
	      eat_current();
	    if (eat_current() != '_')
	      _GLIBCXX_DEMANGLER_FAILURE;
	    if (!decode_type(output))
	      _GLIBCXX_DEMANGLER_FAILURE;
	    output += "-in-";
	    output += first;
	    _GLIBCXX_DEMANGLER_RETURN;
	  }
	  default:
	    if (current() == 'v')
	      output += "virtual thunk to ";
	    else
	      output += "non-virtual thunk to ";
	    if (!decode_call_offset(output)
		|| (M_pos += decode_encoding(output, M_str + M_pos,
		    M_maxpos - M_pos + 1, M_implementation_details)) < 0)
	      _GLIBCXX_DEMANGLER_FAILURE;
	    _GLIBCXX_DEMANGLER_RETURN;
	}
      }

    // <encoding> ::=
    //   <function name> <bare-function-type>	# Starts with 'C', 'D', 'N',
    //						  'S', a digit or a lower case
    //						  character.
    //   <data name>				# Idem.
    //   <special-name>				# Starts with 'T' or 'G'.
    template<typename Tp, typename Allocator>
      int
      session<Tp, Allocator>::decode_encoding(string_type& output,
          char const* in, int len, implementation_details const& id)
      {
#if _GLIBCXX_DEMANGLER_CWDEBUG
	_GLIBCXX_DEMANGLER_DOUT(dc::demangler,
	    "Output thus far: \"" << output << '"');
	string_type input(in, len > 0x40000000 ? strlen(in) : len);
	_GLIBCXX_DEMANGLER_DOUT(
	    dc::demangler, "Entering decode_encoding(\"" << input << "\")");
#endif
	if (len <= 0)
	  return INT_MIN;
	session<Tp, Allocator> demangler_session(in, len, id);
	string_type nested_name_qualifiers;
	int saved_pos;
	demangler_session.store(saved_pos);
	if (demangler_session.decode_special_name(output))
	  return demangler_session.M_pos;
	demangler_session.restore(saved_pos);
	string_type name;
	if (!demangler_session.decode_name(name, nested_name_qualifiers))
	  return INT_MIN;
	if (demangler_session.current() == 0
	    || demangler_session.current() == 'E')
	{
	  output += name;
	  output += nested_name_qualifiers;
	  return demangler_session.M_pos;
	}
	// Must have been a <function name>.
	string_type return_type_postfix;
	if (demangler_session.M_name_is_template
	    && !(demangler_session.M_name_is_cdtor
	         || demangler_session.M_name_is_conversion_operator))
	{
	  // Return type of function
	  if (!demangler_session.decode_type_with_postfix(output,
	      return_type_postfix))
	    return INT_MIN;
	  output += ' ';
	}
	output += name;
	if (!demangler_session.decode_bare_function_type(output))
	  return INT_MIN;
	output += nested_name_qualifiers;
	output += return_type_postfix;
	return demangler_session.M_pos;
      }

    } // namespace demangler

  // Public interface
  template<typename Tp, typename Allocator>
    struct demangle
    {
      typedef typename Allocator::template rebind<char>::other char_Allocator;
      typedef std::basic_string<char, std::char_traits<char>, char_Allocator>
	  string_type;
      static string_type symbol(char const* in,
                                demangler::implementation_details const& id);
      static string_type type(char const* in,
                              demangler::implementation_details const& id);
    };

  // demangle::symbol()
  //
  // Demangle `input' which should be a mangled function name as for
  // instance returned by nm(1).
  template<typename Tp, typename Allocator>
    typename demangle<Tp, Allocator>::string_type
    demangle<Tp, Allocator>::symbol(char const* input,
                                demangler::implementation_details const& id)
    {
      // <mangled-name> ::= _Z <encoding>
      // <mangled-name> ::= _GLOBAL_ _<type>_ <disambiguation part>
      //                    <type> can be I or D (GNU extension)
      typedef demangler::session<Tp, Allocator> demangler_type;
      string_type result;
      bool failure = (input[0] != '_');

      if (!failure)
      {
	if (input[1] == 'G')
	{
	  if (!strncmp(input, "_GLOBAL__", 9)
	      && (input[9] == 'D' || input[9] == 'I')
	      && input[10] == '_')
	  {
	    if (input[9] == 'D')
	      result.assign("global destructors keyed to ", 28);
	    else
	      result.assign("global constructors keyed to ", 29);
	    // Output the disambiguation part as-is.
	    result += input + 11;
	  }
	  else
	    failure = true;
	}
	else if (input[1] == 'Z')
	{
	  int cnt =
	      demangler_type::decode_encoding(result, input + 2, INT_MAX, id);
	  if (cnt < 0 || input[cnt + 2] != 0)
	    failure = true;
	}
	else
	  failure = true;
      }

      // Failure to demangle, return the mangled name.
      if (failure)
	result.assign(input, strlen(input));

      return result;
    }

  // demangle::type()
  // Demangle `input' which must be a zero terminated mangled type
  // name as for instance returned by std::type_info::name().
  template<typename Tp, typename Allocator>
    typename demangle<Tp, Allocator>::string_type
    demangle<Tp, Allocator>::type(char const* input,
                              demangler::implementation_details const& id)
    {
      std::basic_string<char, std::char_traits<char>, Allocator> result;
      if (input == NULL)
	result = "(null)";
      else
      {
	demangler::session<Tp, Allocator> demangler_session(input, INT_MAX, id);
	if (!demangler_session.decode_type(result)
	    || demangler_session.remaining_input_characters())
	{
	  // Failure to demangle, return the mangled name.
	  result = input;
	}
      }
      return result;
    }
} // namespace __gnu_cxx

#endif // __DEMANGLE_H
