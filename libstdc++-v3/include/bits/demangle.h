// C++ IA64 / g++ v3 demangler  -*- C++ -*-

// Copyright (C) 2003 Free Software Foundation, Inc.
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

#ifndef __DEMANGLER_H
#define __DEMANGLER_H 1

#include <limits>
#include <vector>
#include <string>
#include <cctype>

#ifndef _GLIBCPP_DEMANGLER_DEBUG
#define _GLIBCPP_DEMANGLER_CWDEBUG 0
#define _GLIBCPP_DEMANGLER_DEBUG(x)
#define _GLIBCPP_DEMANGLER_DOUT(cntrl, data)
#define _GLIBCPP_DEMANGLER_DOUT_ENTERING(x)
#define _GLIBCPP_DEMANGLER_DOUT_ENTERING2(x)
#define _GLIBCPP_DEMANGLER_RETURN \
    return M_result
#define _GLIBCPP_DEMANGLER_RETURN2 \
    return M_result
#define _GLIBCPP_DEMANGLER_FAILURE \
    do { M_result = false; return false; } while(0)
#else
#define _GLIBCPP_DEMANGLER_CWDEBUG 1
#endif

// The following defines change the behaviour of the demangler.  The
// default behaviour is that none of these macros is defined.

// _GLIBCPP_DEMANGLER_STYLE_VOID
// Default behaviour:					int f()
// Uses (void) instead of ():				int f(void)

// _GLIBCPP_DEMANGLER_STYLE_LITERAL
// Default behaviour:					(long)13, 
//							(unsigned long long)19
// Use extensions 'u', 'l' and 'll' for integral
// literals (as in template arguments):			13l, 19ull

// _GLIBCPP_DEMANGLER_STYLE_LITERAL_INT
// Default behaviour:					4
// Use also an explicit cast for int in literals:	(int)4

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
      unscoped_template_name,
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

    template<typename Allocator>
      class qualifier;

    template<typename Allocator>
      class qualifier_list;

    template<typename Allocator>
      class session;

    template<typename Allocator>
      class qualifier
      {
	typedef std::basic_string<char, std::char_traits<char>, Allocator>
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
	          cv_qualifier_nt cv_qualifier,
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

      };

    template<typename Allocator>
      class qualifier_list
      {
	typedef std::basic_string<char, std::char_traits<char>, Allocator>
	  string_type;

      private:
	bool M_printing_suppressed;
	std::vector<qualifier<Allocator>, Allocator> M_qualifier_starts;
	session<Allocator>& M_demangler;

      public:
	qualifier_list(session<Allocator>& demangler_obj)
	: M_printing_suppressed(false), M_demangler(demangler_obj)
	{ }

	void
	add_qualifier_start(simple_qualifier_nt simple_qualifier,
			    int start_pos,
			    int inside_substitution)
	{ M_qualifier_starts.
	      push_back(qualifier<Allocator>(start_pos,
		  simple_qualifier, inside_substitution)); }

	void
	add_qualifier_start(cv_qualifier_nt cv_qualifier,
			    int start_pos,
			    int count,
			    int inside_substitution)
	{ M_qualifier_starts.
	      push_back(qualifier<Allocator>(start_pos,
		    cv_qualifier, &M_demangler.M_str[start_pos],
		    count, inside_substitution)); }

	void
	add_qualifier_start(param_qualifier_nt param_qualifier,
	    		    int start_pos,
			    string_type optional_type,
			    int inside_substitution)
	{ M_qualifier_starts.
	      push_back(qualifier<Allocator>(start_pos,
		    param_qualifier, optional_type, inside_substitution)); }

	void
	decode_qualifiers(string_type& prefix,
	    		  string_type& postfix,
			  bool member_function_pointer_qualifiers);

	bool
	suppressed(void) const
	{ return M_printing_suppressed; }

	void
	printing_suppressed(void)
	{ M_printing_suppressed = true; }

	size_t
	size(void) const
	{ return M_qualifier_starts.size(); }

      };

    template<typename Allocator>
      class session
      {
	friend class qualifier_list<Allocator>;
	typedef std::basic_string<char, std::char_traits<char>, Allocator>
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
	std::vector<int, Allocator> M_template_arg_pos;
	int M_template_arg_pos_offset;
	std::vector<substitution_st, Allocator> M_substitutions_pos;
#if _GLIBCPP_DEMANGLER_CWDEBUG
	bool M_inside_add_substitution;
#endif

      public:
	explicit session(char const* in, int len)
	: M_str(in), M_pos(0), M_maxpos(len - 1), M_result(true),
	  M_inside_template_args(0), M_inside_type(0),
	  M_inside_substitution(0), M_saw_destructor(false),
	  M_name_is_cdtor(false), M_name_is_template(false),
	  M_name_is_conversion_operator(false),
	  M_template_args_need_space(false), M_template_arg_pos_offset(0)
#if _GLIBCPP_DEMANGLER_CWDEBUG
	  , M_inside_add_substitution(false)
#endif
	{ }

	static int
	decode_encoding(string_type& output, char const* input, int len);

	bool
	decode_type_with_postfix(string_type& prefix,
		                 string_type& postfix,
	            qualifier_list<Allocator>* qualifiers = NULL);

	bool
	decode_type(string_type& output,
	            qualifier_list<Allocator>* qualifiers = NULL)
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
	    qualifier_list<Allocator>* qualifiers = NULL);
	bool decode_template_args(string_type& output);
	bool decode_template_param(string_type& output,
	    qualifier_list<Allocator>* qualifiers = NULL);
	bool decode_unqualified_name(string_type& output);
	bool decode_unscoped_name(string_type& output);
	bool decode_decimal_integer(string_type& output);
	bool decode_special_name(string_type& output);
      };

    template<typename Allocator>
#if !_GLIBCPP_DEMANGLER_CWDEBUG
      inline
#endif
      void
      session<Allocator>::add_substitution(int start_pos,
					   substitution_nt sub_type,
					   int number_of_prefixes = 0)
      {
	if (!M_inside_substitution)
	{
#if _GLIBCPP_DEMANGLER_CWDEBUG
	  if (M_inside_add_substitution)
	    return;
#endif
	  M_substitutions_pos.
	      push_back(substitution_st(start_pos,
		  sub_type, number_of_prefixes));
#if _GLIBCPP_DEMANGLER_CWDEBUG
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
	  _GLIBCPP_DEMANGLER_DEBUG( dc::demangler.off() );
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
	  _GLIBCPP_DEMANGLER_DEBUG( dc::demangler.on() );
	  _GLIBCPP_DEMANGLER_DOUT(dc::demangler,
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

    //
    // <decimal-integer> ::= 0
    //                   ::= 1|2|3|4|5|6|7|8|9 [<digit>+]
    // <digit>           ::= 0|1|2|3|4|5|6|7|8|9
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_decimal_integer(string_type& output)
      {
	char c = current();
	if (c == '0')
	{
	  output += '0';
	  eat_current();
	}
	else if (!std::isdigit(c))
	  M_result = false;
	else
	{
	  do
	  {
	    output += c;
	  }
	  while (std::isdigit((c = next())));
	}
	return M_result;
      }

    // <number> ::= [n] <decimal-integer>
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_number(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_number");
	if (current() != 'n')
	  decode_decimal_integer(output);
	else
	{
	  output += '-';
	  eat_current();
	  decode_decimal_integer(output);
	}
	_GLIBCPP_DEMANGLER_RETURN;
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
    template<typename Allocator>
      bool
      session<Allocator>::decode_builtin_type(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_builtin_type");
	char const* bt;
	if (!islower(current()) || !(bt = builtin_type_c[current() - 'a']))
	  _GLIBCPP_DEMANGLER_FAILURE;
	output += bt;
	eat_current();
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <class-enum-type> ::= <name>
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_class_enum_type(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_class_enum_type");
	string_type nested_name_qualifiers;
	if (!decode_name(output, nested_name_qualifiers))
	  _GLIBCPP_DEMANGLER_FAILURE;
	output += nested_name_qualifiers;
	_GLIBCPP_DEMANGLER_RETURN;
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
    template<typename Allocator>
      bool
      session<Allocator>::decode_substitution(string_type& output,
	  qualifier_list<Allocator>* qualifiers)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_substitution");
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
	      _GLIBCPP_DEMANGLER_RETURN;
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
	      _GLIBCPP_DEMANGLER_RETURN;
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
	      _GLIBCPP_DEMANGLER_RETURN;
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
	      _GLIBCPP_DEMANGLER_RETURN;
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
	      _GLIBCPP_DEMANGLER_RETURN;
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
	      _GLIBCPP_DEMANGLER_RETURN;
	    case 't':
	      output += "std";
	      eat_current();
	      if (qualifiers)
		qualifiers->printing_suppressed();
	      _GLIBCPP_DEMANGLER_RETURN;
	    default:
	      for(;; c = next())
	      {
		if (std::isdigit(c))
		  value = value * 36 + c - '0';
		else if (isupper(c))
		  value = value * 36 + c - 'A' + 10;
		else if (c == '_')
		  break;
		else
		  _GLIBCPP_DEMANGLER_FAILURE;
	      }
	      ++value;
	      break;
	  }
	}
	eat_current();
	if (value >= M_substitutions_pos.size() ||
	    M_inside_type > 20)			// Rather than core dump.
	  _GLIBCPP_DEMANGLER_FAILURE;
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
		  _GLIBCPP_DEMANGLER_FAILURE;
	      }
	      else
	      {
		if (cnt < substitution.M_number_of_prefixes)
		  output += "::";
		if (current() == 'S')
		{
		  if (!decode_substitution(output))
		    _GLIBCPP_DEMANGLER_FAILURE;
		}
		else if (!decode_unqualified_name(output))
		  _GLIBCPP_DEMANGLER_FAILURE;
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
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <template-param> ::= T_			# first template parameter
    //                  ::= T <parameter-2 non-negative number> _
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_template_param(string_type& output,
	  qualifier_list<Allocator>* qualifiers)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_template_parameter");
	if (current() != 'T')
	  _GLIBCPP_DEMANGLER_FAILURE;
	unsigned int value = 0;
	char c;
	if ((c = next()) != '_')
	{
	  while(std::isdigit(c))
	  {
	    value = value * 10 + c - '0';
	    c = next();
	  }
	  ++value;
	}
	if (eat_current() != '_')
	  _GLIBCPP_DEMANGLER_FAILURE;
	value += M_template_arg_pos_offset;
	if (value >= M_template_arg_pos.size())
	  _GLIBCPP_DEMANGLER_FAILURE;
	int saved_pos = M_pos;
	M_pos = M_template_arg_pos[value];
	if (M_inside_type > 20)		// Rather than core dump.
	  _GLIBCPP_DEMANGLER_FAILURE;
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
	_GLIBCPP_DEMANGLER_RETURN;
      }

    template<typename Allocator>
      bool
      session<Allocator>::decode_literal(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_literal");
	eat_current();	// Eat the 'L'.
	if (current() == '_')
	{
	  if (next() != 'Z')
	    _GLIBCPP_DEMANGLER_FAILURE;
	  eat_current();
	  if ((M_pos += decode_encoding(output, M_str + M_pos,
		  M_maxpos - M_pos + 1)) < 0)
	    _GLIBCPP_DEMANGLER_FAILURE;
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
	    _GLIBCPP_DEMANGLER_RETURN;
	  }
	  char c = current();
#ifdef _GLIBCPP_DEMANGLER_STYLE_LITERAL
	  if (c == 'i' || c == 'j' || c == 'l' ||
	      c == 'm' || c == 'x' || c == 'y')
	    eat_current();
	  else
#else
#ifndef _GLIBCPP_DEMANGLER_STYLE_LITERAL_INT
	  if (c == 'i')
	    eat_current();
	  else
#endif
#endif
	  {
	    output += '(';
	    if (!decode_type(output))
	      _GLIBCPP_DEMANGLER_FAILURE;
	    output += ')';
	  }
	  if (!decode_number(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
#ifdef _GLIBCPP_DEMANGLER_STYLE_LITERAL
	  if (c == 'j' || c == 'm' || c == 'y')
	    output += 'u';
	  if (c == 'l' || c == 'm')
	    output += 'l';
	  if (c == 'x' || c == 'y')
	    output += "ll";
#endif
	}
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <operator-name> ::=
    //   nw				# new           
    //   na				# new[]
    //   dl				# delete        
    //   da				# delete[]      
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
    //   sz				# sizeof        
    //   sr				# scope resolution (::), see below        
    //   cv <type>			# (cast)        
    //   v <digit> <source-name>	# vendor extended operator
    //
    //
    // Symbol operator codes exist of two characters, we need to find a
    // quick hash so that their names can be looked up in a table.
    //
    // The puzzle :)
    // Shift the rows so that there is at most one character per column.
    //
    // A perfect solution:
    //                                              horizontal
    //    .....................................     offset + 'a'
    // a, ||a||d|||||||||n||||s||||||||||||||||||	    2
    // c, || || ||lm|o||| |||| ||||||||||||||||||	   -3
    // d, || a| |e  | ||l |||| |||v||||||||||||||	    3
    // e, ||  | |   o q|  |||| ||| ||||||||||||||	   -4
    // g, |e  | |      |  t||| ||| ||||||||||||||	   -3
    // i, |   | |      |   ||| ||| ||||||||||x|||    12
    // l, |   | |      e   ||| ||| ||st|||||| |||	    9
    // m, |   | |          ||| ||| |i  lm|||| |||	   18
    // n, a   e g          ||t |w| |     |||| |||	    0
    // o,                  ||  | | |     ||o| r||	   19
    // p,                  lm  p | t     || |  ||	    6
    // q,                        |       || u  ||	   14
    // r,                        |       |m    |s	   20
    // s,                        r       z     | 	    6
    //    .....................................
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
      0, -95,   0,-100, -94,-101,   0,-100,   0, -85,   0,   0,
      //   l    m    n    o    p    q    r    s    t    u    v
	 -88, -79, -97, -78, -91, -83, -77, -91,   0,   0,   0,
#else
      //   a    b    c    d    e    f    g    h    i    j    k
      0, 161,   0, 156, 162, 155,   0, 156,   0, 171,   0,   0,
      //   l    m    n    o    p    q    r    s    t    u    v
	 168, 177, 159, 178, 165, 173, 179, 165,   0,   0,   0,
#endif
      // ... more zeros
    };

    struct entry_st
    {
      char const* opcode;
      char const* symbol_name;
      bool unary;
    };

    entry_st const symbol_name_table_c[39] = {
      { "na",  "operator new[]", true },
      { "ge",  "operator>=", false },
      { "aa",  "operator&&", false },
      { "da",  "operator delete[]", true },
      { "ne",  "operator!=", false },
      { "ad",  "operator&", true },	// unary
      { "ng",  "operator-", true },	// unary
      { "de",  "operator*", true },	// unary
      { "cl",  "operator()", true },
      { "cm",  "operator,", false },
      { "eo=", "operator^", false },
      { "co",  "operator~", false },
      { "eq",  "operator==", false },
      { "le",  "operator<=", false },
      { "dl",  "operator delete", true },
      { "an=", "operator&", false },
      { "gt",  "operator>", false },
      { "pl=", "operator+", false },
      { "pm",  "operator->*", false },
      { "nt",  "operator!", true },
      { "as=", "operator", false },
      { "pp",  "operator++", true },
      { "nw",  "operator new", true },
      { "sr",  "::", true },
      { "dv=", "operator/", false },
      { "pt",  "operator->", false },
      { "mi=", "operator-", false },
      { "ls=", "operator<<", false },
      { "lt",  "operator<", false },
      { "ml=", "operator*", false },
      { "mm",  "operator--", true },
      { "sz",  "sizeof", true },
      { "rm=", "operator%", false },
      { "oo",  "operator||", false },
      { "qu",  "operator?", false },
      { "ix",  "operator[]", true },
      { "or=", "operator|", false },
      { "", NULL, false },
      { "rs=", "operator>>", false }
    };

    template<typename Allocator>
      bool
      session<Allocator>::decode_operator_name(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_operator_name");

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
	      if (hash == 27 || hash == 28)
		M_template_args_need_space = true;
	      _GLIBCPP_DEMANGLER_RETURN;
	    }
	    else if (opcode0 == 'c' && opcode1 == 'v')
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
		_GLIBCPP_DEMANGLER_FAILURE;
	      if (!M_inside_template_args)
		M_name_is_conversion_operator = true;
	      _GLIBCPP_DEMANGLER_RETURN;
	    }
	  }
	}
	_GLIBCPP_DEMANGLER_FAILURE;
      }

    //
    // <expression> ::= <unary operator-name> <expression>
    //              ::= <binary operator-name> <expression> <expression>
    //              ::= <expr-primary>
    //
    // <expr-primary> ::= <template-param>		# Starts with a T
    //                ::= L <type> <value number> E	# literal
    //                ::= L <mangled-name> E		# external name
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_expression(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_expression");
	if (current() == 'T')
	{
	  if (!decode_template_param(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
	  _GLIBCPP_DEMANGLER_RETURN;
	}
	else if (current() == 'L')
	{
	  if (!decode_literal(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
	  if (current() != 'E')
	    _GLIBCPP_DEMANGLER_FAILURE;
	  eat_current();
	  _GLIBCPP_DEMANGLER_RETURN;
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
		char const* p = entry.symbol_name;
		if (!strncmp("operator", p, 8))
		  p += 8;
		if (*p == ' ')
		  ++p;
		if (entry.unary)
		  output += p;
		bool is_eq = (opcode1 != current());
		eat_current();
		output += '(';
		if (!decode_expression(output))
		  _GLIBCPP_DEMANGLER_FAILURE;
		output += ')';
		if (!entry.unary)
		{
		  output += ' ';
		  output += p;
		  if (is_eq)
		    output += '=';
		  output += ' ';
		  output += '(';
		  if (!decode_expression(output))
		    _GLIBCPP_DEMANGLER_FAILURE;
		  output += ')';
		}
		_GLIBCPP_DEMANGLER_RETURN;
	      }
	    }
	  }
	}
	_GLIBCPP_DEMANGLER_FAILURE;
      }

    //
    // <template-args> ::= I <template-arg>+ E
    // <template-arg> ::= <type>			# type or template
    //                ::= L <type> <value number> E	# literal
    //                ::= L_Z <encoding> E		# external name
    //                ::= X <expression> E		# expression
    template<typename Allocator>
      bool
      session<Allocator>::decode_template_args(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_template_args");
	if (eat_current() != 'I')
	  _GLIBCPP_DEMANGLER_FAILURE;
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
	      _GLIBCPP_DEMANGLER_FAILURE;
	    if (current() != 'E')
	      _GLIBCPP_DEMANGLER_FAILURE;
	    eat_current();
	  }
	  else if (current() == 'L')
	  {
	    if (!decode_literal(output))
	      _GLIBCPP_DEMANGLER_FAILURE;
	    if (current() != 'E')
	      _GLIBCPP_DEMANGLER_FAILURE;
	    eat_current();
	  }
	  else if (!decode_type(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
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
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <bare-function-type> ::=
    //   <signature type>+		# types are parameter types
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_bare_function_type(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_bare_function_type");
	if (M_saw_destructor)
	{
	  if (eat_current() != 'v' || (current() != 'E' && current() != 0))
	    _GLIBCPP_DEMANGLER_FAILURE;
	  output += "()";
	  M_saw_destructor = false;
	  _GLIBCPP_DEMANGLER_RETURN;
	}
#ifndef _GLIBCPP_DEMANGLER_STYLE_VOID
	if (current() == 'v')
	{
	  eat_current();
	  if (current() != 'E' && current() != 0)
	    _GLIBCPP_DEMANGLER_FAILURE;
	  output += "()";
	  M_saw_destructor = false;
	  _GLIBCPP_DEMANGLER_RETURN;
	}
#endif
	output += '(';
	M_template_args_need_space = false;
	if (!decode_type(output))	// Must have at least one parameter.
	  _GLIBCPP_DEMANGLER_FAILURE;
	while (current() != 'E' && current() != 0)
	{
	  output += ", ";
	  if (!decode_type(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
	}
	output += ')';
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <type> ::=
    //   <builtin-type>		# Starts with a lower case character != r.
    //   <function-type>	# Starts with F
    //   <class-enum-type>	# Starts with N, S, C, D, Z, a digit or a lower
    //   			# case character.  Since a lower case character
    //   			# would be an operator name, that would be an
    //   			# error.  The S is a substitution or St
    //   			# (::std::).  A 'C' would be a constructor and
    //   			# thus also an error.
    //   <template-param>	# Starts with T
    //   <substitution>         # Starts with S
    //   <template-template-param> <template-args>  # Starts with T or S,
    //   					    # equivalent with the above.
    //
    //   <array-type>			# Starts with A
    //   <pointer-to-member-type>	# Starts with M
    //   <CV-qualifiers> <type>		# Starts with r, V or K
    //   P <type>   # pointer-to	# Starts with P
    //   R <type>   # reference-to	# Starts with R
    //   C <type>   # complex (C 2000)	# Starts with C
    //   G <type>   # imaginary (C 2000)# Starts with G
    //   U <source-name> <type>     	# vendor extended type qualifier,
    //   				# starts with U
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
    // 						    (<R> and <B> recursive),
    // 						    "M<C><Q2>F<R><B>E".
    // <Q>F<R><B>E 	    ==> R (Q)B		"<R>", "<B>" (<B> recursive)
    //                                              and "F<R><B>E".
    //
    // Note that if <R> has postfix qualifiers (an array), then those
    // are added AFTER the (member) function type.  For example:
    // <Q>FPA<R><B>E ==> R (*(Q)B) [], where the PA added the prefix
    // "(*" and the postfix ") []".
    //
    // <Q>G<T>     	    ==> imaginary T Q	"<T>", "G<T>" (<T> recursive).
    // <Q>C<T>     	    ==> complex T Q	"<T>", "C<T>" (<T> recursive).
    // <Q><T>      	    ==> T Q		"<T>" (<T> recursive).
    //
    // where <Q> is any of:
    //
    // <Q>P   		==> *Q				"P..."
    // <Q>R   		==> &Q				"R..."
    // <Q>[K|V|r]+	==> [ const| volatile| restrict]+Q	"KVr..."
    // <Q>U<S>		==>  SQ				"U<S>..."
    // <Q>M<C>		==> C::*Q			"M<C>..." (<C> recurs.)
    // A<I>		==> [I]				"A<I>..." (<I> recurs.)
    // <Q>A<I>		==>  (Q) [I]			"A<I>..." (<I> recurs.)
    //   Note that when <Q> ends on an A<I2> then the brackets are omitted:
    //   A<I2>A<I>	  ==> [I2][I]
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
    template<typename Allocator>
      void
      qualifier_list<Allocator>::decode_qualifiers(
	  string_type& prefix,
	  string_type& postfix,
	  bool member_function_pointer_qualifiers = false)
      {
	for(typename std::vector<qualifier<Allocator>, Allocator>::
	    reverse_iterator iter = M_qualifier_starts.rbegin();
	    iter != M_qualifier_starts.rend();)
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
		prefix += "*";
		break;
	      case 'R':
		prefix += "&";
		break;
	      case 'K':
		prefix += " const";
		continue;
	      case 'V':
		prefix += " volatile";
		continue;
	      case 'r':
		prefix += " restrict";
		continue;
	      case 'A':
	      {
		string_type index = (*iter).get_optional_type();
		if (++iter != M_qualifier_starts.rend()
		    && (*iter).first_qualifier() != 'A')
		{
		  prefix += " (";
		  postfix = ") [" + index + "]" + postfix;
		}
		else
		  postfix = "[" + index + "]" + postfix;
		break;
	      }
	      case 'M':
		prefix += " ";
		prefix += (*iter).get_optional_type();
		prefix += "::*";
		break;
	      case 'U':
		prefix += " ";
		prefix += (*iter).get_optional_type();
		break;
	      case 'G':	// Only here so we added a substitution.
		break;
	    }
	    break;
	  }
	  if (qualifier_char != 'A')
	    ++iter;
	}
	M_printing_suppressed = false;
      }

    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_type_with_postfix(
	  string_type& prefix, string_type& postfix,
	  qualifier_list<Allocator>* qualifiers)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING2
	    (qualifiers ? "decode_type" : "decode_type[with qualifiers]");
	++M_inside_type;
	bool recursive_template_param_or_substitution_call;
	if (!(recursive_template_param_or_substitution_call = qualifiers))
	    qualifiers = new qualifier_list<Allocator>(*this);
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
		qualifier_list<Allocator> class_type_qualifiers(*this);
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
		if (!decode_type_with_postfix(prefix, postfix))
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
		prefix += ")";
		prefix += bare_function_type;
		prefix += member_function_qualifiers;
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
	  // <Q>G<T>     		==> imaginary T Q
	  //     substitutions: "<T>", "G<T>" (<T> recursive).
	  // <Q>C<T>     		==> complex T Q
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
	      // <Q>F<R><B>E 		==> R (Q)B
	      //     substitution: "<R>", "<B>" (<B> recursive) and "F<R><B>E".
	      eat_current();
	      // Return type.
	      if (!decode_type_with_postfix(prefix, postfix))
		  // Substitution: "<R>".
	      {
		failure = true;
		break;
	      }
	      // Only array (pointer) types have a postfix.
	      // In that case we don't want the space but
	      // expect something like prefix is "int (*"
	      // and postfix is ") [1]".
	      if (postfix.size() == 0)
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
	      prefix += ")";
	      prefix += bare_function_type;
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
	      // <Q><T>      		==> T Q
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
	      // <Q><T>      		==> T Q
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
	  delete qualifiers;
	if (failure)
	  _GLIBCPP_DEMANGLER_FAILURE;
	_GLIBCPP_DEMANGLER_RETURN2;
      }

    // <nested-name> ::= N [<CV-qualifiers>] <prefix> <unqualified-name> E
    //               ::= N [<CV-qualifiers>] <template-prefix> <template-args> E
    //
    // <prefix> ::= <prefix> <unqualified-name>
    //          ::= <template-prefix> <template-args>
    //          ::= # empty
    //          ::= <substitution>
    //
    // <template-prefix> ::= <prefix> <template unqualified-name>
    //                   ::= <substitution>
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_nested_name(string_type& output,
					     string_type& qualifiers)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_nested_name");

	if (current() != 'N' || M_pos >= M_maxpos)
	  _GLIBCPP_DEMANGLER_FAILURE;

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
	      _GLIBCPP_DEMANGLER_FAILURE;
	  }
	  else if (current() == 'I')
	  {
	    if (!decode_template_args(output))
	      _GLIBCPP_DEMANGLER_FAILURE;
	    if (current() != 'E')
	    {
	      // substitution: "<template-prefix> <template-args>".
	      add_substitution(substitution_start, nested_name_prefix,
		  	       number_of_prefixes);
	    }
	  }
	  else
	  {
	    if (!decode_unqualified_name(output))
	      _GLIBCPP_DEMANGLER_FAILURE;
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
	    _GLIBCPP_DEMANGLER_RETURN;
	  }
	  if (current() != 'I')
	    output += "::";
	  else if (M_template_args_need_space)
	    output += ' ';
	  M_template_args_need_space = false;
	}
	_GLIBCPP_DEMANGLER_FAILURE;
      }

    // <local-name> := Z <function encoding> E <entity name> [<discriminator>]
    //              := Z <function encoding> E s [<discriminator>]
    // <discriminator> := _ <non-negative number>
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_local_name(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_local_name");
	if (current() != 'Z' || M_pos >= M_maxpos)
	  _GLIBCPP_DEMANGLER_FAILURE;
	if ((M_pos += decode_encoding(output, M_str + M_pos + 1,
		M_maxpos - M_pos) + 1) < 0 || eat_current() != 'E')
	  _GLIBCPP_DEMANGLER_FAILURE;
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
	    _GLIBCPP_DEMANGLER_FAILURE;
	  output += nested_name_qualifiers;
	}
	string_type discriminator;
	if (current() == '_' && next() != 'n' && !decode_number(discriminator))
	  _GLIBCPP_DEMANGLER_FAILURE;
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <source-name> ::= <positive length number> <identifier>
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_source_name(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_source_name");
	int length = current() - '0';
	if (length < 1 || length > 9)
	  _GLIBCPP_DEMANGLER_FAILURE;
	while(std::isdigit(next()))
	  length = 10 * length + current() - '0';
	char const* ptr = &M_str[M_pos];
	if (length > 11 && !strncmp(ptr, "_GLOBAL_", 8) && ptr[9] == 'N'
	    && ptr[8] == ptr[10])
	{
	  output += "(anonymous namespace)";
	  if ((M_pos += length) > M_maxpos + 1)
	    _GLIBCPP_DEMANGLER_FAILURE;
	}
	else
	  while(length--)
	  {
	    if (current() == 0)
	      _GLIBCPP_DEMANGLER_FAILURE;
	    output += eat_current();
	  }
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <unqualified-name> ::= <operator-name>	# Starts with lower case.
    //                    ::= <ctor-dtor-name>  # Starts with 'C' or 'D'.
    //                    ::= <source-name>   	# Starts with a digit.
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_unqualified_name(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_unqualified_name");
	if (std::isdigit(current()))
	{
	  if (!M_inside_template_args)
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
	      _GLIBCPP_DEMANGLER_FAILURE;
	    if (!recursive_unqualified_name)
	      output += M_function_name;
	  }
	  else if (!decode_source_name(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
	  _GLIBCPP_DEMANGLER_RETURN;
	}
	if (islower(current()))
	{
	  if (!M_inside_template_args)
	  {
	    M_function_name.clear();
	    M_name_is_template = false;
	    M_name_is_cdtor = false;
	    M_name_is_conversion_operator = false;
	    if (!decode_operator_name(M_function_name))
	      _GLIBCPP_DEMANGLER_FAILURE;
	    output += M_function_name;
	  }
	  _GLIBCPP_DEMANGLER_RETURN;
	}
	if (current() == 'C' || current() == 'D')
	{
	  if (M_inside_template_args)
	    _GLIBCPP_DEMANGLER_FAILURE;
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
	      _GLIBCPP_DEMANGLER_FAILURE;
	  }
	  else
	  {
	    char c = next();
	    if (c < '0' || c > '2')
	      _GLIBCPP_DEMANGLER_FAILURE;
	    output += '~';
	    M_saw_destructor = true;
	  }
	  M_name_is_cdtor = true;
	  eat_current();
	  output += M_function_name;
	  _GLIBCPP_DEMANGLER_RETURN;
	}
	_GLIBCPP_DEMANGLER_FAILURE;
      }

    // <unscoped-name> ::=
    //   <unqualified-name>		# Starts not with an 'S'
    //   St <unqualified-name>	# ::std::
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_unscoped_name(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_unscoped_name");
	if (current() == 'S')
	{
	  if (next() != 't')
	    _GLIBCPP_DEMANGLER_FAILURE;
	  eat_current();
	  output += "std::";
	}
	decode_unqualified_name(output);
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <name> ::=
    //   <nested-name>				# Starts with 'N'
    //   <unscoped-template-name> <template-args> # idem
    //   <local-name>				# Starts with 'Z'
    //   <unscoped-name>			# Starts with 'S', 'C', 'D',
    //   					# a digit or a lower case
    //   					# character.
    //
    // <unscoped-template-name> ::= <unscoped-name>
    //                          ::= <substitution>
    template<typename Allocator>
      bool
      session<Allocator>::decode_name(string_type& output,
				      string_type& nested_name_qualifiers)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_name");
	int substitution_start = M_pos;
	if (current() == 'S' && (M_pos >= M_maxpos || M_str[M_pos + 1] != 't'))
	{
	  if (!decode_substitution(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
	}
	else if (current() == 'N')
	{
	  decode_nested_name(output, nested_name_qualifiers);
	  _GLIBCPP_DEMANGLER_RETURN;
	}
	else if (current() == 'Z')
	{
	  decode_local_name(output);
	  _GLIBCPP_DEMANGLER_RETURN;
	}
	else if (!decode_unscoped_name(output))
	  _GLIBCPP_DEMANGLER_FAILURE;
	if (current() == 'I')
	{
	  // Must have been an <unscoped-template-name>.
	  add_substitution(substitution_start, unscoped_template_name);
	  if (!decode_template_args(output))
	    _GLIBCPP_DEMANGLER_FAILURE;
	}
	M_template_args_need_space = false;
	_GLIBCPP_DEMANGLER_RETURN;
      }

    // <call-offset> ::= h <nv-offset> _
    //               ::= v <v-offset> _
    // <nv-offset>   ::= <offset number> 
    //     non-virtual base override
    //
    // <v-offset>    ::= <offset number> _ <virtual offset number>
    //     virtual base override, with vcall offset
    template<typename Allocator>
      bool
      session<Allocator>::decode_call_offset(string_type&
#if _GLIBCPP_DEMANGLER_CWDEBUG
	  output
#endif
	  )
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_call_offset");
	if (current() == 'h')
	{
	  string_type dummy;
	  eat_current();
	  if (decode_number(dummy) && current() == '_')
	  {
	    eat_current();
	    _GLIBCPP_DEMANGLER_RETURN;
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
	      _GLIBCPP_DEMANGLER_RETURN;
	    }
	  }
	}
	_GLIBCPP_DEMANGLER_FAILURE;
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
    //   				  initialization of static objects in
    //   				  a local scope.
    //   T <call-offset> <base encoding># base is the nominal target function
    //   				  of thunk.
    //   Tc <call-offset> <call-offset> <base encoding> # base is the nominal
    //                                    target function of thunk; first
    //                                    call-offset is 'this' adjustment;
    //					  second call-offset is result
    //					  adjustment
    //
    template<typename Allocator>
      bool
      session<Allocator>::decode_special_name(string_type& output)
      {
	_GLIBCPP_DEMANGLER_DOUT_ENTERING("decode_special_name");
	if (current() == 'G')
	{
	  if (next() != 'V')
	    _GLIBCPP_DEMANGLER_FAILURE;
	  output += "guard variable for ";
	  string_type nested_name_qualifiers;
	  eat_current();
	  if (!decode_name(output, nested_name_qualifiers))
	    _GLIBCPP_DEMANGLER_FAILURE;
	  output += nested_name_qualifiers;
	  _GLIBCPP_DEMANGLER_RETURN;
	}
	else if (current() != 'T')
	  _GLIBCPP_DEMANGLER_FAILURE;
	switch(next())
	{
	  case 'V':
	    output += "vtable for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCPP_DEMANGLER_RETURN;
	  case 'T':
	    output += "VTT for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCPP_DEMANGLER_RETURN;
	  case 'I':
	    output += "typeinfo for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCPP_DEMANGLER_RETURN;
	  case 'S':
	    output += "typeinfo name for ";
	    eat_current();
	    decode_type(output);
	    _GLIBCPP_DEMANGLER_RETURN;
	  case 'c':
	    output += "covariant return thunk to ";
	    if (!decode_call_offset(output)
		|| !decode_call_offset(output)
		|| (M_pos += decode_encoding(output, M_str + M_pos,
		    M_maxpos - M_pos + 1)) < 0)
	      _GLIBCPP_DEMANGLER_FAILURE;
	    _GLIBCPP_DEMANGLER_RETURN;
	  case 'C':		// GNU extension?
	  {
	    string_type first;
	    output += "construction vtable for ";
	    eat_current();
	    if (!decode_type(first))
	      _GLIBCPP_DEMANGLER_FAILURE;
	    while(std::isdigit(current()))
	      eat_current();
	    if (eat_current() != '_')
	      _GLIBCPP_DEMANGLER_FAILURE;
	    if (!decode_type(output))
	      _GLIBCPP_DEMANGLER_FAILURE;
	    output += "-in-";
	    output += first;
	    _GLIBCPP_DEMANGLER_RETURN;
	  }
	  default:
	    if (current() == 'v')
	      output += "virtual thunk to ";
	    else
	      output += "non-virtual thunk to ";
	    if (!decode_call_offset(output)
		|| (M_pos += decode_encoding(output, M_str + M_pos,
		    M_maxpos - M_pos + 1)) < 0)
	      _GLIBCPP_DEMANGLER_FAILURE;
	    _GLIBCPP_DEMANGLER_RETURN;
	}
      }

    // <encoding> ::=
    //   <function name> <bare-function-type>	# Starts with 'C', 'D', 'N',
    //                                        	  'S', a digit or a lower case
    //                                        	  character.
    //   <data name>				# Idem.
    //   <special-name>				# Starts with 'T' or 'G'.
    template<typename Allocator>
      int
      session<Allocator>::decode_encoding(string_type& output,
					  char const* in,
					  int len)
      {
#if _GLIBCPP_DEMANGLER_CWDEBUG
	_GLIBCPP_DEMANGLER_DOUT(dc::demangler,
	    "Output thus far: \"" << output << '"');
	string_type input(in, len > 0x40000000 ? strlen(in) : len);
	_GLIBCPP_DEMANGLER_DOUT(
	    dc::demangler, "Entering decode_encoding(\"" << input << "\")");
#endif
	if (len <= 0)
	  return INT_MIN;
	session<Allocator> demangler_session(in, len);
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
	if (demangler_session.M_name_is_template
	    && !(demangler_session.M_name_is_cdtor
	         || demangler_session.M_name_is_conversion_operator))
	{
	  if (!demangler_session.decode_type(output))
	      // Return type of function
	    return INT_MIN;
	  output += ' ';
	}
	output += name;
	if (!demangler_session.decode_bare_function_type(output))
	  return INT_MIN;
	output += nested_name_qualifiers;
	return demangler_session.M_pos;
      }

    } // namespace demangler

  // Public interface
  template<typename Allocator>
    struct demangle
    {
      typedef Allocator allocator_type;
      typedef std::basic_string<char, std::char_traits<char>, Allocator> 
	  string_type;
      static string_type symbol(char const* in);
      static string_type type(char const* in);
    };

  // demangle::symbol()
  //
  // Demangle `input' which should be a mangled function name as for
  // instance returned by nm(1).
  template<typename Allocator>
    std::basic_string<char, std::char_traits<char>, Allocator>
    demangle<Allocator>::symbol(char const* input)
    {
      // <mangled-name> ::= _Z <encoding>
      // <mangled-name> ::= _GLOBAL_ _<type>_ _Z <encoding>		
      //                    <type> can be I or D (GNU extension)
      typedef demangler::session<Allocator> demangler_type;
      string_type result;
      bool failure = (input[0] != '_');

      if (!failure)
      {
	if (input[1] == 'G')
	{
	  if (!strncmp(input, "_GLOBAL__", 9)
	      && (input[9] == 'D' || input[9] == 'I')
	      && input[10] == '_' && input[11] == '_' && input[12] == 'Z')
	  {
	    if (input[9] == 'D')
	      result.assign("global destructors keyed to ", 28);
	    else
	      result.assign("global constructors keyed to ", 29);
	    int cnt = demangler_type::decode_encoding(result, input + 13,
						      INT_MAX);
	    if (cnt < 0 || input[cnt + 13] != 0)
	      failure = true;
	  }
	  else
	    failure = true;
	}
	else if (input[1] == 'Z')
	{
	  int cnt = demangler_type::decode_encoding(result, input + 2,
						    INT_MAX);
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
  template<typename Allocator>
    std::basic_string<char, std::char_traits<char>, Allocator> 
    demangle<Allocator>::type(char const* input)
    {
      std::basic_string<char, std::char_traits<char>, Allocator> result;
      if (input == NULL)
	result = "(null)";
      else
      {
	demangler::session<Allocator> demangler_session(input, INT_MAX);
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
