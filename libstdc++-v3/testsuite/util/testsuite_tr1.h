// -*- C++ -*-
// Testing utilities for the tr1 testsuite.
//
// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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
//

#ifndef _GLIBCXX_TESTSUITE_TR1_H
#define _GLIBCXX_TESTSUITE_TR1_H

#include <ext/type_traits.h>

namespace __gnu_test
{
  // For tr1/type_traits.
  template<template<typename> class Category, typename Type>
#if __cplusplus >= 201103L
    constexpr
#endif
    bool
    test_category(bool value)
    {
      return (Category<Type>::value == value
	      && Category<const Type>::value == value
	      && Category<volatile Type>::value == value
	      && Category<const volatile Type>::value == value
	      && Category<Type>::type::value == value
	      && Category<const Type>::type::value == value
	      && Category<volatile Type>::type::value == value
	      && Category<const volatile Type>::type::value == value);
    }

  template<template<typename> class Property, typename Type>
#if __cplusplus >= 201103L
    constexpr
#endif
    bool
    test_property(typename Property<Type>::value_type value)
    {
      return (Property<Type>::value == value
	      && Property<Type>::type::value == value);
    }

  // For testing tr1/type_traits/extent, which has a second template
  // parameter.
  template<template<typename, unsigned> class Property,
	   typename Type, unsigned Uint>
#if __cplusplus >= 201103L
    constexpr
#endif
    bool
    test_property(typename Property<Type, Uint>::value_type value)
    {
      return (Property<Type, Uint>::value == value
	      && Property<Type, Uint>::type::value == value);
    }

#if __cplusplus >= 201103L
  template<template<typename...> class Property,
	   typename Type1, typename... Types>
    constexpr bool
    test_property(typename Property<Type1, Types...>::value_type value)
    {
      return (Property<Type1, Types...>::value == value
	      && Property<Type1, Types...>::type::value == value);
    }
#endif

  template<template<typename, typename> class Relationship,
	   typename Type1, typename Type2>
#if __cplusplus >= 201103L
    constexpr
#endif
    bool
    test_relationship(bool value)
    {
      return (Relationship<Type1, Type2>::value == value
	      && Relationship<Type1, Type2>::type::value == value);
    }

  // Test types.
  class ClassType { };
  typedef const ClassType           cClassType;
  typedef volatile ClassType        vClassType;
  typedef const volatile ClassType  cvClassType;

  class DerivedType : public ClassType { };

  enum EnumType { e0 };

  struct ConvType
  { operator int() const; };

  class AbstractClass
  {
    virtual void rotate(int) = 0;
  };

  class PolymorphicClass
  {
    virtual void rotate(int);
  };

  class DerivedPolymorphic : public PolymorphicClass { };

  class VirtualDestructorClass
  {
    virtual ~VirtualDestructorClass();
  };

  union UnionType { };

  class IncompleteClass;

  struct ExplicitClass
  {
    ExplicitClass(double&);
    explicit ExplicitClass(int&);
    ExplicitClass(double&, int&, double&);
  };

  struct NothrowExplicitClass
  {
    NothrowExplicitClass(double&) throw();
    explicit NothrowExplicitClass(int&) throw();
    NothrowExplicitClass(double&, int&, double&) throw();
  };

  struct ThrowExplicitClass
  {
    ThrowExplicitClass(double&) throw(int);
    explicit ThrowExplicitClass(int&) throw(int);
    ThrowExplicitClass(double&, int&, double&) throw(int);
  };

  struct ThrowDefaultClass
  {
    ThrowDefaultClass() throw(int);
  };

  struct ThrowCopyConsClass
  {
    ThrowCopyConsClass(const ThrowCopyConsClass&) throw(int);
  };

#if __cplusplus >= 201103L
  struct ThrowMoveConsClass
  {
    ThrowMoveConsClass(ThrowMoveConsClass&&) throw(int);
  };

  struct NoexceptExplicitClass
  {
    NoexceptExplicitClass(double&) noexcept(true);
    explicit NoexceptExplicitClass(int&) noexcept(true);
    NoexceptExplicitClass(double&, int&, double&) noexcept(true);
  };

  struct ExceptExplicitClass
  {
    ExceptExplicitClass(double&) noexcept(false);
    explicit ExceptExplicitClass(int&) noexcept(false);
    ExceptExplicitClass(double&, int&, double&) noexcept(false);
  };

  struct NoexceptDefaultClass
  {
    NoexceptDefaultClass() noexcept(true);
  };

  struct ExceptDefaultClass
  {
    ExceptDefaultClass() noexcept(false);
  };

  struct NoexceptCopyConsClass
  {
    NoexceptCopyConsClass(const NoexceptCopyConsClass&) noexcept(true);
  };

  struct ExceptCopyConsClass
  {
    ExceptCopyConsClass(const ExceptCopyConsClass&) noexcept(false);
  };

  struct NoexceptMoveConsClass
  {
    NoexceptMoveConsClass(NoexceptMoveConsClass&&) noexcept(true);
    NoexceptMoveConsClass& operator=(NoexceptMoveConsClass&&) = default;
  };

  struct ExceptMoveConsClass
  {
    ExceptMoveConsClass(ExceptMoveConsClass&&) noexcept(false);
  };

  struct NoexceptCopyAssignClass
  {
    NoexceptCopyAssignClass&
    operator=(const NoexceptCopyAssignClass&) noexcept(true);
  };

  struct ExceptCopyAssignClass
  {
    ExceptCopyAssignClass&
    operator=(const ExceptCopyAssignClass&) noexcept(false);
  };

  struct NoexceptMoveAssignClass
  {
    NoexceptMoveAssignClass(NoexceptMoveAssignClass&&) = default;
    NoexceptMoveAssignClass&
    operator=(NoexceptMoveAssignClass&&) noexcept(true);
  };

  struct ExceptMoveAssignClass
  {
    ExceptMoveAssignClass&
    operator=(ExceptMoveAssignClass&&) noexcept(false);
  };

  struct DeletedCopyAssignClass
  {
    DeletedCopyAssignClass&
    operator=(const DeletedCopyAssignClass&) = delete;
  };

  struct DeletedMoveAssignClass
  {
    DeletedMoveAssignClass&
    operator=(DeletedMoveAssignClass&&) = delete;
  };

  struct NoexceptMoveConsNoexceptMoveAssignClass
  {
    NoexceptMoveConsNoexceptMoveAssignClass
    (NoexceptMoveConsNoexceptMoveAssignClass&&) noexcept(true);

    NoexceptMoveConsNoexceptMoveAssignClass&
    operator=(NoexceptMoveConsNoexceptMoveAssignClass&&) noexcept(true);
  };

  struct ExceptMoveConsNoexceptMoveAssignClass
  {
    ExceptMoveConsNoexceptMoveAssignClass
    (ExceptMoveConsNoexceptMoveAssignClass&&) noexcept(false);

    ExceptMoveConsNoexceptMoveAssignClass&
    operator=(ExceptMoveConsNoexceptMoveAssignClass&&) noexcept(true);
  };

  struct NoexceptMoveConsExceptMoveAssignClass
  {
    NoexceptMoveConsExceptMoveAssignClass
    (NoexceptMoveConsExceptMoveAssignClass&&) noexcept(true);

    NoexceptMoveConsExceptMoveAssignClass&
    operator=(NoexceptMoveConsExceptMoveAssignClass&&) noexcept(false);
  };

  struct ExceptMoveConsExceptMoveAssignClass
  {
    ExceptMoveConsExceptMoveAssignClass
    (ExceptMoveConsExceptMoveAssignClass&&) noexcept(false);

    ExceptMoveConsExceptMoveAssignClass&
    operator=(ExceptMoveConsExceptMoveAssignClass&&) noexcept(false);
  };
#endif

  struct NType   // neither trivial nor standard-layout
  {
    int i;
    int j;
    virtual ~NType();
  };

  struct TType   // trivial but not standard-layout
  {
    int i;
  private:
    int j;
  };

  struct SLType  // standard-layout but not trivial
  {
    int i;
    int j;
    ~SLType();
  };

  struct PODType // both trivial and standard-layout
  {
    int i;
    int j;
  };

#if __cplusplus >= 201103L
  struct LType // literal type
  {
    int _M_i;

    constexpr LType(int __i) : _M_i(__i) { }
  };

  struct LTypeDerived : public LType
  {
    constexpr LTypeDerived(int __i) : LType(__i) { }
  };

  struct NLType // not literal type
  {
    int _M_i;

    NLType() : _M_i(0) { }

    constexpr NLType(int __i) : _M_i(__i) { }

    NLType(const NLType& __other) : _M_i(__other._M_i) { }

    ~NLType() { _M_i = 0; }
  };
#endif

  int truncate_float(float x) { return (int)x; }
  long truncate_double(double x) { return (long)x; }

  struct do_truncate_float_t
  {
    do_truncate_float_t()
    {
      ++live_objects;
    }

    do_truncate_float_t(const do_truncate_float_t&)
    {
      ++live_objects;
    }

    ~do_truncate_float_t()
    {
      --live_objects;
    }

    int operator()(float x) { return (int)x; }

    static int live_objects;
  };

  int do_truncate_float_t::live_objects = 0;

  struct do_truncate_double_t
  {
    do_truncate_double_t()
    {
     ++live_objects;
    }

    do_truncate_double_t(const do_truncate_double_t&)
    {
      ++live_objects;
    }

    ~do_truncate_double_t()
    {
      --live_objects;
    }

    long operator()(double x) { return (long)x; }

    static int live_objects;
  };

  int do_truncate_double_t::live_objects = 0;

  struct X
  {
    int bar;

    int foo()                   { return 1; }
    int foo_c() const           { return 2; }
    int foo_v()  volatile       { return 3; }
    int foo_cv() const volatile { return 4; }
  };

  // For use in 8_c_compatibility.
  template<typename R, typename T>
    typename __gnu_cxx::__enable_if<std::__are_same<R, T>::__value,
				    bool>::__type
    check_ret_type(T)
    { return true; }

#if __cplusplus >= 201103L
  namespace construct
  {
    struct Empty {};

    struct B { int i; B(){} };
    struct D : B {};

    enum E { ee1 };
    enum E2 { ee2 };
    enum class SE { e1 };
    enum class SE2 { e2 };

    enum OpE : int;
    enum class OpSE : bool;

    union U { int i; Empty b; };

    struct Abstract
    {
      virtual ~Abstract() = 0;
    };

    struct AbstractDelDtor
    {
      ~AbstractDelDtor() = delete;
      virtual void foo() = 0;
    };

    struct Ukn;

    template<class To>
      struct ImplicitTo
      {
	operator To();
      };

    template<class To>
      struct DelImplicitTo
      {
	operator To() = delete;
      };

    template<class To>
      struct ExplicitTo
      {
	explicit operator To();
      };

    struct Ellipsis
    {
      Ellipsis(...){}
    };

    struct DelEllipsis
    {
      DelEllipsis(...) = delete;
    };

    struct Any
    {
      template<class T>
      Any(T&&){}
    };

    struct nAny
    {
      template<class... T>
      nAny(T&&...){}
    };

    struct DelnAny
    {
      template<class... T>
        DelnAny(T&&...) = delete;
    };

    template<class... Args>
      struct FromArgs
      {
	FromArgs(Args...);
      };

    struct DelDef
    {
      DelDef() = delete;
    };

    struct DelCopy
    {
      DelCopy(const DelCopy&) = delete;
    };

    struct DelDtor
    {
      DelDtor() = default;
      DelDtor(const DelDtor&) = default;
      DelDtor(DelDtor&&) = default;
      DelDtor(int);
      DelDtor(int, B, U);
      ~DelDtor() = delete;
    };

    struct Nontrivial
    {
      Nontrivial();
      Nontrivial(const Nontrivial&);
      Nontrivial& operator=(const Nontrivial&);
      ~Nontrivial();
    };

    union NontrivialUnion
    {
      int i;
      Nontrivial n;
    };

    struct UnusualCopy
    {
      UnusualCopy(UnusualCopy&);
    };
  }

  namespace destruct
  {
    struct E
    {};
    
    struct NTD1
    {
      ~NTD1() = default;
    };
    
    struct NTD2
    {
      ~NTD2();
    };
    
    struct NTD3
    {
      ~NTD3() throw();
    };
    
    struct TD1
    {
      ~TD1() noexcept(false);
    };
    
    struct TD2
    {
      ~TD2() throw(int);
    };
    
    struct Aggr
    {
      int i;
      bool b;
      E e;
    };
    
    struct Aggr2
    {
      int i;
      bool b;
      TD1 r;
    };
    
    struct Del
    {
      ~Del() = delete;
    };
    
    struct Del2
    {
      ~Del2() noexcept = delete;
    };
    
    struct Del3
    {
      ~Del3() noexcept(false) = delete;
    };
    
    struct Der : Aggr
    {};
    
    struct Der2 : Aggr2
    {};
    
    union U1
    {
      int i;
      double d;
      void* p;
      TD1* pt;
    };
    
    union Ut
    {
      int i;
      double d;
      void* p;
      TD1 pt;
    };
    
    enum class En { a, b, c, d };
    enum En2 { En2a, En2b, En2c, En2d };

    enum OpE : int;
    enum class OpSE : bool;

    struct Abstract1
    {
      virtual ~Abstract1() = 0;
    };
    
    struct AbstractDelDtor
    {
      ~AbstractDelDtor() = delete;
      virtual void foo() = 0;
    };

    struct Abstract2
    {
      virtual ~Abstract2() noexcept(false) = 0;
    };
    
    struct Abstract3
    {
      ~Abstract3() noexcept(false);
      virtual void foo() noexcept = 0;
    };

    struct Nontrivial
    {
      Nontrivial();
      Nontrivial(const Nontrivial&);
      Nontrivial& operator=(const Nontrivial&);
      ~Nontrivial();
    };

    union NontrivialUnion
    {
      int i;
      Nontrivial n;
    };

    struct UnusualCopy
    {
      UnusualCopy(UnusualCopy&);
    };

    struct Ellipsis
    {
      Ellipsis(...){}
    };

    struct DelEllipsis
    {
      DelEllipsis(...) = delete;
    };

    struct DelDef
    {
      DelDef() = delete;
    };

    struct DelCopy
    {
      DelCopy(const DelCopy&) = delete;
    };
  }
  
  namespace assign
  {
    struct Empty {};

    struct B { int i; B(){} };
    struct D : B {};

    enum E { ee1 };
    enum E2 { ee2 };
    enum class SE { e1 };
    enum class SE2 { e2 };

    enum OpE : int;
    enum class OpSE : bool;

    union U { int i; Empty b; };

    union UAssignAll
    {
      bool b;
      char c;
      template<class T>
      void operator=(T&&);
    };

    union UDelAssignAll
    {
      bool b;
      char c;
      template<class T>
      void operator=(T&&) = delete;
    };

    struct Abstract
    {
      virtual ~Abstract() = 0;
    };

    struct AbstractDelDtor
    {
      ~AbstractDelDtor() = delete;
      virtual void foo() = 0;
    };

    struct Ukn;

    template<class To>
      struct ImplicitTo
      {
	operator To();
      };

    template<class To>
      struct ExplicitTo
      {
	explicit operator To();
      };

    template<class To>
      struct DelImplicitTo
      {
	operator To() = delete;
      };

    template<class To>
      struct DelExplicitTo
      {
	explicit operator To() = delete;
      };

    struct Ellipsis
    {
      Ellipsis(...){}
    };

    struct DelEllipsis
    {
      DelEllipsis(...) = delete;
    };

    struct Any
    {
      template<class T>
        Any(T&&){}
    };

    struct nAny
    {
      template<class... T>
        nAny(T&&...){}
    };

    struct DelnAny
    {
      template<class... T>
        DelnAny(T&&...) = delete;
    };

    template<class... Args>
      struct FromArgs
      {
	FromArgs(Args...);
      };

    template<class... Args>
      struct DelFromArgs
      {
	DelFromArgs(Args...) = delete;
      };

    struct DelDef
    {
      DelDef() = delete;
    };

    struct DelCopy
    {
      DelCopy(const DelCopy&) = delete;
    };

    struct DelDtor
    {
      DelDtor() = default;
      DelDtor(const DelDtor&) = default;
      DelDtor(DelDtor&&) = default;
      DelDtor(int);
      DelDtor(int, B, U);
      ~DelDtor() = delete;
    };

    struct Nontrivial
    {
      Nontrivial();
      Nontrivial(const Nontrivial&);
      Nontrivial& operator=(const Nontrivial&);
      ~Nontrivial();
    };

    union NontrivialUnion
    {
      int i;
      Nontrivial n;
    };

    struct UnusualCopy
    {
      UnusualCopy(UnusualCopy&);
    };

    struct AnyAssign
    {
      template<class T>
        void operator=(T&&);
    };

    struct DelAnyAssign
    {
      template<class T>
        void operator=(T&&) = delete;
    };

    struct DelCopyAssign
    {
      DelCopyAssign& operator=(const DelCopyAssign&) = delete;
      DelCopyAssign& operator=(DelCopyAssign&&) = default;
    };

    struct MO
    {
      MO(MO&&) = default;
      MO& operator=(MO&&) = default;
    };
  }

  struct CopyConsOnlyType
  {
    CopyConsOnlyType(int) { }
    CopyConsOnlyType(CopyConsOnlyType&&) = delete;
    CopyConsOnlyType(const CopyConsOnlyType&) = default;
    CopyConsOnlyType& operator=(const CopyConsOnlyType&) = delete;
    CopyConsOnlyType& operator=(CopyConsOnlyType&&) = delete;
  };

  struct MoveConsOnlyType
  {
    MoveConsOnlyType(int) { }
    MoveConsOnlyType(const MoveConsOnlyType&) = delete;
    MoveConsOnlyType(MoveConsOnlyType&&) = default;
    MoveConsOnlyType& operator=(const MoveConsOnlyType&) = delete;
    MoveConsOnlyType& operator=(MoveConsOnlyType&&) = delete;
  };
#endif

} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_TR1_H
