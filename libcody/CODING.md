# Coding standard

I guess I should document this, it might not be obvious.

libcody is implemented in C++11.  Because it's used in compiler
development, we can't use the latest and greatest.

The formatting is close to GNU, but with a few differences.

## Extensions to C++11

It uses __VA_OPT__ when available, falling back on GNU's variadic
macro `,#` extension.  This is in the `Assert` macro, so one can have
multi-argument template instantiations there.  Not that libcody does
that, but this is code I used elsewhere.

## GNU

The underlying formatting is GNU style.  Here are a few notes about
things that commonly catches programmers unfamiliar with it is:

* Spaces between binary operators.  Particularly in a function call,
  between the name and the open paren:

  ```c++
  Fn (a + b, ary[4], *ptr);
  ```

  In general GNU style uses a lot more whitespace than Clang-style.
  We're not trying to cram as much code as possible onto a page!

* Scope braces are always on a line of their own, indented by 2
  spaces, if they're a sub-statement of an `if`, `for` or whatever:

  ```c++
  if (bob)
    {
      Frob ();
      Quux ();
    }
  ```

  Conditions and loops containing a single statement should not use `{}`.
  FWIW this was my personal indentation scheme, before I even met GNU code!

* The same is true for a function definition body, except the
  indentation is zero:

  ```c++
  int Foo ()
    noexcept // indented
  {
    return 0;
  }
  ```

* Initialization bracing is not like scope bracing.  There tends to be
  more flexibility.

* Break lines at 80 chars, this should be /before/ the operator, not after:

  ```c++
  a = (b
       + c);
  ptr
  ->MemberFn (stuff);
  Func
  (arg);
  ```

  Thus you can tell what lines are continued from the previous by
  looking at their start.  Use parens to control indentation.

  If you find yourself wanting to break a line at `.`, don't.
  Refactor your code to avoid needing that.

* Template instantiations and C++ casts should have no space before the `<`:

  ```c++
  std::vector<int> k;
  static_cast<T> (arg); // space before the ( though
  ```

* Pointer and reference types need a space before the `*` or `&`, if
  the preceding token is ascii text (a cpp-identifier):

  ```
  int *ptr;
  int **ptr_ptr;
  int *&pref = ptr;
  ```

  See below a difference in qualifier placement.

* Code should compile without warnings.

## Not GNU

### Names

Unlike GNU code, variants of Camel Case are used.  use `PascalCase`
for function, type and global variable names.  Use `dromedaryCase` for
member variables.  Block-scope vars can be `dromedaryCase` or
`snake_case`, your choice.

### Type qualifiers

Type qualifiers go after the thing they qualify.  You have to do this
for pointers anyway, and read them inside-out, because, C Just being
consistent:

```c++
int const foo = 5; // constant int
int *const pfoo = nullptr;  // constant pointer to int
```
