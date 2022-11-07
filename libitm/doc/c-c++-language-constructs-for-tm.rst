..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c-c++-language-constructs-for-tm:

C/C++ Language Constructs for TM
--------------------------------

Transactions are supported in C++ and C in the form of transaction statements,
transaction expressions, and function transactions. In the following example,
both ``a`` and ``b`` will be read and the difference will be written to
``c``, all atomically and isolated from other transactions:

.. code-block:: c++

  __transaction_atomic { c = a - b; }

Therefore, another thread can use the following code to concurrently update
``b`` without ever causing ``c`` to hold a negative value (and without
having to use other synchronization constructs such as locks or C++11
atomics):

.. code-block:: c++

  __transaction_atomic { if (a > b) b++; }

GCC follows the `Draft
Specification of Transactional Language Constructs for C++ (v1.1) <https://sites.google.com/site/tmforcplusplus/>`_ in its
implementation of transactions.

The precise semantics of transactions are defined in terms of the C++11/C11
memory model (see the specification). Roughly, transactions provide
synchronization guarantees that are similar to what would be guaranteed when
using a single global lock as a guard for all transactions. Note that like
other synchronization constructs in C/C++, transactions rely on a
data-race-free program (e.g., a nontransactional write that is concurrent
with a transactional read to the same memory location is a data race).