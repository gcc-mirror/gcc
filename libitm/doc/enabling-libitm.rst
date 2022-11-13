..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _enabling-libitm:

Enabling libitm
---------------

To activate support for TM in C/C++, the compile-time flag :option:`-fgnu-tm`
must be specified. This enables TM language-level constructs such as
transaction statements (e.g., ``__transaction_atomic``, see :ref:`c-c++-language-constructs-for-tm` for details).