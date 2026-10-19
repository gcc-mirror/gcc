// { dg-options "-Os -g0" }
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "E4loadESt12memory_order" } }

#include <atomic>

enum E { Ea, Eb = 1000 };

std::atomic<bool> b;
std::atomic<unsigned char> u;
std::atomic<float> f;
std::atomic<int*> p;
std::atomic<enum E> e;
int sink;

void bool_1() { if (b.load(std::memory_order_relaxed)) sink = 1; }
void bool_2() { if (b.load(std::memory_order_relaxed)) sink = 2; }
void bool_3() { if (b.load(std::memory_order_relaxed)) sink = 3; }
void bool_4() { if (b.load(std::memory_order_relaxed)) sink = 4; }
void bool_5() { if (b.load(std::memory_order_relaxed)) sink = 5; }
void bool_6() { if (b.load(std::memory_order_relaxed)) sink = 6; }
void bool_7() { if (b.load(std::memory_order_relaxed)) sink = 7; }
void bool_8() { if (b.load(std::memory_order_relaxed)) sink = 8; }
void bool_9() { if (b.load(std::memory_order_relaxed)) sink = 9; }
void bool_10() { if (b.load(std::memory_order_relaxed)) sink = 10; }

void uchar_1() { if (u.load(std::memory_order_relaxed)) sink = 1; }
void uchar_2() { if (u.load(std::memory_order_relaxed)) sink = 2; }
void uchar_3() { if (u.load(std::memory_order_relaxed)) sink = 3; }
void uchar_4() { if (u.load(std::memory_order_relaxed)) sink = 4; }
void uchar_5() { if (u.load(std::memory_order_relaxed)) sink = 5; }
void uchar_6() { if (u.load(std::memory_order_relaxed)) sink = 6; }
void uchar_7() { if (u.load(std::memory_order_relaxed)) sink = 7; }
void uchar_8() { if (u.load(std::memory_order_relaxed)) sink = 8; }
void uchar_9() { if (u.load(std::memory_order_relaxed)) sink = 9; }
void uchar_10() { if (u.load(std::memory_order_relaxed)) sink = 10; }

void float_1() { if (f.load(std::memory_order_relaxed)) sink = 1; }
void float_2() { if (f.load(std::memory_order_relaxed)) sink = 2; }
void float_3() { if (f.load(std::memory_order_relaxed)) sink = 3; }
void float_4() { if (f.load(std::memory_order_relaxed)) sink = 4; }
void float_5() { if (f.load(std::memory_order_relaxed)) sink = 5; }
void float_6() { if (f.load(std::memory_order_relaxed)) sink = 6; }
void float_7() { if (f.load(std::memory_order_relaxed)) sink = 7; }
void float_8() { if (f.load(std::memory_order_relaxed)) sink = 8; }
void float_9() { if (f.load(std::memory_order_relaxed)) sink = 9; }
void float_10() { if (f.load(std::memory_order_relaxed)) sink = 10; }

void ptr_1() { if (p.load(std::memory_order_relaxed)) sink = 1; }
void ptr_2() { if (p.load(std::memory_order_relaxed)) sink = 2; }
void ptr_3() { if (p.load(std::memory_order_relaxed)) sink = 3; }
void ptr_4() { if (p.load(std::memory_order_relaxed)) sink = 4; }
void ptr_5() { if (p.load(std::memory_order_relaxed)) sink = 5; }
void ptr_6() { if (p.load(std::memory_order_relaxed)) sink = 6; }
void ptr_7() { if (p.load(std::memory_order_relaxed)) sink = 7; }
void ptr_8() { if (p.load(std::memory_order_relaxed)) sink = 8; }
void ptr_9() { if (p.load(std::memory_order_relaxed)) sink = 9; }
void ptr_10() { if (p.load(std::memory_order_relaxed)) sink = 10; }

void enum_1() { if (e.load(std::memory_order_relaxed)) sink = 1; }
void enum_2() { if (e.load(std::memory_order_relaxed)) sink = 2; }
void enum_3() { if (e.load(std::memory_order_relaxed)) sink = 3; }
void enum_4() { if (e.load(std::memory_order_relaxed)) sink = 4; }
void enum_5() { if (e.load(std::memory_order_relaxed)) sink = 5; }
void enum_6() { if (e.load(std::memory_order_relaxed)) sink = 6; }
void enum_7() { if (e.load(std::memory_order_relaxed)) sink = 7; }
void enum_8() { if (e.load(std::memory_order_relaxed)) sink = 8; }
void enum_9() { if (e.load(std::memory_order_relaxed)) sink = 9; }
void enum_10() { if (e.load(std::memory_order_relaxed)) sink = 10; }
