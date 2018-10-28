/**
 * D header file for GNU/Linux
 */
/* Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */

module core.sys.linux.termios;

version (linux):
    public import core.sys.posix.termios;

    enum B57600 = 0x1001; // 0010001
    enum B115200 = 0x1002; // 0010002
    enum B230400 = 0x1003; // 0010003
    enum B460800 = 0x1004; // 0010004
    enum B500000 = 0x1005; // 0010005
    enum B576000 = 0x1006; // 0010006
    enum B921600 = 0x1007; // 0010007
    enum B1000000 = 0x1008; // 0010010
    enum B1152000 = 0x1009; // 0010011
    enum B1500000 = 0x100A; // 0010012
    enum B2000000 = 0x100B; // 0010013
    enum B2500000 = 0x100C; // 0010014
    enum B3000000 = 0x100D; // 0010015
    enum B3500000 = 0x100E; // 0010016
    enum B4000000 = 0x100F; // 0010017

    enum CRTSCTS = 0x80000000; // 020000000000
