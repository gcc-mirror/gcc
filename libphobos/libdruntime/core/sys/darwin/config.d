/**
 * D header file for Darwin.
 *
 * Copyright: Copyright (c) 2021 D Language Foundation
 * Authors: Iain Buclaw
 */
module core.sys.darwin.config;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):

public import core.sys.posix.config;

enum __MAC_10_0    = 100000;
enum __MAC_10_1    = 100100;
enum __MAC_10_2    = 100200;
enum __MAC_10_3    = 100300;
enum __MAC_10_4    = 100400;
enum __MAC_10_5    = 100500;
enum __MAC_10_6    = 100600;
enum __MAC_10_7    = 100700;
enum __MAC_10_8    = 100800;
enum __MAC_10_9    = 100900;
enum __MAC_10_10   = 101000;
enum __MAC_10_10_2 = 101002;
enum __MAC_10_10_3 = 101003;
enum __MAC_10_11   = 101100;
enum __MAC_10_11_2 = 101102;
enum __MAC_10_11_3 = 101103;
enum __MAC_10_11_4 = 101104;
enum __MAC_10_12   = 101200;
enum __MAC_10_12_1 = 101201;
enum __MAC_10_12_2 = 101202;
enum __MAC_10_12_4 = 101204;
enum __MAC_10_13   = 101300;
enum __MAC_10_13_1 = 101301;
enum __MAC_10_13_2 = 101302;
enum __MAC_10_13_4 = 101304;
enum __MAC_10_14   = 101400;
enum __MAC_10_14_1 = 101401;
enum __MAC_10_14_4 = 101404;
enum __MAC_10_15   = 101500;
enum __MAC_10_15_1 = 101501;
enum __MAC_10_16   = 101501;
enum __MAC_11_0    = 110000;
