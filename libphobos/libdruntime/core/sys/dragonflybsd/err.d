/**
  * D header file for DragonFlyBSD err.h.
  *
  * Copyright: Copyright © 2019, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Ernesto Castellotti
  */
module core.sys.dragonflybsd.err;
import core.stdc.stdarg : va_list;

version (DragonFlyBSD):
extern (C):
nothrow:
@nogc:

alias ExitFunction = void function(int);

void err(int eval, scope const char* fmt, ...);
void errc(int eval, int code, scope const char* fmt, ...);
void errx(int eval, scope const char* fmt, ...);
void warn(scope const char* fmt, ...);
void warnc(int code, scope const char* fmt, ...);
void warnx(scope const char* fmt, ...);
void verr(int eval, scope const char* fmt, va_list args);
void verrc(int eval, int code, scope const char* fmt, va_list args);
void verrx(int eval, scope const char* fmt, va_list args);
void vwarn(scope const char* fmt, va_list args);
void vwarnc(int code, scope const char* fmt, va_list args);
void vwarnx(scope const char* fmt, va_list args);
void err_set_file(void* vfp);
void err_set_exit(ExitFunction exitf);
