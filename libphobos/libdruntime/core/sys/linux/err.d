/**
  * D header file for Linux err.h.
  *
  * Copyright: Copyright © 2019, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Ernesto Castellotti
  */
module core.sys.linux.err;
import core.stdc.stdarg : va_list;

version (linux):
extern (C):
nothrow:
@nogc:

void err(int eval, scope const char* fmt, ...);
void errx(int eval, scope const char* fmt, ...);
void warn(scope const char* fmt, ...);
void warnx(scope const char* fmt, ...);
void verr(int eval, scope const char* fmt, va_list args);
void verrx(int eval, scope const char* fmt, va_list args);
void vwarn(scope const char* fmt, va_list args);
void vwarnx(scope const char* fmt, va_list args);
