# Flags to pass to gen-params when building _G_config.h. 
# For example: G_CONFIG_ARGS = size_t="unsigned long" 
G_CONFIG_ARGS = DOLLAR_IN_LABEL=1 

# 
# gen-params tries to determine whether or not printf_fp exists by
# simply compiling a test program. Since MPE is by definition a
# shared runtime environment, this won't work unless the resulting
# program is run. Simply run _G_config.h through a sed script to
# update the values accordingly. 
# 
_G_CONFIG_H = stmp-Gconfig

stmp-Gconfig: _G_config.h 
      sed -e "s/_G_HAVE_PRINTF_FP 1/_G_HAVE_PRINTF_FP 0/" \ 
          -e "s/_G_HAVE_LONG_DOUBLE_IO 1/_G_HAVE_LONG_DOUBLE_IO 0/" \
          <_G_config.h > tmp-config.h 
      mv -f tmp-config.h _G_config.h 
      touch stmp-Gconfig 
      $(MAKE) $(FLAGS_TO_PASS) _G_CONFIG_H=_G_config.h all 
