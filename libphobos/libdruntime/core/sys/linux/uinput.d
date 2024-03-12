module core.sys.linux.uinput;

version (linux):
extern(C):
nothrow:

import core.sys.posix.sys.ioctl;

public import core.sys.linux.input;
public import core.sys.linux.input_event_codes;

enum UINPUT_VERSION = 5;
enum UINPUT_MAX_NAME_SIZE = 80;

struct uinput_ff_upload
{
    uint            request_id;
    int             retval;
    ff_effect       effect;
    ff_effect       old;
}

struct uinput_ff_erase
{
    uint           request_id;
    int            retval;
    uint           effect_id;
}

enum UINPUT_IOCTL_BASE  = 'U';
enum UI_DEV_CREATE              = _IO(UINPUT_IOCTL_BASE, 1);
enum UI_DEV_DESTROY             = _IO(UINPUT_IOCTL_BASE, 2);

struct uinput_setup
{
    input_id id;
    char[UINPUT_MAX_NAME_SIZE] name;
    uint ff_effects_max;
}

enum UI_DEV_SETUP = _IOW!uinput_setup(UINPUT_IOCTL_BASE, 3);

struct uinput_abs_setup
{
    ushort  code;

    input_absinfo absinfo;
}

enum UI_ABS_SETUP = _IOW!uinput_abs_setup(UINPUT_IOCTL_BASE, 4);

enum UI_SET_EVBIT               = _IOW!int(UINPUT_IOCTL_BASE, 100);
enum UI_SET_KEYBIT              = _IOW!int(UINPUT_IOCTL_BASE, 101);
enum UI_SET_RELBIT              = _IOW!int(UINPUT_IOCTL_BASE, 102);
enum UI_SET_ABSBIT              = _IOW!int(UINPUT_IOCTL_BASE, 103);
enum UI_SET_MSCBIT              = _IOW!int(UINPUT_IOCTL_BASE, 104);
enum UI_SET_LEDBIT              = _IOW!int(UINPUT_IOCTL_BASE, 105);
enum UI_SET_SNDBIT              = _IOW!int(UINPUT_IOCTL_BASE, 106);
enum UI_SET_FFBIT               = _IOW!int(UINPUT_IOCTL_BASE, 107);
enum UI_SET_PHYS                = _IOW!(char*)(UINPUT_IOCTL_BASE, 108);
enum UI_SET_SWBIT               = _IOW!int(UINPUT_IOCTL_BASE, 109);
enum UI_SET_PROPBIT             = _IOW!int(UINPUT_IOCTL_BASE, 110);

enum UI_BEGIN_FF_UPLOAD = _IOWR!uinput_ff_upload(UINPUT_IOCTL_BASE, 200);
enum UI_END_FF_UPLOAD   = _IOW!uinput_ff_upload(UINPUT_IOCTL_BASE, 201);
enum UI_BEGIN_FF_ERASE  = _IOWR!uinput_ff_erase(UINPUT_IOCTL_BASE, 202);
enum UI_END_FF_ERASE    = _IOW!uinput_ff_erase(UINPUT_IOCTL_BASE, 203);

enum UI_GET_SYSNAME(len) = _IOC(_IOC_READ, UINPUT_IOCTL_BASE, 44, len);

enum UI_GET_VERSION             = _IOR!uint(UINPUT_IOCTL_BASE, 45);

enum EV_UINPUT          = 0x0101;
enum UI_FF_UPLOAD               = 1;
enum UI_FF_ERASE                = 2;

struct uinput_user_dev {
    char[UINPUT_MAX_NAME_SIZE] name;
    input_id id;
    uint ff_effects_max;
    int[ABS_CNT] absmax;
    int[ABS_CNT] absmin;
    int[ABS_CNT] absfuzz;
    int[ABS_CNT] absflat;
}
