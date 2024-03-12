module core.sys.linux.input;

version (linux):
extern(C):
nothrow:

import core.sys.linux.sys.time;
import core.sys.posix.sys.ioctl;

public import core.sys.linux.input_event_codes;

struct input_event
{
    timeval time;
    ushort type;
    ushort code;
    int value;
}

enum EV_VERSION     = 0x010001;

struct input_id
{
    ushort bustype;
    ushort vendor;
    ushort product;
    ushort version_;
}

struct input_absinfo
{
    int value;
    int minimum;
    int maximum;
    int fuzz;
    int flat;
    int resolution;
}

enum INPUT_KEYMAP_BY_INDEX = (1 << 0);
struct input_keymap_entry
{
    ubyte  flags;
    ubyte  len;
    ushort index;
    uint keycode;
    ubyte[32]  scancode;
}

struct input_mask
{
    uint type;
    uint codes_size;
    ulong codes_ptr;
}

enum EVIOCGVERSION       = _IOR!int('E', 0x01);
enum EVIOCGID            = _IOR!input_id('E', 0x02);
enum EVIOCGREP           = _IOR!(uint[2])('E', 0x03);
enum EVIOCSREP           = _IOW!(uint[2])('E', 0x03);

enum EVIOCGKEYCODE       = _IOR!(uint[2])('E', 0x04);
enum EVIOCGKEYCODE_V2    = _IOR!input_keymap_entry('E', 0x04);
enum EVIOCSKEYCODE       = _IOW!(uint[2])('E', 0x04);
enum EVIOCSKEYCODE_V2    = _IOW!input_keymap_entry('E', 0x04);

enum EVIOCGNAME(len)     = _IOC(_IOC_READ, 'E', 0x06, len);
enum EVIOCGPHYS(len)     = _IOC(_IOC_READ, 'E', 0x07, len);
enum EVIOCGUNIQ(len)     = _IOC(_IOC_READ, 'E', 0x08, len);
enum EVIOCGPROP(len)     = _IOC(_IOC_READ, 'E', 0x09, len);

enum EVIOCGMTSLOTS(len)  = _IOC(_IOC_READ, 'E', 0x0a, len);

enum EVIOCGKEY(len)      = _IOC(_IOC_READ, 'E', 0x18, len);
enum EVIOCGLED(len)      = _IOC(_IOC_READ, 'E', 0x19, len);
enum EVIOCGSND(len)      = _IOC(_IOC_READ, 'E', 0x1a, len);
enum EVIOCGSW(len)       = _IOC(_IOC_READ, 'E', 0x1b, len);

enum EVIOCGBIT(ev,len)   = _IOC(_IOC_READ, 'E', 0x20 + (ev), len);
enum EVIOCGABS(abs)      = _IOR!input_absinfo('E', 0x40 + (abs));
enum EVIOCSABS(abs)      = _IOW!input_absinfo('E', 0xc0 + (abs));

enum EVIOCSFF            = _IOW!ff_effect('E', 0x80);
enum EVIOCRMFF           = _IOW!int('E', 0x81);
enum EVIOCGEFFECTS       = _IOR!int('E', 0x84);

enum EVIOCGRAB           = _IOW!int('E', 0x90);
enum EVIOCREVOKE         = _IOW!int('E', 0x91);

enum EVIOCGMASK      = _IOR!input_mask('E', 0x92);

enum EVIOCSMASK      = _IOW!input_mask('E', 0x93);

enum EVIOCSCLOCKID   = _IOW!int('E', 0xa0);

enum ID_BUS          = 0;
enum ID_VENDOR       = 1;
enum ID_PRODUCT      = 2;
enum ID_VERSION      = 3;

enum BUS_PCI         = 0x01;
enum BUS_ISAPNP      = 0x02;
enum BUS_USB         = 0x03;
enum BUS_HIL         = 0x04;
enum BUS_BLUETOOTH   = 0x05;
enum BUS_VIRTUAL     = 0x06;

enum BUS_ISA         = 0x10;
enum BUS_I8042       = 0x11;
enum BUS_XTKBD       = 0x12;
enum BUS_RS232       = 0x13;
enum BUS_GAMEPORT    = 0x14;
enum BUS_PARPORT     = 0x15;
enum BUS_AMIGA       = 0x16;
enum BUS_ADB         = 0x17;
enum BUS_I2C         = 0x18;
enum BUS_HOST        = 0x19;
enum BUS_GSC         = 0x1A;
enum BUS_ATARI       = 0x1B;
enum BUS_SPI         = 0x1C;
enum BUS_RMI         = 0x1D;
enum BUS_CEC         = 0x1E;
enum BUS_INTEL_ISHTP = 0x1F;

enum MT_TOOL_FINGER  = 0;
enum MT_TOOL_PEN     = 1;
enum MT_TOOL_PALM    = 2;
enum MT_TOOL_MAX     = 2;

enum FF_STATUS_STOPPED   = 0x00;
enum FF_STATUS_PLAYING   = 0x01;
enum FF_STATUS_MAX       = 0x01;

struct ff_replay {
    ushort length;
    ushort delay;
};

struct ff_trigger {
    ushort button;
    ushort interval;
};

struct ff_envelope {
    ushort attack_length;
    ushort attack_level;
    ushort fade_length;
    ushort fade_level;
};

struct ff_constant_effect {
    short level;
    ff_envelope envelope;
};

struct ff_ramp_effect {
    short start_level;
    short end_level;
    ff_envelope envelope;
};

struct ff_condition_effect {
    ushort right_saturation;
    ushort left_saturation;

    short right_coeff;
    short left_coeff;

    ushort deadband;
    short center;
};

struct ff_periodic_effect {
    ushort waveform;
    ushort period;
    short magnitude;
    short offset;
    ushort phase;

    ff_envelope envelope;

    uint custom_len;
    short *custom_data;
};

struct ff_rumble_effect {
    ushort strong_magnitude;
    ushort weak_magnitude;
};

struct ff_effect {
    ushort type;
    short id;
    ushort direction;
    ff_trigger trigger;
    ff_replay replay;

    union U {
        ff_constant_effect constant;
        ff_ramp_effect ramp;
        ff_periodic_effect periodic;
        ff_condition_effect[2] condition;
        ff_rumble_effect rumble;
    }
    U u;
};

enum FF_RUMBLE   = 0x50;
enum FF_PERIODIC = 0x51;
enum FF_CONSTANT = 0x52;
enum FF_SPRING   = 0x53;
enum FF_FRICTION = 0x54;
enum FF_DAMPER   = 0x55;
enum FF_INERTIA  = 0x56;
enum FF_RAMP     = 0x57;

enum FF_EFFECT_MIN   = FF_RUMBLE;
enum FF_EFFECT_MAX   = FF_RAMP;

enum FF_SQUARE   = 0x58;
enum FF_TRIANGLE = 0x59;
enum FF_SINE     = 0x5a;
enum FF_SAW_UP   = 0x5b;
enum FF_SAW_DOWN = 0x5c;
enum FF_CUSTOM   = 0x5d;

enum FF_WAVEFORM_MIN = FF_SQUARE;
enum FF_WAVEFORM_MAX = FF_CUSTOM;

enum FF_GAIN     = 0x60;
enum FF_AUTOCENTER   = 0x61;

enum FF_MAX_EFFECTS  = FF_GAIN;

enum FF_MAX      = 0x7f;
enum FF_CNT      = (FF_MAX+1);
