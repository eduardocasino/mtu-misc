# MTU CODOS 2.0 Annotated Disassembly

This repository used to contain assorted stuff related to MTU products that I was working on. Over the time, it has evolved to something more specific, a fully annotated CODOS 2.0 Operating System disassembly.

A few components are necessary to at least boot into a prompt in an MTU-130 computer:

* CODOS.Z: The kernel (or nucleous, as MTU calls it)
* COMDPROC.Z: The Command Processor.
* STARTUP.J: A text file, similar to MD-DOS autoexec.bat, which loads other system components.
* IODRIVER.Z: The input/output driver.

Not necessary, but useful:

* GRAPHDRIVER.Z: The Graphics Display I/O Driver.
* SYSERRMSG.Z: A text file containing human readable error messages.
* SVCPROC.Z: The Supervisor Calls Processor, which provide a convenient way for programs to communicate with the nucleous.
* OVERLAYS: These are parts of the system (internal commands and SVCs) that reside in the system disk and are loaded when necessary, so memory usage is optimised.

There are also some system utilities which make the OS minimally useful:

* DIR.C: Displays file attributes for selected file(s), using "wildcard" name matching. 
* KILL.C: Deletes files on disk using "wildcard" name-matching.
* FORMAT.C: You guessed it, formats disks and also transfer system files.
* COPYF.C: Copies files to another disk with more than one drive.
* COPYF1DSK.C: Copies files to another disk with only one drive.
* SYSGENDISK.C: Generates a customised boot disk adapted to the system capabilities (like number of floppy drives)
* SYSGENDEVICE.C: Adds new Input-Output devices to CODOS.
* SYSGENPRINTR.C: Generates a machine language printer driver routine.
* CPUID.C: Prints the system identification numbers

And, of course, some programming interpreters/compilers/assemblers:

* BASIC.C
* ...

## Current status

* `CODOS.Z`, `COMDPROC.Z`, `SVCPROC.Z`, `OVERLAYS`, `STARTUP.J`, `SYSERRMSG.Z`, `DIR.C`, `KILL.C`, `FORMAT.C`, `COPYF.C`, `COPYF1DRIVE.C` and `CPUID.C` are completed.
* `IODRIVER.Z` is disassembled, partially annotated and builds from source, but it is far from complete. All the credit for `IODRIVER.Z` goes to [Michał Staręga](https://github.com/McGyverMauser98k/MTU-130-CODOS), who figured out the internal structure of the driver.
* `GRAPHDRIVER.Z` is disassembled and partially annotated.
* `BASIC.C` is dissassembled and matched to the msbasic sources at [https://github.com/mist64/msbasic]( https://github.com/mist64/msbasic), and some of the CODOS specific code has been annotated, though not much.
* `SYSGENDEVICE.C` and `SYSGENPRINTR.C` are not even started.

A backport for the KIM-1 is also working, using a simple TTY based IO driver, named `KIMIOTTY.Z`.

## Why?

Why not? Also, I had an even useless purpose: to reconstruct CODOS for the KIM-1, CODOS 2.0 predecessor, which seems to be lost forever.

## Build

Only unix-like systems with the GNU development tools are supported (Linux, including WSL, MacOS with ports, etc.)

For the 6502 code, I'm using the [CC65 suite](https://www.cc65.org/). The building process also needs Python with `re` and `argparse`.

It is possible to build images for two architectures: MTU-130/140 and KIM-1, but not both at the same time. Make sure to do a `make clean` before building for a different arch.

On the base directory, just type `make clean; make`. If `cc65` binaries are not installed in a directory that is not included in your `PATH`, you can pass the `CC65` environment variable to `make`:
```
$ make clean; make CC65=/cc65/binaries/
```
Note the trailing slash.

This will build images for the MTU-130. To build for the KIM-1, just type:
```
$ make clean; make ARCH=kim1
```

This will also build the `codosdsk` utility, which can create CODOS formatted disk images and transfer file to and rom the host. Evetually, there will be some documentation in the `utils` directory.

Three different images are generated: `codos14.imd`, `codos15.imd` and `codos17.imd`. They are basically the same, but each one contains binary identical files to revisions 14, 15 and 17 of CODOS 2.

## Test

For the MTU-130, you can use a recent version of [MAME](https://www.mamedev.org/) to run the images using the `MTU-130`target.

![image](https://raw.githubusercontent.com/eduardocasino/mtu-misc/main/img/screenshot.png)

I'm not aware of an emulator with the necessary hardware support for the KIM-1 with the K-1013 controller. For someone familiar with MAME, it should be pretty straightforward to add K-1013 emulation to the KIM-1, but I'm not that one.

Running on a KIM-1 clone with a K-1013 replica:

![image](https://raw.githubusercontent.com/eduardocasino/mtu-misc/main/img/screenshot-kim.png)

## Documentation

The best source of information is [Hans Otten's site](http://retro.hansotten.nl/). If it is not there, probably it does not exist. Look for the MTU pages, as links may change.
