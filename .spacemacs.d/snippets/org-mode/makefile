# -*- mode: snippet -*-
# name: makefile
# key: makefile
# --
* Makefile                                                            :noexport:
** Configurare toolchain
#+name: toolchain
#+begin_src make
  #=============================================================================#
  # toolchain configuration
  #=============================================================================#

  TOOLCHAIN = arm-none-eabi-

  CC = \$(TOOLCHAIN)gcc
  AS = \$(TOOLCHAIN)gcc -x assembler-with-cpp
  OBJCOPY = \$(TOOLCHAIN)objcopy
  OBJDUMP = \$(TOOLCHAIN)objdump
  SIZE = \$(TOOLCHAIN)size
  RM = rm

#+end_src
** Configurare specifica proiect
#+name: project
#+begin_src make
  #=============================================================================#
  # project configuration
  #=============================================================================#

  # project name
  PROJECT = ${1:proj}

  # core type
  CORE = arm926ej-s

  ROOT_DIR = src
  INC_DIR = inc

  # linker script
  LD_SCRIPT = \$(ROOT_DIR)/armulator-ram-hosted.ld

  # output folder (absolute or relative path, leave empty for in-tree compilation)
  OUT_DIR = target

  # source dirs
  TEST_DIR = \$(ROOT_DIR)/test

  # additional directories with source files (absolute or relative paths to
  # folders with source files, current folder is always included)
  # SRCS_DIRS = \$(ROOT_DIR) \$(BOARD_DIR) \$(EVMOMAP_DIR) \$(MAIN_DIR) \$(TEST_DIR)
  SRCS_DIRS = \$(ROOT_DIR) \$(TEST_DIR)

  # C definitions
  C_DEFS =

  # ASM definitions
  AS_DEFS =

  # include directories (absolute or relative paths to additional folders with
  # headers, current folder is always included)
  # INC_DIRS = \$(SRCS_DIRS) \$(INC_DIR)/Philips \$(INC_DIR)/CMSIS
  INC_DIRS = \$(INC_DIR) \$(INC_DIR)/test

  # library directories (absolute or relative paths to additional folders with
  # libraries)
  LIB_DIRS =

  # libraries (additional libraries for linking, e.g. "-lm -lsome_name" to link
  # math library libm.a and libsome_name.a)
  LIBS =

#+end_src
** Extensii si vpath
#+name: ext-vpath
#+begin_src make
  # extension of C files
  C_EXT = c

  # wildcard for C source files (all files with C_EXT extension found in current
  # folder and SRCS_DIRS folders will be compiled and linked)
  C_SRCS = \$(wildcard \$(patsubst %, %/*.\$(C_EXT), . \$(SRCS_DIRS)))

  # extension of ASM files
  AS_EXT = S

  # wildcard for ASM source files (all files with AS_EXT extension found in
  # current folder and SRCS_DIRS folders will be compiled and linked)
  AS_SRCS = \$(wildcard \$(patsubst %, %/*.\$(AS_EXT), . \$(SRCS_DIRS)))

  # optimization flags ("-O0" - no optimization, "-O1" - optimize, "-O2" -
  # optimize even more, "-Os" - optimize for size or "-O3" - optimize yet more) 
  OPTIMIZATION = -O0

  # set to 1 to optimize size by removing unused code and data during link phase
  REMOVE_UNUSED = 0

  # define warning options here
  # C_WARNINGS = -Wall -Wstrict-prototypes -Wextra
  C_WARNINGS = -Wall

  # C language standard ("c89" / "iso9899:1990", "iso9899:199409",
  # "c99" / "iso9899:1999", "gnu89" - default, "gnu99")
  C_STD = gnu89

  #=============================================================================#
  # set the VPATH according to SRCS_DIRS
  #=============================================================================#

  VPATH = \$(SRCS_DIRS)

  #=============================================================================#
  # when using output folder, append trailing slash to its name
  #=============================================================================#
  ifeq (\$(strip \$(OUT_DIR)), )
          OUT_DIR_F =
  else
          OUT_DIR_F = \$(strip \$(OUT_DIR))/
  endif
#+end_src

** Optiunile de compilare
#+end_src
#+name: comp-flags
#+begin_src make
  #=============================================================================#
  # various compilation flags
  #=============================================================================#

  # core flags
  CORE_FLAGS = -mcpu=\$(CORE)

  # flags for C compiler
  # C_FLAGS = -std=\$(C_STD) -g -ggdb3 -fverbose-asm -Wa,-ahlms=\$(OUT_DIR_F)\$(notdir \$(<:.\$(C_EXT)=.lst))
  #C_FLAGS = -nostdinc -isystem \$(ROOT_DIR)/inc/KEIL -D__EVAL -g  -Wa,-ahlms=\$(OUT_DIR_F)\$(notdir \$(<:.\$(C_EXT)=.lst))
  C_FLAGS = -std=\$(C_STD) -D__EVAL -g  -Wa,-ahlms=\$(OUT_DIR_F)\$(notdir \$(<:.\$(C_EXT)=.lst))

  # flags for assembler
  AS_FLAGS = -g -ggdb3 -Wa,-amhls=\$(OUT_DIR_F)\$(notdir \$(<:.\$(AS_EXT)=.lst))

  # flags for linker
  LD_FLAGS = -T\$(LD_SCRIPT) -g -Wl,-Map=\$(OUT_DIR_F)\$(PROJECT).map,--cref,--no-warn-mismatch

  # process option for removing unused code
  ifeq (\$(REMOVE_UNUSED), 1)
          LD_FLAGS += -Wl,--gc-sections
          OPTIMIZATION += -ffunction-sections -fdata-sections
  endif

#+end_src

** Formatare
#+name: format
#+begin_src make
  #=============================================================================#
  # do some formatting
  #=============================================================================#

  C_OBJS = \$(addprefix \$(OUT_DIR_F), \$(notdir \$(C_SRCS:.\$(C_EXT)=.o)))
  AS_OBJS = \$(addprefix \$(OUT_DIR_F), \$(notdir \$(AS_SRCS:.\$(AS_EXT)=.o)))
  OBJS = \$(AS_OBJS) \$(C_OBJS) \$(CXX_OBJS) \$(USER_OBJS)
  DEPS = \$(OBJS:.o=.d)
  INC_DIRS_F = -I. \$(patsubst %, -I%, \$(INC_DIRS))
  LIB_DIRS_F = \$(patsubst %, -L%, \$(LIB_DIRS))

  ELF = \$(OUT_DIR_F)\$(PROJECT).elf
  HEX = \$(OUT_DIR_F)\$(PROJECT).hex
  BIN = \$(OUT_DIR_F)\$(PROJECT).bin
  LSS = \$(OUT_DIR_F)\$(PROJECT).lss
  DMP = \$(OUT_DIR_F)\$(PROJECT).dmp

  # format final flags for tools, request dependancies for C++, C and asm
  C_FLAGS_F = \$(CORE_FLAGS) \$(OPTIMIZATION) \$(C_WARNINGS) \$(C_FLAGS) \$(C_DEFS) -MD -MP -MF \$(OUT_DIR_F)\$(@F:.o=.d) \$(INC_DIRS_F)
  AS_FLAGS_F = \$(CORE_FLAGS) \$(AS_FLAGS) \$(AS_DEFS) -MD -MP -MF \$(OUT_DIR_F)\$(@F:.o=.d) \$(INC_DIRS_F)
  LD_FLAGS_F = \$(CORE_FLAGS) \$(LD_FLAGS) \$(LIB_DIRS_F)

  #contents of output directory
  GENERATED = \$(wildcard \$(patsubst %, \$(OUT_DIR_F)*.%, bin d dmp elf hex lss lst map o))

#+end_src
** Taskurile principale
#+name: make-all
#+begin_src make
  #=============================================================================#
  # make all
  #=============================================================================#

  #all : make_output_dir \$(ELF) \$(LSS) \$(DMP) \$(HEX) \$(BIN) print_size
  #all : make_output_dir \$(OBJS)
  all : make_output_dir \$(ELF)

  # make object files dependent on Makefile
  \$(OBJS) : Makefile
  # make .elf file dependent on linker script
  \$(ELF) : \$(LD_SCRIPT)

  #-----------------------------------------------------------------------------#
  # linking - objects -> elf
  #-----------------------------------------------------------------------------#

  \$(ELF) : \$(OBJS)
	  @echo 'Linking target: \$(ELF)'
	  \$(CC) \$(LD_FLAGS_F) \$(OBJS) \$(LIBS) -o \$@
	  @echo ' '

  #-----------------------------------------------------------------------------#
  # compiling - C source -> objects
  #-----------------------------------------------------------------------------#

  \$(OUT_DIR_F)%.o : %.\$(C_EXT)
	  @echo 'Compiling file: \$<'
	  \$(CC) -c \$(C_FLAGS_F) \$< -o \$@
	  @echo ' '

  #-----------------------------------------------------------------------------#
  # assembling - ASM source -> objects
  #-----------------------------------------------------------------------------#

  \$(OUT_DIR_F)%.o : %.\$(AS_EXT)
	  @echo 'Assembling file: \$<'
	  \$(AS) -c \$(AS_FLAGS_F) \$< -o \$@
	  @echo ' '

  #-----------------------------------------------------------------------------#
  # memory images - elf -> hex, elf -> bin
  #-----------------------------------------------------------------------------#

  \$(HEX) : \$(ELF)
	  @echo 'Creating IHEX image: \$(HEX)'
	  \$(OBJCOPY) -O ihex \$< \$@
	  @echo ' '

  \$(BIN) : \$(ELF)
	  @echo 'Creating binary image: \$(BIN)'
	  \$(OBJCOPY) -O binary \$< \$@
	  @echo ' '

  #-----------------------------------------------------------------------------#
  # memory dump - elf -> dmp
  #-----------------------------------------------------------------------------#

  \$(DMP) : \$(ELF)
	  @echo 'Creating memory dump: \$(DMP)'
	  \$(OBJDUMP) -x --syms \$< > \$@
	  @echo ' '

  #-----------------------------------------------------------------------------#
  # extended listing - elf -> lss
  #-----------------------------------------------------------------------------#

  \$(LSS) : \$(ELF)
	  @echo 'Creating extended listing: \$(LSS)'
	  \$(OBJDUMP) -S \$< > \$@
	  @echo ' '


  tags :
	  rm -f app/TAGS
	  find . -name "*.[chCH]" -print | etags -o TAGS -

  #etags -R --fields="+afikKlmnsSzt"

  #-----------------------------------------------------------------------------#
  # print the size of the objects and the .elf file
  #-----------------------------------------------------------------------------#

  print_size :
	  @echo 'Size of modules:'
	  \$(SIZE) -B -t --common \$(OBJS) \$(USER_OBJS)
	  @echo ' '
	  @echo 'Size of target .elf file:'
	  \$(SIZE) -B \$(ELF)
	  @echo ' '

  #-----------------------------------------------------------------------------#
  # create the desired output directory
  #-----------------------------------------------------------------------------#

  make_output_dir :
	  \$(shell mkdir \$(OUT_DIR_F) 2>/dev/null)

#+end_src

** Blocul principal
#+name: makefile
#+begin_src make :tangle Makefile :noweb yes :padline no :comments no
  # -*- mode: makefile; indent-tabs-mode: t -*- 
  #=============================================================================#
  # ARM makefile
  #
  # author: Dan Pomohaci
  #
  # this makefile is based strongly on many examples found in the network
  #=============================================================================#

  <<toolchain>>
  <<project>>
  <<ext-vpath>>
  <<comp-flags>>
  <<format>>
  <<make-all>>

  #=============================================================================#
  # make clean
  #=============================================================================#

  clean:
  ifeq (\$(strip \$(OUT_DIR_F)), )
	  @echo 'Removing all generated output files'
  else
	  @echo 'Removing all generated output files from output directory: \$(OUT_DIR_F)'
  endif
  ifneq (\$(strip \$(GENERATED)), )
	  \$(RM) \$(GENERATED)
  else
	  @echo 'Nothing to remove...'
  endif

  #=============================================================================#
  # global exports
  #=============================================================================#

  .PHONY: all clean dependents tags

  .SECONDARY:

  # include dependancy files
  -include \$(DEPS)

  # end
#+end_src




$0
