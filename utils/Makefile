# CODOS tools - Command line utilities for managing CODOS IMD image files
#
#   https://github.com/eduardocasino/mtu-misc
#
#  Copyright (C) 2025 Eduardo Casino
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, Version 3.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA  02110-1301, USA.
#

CC = gcc
ALL = codosdsk
DSKOBJ = imd.o codosfs.o codosdsk.o hexdump.o

all: $(ALL)

codosdsk: $(DSKOBJ)
	$(CC) -o $@ $^

clean:
	rm -f $(TARGET) $(DSKOBJ)

imd.o: imd.h

codosfs.o: codosfs.h imd.h hexdump.h

codosdsk.o: codosfs.h imd.h


