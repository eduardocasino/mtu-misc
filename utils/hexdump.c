/*
 * CODOS tools - Command line utilities for managing CODOS IMD image files
 *   hhttps://github.com/eduardocasino/mtu-misc
 *
 * Human readable hexadecimal dump
 * 
 *  Copyright (C) 2024 Eduardo Casino
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, Version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA  02110-1301, USA.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>

#define COLUMNS 16
#define HALF_COLUMNS ( COLUMNS / 2 )

int hexdump( FILE *file, const void* datap, size_t size, uint64_t base_addr )
{
    int status = 0;
    int rc;
	static char ascii[COLUMNS + 1];
    uint8_t *data = (uint8_t *) datap;
	
	for ( size_t i = 0; i < size; ++i )
    {
        if ( ! ( i % COLUMNS ) )
        {
            rc = fprintf( file, "%010lX: ", base_addr + (unsigned) i );
            if ( rc < 0 ) status = -1;
            memset( ascii, 0, sizeof( ascii) );
        }
        else if ( ! ( i % HALF_COLUMNS ) )
        {
            rc = fputs( " ", file );
            if ( rc < 0 ) status = -1;
        }

		rc = fprintf( file, "%02X ", ((uint8_t *)data)[i] );
        if ( rc < 0 ) status = -1;

		ascii[i % COLUMNS] = isprint( ((uint8_t *)data)[i] ) ? ((uint8_t *)data)[i] : '.';

		if ( (i+1) % COLUMNS == 0 || i+1 == size )
        {
            size_t j;
			for ( j = (i+1) % COLUMNS; j && j < COLUMNS; ++j )
            {
				rc = fputs( "   ", file );
                if ( rc < 0 ) status = -1;
			}
            if ( j && (i+1) % COLUMNS <= HALF_COLUMNS )
            {
				rc = fputs( " ", file );
                if ( rc < 0 ) status = -1;
		    }
		    rc = fprintf( file, " %s\n", ascii);
            if ( rc < 0 ) status = -1;
		}
	}
    if ( -1 == status )
    {
        fputs( "Error writing to output file\n", stderr );
    }

    return status;
}
