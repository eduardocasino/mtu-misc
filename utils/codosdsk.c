/*
 * CODOS tools - Command line utilities for managing CODOS IMD image files
 *   hhttps://github.com/eduardocasino/mtu-misc
 *
 * codosdsk - CODOS Disk Image manipulation utility
 * 
 *  Copyright (C) 2025 Eduardo Casino
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
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <libgen.h>
#include <errno.h>

#include "imd.h"
#include "codosfs.h"

typedef int (*command_fn_t)( disk_t *disk, uint8_t *buffer, int argc, char **argv );

typedef struct {
    const char *command_string;
    command_fn_t command_fn;
} command_t;

static void usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s <image> <command>\n\nCommands:\n", myname);
    fputs( "    dir [<pattern>]\n", stderr );
    fputs( "    extract [<pattern>]\n", stderr );
}

static const command_t commands[] = {
    { "dir",        dir } ,
    { "extract",    extract },
    { NULL }
};

static command_fn_t parse_command( char *myname, int argc, char **argv )
{
    if ( argc > 1 )
    {
        if ( !strcmp( argv[1], "-h" ) || !strcmp( argv[1], "--help") )
        {
            usage( myname );
            return NULL;
        }

        for ( int c = 0; commands[c].command_string != NULL; ++c )
        {
            if ( !strcmp( argv[1], commands[c].command_string ) )
            {
                assert( commands[c].command_fn );
                
                return commands[c].command_fn;

            }
        }
    }

    // If it reaches here, no command match
    //
    fputs( "Error: Missing or unknown command.\n\n", stderr );

    usage( myname );
    
    return NULL;
}

int main( int argc, char **argv )
{
    disk_t disk = {0};
    uint8_t buffer[DS_BLOCK_SIZE];
    int ret;

    char *myname = basename( argv[0] );

    // Open image name

    if ( argc < 2 )
    {
        fputs( "Error: Image name not specified.\n", stderr );
        usage( myname );
        return -1;
    }

    if ( NULL == ( disk.image.file = fopen( argv[1], "rb") ) )
    {
        fprintf( stderr, "Error: Can't open '%s' image file: %s\n", argv[1], strerror(errno) );
        return -1;
    }
    ++argv; --argc;

    ret = codos_parse_disk_image( &disk );

    if ( !ret )
    {
        ret = -1;

        command_fn_t fn = parse_command( myname, argc, argv );

        if ( NULL != fn )
        {
            ret = fn( &disk, buffer, --argc, ++argv );
        }
    }

    fclose( disk.image.file );

    return ret;
}
