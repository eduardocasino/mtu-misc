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
#include <getopt.h>
#include <libgen.h>
#include <errno.h>

#include "imd.h"
#include "codosfs.h"

typedef int (*command_fn_t)( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv );
typedef void (*help_fn_t)(char *myname);

typedef struct {
    const char *command_string;
    command_fn_t command_fn;
    help_fn_t help_fn;
} command_t;

static void usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s [--help] <command>\n\n", myname );
    fputs( "Options:\n", stderr );
    fputs( "    --help|-h       Print this help\n\n", stderr );

    fputs( "Commands:\n", stderr );
    fputs( "    format, dir, copy, delete, extract, overlays\n", stderr );

    fprintf( stderr, "\nType '%s <command> --help' for command details.\n\n", myname );
}

static void dir_usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s dir [--help] [--bat2] [--lowercase] <image> <pattern>\n", myname );

    fputs( "Arguments:\n", stderr );
    fputs( "    <image>          Name of the CODOS image file.\n", stderr );
    fputs( "    <pattern>        File pattern (see glob(7)).\n\n", stderr );

    fputs( "Options:\n", stderr );
    fputs( "    --help|-h        Print this help\n", stderr );
    fputs( "    --bat2|-2        Use second copy of the disk BAT\n\n", stderr );
    fputs( "    --lowercase|-l   Use lowercase names in the dir listing.\n", stderr );
}

static void extract_usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s extract [--help] [--bat2] [--lowercase] <image> <pattern>\n", myname );

    fputs( "Arguments:\n", stderr );
    fputs( "    <image>          Name of the CODOS image file.\n", stderr );
    fputs( "    <pattern>        File pattern (see glob(7)).\n\n", stderr );

    fputs( "Options:\n", stderr );
    fputs( "    --help|-h        Print this help\n", stderr );
    fputs( "    --bat2|-2        Use second copy of the disk BAT\n\n", stderr );
    fputs( "    --lowercase|-l   Generate lowercase names for the extracted files.\n", stderr );
}

static void format_usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s format [--help] [--packed] [--interleave <n>] [--skew <n>] [--volid <n>] \\\n", myname );
    fputs("                       [--codos <file>] [--name <file>] [--overlays <file>] <image>\n\n", stderr );

    fputs( "Arguments:\n", stderr );
    fputs( "    <image>          Name of the newly formated image file.\n\n", stderr );

    fputs( "Options:\n", stderr );
    fputs( "    --help|-h        Print this help\n", stderr );
    fputs( "    --packed|-p      Create a packed (compressed) IMD image\n", stderr );
    fputs( "    --interleave|-s  Sector interleave, from 1 to 25. Defaults to 0.\n", stderr );
    fputs( "    --skew|-t        Track skew, from 0 to 25. Defaults to 0.\n", stderr );
    fputs( "    --volid|-v       Disk volume number ('n' is a 16-bit integer).\n", stderr );
    fputs( "                     If omitted, defaults to $0000.\n", stderr );
    fputs( "    --codos|-c       CODOS kernel file.\n", stderr );
    fputs( "    --name|-n        Internal file name for CODOS kernel file. If not\n", stderr );
    fputs( "                     specified, defaults to --codos file name.\n", stderr );
    fputs( "    --date|-d        Use date string instead of current date.\n", stderr );
    fputs( "    --overlays|-o    CODOS overlays file.\n", stderr );
}

static void copy_usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s copy [--help] [--bat2] <image> [0:]<orig> [0:]<dest>\n", myname );

    fputs( "Arguments:\n", stderr );
    fputs( "    <image>          Name of the CODOS image file.\n", stderr );
    fputs( "    <orig>           Source file.\n", stderr );
    fputs( "    <dest>           Dest file.\n\n", stderr );
    fputs( "Note:                Either source or dest must be prepended by the ':0'\n", stderr );
    fputs( "                     prefix, meaning that it is a file within the image.\n\n", stderr );

    fputs( "Options:\n", stderr );
    fputs( "    --help|-h        Print this help\n", stderr );
    fputs( "    --bat2|-2        Use second copy of the disk BAT\n\n", stderr );
    fputs( "    --date|-d        Use date string instead of current date.\n", stderr );
}

static void delete_usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s delete [--help] [--bat2] <image> [0:]<file>\n", myname );

    fputs( "Arguments:\n", stderr );
    fputs( "    <image>          Name of the CODOS image file.\n", stderr );
    fputs( "    <file>           File to be deleted. The prefix ':0' is optional and\n", stderr );
    fputs( "                     has no effect, as the file is always within the image.\n\n", stderr );

    fputs( "Options:\n", stderr );
    fputs( "    --help|-h        Print this help\n", stderr );
    fputs( "    --bat2|-2        Use second copy of the disk BAT\n\n", stderr );
}

static void overlays_usage( char *myname )
{
    fprintf( stderr, "\nUsage: %s overlays [--help] <image> [<file>]\n", myname );

    fputs( "Arguments:\n", stderr );
    fputs( "    <image>          Name of the CODOS image file.\n", stderr );
    fputs( "    <file>           File to save the overlays. If omitted, a HEX dump\n", stderr );
    fputs( "                     is printed to stdout.\n\n", stderr );

    fputs( "Options:\n", stderr );
    fputs( "    --help|-h        Print this help\n", stderr );
}

static command_t commands[] = {
    { "dir",        dir,        dir_usage },
    { "extract",    extract,    extract_usage },
    { "format",     format,     format_usage },
    { "copy",       copy,       copy_usage },
    { "delete",     delete,     delete_usage },
    { "overlays",   overlays,   overlays_usage },
    { NULL }
};

static command_t *parse_command( char *myname, int argc, char **argv )
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
                
                return &commands[c];
            }
        }
    }

    // If it reaches here, no command match
    //
    fputs( "Error: Missing or unknown command.\n", stderr );

    usage( myname );
    
    return NULL;
}

int main( int argc, char **argv )
{
    disk_t disk = {0};
    static uint8_t buffer[DS_BLOCK_SIZE];
    int ret = -1;

    char *myname = basename( argv[0] );

    command_t *command = parse_command( myname, argc, argv );

    if ( NULL != command && NULL != command->command_fn )
    {
        ret = command->command_fn( &disk, buffer, DS_BLOCK_SIZE, --argc, ++argv );

        if ( ret > 0 )
        {
            if ( NULL != command->help_fn )
            {
                command->help_fn( myname );
            }
        }
    }
    return ret;
}
