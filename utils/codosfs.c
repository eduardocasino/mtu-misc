/*
 * CODOS tools - Command line utilities for managing CODOS IMD image files
 *   hhttps://github.com/eduardocasino/mtu-misc
 *
 * CODOS File System functions
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <errno.h>
#include <fnmatch.h>
#include <getopt.h>
#include <libgen.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <ctype.h>
#include <sys/stat.h>

#include "imd.h"
#include "codosfs.h"
#include "hexdump.h"

// TODO: Support 2 sides!!!
//
static void get_block_sector( disk_t *disk, uint8_t block_num, uint8_t *track, uint8_t *sector )
{
    uint16_t trk  = ((block_num-1)*disk->sectors_block)/disk->sectors_track;
    uint16_t sect = ((block_num-1)*disk->sectors_block)%disk->sectors_track;

    if ( block_num > 39 )
    {
        sect += 18;
        trk  += sect / CODOS_SECTORS_PER_TRACK_SIDE;
    }

    *sector = (uint8_t)(sect % CODOS_SECTORS_PER_TRACK_SIDE);
    *track  = (uint8_t)trk;

    // printf( "Block: %d, Track: %d, Sector: %d\n", block_num, *track, *sector );
}


typedef enum { OP_READ, OP_WRITE } diskop_t;

static int read_write_block( diskop_t diskop, disk_t *disk, uint8_t *buffer, uint8_t block, uint8_t nsect )
{
    int ret = 0;
    uint8_t track, sector, head;

    assert( nsect <= disk->sectors_block );

    get_block_sector( disk, block, &track, &sector );
    
    while ( nsect )
    {
        uint8_t count = ( nsect > disk->sectors_track/disk->image.heads - sector ) ? disk->sectors_track/disk->image.heads - sector : nsect;
        head = ( sector < CODOS_SECTORS_PER_TRACK_SIDE ) ? 0 : 1;

        if ( imd_seek_track( &disk->image, head, track ) )
        {
            return -1;
        }

        int sector_size = imd_get_sector_size( &disk->image.current_track );

        if ( sector_size != CODOS_SECTOR_SIZE )
        {
            fprintf( stderr, "Error: Invalid CODOS IMD image: Bad sector size: %d", sector_size );
            return -1;
        }

        if ( diskop == OP_READ )
        {
            ret = imd_read_data( &disk->image, buffer, sector, count );
        }
        else
        {
            ret = imd_write_data( &disk->image, buffer, sector, count );
        }

        if ( ret )
        {
            return ret;
        }

        nsect -= count;

        if ( nsect )
        {
            sector += count;
            buffer += count * CODOS_SECTOR_SIZE;

            if ( sector >= disk->sectors_track )
            {
                sector = sector % disk->sectors_track;
                ++track;
            }
        }
    }

    return 0;
}

static inline int read_block( disk_t *disk, uint8_t *buffer, uint8_t block, uint8_t nsect )
{
    return read_write_block( OP_READ, disk, buffer, block, nsect );
}

static inline int write_block( disk_t *disk, uint8_t *buffer, uint8_t block, uint8_t nsect )
{
    return read_write_block( OP_WRITE, disk, buffer, block, nsect );
}

static int write_bat( disk_t *disk )
{
    // Track 12
    //   Sector 0: BAT
    //   Sectors 1-16: Directory entries
    //   Sector 17: BAT copy

    if ( ! imd_seek_track( &disk->image, 0, 12 ) )
    {
        int sector_size = imd_get_sector_size( &disk->image.current_track );

        if ( sector_size != CODOS_SECTOR_SIZE )
        {
            fprintf( stderr, "Error: Invalid CODOS IMD image: Bad sector size: %d", sector_size );
            return -1;
        }

        return ( 
                   imd_write_data( &disk->image, disk->active_bat, 0, 1 )
                   || imd_write_data( &disk->image, disk->dir_entry, 1, 16 )
                   || imd_write_data( &disk->image, disk->active_bat, 17, 1 )
            );
    }

    return -1;
}

static int load_bat( disk_t *disk )
{
    // Track 12
    //   Sector 0: BAT
    //   Sectors 1-16: Directory entries
    //   Sector 17: BAT copy

    if ( ! imd_seek_track( &disk->image, 0, 12 ) )
    {
        int sector_size = imd_get_sector_size( &disk->image.current_track );

        if ( sector_size != CODOS_SECTOR_SIZE )
        {
            fprintf( stderr, "Error: Invalid CODOS IMD image: Bad sector size: %d", sector_size );
            return -1;
        }

        return ( 
                   imd_read_data( &disk->image, &disk->bat, 0, 1 )
                || imd_read_data( &disk->image, disk->dir_entry, 1, 16 )
                || imd_read_data( &disk->image, &disk->bat2, 17, 1 )
            );
    }

    return -1;
}

static long get_free_space( disk_t *disk )
{
    long free_space = 0;

    for ( int i = 1; i < NUM_BLOCKS+1; ++i )
    {
        if ( !disk->active_bat->blocks[i] )
        {
            free_space += disk->sectors_block * CODOS_SECTOR_SIZE;
        }
    }

    return free_space;
}

static int extract_file( disk_t *disk, uint8_t *buffer, char *fname, dir_entry_t *dirent )
{
    FILE *dest_file = fopen( fname, "wb" );

    if ( NULL == dest_file )
    {
        fprintf( stderr, "Error: Can't open file '%s': %s\n", fname, strerror(errno) );
        return -1;
    }

    uint8_t block = dirent->block;
        
    if ( read_block( disk, buffer, block, disk->sectors_block ) )
    {
        fclose( dest_file );
        return -1;
    }

    file_header_t *header = (file_header_t *)buffer;

    int size = header->size2*65536+header->size1*256+header->size0 - sizeof( file_header_t );
    uint8_t *b = buffer + sizeof( file_header_t );
    int bsize = ( disk->sectors_block * CODOS_SECTOR_SIZE ) - sizeof( file_header_t );

    while ( size )
    {
        int towrite = ( size > bsize ) ? bsize : size;
            
        if ( towrite != fwrite( b, 1, towrite, dest_file ) )
        {
            fprintf( stderr, "Error writing to file '%s': %s\n", fname, strerror(errno) );
            fclose( dest_file );
            return -1;        
        }

        size -= towrite;

        block = disk->active_bat->blocks[block];
     
        if ( (size && (block == BLOCK_FREE || block > NUM_BLOCKS )) || (!size && block != BLOCK_LAST) )
        {
            fputs( "Error: corrupted or invalid IMD image.\n", stderr );
            fclose( dest_file );
            return -1;        }

        if ( block < BLOCK_LAST )
        {
            if ( read_block( disk, buffer, block, disk->sectors_block ) )
            {
                fclose( dest_file );
                return -1;
            }
            b = buffer;
            bsize = disk->sectors_block * CODOS_SECTOR_SIZE;
        }

    }

    if ( fclose( dest_file ) )
    {
        fprintf( stderr, "Error writing to file '%s': %s\n", fname, strerror(errno) );
        return -1;
    }

    return 0;
}

static uint8_t get_next_free_block( BAT_t *bat )
{
    uint8_t block;

    for ( block = bat->last_block+1; block <= NUM_BLOCKS; ++block )
    {
        if ( bat->blocks[block] == 0 )
        {
            break;
        }
    }

    if ( block > NUM_BLOCKS )
    {
        // Look for first deleted one
        for ( block = 1; block <= NUM_BLOCKS; ++block )
        {
            if ( bat->blocks[block] == 0 )
            {
                break;
            }
        }    
    }

    if ( block > NUM_BLOCKS )
    {
        fputs( "Disk full.\n", stderr );
        return 0;
    }

    // Marck block as last in the series

    bat->blocks[block] = BLOCK_LAST;

    return block;
}

static dir_entry_t *get_next_free_dirent( disk_t *disk )
{
    dir_entry_t *entry;
    int index;

    for ( index = 0; index < NUM_FILES; ++index )
    {
        entry = &disk->dir_entry[index];

        if ( entry->filename[0] == 0 )
        {
            break;
        }
    }

    if ( index == NUM_FILES )
    {
        fputs( "Directory full.\n", stderr );
        return NULL;
    } 
    else
    {
        return entry;
    }
}

static char *set_date( char *date_buf, size_t size, char *date )
{
    static const char *month[12] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC "};
    time_t now = time( NULL );
    struct tm *local = localtime( &now );

    if ( date )
    {
        snprintf( date_buf, size, "%s         ", date);
    }
    else
    {
        snprintf( date_buf, size, "%d-%s-%2.2d  ", local->tm_mday, month[local->tm_mon], local->tm_year % 100 );
    }
    date_buf[size-1] = '\0';
}

static char *uppercase( char *string )
{
    for ( int c = 0; string[c]; ++c )
    {
        string[c] = toupper( string[c] );
    }

    return string;
}

static char *lowercase( char *string )
{
    for ( int c = 0; string[c]; ++c )
    {
        string[c] = tolower( string[c] );
    }

    return string;
}

// FIXME: Check for file validity
//
static int copy_to_disk( disk_t *disk, uint8_t *buffer, char *filename, char *internal, char *date )
{
    FILE *file;
    struct stat fs;
    int ret = 0;

    uint8_t block = 0;
    dir_entry_t *dirent = NULL;
    size_t buflen = ( disk->image.heads == 2 ) ? DS_BLOCK_SIZE : SS_BLOCK_SIZE;
    
    if ( stat( filename, &fs ) )
    {
        fprintf( stderr, "Error getting '%s' size: %s\n", filename, strerror( errno ) );
        return -1;
    }

    if ( NULL == ( file = fopen( filename, "rb" ) ) )
    {
        fprintf( stderr, "Error opening file '%s': %s\n", filename, strerror( errno ) );
        return -1;
    }

    // Get first free block in BAT. If 0, then no space left
    //
    if ( 0 == ( block = get_next_free_block( disk->active_bat ) ) )
    {
        fclose( file );
        return -1;
    }

    // Get a free directory entry    

    if ( NULL == ( dirent = get_next_free_dirent( disk ) ) )
    {
        fclose( file );
        return -1;
    }

    // Set drirent data

    dirent->flag = 1;
    dirent->block = block;

    if ( NULL == internal )
    {
        internal = uppercase( basename( filename ) );
    }
    strncpy( dirent->filename, internal, sizeof( dirent->filename ) );

    file_header_t header;
    size_t written = 0;

    // Init buffer

    memset( buffer, 0, buflen );

    size_t filesiz = fs.st_size + sizeof( header );

    // Generate file header
    memset( &header, 0, sizeof( header ) );
    memcpy( &header.dir_entry, dirent, sizeof( dir_entry_t ) );
    header.attr = 0x80;
    header.dir_offset = 256 + ( block - 1 ) * 16 + 1;
    header.size0 = (uint8_t) filesiz & 0xff;
    header.size1 = (uint8_t) ( filesiz >> 8 ) & 0xff;
    header.size2 = (uint8_t) ( filesiz >> 16 ) & 0xff;

    set_date( header.date, sizeof( header.date ), date );
 
    // Write file header

    size_t nbytes = sizeof( header );
    uint8_t *b = buffer;

    memcpy( b, &header, nbytes );

    b += nbytes;
    
    size_t bread = 0;
    while (true)
    {
        while ( b < buffer + buflen )
        {
            bread = fread( b, 1, buflen - ( b - buffer), file );

            b += bread;
            nbytes += bread;

            if ( feof( file ) )
            {
                break;
            }

            if ( ferror( file ) )
            {
                fprintf( stderr, "Error reading from file '%s': %s\n",
                            filename, strerror( errno ) );
                ret = -1;
                break;
            }
        }

        if ( ret )
        {
            break;
        }

        if ( 0 != ( ret = write_block( disk, buffer, block, disk->sectors_block ) ) )
        {
            break;
        }

        if ( nbytes >= filesiz )
        {
            break;
        }

        uint8_t cblock = block;

        if ( 0 == ( block = get_next_free_block( disk->active_bat ) ) )
        {
            ret = -1;
            break;
        }
        
        disk->active_bat->blocks[cblock] = block;

        b = buffer;
    } 
 
    fclose( file );

    return ret;
}

static int write_system( disk_t *disk, uint8_t *buffer, char *codos, char *internal, char *date )
{
    // Check that block 1 is free

    if ( disk->active_bat->blocks[1] )
    {
        fputs( " Error: Block 1 is not free.\n", stderr );
        return -1;
    }

    return copy_to_disk( disk, buffer, codos, internal, date );

}

static int write_overlays( disk_t *disk, uint8_t *buffer, char *overlays )
{
    uint8_t track, sector;
    struct stat fs;
    FILE *overlaysf;

    int ret = 0;

    if ( stat( overlays, &fs ) )
    {
        fprintf( stderr, "Error getting '%s' size: %s\n", overlays, strerror( errno ) );
        return -1;
    }

    if ( fs.st_size != CODOS_OVERLAYS_SIZE )
    {
        fprintf( stderr, "Wrong overlay size: %ld. Should be %d.\n", fs.st_size, CODOS_OVERLAYS_SIZE );
        return -1;
    }

    if ( NULL == ( overlaysf = fopen( overlays, "rb" ) ) )
    {
        fprintf( stderr, "Error opening '%s': %s\n", overlays, strerror( errno ) );
        return -1;
    }

    if ( CODOS_OVERLAYS_SIZE != fread( buffer, 1, CODOS_OVERLAYS_SIZE, overlaysf ) )
    {
        if ( feof( overlaysf ) )
        {
            fputs( "EOF while reading overlays file\n", stderr );
        }
        else
        {
            fprintf( stderr, "Error while reading overlays file: %s\n", strerror(errno) );
        }
        
        ret = -1;

    }
    else
    {

        uint8_t last_block = CODOS_OVERLAYS_BLOCK + 3 - disk->image.heads;

        for ( uint8_t block = CODOS_OVERLAYS_BLOCK; block < last_block; ++block )
        {
            // TODO: SUpport 2 sides!!

            get_block_sector( disk, block, &track, &sector );
       
            if ( 0 != ( ret = imd_seek_track( &disk->image, 0, track ) ) )
            {
                break;
            }

            if ( 0 != ( ret = imd_write_data( &disk->image, buffer, sector, disk->sectors_block ) ) )
            {
                break;
            }

            disk->active_bat->blocks[block] = BLOCK_OVERLAY;

            buffer += disk->sectors_block * CODOS_SECTOR_SIZE;
        }
    }

    fclose( overlaysf );

    return ret;
}

static char *get_filename( dir_entry_t *dirent )
{
    static char filename[15];

    char *s = strchr( dirent->filename, '.' );
    s += 2;
    if ( s - dirent->filename < 14 )
    {
        *s = '\0';
    }
    strncpy( filename, dirent->filename, 14 );
    filename[14] = '\0';

    return filename;
}

static dir_entry_t **match_files( disk_t *disk, char *pattern, int *num )
{
    static dir_entry_t *matches[NUM_FILES+1];
    char filename[15];
    int filecount = 0;

    memset( matches, 0, sizeof( matches ) );

    for ( int f = 0; ; ++f )
    {
        dir_entry_t *dirent = &disk->dir_entry[f];

        if ( dirent->flag != 1 )
        {
            break;
        }

        // Check if file is deleted

        if ( ! dirent->filename[0] )
        {
            continue;
        }
   
        if ( !pattern || (pattern && !fnmatch( pattern, get_filename( dirent ), FNM_CASEFOLD )) )
        {
            matches[filecount] = dirent;

            ++filecount;
        }
    }

    if ( num )
    {
        *num = filecount;
    }

    return matches;
}

typedef enum { OP_DIR, OP_EXTRACT } op_t;

static int file_op( op_t op, disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv )
{
    int filecount = 0;
    bool bat2 = false, locase = false;

    int opt;

    static const struct option long_opts[] = {
        {"help",      no_argument, 0, 'h' },
        {"lowercase", no_argument, 0, 'l' },
        {"bat2",      no_argument, 0, '2' },
        {0,           0,           0,  0  }
    };

    optind = 0;

    while (( opt = getopt_long( argc, argv, "hl2", long_opts, NULL)) != -1 )
    {
        switch( opt )
        {
            case '2':
                ++bat2;
                break;

            case 'l':
                ++locase;
                break;

            case 'h':
            default:
                return 1;
        }
    }

    argc -= optind-1;
    argv += optind-1;

    char *fname = ( argc > 1 ) ? argv[1] : NULL;
    char *pattern = ( argc > 2 ) ? argv[2] : NULL;

    if ( NULL == fname )
    {
        return 1;
    }

    if ( NULL == ( disk->image.file = fopen( argv[1], "rb") ) )
    {
        fprintf( stderr, "Error: Can't open '%s' image file: %s\n", argv[1], strerror(errno) );
        return -1;
    }
    ++argv; --argc;

    if ( codos_parse_disk_image( disk ) )
    {
        fclose( disk->image.file );
        return -1;
    }

    if ( bat2 )
    {
        disk->active_bat = &disk->bat2;
    }

    dir_entry_t **matches = match_files( disk, pattern, &filecount );

    for ( int f = 0; f < filecount; ++f )
    {
        char *filename = locase ? lowercase( get_filename( matches[f]) ) : get_filename( matches[f]);

        if ( op == OP_DIR )
        {
            if ( 0 != read_block( disk, buffer, matches[f]->block, 1 ) )
            {
                filecount = -1;
                break;
            }
    
            file_header_t *header = (file_header_t *)buffer;
            char *date = locase ? lowercase( header->date ) : (char *)header->date;
    
            printf( "%-14.14s   %c   %9.9s   %8ld\n",
                            filename,
                            header->attr == 0x80 ? '-' : 'L',
                            date,
                            header->size2*65536+header->size1*256+header->size0 - sizeof( file_header_t ) );
        }
        else
        {
            if ( 0 != extract_file( disk, buffer, filename, matches[f] ) )
            {
                filecount = -1;
                break;
            }
        }
    }

    fclose( disk->image.file );

    return filecount;
}

int dir( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv )
{
    int nfiles = file_op( OP_DIR, disk, buffer, bufsiz, argc, argv );

    if ( nfiles < 0 )
    {
        return -1;
    }

    printf( "\nDisk volume number: $%2.2X%2.2X\n", disk->active_bat->volume_num_h, disk->active_bat->volume_num_l);
    printf( "%d File(s).\n", nfiles );
    printf( "%ld bytes free.\n", get_free_space(disk) );

    return 0;
}

int extract( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv )
{
    int ret = file_op( OP_EXTRACT, disk, buffer, bufsiz, argc, argv );

    if ( ret == 0 )
    {
        fputs( "File not found.\n", stderr );
    }

    if ( ret > -1 )
    {
        ret = 0;
    }

    return ret;
}

typedef enum { ORIG_IMAGE, ORIG_HOST } origin_t;

int copy( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv )
{
    char ret = -1;
    bool bat2 = false;
    char *date = NULL;

    int opt;

    static const struct option long_opts[] = {
        {"help", no_argument,       0, 'h' },
        {"bat2", no_argument,       0, '2' },
        {"date", required_argument, 0, 'd' },
        {0,      0,                 0,  0  }
    };

    optind = 0;

    while (( opt = getopt_long( argc, argv, "h2d:", long_opts, NULL)) != -1 )
    {
        switch( opt )
        {
            case '2':
                ++bat2;
                break;

            case 'd':
                date = optarg;
                break;

                case 'h':
            default:
                return 1;
        }
    }

    argc -= optind-1;
    argv += optind-1;

    if ( argc != 4 )
    {
        fputs( "Invalid argument count.\n", stderr );
        return 1;
    }

    char *image  = argv[1];
    char *first  = argv[2];
    char *second = argv[3];

    if ( NULL == ( disk->image.file = fopen( image, "rb+") ) )
    {
        fprintf( stderr, "Error: Can't open '%s' image file: %s\n", image, strerror(errno) );
        return -1;
    }

    if ( codos_parse_disk_image( disk ) )
    {
        fclose( disk->image.file );
        return -1;
    }

    if ( bat2 )
    {
        disk->active_bat = &disk->bat2;
    }

    origin_t origin;

    if ( strlen( first ) > 2 && !strncmp( "0:", first, 2 ) )
    {
        if ( strlen( second ) > 2 && !strncmp( "0:", second, 2 ) )
        {
            fputs( "Destination must be a host file.\n", stderr );
            fclose( disk->image.file );
            return -1;
        }
        origin = ORIG_IMAGE;
        first = &first[2];
    }
    else
    {
        if ( strlen( second ) < 3 || strncmp( "0:", second, 2 ) )
        {
            fputs( "Destination must be a CODOS file.\n", stderr );
            fclose( disk->image.file );
            return -1;
        }
        origin = ORIG_HOST;
        second = &second[2];
    }

    if ( origin == ORIG_IMAGE )
    {
        dir_entry_t **matches = match_files( disk, first, NULL );

        if ( *matches == NULL )
        {
            fprintf( stderr, "File 0:'%s' not found.\n", first );
        }
        else
        {
            ret = extract_file( disk, buffer, second, *matches );
        }
    }
    else
    {
        dir_entry_t **matches = match_files( disk, second, NULL );

        if ( *matches == NULL )
        {
            ret = copy_to_disk( disk, buffer, first, uppercase( second ), date );

            if ( ! ret )
            {
                ret = write_bat( disk );
            }
        }
        else
        {
            fprintf( stderr, "Error: File '0:%s' exists.\n", uppercase( second ) );
        }
    }

    if ( fclose( disk->image.file ) )
    {
        fprintf( stderr, "Error writing to image '%s': %s\n", image, strerror(errno) );
        return -1;
    }
    
    return ret;
}

static int delete_file( disk_t *disk, dir_entry_t *dirent )
{
    dirent->filename[0] = '\0';

    uint8_t block = dirent->block;

    do
    {
        uint8_t next = disk->active_bat->blocks[block];

        if ( !next || next > BLOCK_LAST )
        {
            fputs( "Corrupted BAT.\n", stderr );
            return -1;
        }

        disk->active_bat->blocks[block] = 0;
        block = next;

    } while (block < BLOCK_LAST );

    return 0;
}

int delete( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv )
{
    char ret = -1;
    bool bat2 = false;

    int opt;

    static const struct option long_opts[] = {
        {"help", no_argument, 0, 'h' },
        {"bat2", no_argument, 0, '2' },
        {0,      0,           0,  0  }
    };

    optind = 0;

    while (( opt = getopt_long( argc, argv, "h2", long_opts, NULL)) != -1 )
    {
        switch( opt )
        {
            case '2':
                ++bat2;
                break;

            case 'h':
            default:
                return 1;
        }
    }

    argc -= optind-1;
    argv += optind-1;

    if ( argc != 3 )
    {
        fputs( "Invalid argument count.\n", stderr );
        return 1;
    }

    char *image  = argv[1];
    char *fname  =  argv[2];

    if ( NULL == ( disk->image.file = fopen( image, "rb+") ) )
    {
        fprintf( stderr, "Error: Can't open '%s' image file: %s\n", image, strerror(errno) );
        return -1;
    }

    if ( codos_parse_disk_image( disk ) )
    {
        fclose( disk->image.file );
        return -1;
    }

    if ( bat2 )
    {
        disk->active_bat = &disk->bat2;
    }

    if ( strlen( fname ) > 2 && !strncmp( "0:", fname, 2 ) )
    {
        fname = &fname[2];
    }
    
    int num_matches;
    dir_entry_t **matches = match_files( disk, fname, &num_matches );

    if ( *matches == NULL )
    {
        fprintf( stderr, "File 0:'%s' not found.\n", fname );
    }
    else
    {
        fprintf( stderr, "Warning: About to delete %d file(s). Confirm? ", num_matches );

        int c = getchar();

        if ( 'y' != c && 'Y' != c)
        {
            fputs( "Aborted by user.\n", stderr );
        }
        else
        {
            for ( int entry = 0; matches[entry] != NULL; ++entry )
            {
                ret = delete_file( disk, matches[entry] );

                if ( ret )
                {
                    break;
                }
            }
        }
    }

    if ( ! ret )
    {
        ret = write_bat( disk );
    }

    if ( fclose( disk->image.file ) )
    {
        fprintf( stderr, "Error writing to image '%s': %s\n", image, strerror(errno) );
        return -1;
    }
    
    return ret;
}

int format( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv )
{
    FILE *file = NULL;
    bool pflag = false;
    int ret;
    uint16_t volid = 0;
    uint8_t interleave = 1, skew = 0;
    char *codos = NULL, *overlays = NULL, *date = NULL, *internal = NULL;

    int opt;
 
    static const struct option long_opts[] = {
        {"packed",     no_argument,       0, 'p' },
        {"date",       required_argument, 0, 'd' },
        {"interleave", required_argument, 0, 's' },
        {"skew",       required_argument, 0, 't' },
        {"volid",      required_argument, 0, 'v' },
        {"codos",      required_argument, 0, 'c' },
        {"overlays",   required_argument, 0, 'o' },
        {"name",       required_argument, 0, 'n' },
        {0,            0,                 0,  0  }
    };

    optind = 0;

    while (( opt = getopt_long( argc, argv, "pd:s:t:v:c:o:n:", long_opts, NULL)) != -1 )
    {
        switch( opt )
        {
            case 'p':
                pflag = true;
                break;

            case 's':
                interleave = (uint8_t) strtol( optarg, NULL, 0 );
                if ( interleave < 1 || interleave > 25 )
                {
                    fputs( "Interleave factor must be between 1 and 25\n", stderr );
                    return 1;
                }
                break;

            case 't':
                skew = (uint8_t) strtol( optarg, NULL, 0 );
                if ( skew > 25 )
                {
                    fputs( "Skew factor must be between 0 and 25\n", stderr );
                    return 1;
                }
                break;

            case 'v':
                volid = (uint16_t) strtol( optarg, NULL, 0 );
                break;
            
            case 'c':
                codos = optarg;
                break;

            case 'o':
                overlays = optarg;
                break;

            case 'd':
                date = optarg;
                break;

            case 'n':
                internal = uppercase( optarg );
                break;
            default:
                return 1;
        }
    }

    if ( !codos )
    {
        if ( internal )
        {
            fputs( "Option '--name|-n' is only valid when '--codos|-c' is specified.\n", stderr );
            return 1;
        }
        else if ( date )
        {
            fputs( "Option '--date|-d' is only valid when '--codos|-c' is specified.\n", stderr );
            return 1;
        }
    }

    argc -= optind-1;
    argv += optind-1;

    char *fname = ( argc > 1 ) ? argv[1] : NULL;

    if ( fname )
    {
        if ( ! access( fname, F_OK ) )
        {
            fprintf( stderr, "Warning: file %s exists. Overwrite? ", fname );
            int c = getchar();

            if ( 'y' != c && 'Y' != c)
            {
                fputs( "Aborted by user.\n", stderr );
                return -1;
            }

        }

        if ( NULL == ( file = fopen( fname, "wb+" ) ) )
        {
            fprintf( stderr, "Error: Can't open file '%s': %s\n", fname, strerror(errno) );
            return -1;
        }
    }
    else
    {
        fputs( "Missing image name.\n", stderr );
        return 1;
    }

    disk->image.file = file;
    disk->image.cylinders = 77;
    disk->image.heads = 1;

    ret = imd_new( &disk->image, pflag, 26, 1, 0x00, interleave, skew, buffer, bufsiz );

    if ( ! ret )
    {
        fseek( disk->image.file, 0, SEEK_SET );

        if ( 0 == ( ret = codos_parse_disk_image( disk ) ) )
        {
            if ( !ret && volid )
            {
                disk->active_bat->volume_num_l = volid & 0xff;
                disk->active_bat->volume_num_h = (volid >> 8) & 0xff;
            }

            if ( !ret && codos )
            {
                ret = write_system( disk, buffer, codos, internal, date );
            }

            if ( !ret && overlays )
            {
                ret = write_overlays( disk, buffer, overlays );
            }

            if ( !ret )
            {
                ret = write_bat( disk );
            }

        }
    }

    if ( fclose( file ) )
    {
        fprintf( stderr, "Error writing to file '%s': %s\n", fname, strerror(errno) );
        return -1;
    }
    
    return ret;
}

int overlays( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv )
{
    FILE *outfile = stdout;
    int base_addr = 0;

    int opt;

    static const struct option long_opts[] = {
        {"help", no_argument, 0, 'h' },
        {0,      0,           0,  0  }
    };

    optind = 0;

    while (( opt = getopt_long( argc, argv, "h", long_opts, NULL)) != -1 )
    {
        switch( opt )
        {
            case 'h':
            default:
                return 1;
        }
    }

    argc -= optind-1;
    argv += optind-1;

    if ( argc < 2 || argc > 3 )
    {
        fputs( "Invalid argument count.\n", stderr );
        return 1;
    }

    char *image  = argv[1];
    char *fname = ( argc == 3 ) ? argv[2] : NULL;
 
    if ( NULL == ( disk->image.file = fopen( image, "rb") ) )
    {
        fprintf( stderr, "Error: Can't open '%s' image file: %s\n", image, strerror(errno) );
        return -1;
    }

    if ( codos_parse_disk_image( disk ) )
    {
        fclose( disk->image.file );
        return -1;
    }

    if ( fname )
    {
        if ( NULL == ( outfile = fopen( fname, "wb" ) ) )
        {
            fprintf( stderr, "Error: Can't open file '%s': %s\n", fname, strerror(errno) );
            return -1;
        }
    }

    int size = disk->sectors_block * CODOS_SECTOR_SIZE;
    int nblocks = ( disk->image.heads == 1 ) ? 2 : 1;

    for ( int b = 0; b < nblocks; ++b )
    {
        if ( read_block( disk, buffer, CODOS_OVERLAYS_BLOCK + b, disk->sectors_block ) )
        {
            if ( outfile != stdout )
            {
                fclose( outfile );
            }
            return -1;
        }

        if ( outfile == stdout )
        {
            hexdump( outfile, buffer, size, base_addr );
            base_addr += size;
        }
        else
        {
            if ( size != fwrite( buffer, 1, size, outfile ) )
            {
                fprintf( stderr, "Error writing to file '%s': %s\n", fname, strerror(errno) );
                fclose( outfile );
                return -1;        
            }
        }

        buffer += size;
    }

    fclose( disk->image.file );

    if ( outfile != stdout )
    {
        if ( fclose( outfile ) )
        {
            fprintf( stderr, "Error writing to file '%s': %s\n", fname, strerror(errno) );
            return -1;
        }
    }

    return 0;
}

int codos_parse_disk_image( disk_t *disk )
{
    if ( imd_parse_disk_img( &disk->image ) )
    {
        return -1;
    }

    if ( disk->image.cylinders != CODOS_TRACKS )
    {
        fprintf( stderr, "Error: invalid CODOS IMD image. Bad cyls number: %d\n", disk->image.cylinders );
        return -1;
    }

    if ( disk->image.heads == 1 )
    {
        disk->sectors_block = SS_BLOCK_SIZE/CODOS_SECTOR_SIZE;
        disk->sectors_track = CODOS_SECTORS_PER_TRACK_SIDE;
    }
    else
    {
        disk->sectors_block = DS_BLOCK_SIZE/CODOS_SECTOR_SIZE;
        disk->sectors_track = CODOS_SECTORS_PER_TRACK_SIDE*2;
    }

    if ( load_bat( disk ) )
    {
        return -1;
    }

    disk->active_bat = &disk->bat;

    return 0;
}