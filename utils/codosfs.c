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
#include <stdio.h>
#include <errno.h>
#include <fnmatch.h>
#include <string.h>

#include "imd.h"
#include "codosfs.h"

static void get_block_sector( disk_t *disk, uint8_t block_num, uint8_t *track, uint8_t *sector )
{
    uint16_t trk  = ((block_num-1)*disk->sectors_block)/disk->sectors_track;
    uint16_t sect = ((block_num-1)*disk->sectors_block)%disk->sectors_track;

    if ( block_num > 39 )
    {
        sect += 18;
        trk  += sect / 26;
    }

    *sector = (uint8_t)(sect % 26);
    *track  = (uint8_t)trk;

    // printf( "Block: %d, Track: %d, Sector: %d\n", block_num, *track, *sector );
}

static int read_block( disk_t *disk, uint8_t *buffer, uint8_t block, uint8_t nsect )
{
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

        if ( imd_read_data( &disk->image, buffer, sector, count ) )
        {
            return -1;
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
            return -1;        }

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

typedef enum { OP_DIR, OP_EXTRACT } op_t;

static int file_op( op_t op, disk_t *disk, uint8_t *buffer, int argc, char **argv )
{
    char filename[15];
    char *pattern = ( argc > 1 ) ? argv[1] : NULL;
    int filecount = 0;

    for ( int f = 0; ; ++f )
    {
        dir_entry_t dirent = disk->dir_entry[f];

        if ( dirent.flag != 1 )
        {
            break;
        }

        // Check if file is deleted

        if ( ! dirent.filename[0] )
        {
            continue;
        }

        char *s = strchr( dirent.filename, '.' );
        s += 2;
        if ( s - dirent.filename < 14 )
        {
            *s = '\0';
        }
        strncpy( filename, dirent.filename, 14 );
        filename[14] = '\0';
   
        if ( read_block( disk, buffer, dirent.block, 1 ) )
        {
            return -1;
        }

        file_header_t *header = (file_header_t *)buffer;

        if ( !pattern || (pattern && !fnmatch( pattern, filename, FNM_CASEFOLD )) )
        {
            ++filecount;

            if ( op == OP_DIR )
            {
                printf( "%-14.14s   %c   %9.9s   %8ld\n",
                                filename,
                                header->attr == 0x80 ? '-' : 'L',
                                header->date,
                                header->size2*65536+header->size1*256+header->size0 - sizeof( file_header_t ) );
            }
            else
            {
                if ( extract_file( disk, buffer, filename, &dirent ) )
                {
                    return -1;
                }
            }
        }
    }

    return filecount;
}

int dir( disk_t *disk, uint8_t *buffer, int argc, char **argv )
{
    int nfiles = file_op( OP_DIR, disk, buffer, argc, argv );

    if ( nfiles < 0 )
    {
        return -1;
    }

    printf( "\n%d File(s).\n", nfiles );
    printf( "%ld bytes free.\n", get_free_space(disk) );

    return 0;
}

int extract( disk_t *disk, uint8_t *buffer, int argc, char **argv )
{
    int ret = file_op( OP_EXTRACT, disk, buffer, argc, argv );

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
        disk->sectors_track = 26;
    }
    else
    {
        disk->sectors_block = DS_BLOCK_SIZE/CODOS_SECTOR_SIZE;
        disk->sectors_track = 52;
    }

    if ( load_bat( disk ) )
    {
        return -1;
    }

    return 0;
}