/*
 * CODOS tools - Command line utilities for managing CODOS IMD image files
 *   hhttps://github.com/eduardocasino/mtu-misc
 *
 * Support functions for IMD disk image files
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
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "imd.h"

// Sector sizes
//
int sizes[] = { 128, 256, 512, 1024, 2048, 4096, 8192 };

int imd_get_sector_size( imd_track_t *track )
{
    return sizes[track->imd.data.size];
}

static int imd_check_file_header( FILE *fil )
{
    uint32_t signature;
    uint8_t c;
    size_t res;

    if ( 1 != fread( &signature, sizeof(signature), 1, fil ) )
    {
        fputs( "Error reading image\n", stderr );
        return -1;
    }

    while ( 1 == ( res = fread( &c, 1, 1, fil ) ) )
    {
        if ( c == 0x1A )
        {
            break;
        }
    }

    if ( 1 != res )
    {
        fputs( "Error reading image: Bad image format\n", stderr );
        return -1;
    }

    if ( 0x1A != c || signature != IMD_SIGNATURE )
    {
        fputs( "Bad IMD header\n", stderr );
        return -1;
    }

    return 0;
}

int imd_parse_disk_img( image_t *image )
{
    size_t res;

    assert( image->file != NULL );

    // Make sure it is an IMD image file and find the 0x1A mark if returns 0,
    // it is valid an file index is positioned at the beginning of track info
    //
    if ( imd_check_file_header( image->file ) )
    {
        return -1;
    }

    int track;
    uint8_t lastmode = 0, maxhead = 0, maxcyl = 0;
    bool sect_cyl_map, sect_head_map;
    long ff;

    for ( track = 0; track < MAX_HEADS * MAX_CYLINDERS_PER_DISK; ++track )
    {
        long track_start = ftell( image->file );

		if ( 1 != fread( &image->current_track.imd, sizeof(imd_data_t), 1, image->file ) )
        {
            if ( feof( image->file ) )
            {
                // No more tracks
		        break;
            }
            else
            {
                fprintf( stderr, "Error reading file: %s\n", strerror( errno ) );
                return -1;
            }
	    }

        // printf( "image->current_track.imd.data.head: %d\n", image->current_track.imd.data.head );
        // printf( "image->current_track.imd.data.cylinder: %d\n", image->current_track.imd.data.cylinder );
        // printf( "image->current_track.imd.data.mode: %d\n", image->current_track.imd.data.mode);
        // printf( "image->current_track.imd.data.sectors: %d\n", image->current_track.imd.data.sectors );
        // printf( "image->current_track.imd.data.size: %d\n", image->current_track.imd.data.size );

        // printf( "track %d: %d sectors\n", track, image->current_track.imd.data.sectors );

		sect_cyl_map  = image->current_track.imd.data.head & 0x80;
	    sect_head_map = image->current_track.imd.data.head & 0x40;

        uint8_t head_no = image->current_track.imd.data.head & 0x3F;

	    if ( track != 0 && image->current_track.imd.data.mode != lastmode)
		{
		    fputs("Tracks with different modes not supported\n", stderr );
			return -1;
		}
		lastmode = image->current_track.imd.data.mode;

		if ( head_no > maxhead) maxhead = head_no;
		if ( image->current_track.imd.data.cylinder > maxcyl) maxcyl = image->current_track.imd.data.cylinder;

        image->track_map[head_no][image->current_track.imd.data.cylinder] = (uint32_t) track_start;

        // printf( "track_start: %8.8X\n", image->track_map[head_no][image->current_track.imd.data.cylinder] );

        // Skip sector numbering, cylinder and head maps if present
		ff = ftell( image->file ) + image->current_track.imd.data.sectors * ( 1 + sect_cyl_map ? 1 : 0 + sect_head_map ? 1 : 0 );

        if ( fseek( image->file, ff, SEEK_SET ) )
        {
            fprintf( stderr, "Bad IMD file (truncated?): fseek error: %s (%d)\n", strerror(errno), errno );
            return -1;
        }

        // Skip to next track
		for ( int nsect = 0; nsect < image->current_track.imd.data.sectors; ++nsect )
    	{
            uint8_t stype;

            if ( 1 != fread( &stype, 1, 1, image->file ) )
            {
                fputs( "Bad IMD file (truncated?): fread error\n", stderr );
			    return -1;
            }

            // printf( "  sector: %2.2X, type: %2.2X\n", nsect, stype );

    		if ( stype == IMD_UNAVAILABLE )
	    	{
			    fprintf( stderr, "Bad or corrupt file, unsupported sector type: 0x%2.2X\n", stype );
                return -1;
			}

            ff = ftell( image->file );

            if ( stype & IMD_TYPE_NORMAL_MASK )
            {
                // Advance sector size bytes
                ff += imd_get_sector_size( &image->current_track );
            }
            else
            {
                // Compressed sector, advance 1 byte
                ++ff;
            }

            if (fseek( image->file, ff, SEEK_SET ) )
            {
                fprintf( stderr, "Bad IMD file (truncated?): fseek error: %s (%d)\n", strerror(errno), errno );
                return -1;
            }
		}
    }

    image->cylinders = maxcyl + 1;
    image->heads = maxhead + 1;

    // printf("Cylinders: %d\n", image->cylinders );
    // printf("Heads: %d\n", image->heads );

    return 0;
}

int imd_seek_track( image_t *image, uint8_t head, uint8_t cyl )
{
    long idx;
    bool sect_cyl_map, sect_head_map;

    assert ( head < image->heads && cyl < image->cylinders && image->file != NULL );

    if ( head == image->current_track.imd.data.head && cyl == image->current_track.imd.data.cylinder )
    {
        // Already there
        return 0;
    }

    idx = image->track_map[head][cyl];

    // printf( "Track index: %8.8X\n", (uint32_t) idx );

    if ( fseek( image->file, idx, SEEK_SET ) )
    {
        fprintf( stderr, "fseek error: %s (%d)\n", strerror(errno), errno );
        return -1;
    }

    // Load track info

    image->current_track.track_index = (uint32_t) idx;

    if ( 1 != fread( &image->current_track.imd.data,
                                sizeof(imd_data_t),
                                1,
                                image->file ) )
    {
        fputs( "fread error\n", stderr );
        return -1;
    }

	sect_cyl_map  = image->current_track.imd.data.head & 0x80;
	sect_head_map = image->current_track.imd.data.head & 0x40;
    image->current_track.imd.data.head &= 0x3F;

    // Load sector map
    if ( 1 != fread( image->current_track.imd.sector_map,
                                image->current_track.imd.data.sectors,
                                1,
                                image->file ) )
    {
        fputs( "fread error\n", stderr );
        return -1;
    }

    // Load sector info (info is in track order, not sector order)
    // First, Skip sector cylinder and head maps if present

    idx = ftell( image->file ) + image->current_track.imd.data.sectors * (sect_cyl_map ? 1 : 0 + sect_head_map ? 1 : 0 );

    image->current_track.data_index = (uint32_t)idx;

    if ( fseek( image->file, idx, SEEK_SET ) )
    {
        fputs( "Bad IMD file (truncated?) SHOULD NOT OCCUR\n", stderr );
        return -1;
    }

    for ( int nsect = 0; nsect < image->current_track.imd.data.sectors; ++nsect )
    {
        image->current_track.imd.sector_info[nsect].index = (uint32_t)idx;

        if ( 1 != fread( &image->current_track.imd.sector_info[nsect].type, 1, 1, image->file ) )
        {
            fputs( "Bad IMD file (truncated?) SHOULD NOT OCCUR\n", stderr );
		  	return -1;
        }

        // Calculate and go to next sector index
	    idx = ftell( image->file );

        if ( image->current_track.imd.sector_info[nsect].type & IMD_TYPE_NORMAL_MASK )
        {
            idx += imd_get_sector_size( &image->current_track );
        }
        else
        {
            ++idx;
        }

        if ( fseek( image->file, idx, SEEK_SET ) )
        {
            fputs( "Bad IMD file (truncated?) SHOULD NOT OCCUR\n", stderr );
		  	return -1;
        }
	}

#if 0
    printf( "Head %d, Cylinder %d, Track Index %8.8X, Data index %8.8X\n",
        image->current_track.imd.data.head,
        image->current_track.imd.data.cylinder,
        image->current_track.track_index,
        image->current_track.data_index );

    for ( int nsect = 0; nsect < image->current_track.imd.data.sectors; ++nsect )
    {
         printf( "-> Sector %2.2d, type %2.2d, index %8.8X\n",
            image->current_track.imd.sector_map[nsect],
            image->current_track.imd.sector_info[nsect].type,
            image->current_track.imd.sector_info[nsect].index );
    }
#endif
    return 0;
}

static int imd_get_physical_sector( imd_track_t *track, uint8_t sect )
{
    for ( int nsect = 0; nsect < track->imd.data.sectors; ++nsect )
    {
        if ( sect == track->imd.sector_map[nsect] )
        {
            return nsect;
        }
    }

    return -1;
}

static int f_lseek_read( FILE *fp, long ofs, void *buff, size_t siz, size_t count )
{

    if ( fseek( fp, ofs, SEEK_SET ) )
    {
        fprintf( stderr, "fseek error: %s (%d)\n", strerror(errno), errno );
        return -1;
    }

    // Reads  data
    if ( 1 != fread( buff, siz, count, fp ) )
    {
        fputs( "fread error.\n", stderr );
    }

    return 0;
}

int imd_read_data( image_t *image, void *buf, int sect, int count )
{
    int s;
    uint8_t *buffer = (uint8_t *)buf;
    int sector_size = imd_get_sector_size( &image->current_track );

    // assert( sect + count < image->current_track.imd.data.sectors );

    for ( s= sect; s < sect + count; ++s )
    {
        // Get physical sector from interleave table

        uint8_t phys = imd_get_physical_sector( &image->current_track, s );

        // printf( "Reading from cyl %d, soft sector %d, physical sector %d\n",
        //                image->current_track.imd.data.cylinder, s, phys );

        if ( phys < 0 )
        {
            fprintf( stderr, "Sector %d not present in head %d, cyl %d\n",
                        sect,
                        image->current_track.imd.data.head,
                        image->current_track.imd.data.cylinder );
            return -1;
        }

        imd_sector_t *sector_info = &image->current_track.imd.sector_info[phys];

        if (   sector_info->type == IMD_NORMAL_DEL_ERR || sector_info->type == IMD_NORMAL_ERR
            || sector_info->type == IMD_COMPRESSED_DEL_ERR ||  sector_info->type == IMD_COMPRESSED_ERR )
        {
            fprintf( stderr, "Sector type is %d\n", sector_info->type );
            return -1;
        }

        // Read sector data
        //
        int rdsize = sector_info->type & IMD_TYPE_NORMAL_MASK ? sector_size : 1;

        if ( f_lseek_read( image->file, sector_info->index+1, buffer, rdsize, 1 ) )
        {
            return -1;
        }

        // printf( "Read %d bytes from software sector %d, physical sector %d\n",
        //                    sector_size, s, phys );

        if ( !(sector_info->type & IMD_TYPE_NORMAL_MASK) )
        {
            // Fill the buffer with the repeating byte
            for ( int i = 1; i < sector_size; ++i )
            {
                buffer[i] = buffer[0];
            }
        }

        buffer += sector_size;
    }

    return 0;
}
