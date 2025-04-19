#ifndef IMD_H
#define IMD_H
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

#include <stdint.h>
#include <stdio.h>

#define IMD_SIGNATURE                       0x20444D49

#define IMD_UNAVAILABLE         0x00
#define IMD_NORMAL              0x01
#define IMD_COMPRESSED          0x02
#define IMD_NORMAL_DEL          0x03
#define IMD_COMPRESSED_DEL      0x04
#define IMD_NORMAL_ERR          0x05
#define IMD_COMPRESSED_ERR      0x06
#define IMD_NORMAL_DEL_ERR      0x07
#define IMD_COMPRESSED_DEL_ERR  0x08
#define IMD_TYPE_NORMAL_MASK    0x01

#define MAX_HEADS                2
#define MAX_CYLINDERS_PER_DISK  80
#define MAX_SECTORS_PER_TRACK   32      // Should cover all 5,25 and 8 inches formats

#define MAX_SECTOR_SIZE         8192

typedef struct {
    uint8_t     type;
    uint32_t    index;  // Beginning of sector data in the image file
} imd_sector_t;

typedef struct  {
    uint8_t mode;
    uint8_t cylinder;
    uint8_t head;
    uint8_t sectors;
    uint8_t size;
} imd_data_t;

typedef struct {
    imd_data_t data;
    uint8_t sector_map[MAX_SECTORS_PER_TRACK];
    imd_sector_t sector_info[MAX_SECTORS_PER_TRACK];
} imd_t;

typedef struct {
    imd_t       imd;            // IMD track data
    uint32_t    track_index;    // Position of track in the image file
    uint32_t    data_index;     // Beginning of track data in the image file
} imd_track_t;

typedef struct {
    FILE *file;
    uint8_t heads;
    uint8_t cylinders;

    // I don't know if the track ordering in an IMD file is always the same, so I will
    // assume it isn't. Hence, we need a "track map" so we can easily and quickly jump
    // to any track in the image. Each entry marks the position (index) of the track
    // in the image file
    //
    uint32_t track_map[MAX_HEADS][MAX_CYLINDERS_PER_DISK];
    
    imd_track_t current_track;

} image_t;

int imd_parse_disk_img( image_t *image );
int imd_seek_track( image_t *image, uint8_t head, uint8_t cyl );
int imd_read_data( image_t *image, void *buf, int sect, int count );
int imd_get_sector_size( imd_track_t *track );


#endif