#ifndef CODOSFS_H
#define CODOSFS_H
/*
 * CODOS tools - Command line utilities for managing CODOS IMD image files
 *   hhttps://github.com/eduardocasino/mtu-misc
 *
 * CODOS File System definitions
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
#include <stddef.h>
#include <assert.h>

#include "imd.h"

typedef enum {
    BLOCK_FREE = 0x00,
    BLOCK_LAST = 0xfc,
    BLOCK_OVERLAY = 0xfe,
    BLOCK_UNUSABLE = 0xff
} block_id_t;

#define CODOS_TRACKS 77
#define CODOS_SECTORS_PER_TRACK_SIDE 26
#define CODOS_SECTOR_SIZE 256

#define NUM_BLOCKS 247
#define NUM_FILES NUM_BLOCKS
#define SS_BLOCK_SIZE 2048
#define DS_BLOCK_SIZE 4096

#define CODOS_OVERLAYS_BLOCK    40
#define CODOS_OVERLAYS_SIZE     DS_BLOCK_SIZE

typedef struct {
    uint8_t blocks[NUM_BLOCKS+1];
    uint16_t unused2;
    uint8_t volume_num_l;
    uint8_t volume_num_h;
    uint8_t unused3;
    uint8_t num_files;
    uint8_t unused4;
    uint8_t last_block;
} BAT_t;

static_assert( sizeof( BAT_t ) == CODOS_SECTOR_SIZE );

typedef struct {
    uint8_t flag;       // 01 == present
    char filename[14];  // First char == 0x00 if deleted
    uint8_t block;      // Pointer to first allocated block
} dir_entry_t;

static_assert( sizeof( dir_entry_t ) == 16 );

typedef struct {
    image_t image;

    uint8_t sectors_block;
    uint8_t sectors_track;

    BAT_t bat;
    dir_entry_t dir_entry[NUM_FILES];
    BAT_t bat2;

    BAT_t *active_bat;

} disk_t;

#define ATTR_RW 0x80
#define ATTR_LK 0xA0

typedef struct {
    dir_entry_t dir_entry;
    uint8_t attr;
    uint8_t size0;
    uint8_t size1;
    uint8_t size2;
    uint16_t dir_offset;
    uint8_t date[10];
    uint8_t reserved[28];
    uint8_t finals;         // Final sector number for the load 
    uint8_t dmaload;        // DMA address code for loading 1st sector
    uint16_t entry;         // Address-1 of entry point into program
} file_header_t;

static_assert( sizeof( file_header_t ) == 64 );

#define BAT_TABLE_TRACK 0x12
#define BAT_TABLE_SECT1 0x00
#define DIRECTORY_SECT  0x01
#define BAT_TABLE_SECT2 0x11

int dir( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv );
int extract( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv );
int format( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv );
int copy( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv );
int delete( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv );
int overlays( disk_t *disk, uint8_t *buffer, size_t bufsiz, int argc, char **argv );
int codos_parse_disk_image( disk_t *disk );

#endif
