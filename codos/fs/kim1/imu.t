
IMAGE MANIPULATION UTILITY
==========================

USAGE:  IMU [{L | S}]
        IMU M <drive> <image> [R]
        IMU U <drive>
        IMU N <image> {1 | 2} [P]
        IMU D <image>
        IMU {C | R} <src image> <dst image>

IMU, without arguments, lists the mounted drives.

SWITCHES
--------

L      - Lists the files on the SD card.

S      - Saves the current mounts to the SD card config file.

M <drive> <image> [R]
        - Mounts <image> into <drive>. Switch 'R' makes it read-only.

U <drive>
        - Unmounts image from <drive>

N <image> {1 | 2} [/P]
        - Creates a formatted, empty image file on the SD card. 
          '1' creates a single-sided image, '2' a double-sided one.
        - Switch 'P' creates the image in a compressed format,
          which optimizes space usage, but may be slower.

D <image>
        - Deletes a file from the SD card.

C | R <src image> <dst image>
        - Copies / Renames a file on the SD card.
