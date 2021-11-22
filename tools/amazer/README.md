This file contains a 3d maze generator. For now it spits out json (see example-input.maze and example-output.json).

The input format is as follows: the maze is made of rooms and
corridors. A room contains atoms. An atom is a character, NOT A
STRING.  A room looks like [A T O M S] or (A T O M S). Whitespace does
not matter. The comment syntax is "//" at the beginning of a line.

A maze is described by a list of floors separated by "--". Each floor
is a sequence of rows of rooms, with corridors written as "-" or "|"
(horizontal or vertical).

Links on the "z" axis (inter-floor) exist when the LOWER room is
written using square brackets. Otherwise, there's no link.
