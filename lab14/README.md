# gogame

## How to play

To start:

- Run the executable

You will be prompted with `Enter board size (1-19):`

- Enter desired board size.

You will be prompted `Play against the computer? [Y/n]`

- Enter n or N to play locally against another person sitting next to you or enything else to play against the computer.

If you play against the computer, you will be prompted `Play as color: [B]lack or [W]hite?`

- Enter B to play as black pieces (thus playing first) or W to play as white pieces. Any other input defaults to playing black.

The ascii-go board will appear, with the x and y axis labeled, and + signs representing an empty spot

- Enter any coordinates in the syntax `x y` to place a stone at that position, or enter `pass` to pass the turn

The turn is passed to the other player which repeats the process, or the bot who automatically plays, giving the turn right back.


## Features
#### The game is a datatype:
- the current board state 
- whose turn it is 
- a history of all board states
- passcount (0 on play, 1 on pass, 2 on two consecutive passes, ending the game)
- gamesize
- the name of the file storing the game metadata and move order
- if the game is against a bot, and what color the bot is if it is playing
- the amount of stones captured by each player

#### .sgf file support
- Run the program with a file to start from that position
- A file is created every new game
- File gets updated every move

## Everey REPL loop:
#### Print the board

#### A move is gotten from stdin
- Verifies it is within the board boundries
- Applies the move and checks if it is legal
    - Not already occupied
    - Does not result in suicide without capturing
    - Is not a repeated position
- Checks if it causes capture
    - Performs flood fill starting on the placed piece
    - Checks that all pieces in the group's neighbors are the opponents color, and no free spaces (libereties).
- Updates the game by adding the new game state to the history, changes color-to-move and updates the stones captured

#### Computer's turn
- Creates a set of all legal moves
- Tries all moves and decide which move results in:
    - Most liberties
    - Fewest separate groups
    - Most captures
- 80% chance of performing that move, 20% chance of random move.

## File Parsing
#### Import file
- Run `./gogame.exe file.sgf` or `stack run file.sgf` to start from the end of that file
- Uses RegEx to extract data
- File meta data and moves are validated, and returns `Invalid SGF File` if invalid.

#### Automatic file creation and updating
- If no file is provided, `currentunixtime.sgf` is created
- File includes one line of metadata, then all moves
- After every move, it is appended to the file

#### File contents
- FF[file format version (4)]
- CA[character set (UTF8)]
- GM[game id (1=go)]
- SZ[game size (1-19)]
- Moves noted as color[xy] where color is B or W and xy is the coordinates of the placed stone. `2 4` -> `ce` (zero-indexed)
Example file:
```
(;FF[4]CA[UTF-8]GM[1]SZ[9]
;B[dd]
;W[hh]
;B[ee]
;W[eb]
;B[ed]
;W[hf]
;B[fb]
;W[hd]
;B[db]
;W[hb]
;B[ea]
;W[gg]
;B[ge]
;W[gc]
;B[fh]
;W[ff]
;B[fd]
;W[eg]
;B[df]
;W[dh]
;B[hg]
;W[ig]
)
```
Should give the following game:
```
  8  +  +  +  +  +  +  +  +  +
  7  +  +  +  ●  +  ○  +  ●  +
  6  +  +  +  +  ●  +  ●  +  ●
  5  +  +  +  ○  +  ●  +  ●  +
  4  +  +  +  +  ○  +  ○  +  +
  3  +  +  +  ○  ○  ○  +  ●  +
  2  +  +  +  +  +  +  ●  +  +
  1  +  +  +  ○  ●  ○  +  ●  +
  0  +  +  +  +  ○  +  +  +  +
     0  1  2  3  4  5  6  7  8
Black to play>
```
