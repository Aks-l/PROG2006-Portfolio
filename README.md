# PROG2006 Portfolio

In the repository you are reading this README.md file, 

## Labs

### Easy tasks
- [lab3](./lab03/helloworld/) (Input/Output and head in Haskell)
- lab4 (Formatting output and frequency countinbg in Haskell)
- lab5 (Use of functions and recursiveness in Haskell)
- lab6 (Message decoding using monads and bind in Haskell)
- lab7 (Lotto winnings calculator using more functions and lambdas in Haskell)
- lab8 (Bird counting using iterators and other methods in Rust)

### Hard tasks
- lab9 (API)
- lab12 (BPROG Interpreter)
- lab14 (Game of Go)

### Additional tasks
- Lab1 (Rock Paper Scissors)
- Lab2 (API for creating, funding and solving "gitlab" issues)
- Lab10 (UNCOMPLETED. A very simple test of gui and keyboard inputs in Haskell)

## Overall difficulty feeling
In the start of the semester, i had never touched haskell or any purely functional programming language. This made the first labs require a pretty steep learning curve.
This made initial _easy_ tasks seem harder than the latter, because of the complete shift of thinking and thought process.

Because the easier taks felt more of use for my own learning, I want to focus most on the tasks i labeled as harder as they feel more definite of what I have learned across the semester. They are all done in haskell, but they explore very different themes regardless.

### Lab9 - API
The api works with web technologies through the use of `Network.Wai`, `Servant.Client` as well as `Aeson` and HTML5-Libraries. I believe i was the first in class to complete the task, I it was working on the issue tracker queue before the class shifted to using tests instead. After the change, it was always empty for the label `Task Check Submission` So i changed it to `Announcements`, and it still runs well.

To make it easy for other to use the API for themselves, the required Gitlab token is easy to edit in the Token.hs file. 

### Lab 12 - Interpreter
While the api works with web stuff, this task is about custom types and data structures in Haskell. It proved to me how important it is to have a good plan and idea before starting to code and not do everything _on the go_, as I had to redo much of this task when the poorly structured first attemt got too overwhelming.

After redoing it with a proper skeleton, it was much easier to add new features and everything required according to the task description. The program passes all tests, and is also easy to expand furter.

### Lab 14 - Game of Go
Opposed to web and types, this lab lets me work on writing functions in Haskell. It involves data structures for The game, The pieces and the coordinates, although not nearly as expansive as in lab12.

The lab includes functions for Flood-fill, Evaluating the board state, and reading and writing to files.

The first part of the task is to create a working game. Not only did i have to program a working Go-game for this task, but i also had to learn the game of go myself to understand all rulings and edge cases. For every move there are mainly 3 checks. It checks if the desired square is occupied, if it will cause itself to be removed (suicide) and if it causews a board that has already existed (simple/super KO). If none of these apply, if performs the move.

The second part was creating a bot to play against. The way i did this was to check every legal move (one in depth), then calculating a heuristic value base on the aomunt of captures it cause, the amount of liberties it creates and the amount of groups it results in. The best of these values is the move it ends up performing. Using this strategy, it _could_ expand to check two and maybe 3 in depth to become even better, but any more than that would become very inefficient very fast, as it needs to check at most (19*19)^(movesAhead*2-1) moves. Anyways, one in depth is at least playable as it doesnt miss obvious captures. 

The third part was the file parsing. For this i use regex to find all nessecary fields like size `SZ[..]`, komi `KM[..]` and moves `B[..]` and `W[..]`. Then i translate the moves into the same notatino as i use for manual inputs, then run all moves in sequence as if a game was being played. If any move along the parsing is illegal, it returns "Invalid SGF". Whenever a new game is started, it creates a new file beased on the game, and it is rapidly updated after every move. This ensures that every game is saved even in the case of a crash or power out.

Additionally, i added a playbackmode where the player can move back and forth to review the game so far. This works very efficiently, as all game states are saved (originally to check for KO).

These three tasks showed me that haskell is very versatile and is very good at working with large projects. It is often a bit annoying that every new library has several new operators that are often not intuitive, like `:<|>`, `=~` `:|` and even `.:`. However in the end it create very compact and elegant solutions _because_ of these operators.


### The other tasks
The first solidity task was very interesting, as i had not worked with blockchans before. The language itsef was pretty similar to other languages like java i have written before, but the main problem was the deploying. The fact that that the wallets had to be manually worked and in a live environment caused testing to be much slower than most other languages.

The second solidity task was a bit more challenging. I got all the important functions to work, including adding a payout, submitting an issue and claiming an issue, however i was not able to enforce a persistant change of values in transactions across the different files. I couldn't fix this before time went out, and since the hard deadline for the task passed, and there was not going to be any solidity on the exam, i was not as motivated to complete it.

Lab 10 in my project is not snake, but a small test of gui in Haskell. I managed to create a circle that can be moved with arrow keys, but i quickly realized that this is not what haskell is best at, and doing it in Rust/Iced was more apropriate. I decided not to do the snake task to spend my time on doing the higher level tasks isntead.
