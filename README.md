# FALSE

This is a FALSE interpreter written in Haskell. It possibly holds the record for the
largest FALSE compiler available (at the time of writing these lines, the unstripped ELF
has a size of 2.4 MB!).

The interpreter consists of a back-end and a front-end. The back-end is called FalseLib
and can be used in any Haskell program. It parses and interprets FALSE code.

One front-end is shipped as FalseExec.

## Execution

The FalseExec front-end is quite minimalistic, but works fine. Once started, it shows the following prompt:

    Haskell FALSE interpreter
    Waiting for code... (end with Ctrl-D)

After entering the code on stdin the user has to show the interpreter that he's finished; this is done by pressing [Ctrl-D]. 
This is not necessary if the code is read from a file (using input redirection: `$ ./FalseExec < script.f`)

As soon as the interpreter received the EOF signal, it starts processing the code. All of the I/O actions of the FALSE
script appear below this line:

    ---------- I/O ----------

If something goes wrong, an error message is displayed. For example, if sometimes during execution
a command wanted to pop an item although the stack was empty, the following error is thrown:

    # Script: 1 2 3 . . . .

    Runtime error :: Empty stack at 12!

The number (12) is the position where the error occurred. To make finding the error more easy,
the code is enumerated with the positions, starting at 0:

     0 `1 2 3´
     5 ` . . ´
    10 `. .´

The chunks are each five characters long. Position 12 is the very last character in the last chunk; we now
know that we tried to print the top-most stack item which failed because there wasn't any item on the
stack.

