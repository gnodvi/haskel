================================================================================

                            Yet Another Haskell Tutorial
                                    Hal Daume III

================================================================================

5.4 A File Reading Program

We can write a simple program that allows a user to read and write files. The interface
is admittedly poor, and it does not catch all errors (try reading a non-existant file).
Nevertheless, it should give a fairly complete example of how to use IO. Enter the
following code into "FileRead.hs," and compile/run:

> module HT_FileRead
>     where

> import IO

> main_fileread = do
>   hSetBuffering stdin LineBuffering
>   doLoop

> doLoop = do
>   putStrLn "Enter a command rFN wFN or q to quit:"
>   command <- getLine
>   case command of
>     'q':_ -> return ()
>     'r':filename -> do putStrLn ("Reading " ++ filename)
>                        doRead filename
>                        doLoop
>     'w':filename -> do putStrLn ("Writing " ++ filename)
>                        doWrite filename
>                        doLoop
>     _ -> doLoop

> doRead filename =
>     bracket (openFile filename ReadMode) hClose
>             (\h -> do contents <- hGetContents h
>                       putStrLn "The first 100 chars:"
>                       putStrLn (take 100 contents))

> doWrite filename = do
>   putStrLn "Enter text to go into the file:"
>   contents <- getLine
>   bracket (openFile filename WriteMode) hClose
>           (\h -> hPutStrLn h contents)


What does this program do? First, it issues a short string of instructions and reads
a command. It then performs a case switch on the command and checks first to see if
the first character is a 'q.' If it is, it returns a value of unit type.

-------------------------------
NOTE The return function is a function that takes a value of type
a and returns an action of type IO a. Thus, the type of return () is
IO ().
-------------------------------

If the first character of the command wasn't a 'q,' the program checks to see if it
was an 'r' followed by some string that is bound to the variable filename. It then
tells you that it's reading the file, does the read and runs doLoop again. The check
for 'w' is nearly identical. Otherwise, it matches , the wildcard character, and loops
to doLoop.
  The doRead function uses the bracket function to make sure there are no problems
reading the file. It opens a file in ReadMode, reads its contents and prints the first
100 characters (the take function takes an integer n and a list and returns the first n
elements of the list).
  The doWrite function asks for some text, reads it from the keyboard, and then
writes it to the file specified.

-------------------------------
NOTE Both doRead and doWrite could have been made simpler
by using readFile and writeFile, but they were written in the extended
fashion to show how the more complex functions are used.
-------------------------------

  The only major problem with this program is that it will die if you try to read a file
that doesn't already exists or if you specify some bad filename like *n?# @. You may
think that the calls to bracket in doRead and doWrite should take care of this,
but they don't. They only catch exceptions within the main body, not within the startup
or shutdown functions (openFile and hClose, in these cases). We would need to
catch exceptions raised by openFile, in order to make this complete. We will do this
when we talk about exceptions in more detail in Section 10.1.

================================================================================
