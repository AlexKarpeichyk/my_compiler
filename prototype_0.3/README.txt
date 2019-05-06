1) GHCi must be intalled and run to execute .hs files.
2) Launch GHCi, and naviagte to the folder prototype_0.3 using :cd command. (:cd dir1/dir2/prototype_0.3)
3) Load the compiler by runnning :l Compiler.hs (:l Compiler.hs)
4) To compile the source language, you need a program string. Example string is in the input folder, examples.txt.
5) Type 'compile' into the GHCi terminal followed by the desired name of the output file, followed by the string coppied from examples.txt or write your own. (compile "file" "def main() = {x := \"Hello world!\";}")
6) Run the command from 5).
7) If compilation was succesful, the output file should be in output folder with extension .ll (file.ll)
8) To run .ll files llvm must be installed.
9) If in terminal, type lli (outside GHCi) followed by the name of the output file with .ll extension (lli file.ll)
10) If run sucessfully, there will be some output (depending on the source program), or nothing.
11) Happy compiling!
