# Caesar-Cipher

This is a simple clone of the bottom-up example of Caesar's Cipher shown in Graham Hutton's
exemplary book, "Programming in Haskell". It uses statistical analysis to encrypt an entered string
(lowercase letters are encrypted whilst all other characters are left untouched) by a random shift
factor in the range [0, 25], and then decrypts it without knowledge of the shift factor.


## Usage

Any `ASDF` compliant system will do. For instance, using `SLIME` with `SBCL` + `QuickLisp` on Emacs, a sample run would be as follows:

```
CL-USER> (asdf:load-asd #p"~/projects/caesar-cipher/caesar-cipher.asd")
T
CL-USER> (main)
Please enter a string: 
haskell is fun
Encrypted string = "ibtlfmm jt gvo", and the decrypted version is "haskell is fun"
NIL
CL-USER> (main)
Please enter a string: 
What is your name, my friend?
Encrypted string = "Wyrk zj pfli erdv, dp wizveu?", and the decrypted version is "What is your name, my friend?"
NIL
CL-USER> (main)
Please enter a string: 
The quick brown fox jumps over the lazy dog
Encrypted string = "Tax jnbvd ukhpg yhq cnfil hoxk max etsr whz", and the decrypted version is "The quick brown fox jumps over the lazy dog"
NIL
CL-USER>
```
The script can also be loaded into any standards-compliant Common Lisp distribution and run in
an interactive mode.


## Author

* Timmy Jose (zoltan.jose@gmail.com)


## Copyright

Copyright (c) 2017 Timmy Jose (zoltan.jose@gmail.com)
