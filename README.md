# lifoo
#### a fresh take on Forth, in the spirit of Lisp

### welcome
Welcome to Lifoo, a Forthy, Lispy language fused with Common Lisp. Lifoo is still very much under construction; besides [tests](https://github.com/codr4life/lifoo/blob/master/tests.lisp), inline documentation in the [implementation](https://github.com/codr4life/lifoo/blob/master/lifoo.lisp), and [built-in words](https://github.com/codr4life/lifoo/blob/master/init.lisp); the language is documented in a series of [blog](https://github.com/codr4life/vicsydev/blob/master/lispy_forth.md) [posts](https://github.com/codr4life/vicsydev/blob/master/consing_forth.md) [here](https://github.com/codr4life/vicsydev).

### repl
A basic REPL is provided for playing around with code in real time.

```
CL-USER> (lifoo:lifoo-repl)
Welcome to Lifoo,
press enter on empty line to evaluate,
exit ends session

Lifoo> "hello Lifoo!" print ln

hello Lifoo!
NIL

Lifoo> exit

NIL
CL-USER> 
```

### encryption
The ```crypt``` package is based on AES in CTR mode with SHA256 hashed keys, and requires identical seed and message sequence for ```encrypt``` and ```decrypt```.

```
Lifoo> :seed var crypt-seed set
       :key var "secret key" set
       :seed var :key var crypt "secret message" encrypt
       :seed var :key var crypt swap decrypt

"secret message"

Lifoo> 
```

### multi-threading
All Lifoo code runs in a ```lifoo-exec``` object, the result of accessing a ```lifoo-exec``` from multiple threads at the same time is undefined. The ```thread``` package allows spawning new threads as clones of the current exec. [Channels](http://vicsydev.blogspot.de/2017/01/channels-in-common-lisp.html) are used for communicating between threads.

```
Lifoo> 0 chan 
       (1 2 + send :done) 1 spawn swap 
       recv swap drop swap 
       wait cons

(:DONE . 3)
```



### tests
Lifoo comes with a modest but grow set of tests in ```tests.lisp```, evaluating ```(cl4l-test:run-suite '(:lifoo) :reps 3)``` repeats all tests 3 x 30 times.

```
(cl4l-test:run-suite '(:lifoo) :reps 3)
(lifoo abc)                   0.072
(lifoo array)                 0.032
(lifoo compare)               0.012
(lifoo env)                   0.012
(lifoo error)                 0.036
(lifoo flow)                  0.272
(lifoo io)                      0.0
(lifoo list)                  0.044
(lifoo log)                     0.0
(lifoo meta)                   0.08
(lifoo stack)                 0.008
(lifoo string)                0.024
(lifoo struct)                1.072
(lifoo thread)                0.084
(lifoo word)                  0.048
TOTAL                         1.796
NIL
```

### support
This project is running on a shoestring budget. I am completely fed up with the creative and collective compromises that come with playing the profit game. And despite hard times, I remain convinced that doing the right thing is the only way forward from here; information wants to be free and knowledge belongs everyone. Please consider [helping out](https://www.paypal.me/c4life) if you can, every contribution counts.

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
