# lifoo
#### a fresh take on Forth, in the spirit of Lisp

### welcome
Welcome to Lifoo, a Forthy, Lispy language fused with Common Lisp. Lifoo is still very much under construction; besides [tests](https://github.com/codr4life/lifoo/blob/master/tests.lisp), inline documentation in the [implementation](https://github.com/codr4life/lifoo/blob/master/lifoo.lisp), and [built-in words](https://github.com/codr4life/lifoo/blob/master/init.lisp); the language is documented in a series of [blog](https://github.com/codr4life/vicsydev/blob/master/lispy_forth.md) [posts](https://github.com/codr4life/vicsydev/blob/master/consing_forth.md) [here](https://github.com/codr4life/vicsydev).

### tests
Lifoo comes with a modest but grow set of tests in ```tests.lisp```, evaluating ```(cl4l-test:run-suite '(:lifoo) :reps 3)``` repeats all tests 3 x 30 times.

```
(cl4l-test:run-suite '(:lifoo) :reps 3)
(lifoo abc)                   0.208
(lifoo array)                 0.176
(lifoo compare)               0.084
(lifoo env)                     0.1
(lifoo error)                 0.036
(lifoo flow)                  1.284
(lifoo io)                      0.0
(lifoo list)                  0.204
(lifoo log)                   0.024
(lifoo meta)                  0.108
(lifoo stack)                 0.256
(lifoo string)                0.132
(lifoo struct)                1.232
(lifoo thread)                0.308
(lifoo word)                  0.972
TOTAL                         5.124
NIL
```

### support
This project is running on a shoestring budget. I am completely fed up with the creative and collective compromises that come with playing the profit game. And despite hard times, I remain convinced that doing the right thing is the only way forward from here; information wants to be free and knowledge belongs everyone. Please consider [helping out](https://www.paypal.me/c4life) if you can, every contribution counts.

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
