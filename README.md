# lifoo
#### a fresh take on Forth, in the spirit of Lisp

### welcome
Welcome to Lifoo, a Forthy, Lispy language fused with Common Lisp. Lifoo is still very much under construction; besides [tests](https://github.com/codr4life/lifoo/blob/master/tests.lisp), inline documentation in the [implementation](https://github.com/codr4life/lifoo/blob/master/lifoo.lisp), and [built-in words](https://github.com/codr4life/lifoo/blob/master/init.lisp); the language is documented in a series of [blog](https://github.com/codr4life/vicsydev/blob/master/lispy_forth.md) [posts](https://github.com/codr4life/vicsydev/blob/master/consing_forth.md) [here](https://github.com/codr4life/vicsydev).

### tests
Lifoo comes with a modest but grow set of tests in ```tests.lisp```, evaluating ```(cl4l-test:run-suite '(:lifoo) :warmup 10 :reps 100)``` repeats all tests 100 times after 10 warm-up runs.

```
CL-USER> (cl4l-test:run-suite '(:lifoo) :warmup 10 :reps 100)
(lifoo abc)                    0.34
(lifoo array)                 0.396
(lifoo compare)               0.256
(lifoo env)                   0.028
(lifoo error)                 0.072
(lifoo flow)                  1.192
(lifoo io)                    0.004
(lifoo list)                  0.244
(lifoo log)                   0.008
(lifoo meta)                  0.084
(lifoo stack)                 0.016
(lifoo string)                0.248
(lifoo struct)                1.228
(lifoo thread)                0.176
(lifoo word)                  0.084
TOTAL                         4.376
NIL
```

### support
This project is running on a shoestring budget. I am completely fed up with the creative and collective compromises that come with playing the profit game. And despite hard times, I remain convinced that doing the right thing is the only way forward from here; information wants to be free and knowledge belongs everyone. Please consider [helping out](https://www.paypal.me/c4life) if you can, every contribution counts.

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
