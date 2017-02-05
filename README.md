# lifoo
#### a fresh take on Forth, in the spirit of Lisp

### tests
Lifoo comes with a modest but grow set of tests in ```tests.lisp```, evaluating ```(cl4l-test:run-suite '(:lifoo) :warmup 10 :reps 100)``` runs all tests with 100 repetitions and 10 warmups.

```
CL-USER> (cl4l-test:run-suite '(:lifoo) :warmup 10 :reps 100)
(lifoo)                       0.028
(lifoo array)                  0.28
(lifoo compare)               0.256
(lifoo env)                   0.036
(lifoo error)                 0.012
(lifoo flow)                  0.312
(lifoo io)                    0.008
(lifoo list)                  0.036
(lifoo log)                   0.016
(lifoo meta)                   0.08
(lifoo stack)                 0.016
(lifoo string)                 0.16
(lifoo struct)                  1.2
(lifoo thread)                0.172
(lifoo word)                  0.076
TOTAL                         2.688
NIL
```

### support
This project is running on a shoestring budget. I am completely fed up with the creative and collective compromises that come with playing the profit game. And despite hard times, I remain convinced that doing the right thing is the only way forward from here; information wants to be free and knowledge belongs everyone. Please consider [helping out](https://www.paypal.me/c4life) if you can, every contribution counts.

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
