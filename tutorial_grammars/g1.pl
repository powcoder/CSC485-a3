https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
bot sub [s, lexical, phrase].
    phrase sub [np, vp].
    lexical sub [n, v, det].

dog ---> n.
cat ---> n.
the ---> det.
likes ---> v.
record ---> n.
record ---> v.


det_n__np rule
    np ===>
    cat> det,
    cat> n.

v_np__vp rule
    vp ===>
    cat> v,
    cat> np.

np_vp__s rule
    s ===>
    cat> np,
    cat> vp.