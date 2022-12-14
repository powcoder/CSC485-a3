https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
bot sub [cat, sem].
    cat sub [v, det, s, has_gender] intro [sem:sem,person:per_type].
        has_gender sub [np, vp, n, pn] intro [gender:gen_type].
    gen_type sub [m, f, neu].
    per_type sub [first, second, third].
    sem sub [waiter, dog].

    np sub [nprp].
  list sub [e_list,ne_list].
  ne_list intro [hd:bot, tl:list].

she ---> (pn, gender:f,person:third).
he ---> (pn, gender:m,person:third).
i ---> (pn, person:first).
is ---> (v,person:third).
the ---> (det).
waiter ---> (n, gender:m, sem:waiter, person:third).
waitress ---> (n, gender:f, sem:waiter, person:third).
dog ---> (n, gender:neu, sem:dog, person:third).

det_n__np rule
    (np, gender:Gender, person:Person, sem:Sem) ===>
    cat> det,
    cat> (gender:Gender, person:Person, sem:Sem1, n).  % sem:(Sem, dog), n).

pn__np rule
    (np, gender:Gender,person:Person) ===>
    cat> (pn, gender:Gender,person:Person).

v_np__vp rule
    (vp, person:Person) ===>
    cat> (v, person:Person),
    cat> (np).

np_vp__s rule
    (s, person:Person) ===>
    cat> (np, gender:Gender, person:Person),
    cat> (vp, gender:Gender, person:Person).
