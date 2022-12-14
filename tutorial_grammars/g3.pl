https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
bot sub [cat, sem, list, agr, voice].
    cat sub [det, aux, has_sem].
        has_sem sub [nominal, verbal] intro [sem:sem, gap:gap_struct].
            nominal sub [n, np] intro [sem:n_sem].
            verbal sub [s, v, vp] intro [sem:v_sem, agr:agr, subcat:list].

    gap_struct sub [np, none].

    agr intro [voice:voice].

    sem sub [n_sem, v_sem].
        v_sem sub [kick] intro [agent:np, theme:np].
        n_sem sub [goalie, ball].

    voice sub [active, passive].

    list sub [e_list, ne_list].
        ne_list intro [hd:cat, tl:list].

empty (np, sem:Sem, gap:(np, sem:Sem, gap:none)).

the ---> det.
goalie ---> (n, sem:goalie).
ball ---> (n, sem:ball).

kicks ---> (v, sem:(kick, agent:Agent, theme:Theme), agr:voice:active, subcat:[Theme, Agent]).
kicked ---> (v, sem:(kick, agent:Agent, theme:Theme), subcat:[Theme, Agent]).

was ---> aux.

np rule
    (np, sem:Sem, gap:(none, None)) ===>
    cat> det,
    cat> (n, sem:Sem, gap:None).

vp rule
    (vp, sem:Sem, agr:Agr, subcat:Rest, gap:Gap) ===>
    cat> (v, sem:Sem, agr:Agr, subcat:[NP|Rest], gap:Gap),
    cat> (np, NP, gap:Gap).

aux rule
    (vp, sem:Sem, agr:Agr, subcat:SubCat, gap:Gap) ===>
    cat> aux,
    cat> (vp, sem:Sem, agr:(Agr, voice:passive), subcat:SubCat, gap:Gap).

s_active rule
    (s, sem:Sem, agr:Agr, subcat:(Rest, []), gap:None) ===>
    cat> (np, NP),
    cat> (vp, sem:Sem, agr:(voice:active, Agr), subcat:[NP|Rest], gap:(none, None)).

s_passive rule
    (s, sem:Sem, agr:Agr, gap:Gap) ===>
    cat> (np, Gap),
    cat> (vp, sem:Sem, agr:(voice:passive, Agr), gap:Gap).
