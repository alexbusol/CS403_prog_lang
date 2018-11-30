Object subclass: #Node.
Node instanceVariableNames: 'data next'.

Node extend [
    getData [^data]
    getNext [^next]
    setData: dval [data := dval]
    setNext: nval [next := nval]
]

Object subclass: #Queue.
Queue instanceVariableNames: 'front back'.

Queue extend [
    insert: key [
            | tnode |
            tnode := Node new setData: key.
            front inNil
                ifTrue: [front := tnode. back := tnode]
                ifFalse: [back next: tnode. back := tnode]
    ]

    remove: key [
            front isNil ifTrue: [^nil].

            key := front data.
            front := front next.
            ^key
    ]

    display [
        | tnode |
        tnode := front.
        [tnode notNil] whileTrue: [Transcript display: tnode data; cr. tnode := tnode next]

    ]
]

q := Queue new.
q insert: 20; insert: 40; insert: 10; insert:30.
5 timesRepeat: [Transcript display: q remove; cr]. "removes 5 first objects from the queue"

Transcript cr.

Queue extend [
    do: aBlock [
        | tnode |
        tnode := front.
        [tnode notNil] whileTrue: [aBlock value: tnode. tnode := tnode next]
    ]

    collect: block [
        | tqueue |
        tqueue := Queue new.
        self do: [:each | tqueue insert: (block value: each)]
        ^tqueue
    ]

]

