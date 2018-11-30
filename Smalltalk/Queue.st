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
                ifFalse: [back getNext: tnode. back := tnode]
    ]

    remove: key [
            front isNil ifTrue: [^nil].

            key := front getData.
            front := front getNext.
            ^key
    ]

    display [
        | tnode |
        tnode := front.
        [tnode notNil] whileTrue: [Transcript display: tnode getData; cr. tnode := tnode getNext]

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
        [p notNil] whileTrue: [aBlock value: tnode. tnode := tnode getNext]
    ]

    collect: block [
        | tqueue |
        tqueue := Queue new.
        self do: [:each | tqueue insert: (block value: each)]
        ^tqueue
    ]

]

q2 := Queue new.
q2 := insert: 20; insert: 40; insert: 10; insert: 1003; insert: 90.

q2 do: [:x | Transcript display: x; cr].

res1 := q collect: [:x | x*x].
res do: [:x | Transcript display: x; cr].
Transcript cr.

Queue extend [
    select: block [
        | tqueue |
        self do: [:each | (block value: each) ifTrue: tqueue insert: each]
        ^tqueue
    ]

    reject: block [
        | tqueue |
        self do: [:each | (block value: each) ifFalse: tqueue insert: each]
        ^tqueue
    ]

    inject: start into: block [
        | result |
        result := start.
        self do: [:each | result := block value: result value: each].
        ^result
    ]

    detect: block ifNone: defaultBlock [
        self do: [:each | (block value: each) ifTrue: [^each]].
        "else if no matches were found"
        ^defaultBlock value
    ]

]

sel := q2 select: [:x | x>15 and: [x<35]].
sel do: [:x | Transcript display: x; cr].
Transcript cr.

rej := q2 reject: [:x | x>15 and: [x<35]]/
rej do: [:x | Transcript display: x; cr].
Transcript cr.


Transcript display: (q2 inject: 0 into: [:a :b | a + b]); cr.
Transcript display: (q2 inject: 1 into: [:a :b | a * b]); cr.
Transcript cr.

Transcript display: (q2 detect: [:x | x< 15] ifNone: [Transcript display: 'none found']).


"================ PRIORITY QUEUE ================="

Queue subclass: #PriorityQueue.

PriorityQueue extend [
    insert: key [
        | tnode |
        (front isNil or: [key < front getData])
            ifTrue: [front := Node new setData: key; setNext: front. ^self].

        tnode := front.
        [tnode getNext isNil or: [key < (p getNext) getData]]
            whileFalse: [tnode := tnode getNext].
        tnode setNext: (Node new setData: key; setNext: tnode getNext)
    ]

    remove [
        | tnode key |
        front isNil ifTrue: [^nil].

        tnode := front.
        key := front getData.

        [tnode isNil] whileFalse: [
                (tnode getData < key) ifTrue: [key := tnode getData].
                tnode := tnode next
        ].

        (key == front getData) ifTrue: [front := front getNext. ^key].

        tnode := front.

        [key ~= (tnode getNext) getData] whileTrue: [tnode := tnode getNext].
        tnode setNext: (tnode getNext) getNext.
        ^key
    ]
    
]

pq := PriorityQueue new.
pq insert: 20; insert: 40; insert: 10; insert: 35.
Transcript display: pq; cr.
4 timesRepeat: [Transcript display: q remove; cr].
Transcript cr.

