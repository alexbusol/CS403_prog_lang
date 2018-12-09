Object subclass: #EpicFight.
EpicFight instanceVariableNames: 'k alexHP tomHP hps'.

EpicFight extend [
    setHps: alex and: tom [alexHP := alex. tomHP := tom]
    getHps [hps := Dictionary new. hps at: 'ALEX Hp' put: alexHP. hps at: 'TOM Hp' put: tomHP. ^hps.]
    
    attackTom: skill dealing: damage [
        Transcript display: 'Alex attacks Tom with ', skill,' dealing '; display: damage; display: ' amount of damage'; cr. tomHP := tomHP - damage. self getHps display. Transcript cr.]
        
    attackAlex: skill dealing: damage [
        Transcript display: 'Tom attacks Alex with ', skill,' dealing '; display: damage; display: ' amount of damage'; cr. alexHP := alexHP - damage. self getHps display. Transcript cr.]
        
    getAlexHP [^hps at: 'ALEX Hp']
    getTomHP [^hps at: 'TOM Hp']
            
]

Object subclass: #Skill.
Skill instanceVariableNames: 'skillname damage'.

Skill extend [
    setName: name damage: dmg [skillname:=name. damage:= dmg.]
    skDisplay [Transcript display: skillname, ' -> ', damage asString; cr.]
    
    getName [^skillname]
    getDamage [^damage]
]
    
obj := EpicFight new.
obj setHps: 10000 and: 100.
obj getHps display.
Transcript cr.

sk1 := Skill new setName: 'fireball' damage: 15.
sk2 := Skill new setName: 'lightning bolt' damage: 35.
sk3 := Skill new setName: 'knife strike' damage: 10.
sk4 := Skill new setName: 'simple punch' damage: 5.
sk5 := Skill new setName: 'one punch man''s punch' damage: 99999999.

skillArray := Array new: 5.
skillArray at: 1 put: sk1; at: 2 put: sk2; at: 3 put: sk3; at: 4 put: sk4; at: 5 put: sk5.


isFighting := true.
k := 0.

[isFighting == true] whileTrue: [
    
    skillSelect := (1 to: 5) atRandom.
   

    (k\\2 == 1) ifTrue: [
        obj attackTom: ((skillArray at: skillSelect) getName) dealing: ((skillArray at: skillSelect) getDamage)]
    ifFalse: [
        obj attackAlex: ((skillArray at: skillSelect) getName) dealing: ((skillArray at: skillSelect) getDamage)].
        
    ((obj getAlexHP) < 0) ifTrue: [isFighting := false. Transcript show: 'Alex is dead :('; cr.].
    ((obj getTomHP) < 0) ifTrue: [isFighting := false. Transcript show: 'Tom is dead :('; cr.].
    k := k+1.
  
  ]
    
    
    

